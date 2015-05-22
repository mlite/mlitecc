include Common
module A  = Cmm_ast
module E  = Error
module F  = Fenv.Dirty
module FE = Fenv
module M = Metrics
  
type nm_or_mem = Cmm_ast.name_or_mem
type link = Reloc.t
    
let impossf fmt = Printf.kprintf Impossible.impossible fmt

let (<<) f g = fun x -> f (g x) (* function composition *)
    
let is2power x = x > 0 && x land (x - 1) = 0

let tywidth a_bad_thing = function
  | Types.Bits n -> n
  | Types.Bool   -> E.error (Printf.sprintf "A boolean may not be %s" a_bad_thing)

let emap f x = E.ematch x f

let elab_full_ty env =
  let catch r = E.catch (E.errorRegionPrt (F.srcmap env, r)) in
  let rec elab = function
    | A.TyAt (x,r)    -> catch r elab x
    | A.BitsTy size   -> E.Ok (Types.Bits size)
    | A.TypeSynonym n -> snd (F.findt n env) in
  catch Srcmap.null elab

let astwidth msg env = emap (tywidth msg) << elab_full_ty env
let elab_ty env = astwidth "expressed in abstract syntax" env
let no_kind_or_alignment k a f x = match k, a with
  | None, None     -> f x
  | Some _, None   -> E.error "kind permissible only on parameter or result"
  | None, Some _   -> E.error "alignment permissible only on parameter or result"
  | Some _ ,Some _ -> 
      E.error "kind and alignment permissible only on parameter or result"
	
let alignment n =
  if is2power n then n else E.errorf "alignment %d is not a power of 2" n
    
let assertion = function
  | None -> Rtl.none
  | Some n -> Rtl.aligned (alignment n)
      
let loc_region r l = match l with A.NameOrMemAt(_, r) -> r | _ -> r
  
let exp_region r l = match l with A.ExprAt     (_, r) -> r | _ -> r

let aligned metrics =
  let wordsize = metrics.M.wordsize in
  let wordmult = Cell.divides (Cell.of_size wordsize) in
  let aligned w = function
    | Some a -> alignment a
    | None   -> if wordmult w then w / wordsize else 1 
  in
  aligned
    
let exprfuns env =
  let metrics     = F.metrics env in
  let mcell       = Cell.of_size metrics.M.memsize in
  let mspace      = ('m', metrics.M.byteorder, mcell) in
  let eprint r    = E.errorRegionPrt (F.srcmap env, r) in
  let errorf fmt  = Printf.kprintf E.error fmt in
  let catch r f x = E.catch (eprint r) f x in
  let aligned     = aligned metrics in
  let lvalue_name x = 
    E.ematch (snd (F.findv x env))
      (fun (den,t) ->
	let w = tywidth "assigned to" t  
	in
	match den with
	  | FE.Variable {FE.loc=l} -> l, w
	  | _ -> errorf "may not assign to %s %s" (FE.denotation's_category den) x) 
  in
  let rvalue_name x =
    E.seq (snd (F.findv x env))
      (fun (denot, t) ->
	let w = tywidth "named variable" t in
	let rval = match denot with
	  | FE.Constant bits        -> E.Ok (Rtl.bits bits w)
	  | FE.Label (FE.Proc s)    -> E.Ok (Rtl.codesym s w)
	  | FE.Label (FE.Code s)    -> E.Ok (Rtl.codesym s w)
	  | FE.Label (FE.Data s)    -> E.Ok (Rtl.datasym s w)
	  | FE.Import(_,s)          -> E.Ok (Rtl.impsym  s w)
	  | FE.Continuation c       -> (c.FE.escapes <- true; E.Ok (Block.base c.FE.base))
	  | FE.Label(FE.Stack addr) -> E.Ok addr  (* a fetch would be wrong *)
	  | FE.Variable _           -> emap (fun (l,w) -> Rtl.fetch l w) (lvalue_name x) in
	E.Raise.left (rval, t)) 
  in
  let rec kinded_name lhs = match lhs with
    | A.NameOrMemAt(lhs,r) -> catch r kinded_name lhs
    | A.Name(kind,x,a)     -> E.ematch (lvalue_name x) (fun (loc, w) ->
        (Auxfuns.Option.get "" kind, (loc, w), aligned w a))
    | A.Mem _              -> E.error "only a name may appear to left of a call" 
  in
  let rec lvalue lhs = match lhs with
    | A.NameOrMemAt(lhs,r)    -> catch r lvalue lhs
    | A.Name(k,x,a) -> no_kind_or_alignment k a lvalue_name x
    | A.Mem(t,addr,aligned,aliasing) ->
        let w = astwidth "value in memory" env t in
        E.ematch2 w (typed_expr addr) (fun w (addre, addrt) ->
          if tywidth "address" addrt <> metrics.M.pointersize then
            E.error "address's type is not the machine's pointer size"
          else if not (Cell.divides mcell w) then
            E.error "reference's type is not a multiple of target memsize"
          else
            Rtl.mem (assertion aligned) mspace (Cell.to_count mcell w) addre, w)
	  
  and rvalue_name_or_mem nm = match nm with
    | A.NameOrMemAt(v, r) -> catch r rvalue_name_or_mem v
    | A.Name(k, x, a)     -> no_kind_or_alignment k a rvalue_name x
    | A.Mem(_, _, _, _)   ->
        E.ematch (lvalue nm) (fun (loc, w) -> Rtl.fetch loc w, Types.Bits w)
	  
  and typed_expr exp = 
    let apply tx op args =
      E.ematch (E.Raise.list (List.map typed_expr args))
	(fun args ->
          let arges, argtys = List.split args in
          let resty,  opr   = tx op argtys    in
          Rtl.app opr arges, resty) in
    let literal cvt v width = Rtl.bits (cvt v width) width, Types.Bits width in
    let literal cvt v = emap (literal cvt v) in
    let signed, unsigned, float =
      let does_not_overflow f v w =
        try (ignore (f v w); true) with Bits.Overflow -> false in
      let cvt signed what cvt v width =
        try cvt v width
        with Bits.Overflow ->
          if signed && not (v.[0] =<= '-') && does_not_overflow Bits.U.of_string v width
          then
            E.errorf "literal %s does not fit in %d signed bits, but it will\n\
                      fit in %d unsigned bits---perhaps you want '%sU'?"
              v width width v
          else
            E.errorf "%s overflow in literal %s" what v in
      cvt true  "signed-integer"   Bits.S.of_string,
      cvt false "unsigned-integer" Bits.U.of_string,
      cvt false "floating-point"   Bits.U.of_string (* surprising but true *)
    in  
    let const default_width = function
      | Some ty -> astwidth "literal constant" env ty
      | None    -> E.Ok default_width 
    in
    match exp with
      | A.ExprAt(x,r)     -> catch r typed_expr x
      | A.Sint(str,t)     -> literal signed        str (const metrics.M.wordsize t) 
      | A.Uint(str,t)     -> literal unsigned      str (const metrics.M.wordsize t) 
      | A.Float(str,t)    -> literal float         str (const metrics.M.wordsize t) 
      | A.Char(int,t)     -> literal Bits.U.of_int int (const 8                  t) 
      | A.Fetch(v)        -> rvalue_name_or_mem v
      | A.BinOp (l,op,r)  -> apply Rtlop.Translate.binary op [l;r]
      | A.UnOp  (op,e)    -> apply Rtlop.Translate.unary  op [e]
      | A.PrimOp(op,args) -> let args = List.map (fun (k, x, a) -> x) args in
        apply Rtlop.Translate.prefix op args 
  in
  let constant simp e = E.ematch (typed_expr e) (fun (k, kt) ->
    match kt with
      | Types.Bits w -> simp k, w
      | _ -> E.error "constant expression may not be a Boolean") 
  in
  let caught r f x = catch (r x) f x in
  let l, e = loc_region Srcmap.null, exp_region Srcmap.null 
  in
  caught e typed_expr, caught l lvalue, caught l kinded_name,
  caught e (constant Simplify.bits), caught e (constant Simplify.link)
    
let elab_exp         env = let f, _, _, _, _ = exprfuns env in f
let elab_loc         env = let _, f, _, _, _ = exprfuns env in f
let elab_kinded_name env = let _, _, f, _, _ = exprfuns env in f
let elab_con         env = let _, _, _, f, _ = exprfuns env in f
let elab_link        env = let _, _, _, _, f = exprfuns env in f
