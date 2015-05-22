type exp = Rtl.exp
type loc = Rtl.loc * Rtl.width
type rtl = Rtl.rtl

type name = string
type kind = string
type convention = string
type aligned    = int
type actual = kind * exp * Rtl.width * aligned
type 'a kinded = kind * 'a * aligned
type 'a flow  = 
    { cuts : 'a list
    ; unwinds : 'a list
    ; areturns : 'a list
    ; returns : bool
    ; aborts : bool 
    }

type 'a cflow = 
    { ccuts : 'a list
    ; caborts : bool 
    }
      
type 'a alias = 
    { reads : 'a
    ; writes : 'a 
    }
      
type range = Bits.bits * Bits.bits   (* lo `leu` x `leu` hi, as in manual *)
type procname = string
type label    = string
type linktime = Reloc.t
    
type stmt =
  | If         of exp * stmt list * stmt list
  | Switch     of range option * exp * (range list * stmt list) list
  | Label      of label
  | Cont       of name * convention * Fenv.variable kinded list
  | Span       of (Bits.bits * linktime) * stmt list
  | Assign     of rtl
  | Call       of loc kinded list * convention * exp * actual list * procname list 
      * name flow * name list option alias
  | Call'      of convention * exp * actual list * procname list
                    (* the dog ate the annotations *)
  | Goto       of exp * label list
  | Jump       of convention * exp * actual list * procname list
  | Cut        of convention * exp * actual list * name cflow 
  | Return     of convention * int * int * actual list
  | Limitcheck of convention * exp * limitfailure option
and limitfailure = { failcont : exp; reccont : exp; recname : name; }
    
module A  = Cmm_ast
module E  = Error
module F  = Fenv.Dirty
module FE = Fenv
module N  = Nast
module M = Metrics
  
let impossf fmt = Printf.kprintf Impossible.impossible fmt
  
let is2power x = x > 0 && x land (x - 1) = 0


let elab_functions validate srcmap r env =
  let eprint r    = E.errorRegionPrt (F.srcmap env, r) in
  let errorf r    = Printf.kprintf (fun s -> eprint r s; E.Error) in
  let findve r n  = E.catch (eprint r) (fun env -> snd (F.findv n env)) in
  let aligned     = Elabexp.aligned (F.metrics env) in
  let aligned r w = E.catch (eprint r) (fun a -> E.Ok (aligned w a)) in
  let exp = Elabexp.elab_exp env in
  let con = Elabexp.elab_con env in
  let loc = Elabexp.elab_loc env in
  let metrics = F.metrics env in
  let cformal (rgn, kind, name, a) =
    E.seq (findve rgn name env) (fun den ->
      match den with
	| FE.Variable v, Types.Bool -> impossf "Boolean variable"
	| FE.Variable v, Types.Bits w when F.is_localv name env ->
            E.ematch (aligned rgn w a) (fun a -> (kind, v, a))
	| FE.Variable _, _ ->
            errorf r "continuation parameter '%s' may not be a global variable" name
	| d, _ ->
            errorf r "continuation parameter '%s' is a %s, not a local register variable"
              name (FE.denotation's_category d)) 
  in
  let cformals formals = E.Raise.list (List.map cformal formals) in
  let rec full_stmt r reachable s =
    let rec exp_as_name = function
      | A.ExprAt(x,_) -> exp_as_name x
      | A.Fetch(A.NameOrMemAt(x,_)) -> exp_as_name (A.Fetch x)
      | A.Fetch(A.Name(_,x,_)) -> Some x
      | _ -> None in
    let ispointer =
      function Types.Bits n when n = metrics.M.pointersize -> true | _ -> false 
    in
    let insist_pointer what t =
      if ispointer t then ()
      else E.error (what ^ " must be a bit vector of the native pointer type") 
    in
    let vrtl r rtl =
      match validate rtl with
	| None -> rtl
	| Some msg ->
            (Printf.eprintf "%s: Warning: %s\n" (Srcmap.Str.region r) msg; rtl) 
    in
    let range = function
      | A.Range (l, h) ->
          let l = con l in let h = con h in (* order matters *)
          E.ematch2 l h (fun (l, lw) (h, hw) ->
            if lw = hw then (l, h), lw
            else E.errorf "constants in range %s..%s have different types bits%d and bits%d"
              (Bits.to_string l) (Bits.to_string h) lw hw)
      | A.Point e -> E.ematch (con e) (fun (n, w) -> (n, n), w) 
    in
    let arm (rgs, ss) =
      let rgs = E.Raise.list (List.map range rgs) in (* order matters *)
      E.ematch2 rgs (stmts r true ss) (fun rgs ss ->
        match rgs with
          | [] -> E.error "case arm has an empty range list"
          | (rg, w) :: rgs ->
              let strip w (rg, w') = 
		if w = w' then rg 
		else E.errorf "ranges in case arm have different types bits%d and bits%d" w w' 
	      in
              ((rg :: List.map (strip w) rgs), ss), w) 
    in
    let striprange w (rg, w') =
      if w = w' then rg 
      else E.errorf "switch statement range has type bits%d but scrutinee has type bits%d" w' w 
    in
    let striparms w =
      let strip (arm, w') = if w = w' then arm 
      else E.errorf "case arm uses range of type bits%d but scrutinee has type bits%d" w' w 
      in
      List.map strip 
    in
    let effect lhs (guard,rhs) =
      let guard = Auxfuns.Option.map exp guard 
      in
      E.seq2 (loc lhs) (exp rhs) (fun (loc, w) (e, ty) ->
        if Pervasives.(<>) ty (Types.Bits w) then
          errorf (Elabexp.loc_region r lhs)
            "location of type bits%d cannot hold value of type %s" w (Types.to_string ty)
        else
          let rtl = Rtl.store loc e w in
          match guard with
            | None   -> E.Ok rtl
            | Some g -> E.seq g (fun (g, ty) ->
		match ty with Types.Bool -> E.Ok (Rtl.guard g rtl)
		  | _ -> errorf r "guard must have type bool, not %s" (Types.to_string ty))) 
    in
    let limitcheck cconv cookie cont =
      E.seq (exp cookie) (fun (cookie, cookiety) ->
        insist_pointer "limit cookie" cookiety;
        match cont with
          | None -> Error.Ok (Limitcheck (cconv, cookie, None))
          | Some (failk, recname) ->
              let reck = A.Fetch (A.Name (None, recname, None)) in
              E.ematch2 (exp failk) (exp reck) (fun (failk, fty) (reck, rty) ->
		insist_pointer "overflow continuation" fty;
		insist_pointer "automatically generated recovery continuation" rty;
		let f = { failcont = failk; reccont = reck; recname = recname; } in
		Limitcheck (cconv, cookie, Some f))) 
    in
    let goto_target r x = E.seq (findve r x env) (fun (d, t) ->
      match d with 
	| FE.Label (FE.Code _) when F.is_localv x env -> E.Ok x
	| FE.Label (FE.Code _) -> errorf r "may not goto %s (label in another procedure)" x
	| _ -> errorf r "may not goto %s %s" (FE.denotation's_category d) x) 
    in
    let exp_is_local_code_label r e = match exp_as_name e with
      | None -> false
      | Some x -> match findve r x env with
	  | E.Ok (FE.Label (FE.Code _), _) when F.is_localv x env -> true
	  | E.Ok _ -> false
	  | E.Error -> impossf "good target name came back as error" 
    in
    let actual r (kind, e, a) = E.seq (exp e) (fun (e, ty) ->
      match ty with
	| Types.Bits w -> E.ematch (aligned r w a) (fun a -> (kind, e, w, a))
	| Types.Bool   -> errorf r "an actual parameter may not be a Boolean") 
    in
    let reject_uncallable r e = match exp_as_name e with
      | None -> ()
      | Some x -> match findve r x env with
	  | E.Ok ((FE.Label (FE.Proc _) | FE.Import (_, _) | FE.Variable _), _) | E.Error -> ()
	  | E.Ok (d, _) -> 
              E.errorf "cannot call or jump to %s %s" (FE.denotation's_category	d) x 
    in
    let elab_alias r alias =
      let none = function None -> true | Some _ -> false in
      let rec elab r reads writes = function
        | [] -> E.Ok { reads = reads; writes = writes }
        | A.AliasAt(a,r) :: a's -> E.catch (eprint r) (elab r reads writes) (a :: a's)
        | A.Reads  ns :: a's when none reads  -> elab r (Some ns) writes a's
        | A.Writes ns :: a's when none writes -> elab r reads (Some ns) a's
        | A.Reads  _ :: _ -> errorf r "multiple 'reads' annotations on one call"
        | A.Writes _ :: _ -> errorf r "multiple 'writes' annotations on one call" in
      E.catch (eprint r) (elab r None None) alias 
    in
    let flow r conv default_aborts default_returns =
      let as_cut_to  k = k.FE.cut_to <- true in
      let as_unwinds k = k.FE.unwound_to <- true in
      let as_returns k =
        if not (List.mem conv k.FE.returned_to) then
          k.FE.returned_to <- conv :: k.FE.returned_to in
      let rec ks r as_what prev' ns =
        let continuation n = E.seq (findve r n env) (function
          | FE.Continuation k, _ -> as_what k; E.Ok n
          | _ -> errorf r "%s is not a continuation" n) in
        match ns with
          | [] -> prev'
          | n :: ns -> ks r as_what (continuation n :: prev') ns in
      let rec flow r c' u' r' aborts returns = function
        | [] -> finish_flow c' u' r' aborts returns
        | A.FlowAt(f,r)  :: fs -> flow r c' u' r' aborts returns (f :: fs)
        | A.CutsTo    ns :: fs -> flow r (ks r as_cut_to c' ns) u' r' aborts returns fs
        | A.UnwindsTo ns :: fs -> flow r c' (ks r as_unwinds u' ns) r' aborts returns fs
        | A.ReturnsTo ns :: fs -> flow r c' u' (ks r as_returns r' ns) aborts returns fs
        | A.Aborts       :: fs -> flow r c' u' r' true returns fs
	    (*| A.NeverAborts  :: fs -> flow r c' u' r' false returns fs *)
        | A.NeverReturns :: fs -> flow r c' u' r' aborts false fs 
      and finish_flow c' u' r' aborts returns =
        let list ns = E.Raise.list (List.rev ns) in
        let c = list c' in let u = list u' in let r = list r' in (* order matters *)
        E.ematch3 c u r (fun c u r ->
          { cuts = c; unwinds = u; areturns = r; aborts = aborts; returns = returns }) in
      flow r [] [] [] default_aborts default_returns 
    in
    let call_flow r cc fs = flow r cc false true fs in
    let null = function [] -> true | _ :: _ -> false in
    let cut_flow r cc fs =
      E.seq (flow r cc true false fs) (fun f ->
        if not (null f.unwinds) then
          errorf r "'also unwinds to' is meaningless on a 'cut to'"
        else if not (null f.areturns) then
          errorf r "'also returns to' is meaningless on a 'cut to'"
        else if f.returns then
          impossf "a 'cut to' returns? I think not!"
        else
          E.Ok { ccuts = f.cuts; caborts = f.aborts }) in
    let call_target r x = E.seq (findve r x env) (function
      | (FE.Label (FE.Proc _) | FE.Import _), t when ispointer t -> E.Ok x
      | d, _ -> errorf r "target %s is a %s (must be a procedure or imported pointer)"
          x (FE.denotation's_category d)) 
    in
    let prim_flow r fs =
      E.seq (flow r "C--" false true fs) (fun f ->
        if not (null f.unwinds) then
          errorf r "'also unwinds to' is meaningless on a primitive operator"
        else if not (null f.cuts) then
          errorf r "'also cuts to' is meaningless on a primitive operator"
        else if not (null f.areturns) then
          errorf r "alternate returns on primitive operators are not yet implemented"
        else if f.aborts then
          errorf r "'also aborts' is meaningless on a primitive operator"
        else if not f.returns then
          errorf r "'never returns' is meaningless on a primitive operator"
        else
          E.Ok f) 
    in
    let switch rg e arms =
      let rg = E.Raise.option (Auxfuns.Option.map range rg) in (* order matters *)
      let arms = E.Raise.list (List.map arm arms) in
      E.ematch3 (exp e) rg arms (fun (e, et) rg arms ->
        match et with
          | Types.Bool -> E.error "switch statement scrutinee is a Boolean"
          | Types.Bits w ->
              let rg = Auxfuns.Option.map (striprange w) rg in
              let arms = striparms w arms in
              if null arms then E.error "empty switch statement"
              else Switch(rg, e, arms)) 
    in
    let span k v ss =
      let k = con k in  (* order matters *)
      E.ematch3 k (Elabexp.elab_link env v) ss (fun (k, kw) (v, vw) ss ->
        if kw <> metrics.M.wordsize then
          E.error "span token (key) must be a bit vector of native word size";
        insist_pointer "span value" (Types.Bits vw);
        Span ((k, v), ss)) 
    in
    let assign lhs rhs =
      try E.ematch (E.Raise.list (List.map2 effect lhs rhs)) (fun es ->
        Assign (vrtl (srcmap, r) (Rtl.par es)))
      with Invalid_argument _ -> if List.length lhs < List.length rhs then
        errorf r
          "assignment has %d expressions on the right but only %d locations on the left"
          (List.length rhs) (List.length lhs)
      else
        errorf r
          "assignment has %d locations on the left but only %d expressions on the right"
          (List.length lhs) (List.length rhs) 
    in
    let goto tgt tgts =
      let tgts = E.Raise.list (List.map (goto_target r) tgts) in (* order matters *)
      E.ematch2 (exp tgt) tgts (fun (te, t) tgts ->
        insist_pointer "target of goto" t;
        if null tgts && not (exp_is_local_code_label r tgt) then
          E.error "target list omitted and target is not a local code label";
        Goto (te, tgts)) 
    in 
    let call left conv p args targets flows alias =
      let left  = E.Raise.list (List.map (Elabexp.elab_kinded_name env) left) in
      let args  = E.Raise.list (List.map (actual r) args) in
      let tgts  = E.Raise.list (List.map (call_target r) targets) in
      let flow  = call_flow r conv flows in
      let alias = elab_alias r alias in
      E.seq (exp p) (fun (pe, pt) ->
        reject_uncallable r p;
        E.ematch5 left args tgts flow alias (fun left args tgts flow alias ->
          insist_pointer "procedure value" pt;
          Call (left, conv, pe, args, tgts, flow, alias))) 
    in
    let jump conv p args targets =
      let args = E.Raise.list (List.map (actual r) args) in
      let tgts = E.Raise.list (List.map (call_target r) targets) in
      E.seq (exp p) (fun (pe, pt) ->
        reject_uncallable r p;
        E.ematch2 args tgts (fun args tgts ->
          insist_pointer "procedure value" pt;
          Jump (conv, pe, args, tgts))) 
    in
    let cut conv k args flows =
      let args = E.Raise.list (List.map (actual r) args) in
      let flow = cut_flow r conv flows in
      E.ematch3 args (exp k) flow (fun args (k,kt) flow ->
        insist_pointer "target of 'cut to'" kt;
        Cut (conv, k, args, flow)) 
    in
    let return conv alt args =
      let args = E.Raise.list (List.map (actual r) args) in
      let i, n = match alt with
	| None -> E.Ok 0, E.Ok 0
	| Some (i, n) ->
            let okbits f x = E.Ok (Bits.S.to_int (f x)) in
            let const k = E.seq (exp k) (fun (k, kt) ->
              E.catch (eprint r) (okbits Simplify.bits) k) in
            const i, const n 
      in
      E.ematch3 i n args (fun i n args ->
        if i >= 0 && n >= 0 && i <= n then
          Return (conv, i, n, args)
        else
          if i < 0 then
            E.errorf "in return <i/n>, i must be nonnegative but you have i = %d" i
          else if n < 0 then
            E.errorf "in return <i/n>, n must be nonnegative but you have n = %d" n
          else if i > n then
            E.errorf "in return <i/n>, i must be at most n but you have i = %d and n = %d" i n
          else
            impossf "bad <i/n> with i = %d and n = %d, but no better error message" i n) 
    in
    let prim lhs conv op args flows =
      let strip_decorations = function
        | "", arg, None   -> arg
        | "", arg, Some _ ->
            E.error "call to primitive operator may not use explicit alignment"
        | s, arg,  None   ->
            E.error "call to primitive operator must use empty kind"
        | s, arg,  Some _ ->
            E.error "call to primitive operator must use empty kind and default alignment"
      in
      let lhs  = E.Raise.list (List.map (Elabexp.elab_kinded_name env) lhs) in
      let args = E.Raise.list (List.map (fun arg -> exp (strip_decorations arg)) args) in
      let flow = prim_flow r flows in
      E.ematch3 lhs args flow (fun lhs args flow ->
        match lhs with
          | [_, (result, w), aligned] ->
              let argtys  = List.map snd args in
              let argexps = List.map fst args in
              let t, opr  = Rtlop.Translate.prefix op argtys in
              if Pervasives.(<>) t (Types.Bits w) then
		Printf.kprintf E.error
                  "left-hand side has type bits%d, but primitive operator produces results of type %s"
                  w (Types.to_string t)
              else
		Assign (Rtl.store result (Rtl.app opr argexps) w)
          | _ -> E.error "primitive operator produces exactly one result") 
    in
    let rec stmt s = match s with
      | N.StmtAt (s, r) -> E.catch (eprint r) (full_stmt r reachable) s
      | N.If (c, t, f) -> 
          let c = exp c in let t = stmts r true t in let f = stmts r true f in (*order*)
          E.ematch3 c t f (fun (c, cty) t f ->
            match cty with
              | Types.Bool -> If (c, t, f)
              | Types.Bits n -> E.error "condition in `if' is not a Boolean")
      | N.Switch (range, e, arms) -> switch range e arms
      | N.Label n -> E.Ok (Label n)
      | N.Cont (name, cc, formals) ->
          let c = E.ematch (cformals formals) (fun formals -> Cont (name,cc,formals)) in
          if reachable then
            E.error "control can fall through to continuation"
          else
            c
      | N.Span (k, v, ss)   -> span k v (stmts r reachable ss)
      | N.Assign (lhs, rhs) -> assign lhs rhs
      | N.Goto (tgt, tgts)  -> goto tgt tgts
      | N.Call (lhs, conv, p, args, tgts, f, a) -> call lhs conv p args tgts f a
      | N.Jump (conv, p, args, tgts)            -> jump conv p args tgts
      | N.Cut  (conv, k, args, flow)            -> cut conv k args flow
      | N.Prim (lhs, conv, p, args, flow)       -> prim lhs conv p args flow
      | N.Return (conv, altcont, args)          -> return conv altcont args
      | N.Limitcheck (conv, cookie, cont)       -> limitcheck conv cookie cont 
    in
    stmt s
  and stmts r reachable ss = 
    let rec stmts reachable = function
      | [] -> []
      | N.Span (_, _, []) :: ss -> stmts reachable ss
      | s :: ss ->
          let s' = full_stmt r reachable s in  (* enforce order *)
          s' :: stmts (not (jumps s)) ss in
    Error.Raise.list (stmts reachable ss)
  and jumps = function N.Goto _ | N.Jump _ | N.Cut _ | N.Return _  -> true
    | N.Call (_, _, _, _, _, fl, _) -> List.exists never_returns fl
    | N.StmtAt (s, _) -> jumps s
    | N.Span (_, _, ss) -> jumpsss ss
    | N.If (_, t, f) -> jumpsss t && jumpsss f
    | N.Switch (_, _, arms) -> List.for_all jumpsarm arms
    | _ -> false
  and never_returns = function
    | Cmm_ast.FlowAt (f, _) -> never_returns f
    | Cmm_ast.NeverReturns -> true
    | _ -> false
  and jumpsss = function
    | [] -> false
    | [s] -> jumps s
    | s :: N.Span (_, _, []) :: ss -> jumpsss (s :: ss)
    | _ :: ss -> jumpsss ss
  and jumpsarm (_, ss) = jumpsss ss in
  let stmts ss =
    let falls = not (jumpsss ss) in
    let ss = stmts r true ss in
    if falls then errorf r "control falls off end of procedure" else ss
  in stmts, cformals

let nullmap = Srcmap.mk ()

let nullv   = fun _ -> None

let elab_stmts    v m r env = fst (elab_functions v m           r env)

let elab_cformals     r env = snd (elab_functions nullv nullmap r env)
  
let codelabels ss =
  let rec adds ss labels = List.fold_left add labels ss 
  and add labels s = 
    match s with
      | If     (_, ts, fs) -> adds ts (adds fs labels)
      | Switch (_, _, arms) ->
	  List.fold_left (fun labels (_, ss) -> adds ss labels) labels arms
      | Label  l -> l :: labels
      | Cont   _ -> labels
      | Span   (_, ss) -> adds ss labels
      | Assign _ 
      | Call   _
      | Call'  _
      | Goto   _ 
      | Jump   _ 
      | Cut    _ 
      | Return _
      | Limitcheck _ -> labels 
  in adds ss []

