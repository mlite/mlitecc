include Common
type name    = string
type kind    = string
type aligned = int
type index = int
type linktime = Reloc.t
type 'a proc =
    { env     : 'a Fenv.Dirty.env'
    ; sym     : Symbol.t  (* assembly-language symbol for this procedure *)
    ; cc      : string    (* calling convention *)
    ; name    : name
    ; spans   : (Bits.bits * linktime) list  (* enclose whole procedure *)
    ; formals : (index * kind * Cmm_ast.variance * Rtl.width * name * aligned) list
    ; locals  : Fenv.variable list
    ; continuations : (name * Fenv.continuation) list
    ; stackmem      : Block.t
    ; stacklabels   : Rtl.exp list
    ; code          : Elabstmt.stmt list
    ; basic_block   : bool  (* procedure represents a basic block from source *)
    } 
      
type 'a datum =
  | Datalabel         of Symbol.t (* must be asm level not source level *)
  | Align             of int
  | InitializedData   of (linktime * Rtl.width) list
  | UninitializedData of int  (* counts the number of mems *)
  | Procedure         of 'a proc

type 'a section = name * 'a datum list
type 'a compunit = 
    { globals : (name * Fenv.variable) list
    ; sections : 'a section list
    }
      
type validator = Rtl.rtl -> string option

module A  = Cmm_ast
module B  = Bits
module E  = Error
module F  = Fenv.Dirty
module FE = Fenv
module N  = Nast
module R  = Rtl
module RP = Rtl.Private
module Dn = Rtl.Dn
module Up = Rtl.Up
module T  = Types

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let foldl: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = List.fold_left 
  
let (--) xx yy = List.filter (fun x  -> List.mem x yy) xx 
  
let error r env msg = E.errorRegionPrt (F.srcmap env,r) msg 
  
let is2power x = x > 0 && x land (x - 1) = 0
  
module type SB = 
sig
  type d
  module Decl : Topsort.Sortable with type decl = d N.marked
  val what : string
  type rhs
  val eval : 'a F.env' -> Decl.decl -> rhs E.error
  val bind : string -> rhs E.error N.marked -> 'a F.env' -> 'a F.env'
end
  
module SortAndBind (D : SB) = struct
  module Sort = Topsort.Make (D.Decl)
  let bind_sortable_definitions env ds =
    let bind_each_name rhs env ((r, _) as d) =
      foldl (fun env n -> D.bind n (r, rhs) env) env (D.Decl.defines d) 
    in 
    let bind_to_error env decls = 
      foldl (bind_each_name E.Error) (F.flagError env) decls 
    in
    let rec bind_one env d =
      match D.eval env d with
	| E.Error       -> bind_each_name E.Error (F.flagError env) d
	| E.Ok _ as rhs -> bind_each_name rhs env d 
    in
    let report_cycle env ds = 
      let report d =
        error (fst d) env
          (D.what ^ " definition for "^(List.hd (D.Decl.defines d))^" is cyclic") 
      in
      List.iter report ds 
    in
    let rec bind env ds =
      try
        foldl bind_one env (Sort.sort ds)
      with Sort.Cycle offending ->
        let ds    = ds -- offending         
	in
        let env   = bind_to_error env offending 
	in
        (report_cycle env offending ; bind env ds ) 
    in
    bind env ds
end
  
module Type = 
struct
  type d = N.typedefn
  module Decl = 
  struct
    type decl = N.typedefn N.marked
    let defines (_, (_,names)) = names
    let uses    (_, (t,_    )) =
      let rec u = function 
        | A.BitsTy _    -> []
        | A.TypeSynonym x   -> [x]
        | A.TyAt (t, _) -> u t in
      u t
  end
  let what = "type"
  type rhs = Types.ty
  let eval env (_, (t, _)) = E.ematch (Elabexp.elab_ty env t) (fun w -> Types.Bits w)
  let bind = F.bindt
end
  
module TypeSortBind = SortAndBind(Type)
  
module Const = 
struct
  type d = N.constdefn
  let rec freeExprVars e tail = 
    match e with
      | A.ExprAt(x,_)     -> freeExprVars x tail
      | A.Fetch(lvalue)   -> freeLValueVars lvalue tail
      | A.BinOp(e1,_,e2)  -> freeExprVars e1 (freeExprVars e2 tail)
      | A.UnOp(_,e)       -> freeExprVars e tail
      | A.PrimOp(_,args)  -> foldl (fun t (_,e,_) -> freeExprVars e t) tail args
      | A.Sint _ 
      | A.Uint _           
      | A.Float _         
      | A.Char _          -> tail
	  
  and freeLValueVars l tail = 
    match l with
      | A.NameOrMemAt(x,r) -> freeLValueVars x tail
      | A.Mem(_,e,_, _)    -> freeExprVars e tail
      | A.Name(_,name,_)   -> if List.mem name tail then tail else name :: tail
	  
  module Decl = 
  struct
    type decl = d N.marked
    let defines (_, (t,name,expr)) = [name]
    let uses    (_, (t,name,expr)) = freeExprVars expr []
  end
    
  let what = "constant"
    
  type rhs = Bits.bits * Types.ty
      
  let eval env (r, (ty, name, e)) =
    let errorf x =
      Printf.kprintf (fun s -> E.errorRegionPrt (F.srcmap env, r) s; E.Error) x in
    E.seq (Elabexp.elab_con env e) (fun (b, w) ->
      match ty with
      | None -> E.Ok (b, Types.Bits w)
      | Some ty ->
          E.seq (Elabexp.elab_ty env ty) (fun w' ->
            if w = w' then E.Ok (b, Types.Bits w)
            else errorf "constant %s declared with type bits%d has actual type bits%d"
                           name w' w))
  let bind name (r, typed_exp) env = 
    F.bindv name (r, E.ematch typed_exp (fun (b, t) -> FE.Constant b, t)) env
end


module ConstSortBind = SortAndBind(Const)
  
type scope = Local | Global
    
let program ~(swap:bool) validate srcmap asm ast =
  E.seq (Metrics.of_ast ~swap srcmap ast.N.target)
    (fun metrics ->
      let flag        = function E.Error -> F.flagError | E.Ok _ -> (fun env ->	env) 
      in
      let eprint r s  = E.errorRegionPrt (srcmap, r) s 
      in
      let errorf r    = Printf.kprintf (fun s -> eprint r s; E.Error) 
      in
      let findve r n  = E.catch (eprint r) (fun env -> snd (F.findv n env)) 
      in
      let pointerty   = Types.Bits metrics.Metrics.pointersize 
      in
      let vfp         = Vfp.mk metrics.Metrics.wordsize 
      in
      let memsize     = metrics.Metrics.memsize 
      in
      let aligned     = Elabexp.aligned metrics 
      in
      let aligned r w = E.catch (eprint r) (fun a -> E.Ok (aligned w a)) 
      in
      let bind_and_return_continuations env conts =
	let rec bind prev' env = function
          | [] -> env, List.rev prev'
          | (r, (name, cc, formals)) :: ks ->
              match (Elabstmt.elab_cformals r env formals) with
		| E.Error -> bind prev' (F.bindv name (r, E.Error) env) ks
		| E.Ok formals ->
                    let addrblock =
                      Block.relative vfp ("continuation " ^ name)
			Block.at ~size:0 ~alignment:1 in
                    let k = { FE.cut_to = false; FE.unwound_to = false; FE.returned_to = [];
                    FE.base = addrblock; FE.escapes = false; FE.convention = cc;
                    FE.formals = formals; } in
                    let env = F.bindv name (r, E.Ok (FE.Continuation k, pointerty)) env 
		    in
                    bind ((name, k) :: prev') env ks 
	in
	bind [] env conts 
      in
      let bind_and_remember_import env (r, ty, imports) = 
	let import env (foreign,name) =
          let env = F.import r (Auxfuns.Option.get name foreign) name env in
          let sym = F.symbol env name in
          let den = FE.Import (Auxfuns.Option.get name foreign, sym) in
          let ty  = match ty with None -> E.Ok pointerty
            | Some t -> E.seq (Elabexp.elab_ty env t) (fun w ->
                if w =*= metrics.Metrics.pointersize then E.Ok pointerty
                else errorf r "imported type bits%d is inconsistent with native pointer type %s"
                  w (Types.to_string pointerty)) 
	  in
          F.bindv name (r, E.Raise.right (den, ty)) env 
	in
	foldl import env imports 
      in
      let bind_code_label env (r, n) = 
	F.bindv n (r, E.Ok (FE.Label(FE.Code (F.symbol env n)), pointerty)) env 
      in
      let registers_from scope n rs env =
	let rec regs_from prev' n rs env = match rs with
	  | [] ->
              let regs = E.ematch (E.Raise.list (List.rev prev')) (fun rs ->
		let unreg = function (n,(FE.Variable r,_)) -> (n,r)
		  | _ -> impossf "nonregister" in
		List.map unreg rs) in
              env, regs
	  | (rgn, (v, kind, t, name, hwreg)) :: rs ->
              let entry = E.seq (Elabexp.elab_ty env t) (fun w ->
		let loc   = (match scope with Global -> R.global | Local -> R.var) name n w in
		let reg h = { FE.index = n; FE.rkind = h; FE.loc = loc; FE.variance = v } in
		let den h = E.Ok (FE.Variable (reg h)) in
		let den = match kind, hwreg with
		  | Some _, Some _ -> errorf rgn "can't have kind and hardware register"
		  | Some h, None ->
                      (match scope with Global -> den (FE.RKind h)
			| Local -> errorf rgn
			    "kinds permissible only on formal parameters or global variables") 
		  | None,   None   -> den FE.RNone
		  | None,   Some r ->
                      (match scope with
			| Local  -> errorf rgn "local register can't be mapped to hardware"
			| Global -> den (FE.RReg r)) in
		E.Raise.left (den, Types.Bits w)) in
              let named = E.Raise.right (name, entry) in
              regs_from (named::prev') (n+1) rs (F.bindv name (rgn, entry) env) 
	in
	regs_from [] n rs env 
      in
      let formals_from n rs env =
	let rec formals prev' n rs env = match rs with
	  | [] -> env, n, List.rev prev'
	  | (r, (kind, inv, ty, name, a)) :: rs ->
              let error () = formals prev' n rs (F.bindv name (r, E.Error) env) in
              match Elabexp.elab_ty env ty with
		| E.Error -> error()
		| E.Ok w ->
		    match aligned r w a with
		      | E.Error -> error()
		      | E.Ok a ->
			  let i = { FE.index = n; FE.variance = inv;
                          FE.loc = Rtl.var name n w; FE.rkind = FE.RKind kind; } in
			  let env = F.bindv name (r, E.Ok (FE.Variable i, Types.Bits w)) env in
			  formals ((n, kind, inv, w, name, a)::prev') (n+1) rs env 
	in 
	formals [] n rs env 
      in
      let bind_section_names env (secname, ds) = 
	let rec datum env (r, d) =
          let errorf fmt = Printf.kprintf (fun s -> eprint r s; F.flagError env) fmt in
          match d with
            | N.Datalabel l ->
		F.bindv l (r, E.Ok (FE.Label (FE.Data (F.symbol env l)), pointerty)) env
            | N.Align n when is2power n -> env
            | N.Align n -> errorf "alignment %d is not a power of 2" n
            | N.ReserveMem(ty,size,init) -> env
            | N.Procedure p ->
		let den = FE.Label (FE.Proc (F.symbol env p.N.name)) in
		F.bindv p.N.name (r, E.Ok (den, pointerty)) env
            | N.SSpan (_, _, ds) -> foldl datum env ds in
	foldl datum env ds 
      in
      let stackdata env =
	let rec stackdata mem stacklabels' env ds = match ds with
	  | [] -> Memalloc.freeze mem, List.rev stacklabels', env
	  | d :: ds ->
              stackdatum mem stacklabels' env d
		(fun mem stacklabels' env -> stackdata mem stacklabels' env ds) 
	and stackdatum mem stacklabels' env (r, d) k =
          let errorf fmt = Printf.kprintf
            (fun s -> eprint r s; k mem stacklabels' (F.flagError env)) fmt in
          match d with
            | N.Datalabel l ->
		let loc = Memalloc.current mem in
		let env = F.bindv l (r, E.Ok (FE.Label (FE.Stack loc), pointerty)) env in
		k mem (loc :: stacklabels') env
            | N.Align n when is2power n ->
		k (Memalloc.align mem n) stacklabels' env
            | N.Align n -> errorf "alignment %d is not a power of 2" n
            | N.ReserveMem(ty,size,Some _) -> errorf "initialization not permitted for stackdata"
            | N.ReserveMem(ty,A.DynSize,_) -> errorf "illegal size for stackdata"
            | N.ReserveMem(ty,size,None) ->
		(match Elabexp.elab_ty env ty with
		  | E.Error -> k mem stacklabels' env
		  | E.Ok w ->
                      let finish size =
			if w mod memsize = 0 then
			  k (Memalloc.allocate mem (size * (w / memsize))) stacklabels' env
			else
			  errorf "width %d of initialized data is not a multiple of memsize %d"
			    w memsize in
                      match size with 
			| A.NoSize    -> finish 1 
			| A.DynSize   -> impossf "dynamic memory size"
			| A.FixSize e ->
			    match Elabexp.elab_con env e with
			      | E.Ok (bits, _) -> finish (Bits.U.to_int bits)
			      | E.Error        -> k mem stacklabels' (F.flagError env)
		)
            | N.Procedure _ -> impossf "nested procedures"
            | N.SSpan (_, _, _) -> impossf "span in stackdata" 
	in
	stackdata (Memalloc.relative vfp "stackdata" Memalloc.Up) [] env 
      in
      let remember_export env (r, _, exports) = 
	let export env (name,foreign) =
          F.export r name (Auxfuns.Option.get name foreign) env in
	foldl export env exports 
      in
      let check_export env (r, ty, pairs) = 
	let errorf fmt = Printf.kprintf (fun s -> eprint r s; F.flagError env) fmt in
	let check_pair env (name, foreign) =
          E.seq' env (findve r name env) (fun (den, ty) ->
            if Pervasives.(<>) ty pointerty then
              errorf "exported value must have native pointer type"
            else
              match den with
		| FE.Label (FE.Proc _ | FE.Code _ | FE.Data _) -> env
		| d -> errorf "%s '%s' can't be exported" (FE.denotation's_category d) name) in
	let check_pair env = E.catch' env (eprint r) (check_pair env) in
	let env = match ty with
	  | Some t ->
              E.seq' env (Elabexp.elab_ty env t) (fun w ->
		if w <> metrics.Metrics.pointersize then
		  errorf "exported value must have native pointer type"
		else
		  env)
	  | None -> env 
	in
	foldl check_pair env pairs 
      in
      let section env (secname, ds) =
	let proc env spans p =
          let env = F.push env F.emptyscope in
          let env = TypeSortBind. bind_sortable_definitions env p.N.pdecls.N.types in
          let env = ConstSortBind.bind_sortable_definitions env p.N.pdecls.N.constants in
          let env, n, formals = formals_from 0 p.N.formals env in
          let env, locals = registers_from Local n p.N.locals env in
          let env = foldl bind_code_label env p.N.labels in  (* odd -- explained above *)
          let env, conts = bind_and_return_continuations env p.N.continuations in
          let stackmem, stacklabels, env = stackdata env p.N.stackdata in
          let code = Elabstmt.elab_stmts validate srcmap p.N.region env p.N.code in
          let _, den  = F.findv p.N.name env 
	  in (* lookup cannot fail *)
          E.ematch4 code locals (E.Raise.list spans) den 
	    (fun code locals spans den ->
              let sym = match den with
		| FE.Label (FE.Proc sym), _ -> sym
		| _ -> impossf "procedure %s is not a procedure?!" p.N.name 
	      in
              Procedure
		{ env = env; cc = p.N.cc; name = p.N.name; sym = sym; formals = formals;
		locals = List.map snd locals; continuations = conts; spans = spans;
		stackmem = stackmem; stacklabels = stacklabels; code = code;
		basic_block = false; }
	    )
	in
	let metrics = F.metrics env in
	let rec data spans ds rest = match ds with
	  | [] -> (match rest with [] -> [] | (spans, ds) :: rest -> data spans ds rest)
	  | (r, d) :: ds ->
              let datalabel_meaning l = match findve r l env with
		| E.Ok (FE.Label (FE.Data sym), _) -> E.Ok (Datalabel sym)
		| E.Ok _ -> impossf "data label %s stands for something else?!" l
		| E.Error -> E.Error 
	      in
              match d with
		| N.Datalabel l -> let d = datalabel_meaning l in d :: data spans ds rest
		| N.Align n     -> let d = E.Ok (Align n)      in d :: data spans ds rest
		| N.Procedure p -> let d = proc env spans p    in d :: data spans ds rest
		| N.ReserveMem(ty,size,init) ->
		    let d = reserve_mem r ty size init in d :: data spans ds rest
		| N.SSpan (k, v, ds') ->
		    let span = E.seq2 (Elabexp.elab_con env k) (Elabexp.elab_link env v)
                      (fun (k, kw) (v, vw) ->
			if kw <> metrics.Metrics.wordsize then
			  errorf r "span token (key) must be a bit vector of native word size" 
			else if vw <> metrics.Metrics.pointersize then
			  errorf r "span value must be a bit vector of native pointer size"
			else
			  E.Ok ((k, v))) 
		    in
		    data (span::spans) ds' ((spans, ds) :: rest)
	and reserve_mem r ty size idata =
          E.seq2 (Elabexp.elab_ty env ty) (init_size r size) 
	    (fun w n ->
              match idata with
		| None ->
                    if w mod memsize = 0 then
                      E.Ok (UninitializedData (Auxfuns.Option.get 1 n * (w / memsize)))
                    else
                      errorf r "width %d of initialized data is not a multiple of memsize = %d"
			w memsize
		| Some i ->
                    E.seq (init r (Types.Bits w) i) (fun inits ->
                      let rcount = List.length inits in
                      let lcount = Auxfuns.Option.get rcount n in
                      if      rcount > lcount then errorf r "too many initial values"
                      else if rcount < rcount then errorf r "too few initial values"
                      else E.Ok (InitializedData inits)))
	    
	and init_size r = function
          | A.DynSize   -> E.Ok (None)
          | A.NoSize    -> E.Ok (Some 1)
          | A.FixSize e ->
              E.seq (Elabexp.elab_con env e) 
		(fun (bits, _) -> 
		  (try
                    let size = Bits.S.to_int bits in
                    if size >= 0 then E.Ok (Some size)
                    else errorf r "negative count for initialized data"
		  with Bits.Overflow -> errorf r "overflow computing size")
		)
		
	and init r ty i = 
          match i with
            | A.InitAt (x,r) -> E.catch (error r env) (init r ty) x
            | A.InitExprs(es)  ->
		E.seq (E.Raise.list (List.map (Elabexp.elab_link env) es)) 
		  (fun tes ->
		    if List.for_all (fun (e, w) -> Types.Bits w =*= ty) tes then
                      E.Ok tes
		    else
                      errorf r "type of an initial value does not match declared type %s"
			(Types.to_string ty)
		  )
            | A.InitStr s ->
		let to_texpr c = Reloc.of_const (Bits.U.of_int (Char.code c) 8), 8 in
		if ty =*= T.Bits 8 then
		  E.Ok (Auxfuns.String.foldr (fun c l -> to_texpr c :: l) s [])
		else
		  errorf r "initialized string must specify type bits8"
            | A.InitUStr(s) ->
		if ty =*= T.Bits 16 then
		  Impossible.unimp "Unicode"
		else
		  errorf r "initialized Unicode string must specify type bits16" 
	in
	E.Raise.right (secname, E.Raise.list (data [] ds [])) 
      in
      (* phase one: create environment and bind all names *)
      let env = F.empty srcmap metrics asm in
      let env = F.push env (F.emptyscope) in  (* weird, but so it is *)
      let env = TypeSortBind. bind_sortable_definitions env ast.N.udecls.N.types in
      let env = ConstSortBind.bind_sortable_definitions env ast.N.udecls.N.constants in
      let env = foldl bind_and_remember_import env ast.N.imports in
      let env = foldl remember_export          env ast.N.exports in
      let env, globals = registers_from Global 0 ast.N.globals env in
      let env = foldl (foldl bind_code_label) env ast.N.code_labels in
      let env = foldl bind_section_names env ast.N.sections in
      (* phase two: in environment, elaborate and check the world (only flags env) *)
      let env = foldl check_export env ast.N.exports in
      let ss  = E.Raise.list (List.map (section env) ast.N.sections) in
      let env = flag ss env 
      in
      E.ematch2 globals ss 
	(fun globals ss ->
	  (env, { globals = globals; sections = ss }))
    )
    
    
let rewrite = Obj.magic
