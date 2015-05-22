(* 
   MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
   
   The name `Mlite C Compiler' belongs to us, but the code is available free for
   any use in any field of endeavor.  You may redistribute Mlite C Compiler in
   whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
   source and include this LICENSE file.  You may modify Mlite C Compiler and
   create derived works, with which you may do as you like, but the result may
   not be called Mlite C Compiler without written consent.
   
   The software is placed in the public domain.  It is not protected by copyright,
   and it is not protected by a ``copyleft'' agreement like the one used by the
   Free Software Foundation.
*)

open Ast_ea_expr
let mk_cconst v c t=
  let t = match c with
    | Some (t, f) -> t
    | None -> t
  in
  Cconst (t, Const_folding.cval_ext_of_cval v)

let put_lval eenv lval te = 
  let _ = LvalHashtbl.add eenv.lval_env lval te
  in lval

let put_rval eenv rval te = 
  let _ = RvalHashtbl.add eenv.rval_env rval te
  in rval

let put_rexpr eenv rexpr te = 
  let _ = RexprHashtbl.add eenv.rexpr_env rexpr te
  in rexpr

let put_cexpr eenv cexpr te = 
  let _ = CexprHashtbl.add eenv.cexpr_env cexpr te
  in cexpr


let rec get_lval env eenv lval = 
  try
    LvalHashtbl.find eenv.lval_env lval 
  with
    | Not_found ->
	let te = match lval with
	  | Lladdr (Nlbl ce) -> 
	      let ce_info = Cent_op.ce_info_of ce
	      in  ce_info.Cent.ce_te
		    
	  | Lladdr (Nctn ce) ->
	      let ce_info = Cent_op.ce_info_of ce
	      in  Tent_op.elmt_of (ce_info.Cent.ce_te)
		    
	  | Lreg ce -> 
	      let ce_info = Cent_op.ce_info_of ce
	      in  ce_info.Cent.ce_te
	in 
	LvalHashtbl.add eenv.lval_env lval te;
	te

and get_rval env eenv rval = 
  try
    RvalHashtbl.find eenv.rval_env rval
  with
    | Not_found ->
	let te = match rval with
	  | Rreg ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in ce_info.Cent.ce_te 
		   
	  | Rindir ce ->
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.elmt_of ce_info.Cent.ce_te
		   
	  | Rfun ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of ce_info.Cent.ce_te 
		   
	  | Rvct ce ->
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of (Tent_op.elmt_of ce_info.Cent.ce_te)
		   
	  | Rladdr (Nlbl ce) -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of ce_info.Cent.ce_te

	  | Rladdr (Nctn ce) -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in ce_info.Cent.ce_te
		   
	  | Rconst (te, _) -> te
	  | Rsizeof (te, const) -> env.Senv.size_typ
	  | Ralignof (te, const) -> env.Senv.size_typ
	  | Rvoid -> env.Senv.void_typ
	  | Rcode_label string
	  | Rbyte_value string -> assert false
	  | Rcexpr cexpr -> get_cexpr env eenv cexpr
	in
	RvalHashtbl.add eenv.rval_env rval te;
	te
	  
and get_rexpr env eenv rexpr =
  try
    RexprHashtbl.find eenv.rexpr_env rexpr
  with
    | Not_found ->
	let te = match rexpr with
	  | Rval rval -> get_rval env eenv rval
	  | Binary_arithm (bop, v0, v1) -> 
	      let te0 = get_rval env eenv v0
	      and te1 = get_rval env eenv v1
	      in 
	      let (_, _, te) = Tent_op.bin_arithm_typ bop te0 te1
	      in te
	  | Binary_predicate (bop, v0, v1) -> env.Senv.int_typ
	  | Binary_logic (bop, v0, v1) -> env.Senv.int_typ
	  | Unary_arithm (uop, v) -> get_rval env eenv v
	  | Logic_not rval -> env.Senv.int_typ
	      
	  (* convertion *)
	  | Cast (te, v) -> te
	  | Ne_cast (te, v0, v1) -> te
	in 
	RexprHashtbl.add eenv.rexpr_env rexpr te;
	te
	  
	  
and get_cexpr env eenv cexpr = 
  try
    CexprHashtbl.find eenv.cexpr_env cexpr
  with
    | Not_found -> 
	let te = match cexpr with
	  | CBinary_arithm (bop, e0, e1) ->
	      let te0 = get_cexpr env eenv e0
	      and te1 = get_cexpr env eenv e1
	      in
	      let (c0, c1, r) = 
		Tent_op.bin_arithm_typ bop te0 te1
	      in r
	  
	  | CBinary_predicate (bop, e0, e1) -> env.Senv.int_typ
	  | CBinary_logic (bop, e0, e1) -> env.Senv.int_typ
	  | CUnary_arithm (uop, e0) -> get_cexpr env eenv e0
	  | CLogic_not cexpr -> env.Senv.int_typ
	  | CCast (te, e) -> te
	  | CQuestion (e0, e1, e2) -> 
	      let te1 = get_cexpr env eenv e1
	      and te2 = get_cexpr env eenv e2
	      in Tent_op.lub [te1;te2]
		   
	  | Cconst (te, _) -> te
	  | Csizeof (te, c) -> env.Senv.size_typ
	  | Calignof (te, c) -> env.Senv.size_typ
	  | Cvct ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of (Tent_op.elmt_of ce_info.Cent.ce_te)
		   
	  | Cvar_lbl ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of (ce_info.Cent.ce_te)
		   
	  | Ccode_lbl ce -> assert false

	  | Cfun_lbl ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of (ce_info.Cent.ce_te)
	in
	CexprHashtbl.add eenv.cexpr_env cexpr te;
	te


module CA = Const_folding
module S = C_semantics_symbol
module TO = Tent_op
module E = Senv 
module EO = Senv_op

exception LinkConst of cexpr
exception CResult of cexpr


let rec eval_cexpr env eenv (expr:cexpr) :CA.cval_ext = 
  let convert e = function
    | Some (t, f) -> f e
    | None -> e
  in
  let te = get_cexpr env eenv expr
  in
  match expr with
    | CLogic_not e ->
	begin
	  try
	    let (v,s,sopt) = eval_cexpr env eenv e;
	    in
	    let v' = CA.cval_to_CINT v
	    in CA.cval_ext_of_cval (CA.una_cval CA.NOT v')
	  with
	    | LinkConst e ->
		raise (LinkConst (CLogic_not e))
	end   
	  
    | CBinary_logic (bop, e0, e1) ->
	begin
	  try
	    let (v0, s0, sopt0) = eval_cexpr env eenv e0
	    in
	    try
	      let (v1, s1, sopt1) = eval_cexpr env eenv e1
	      in
	      let v0' = CA.cval_to_CINT v0
	      and v1' = CA.cval_to_CINT v1
	      in
	      let v = match bop with
		| S.And -> CA.cval_and v0' v1'
		| S.Or -> CA.cval_or v0' v1'
	      in CA.cval_ext_of_cval v 
	    with
	      | LinkConst e1 -> assert false
	  with
	    | LinkConst e0 -> assert false
	end
	  
    | CBinary_predicate (bop, e0, e1) ->
	begin
	  try
	    let (v0,s0,sopt0) = eval_cexpr env eenv e0
	    and te0 = get_cexpr env eenv e0
	    in
	    try
	      let (v1,s1,sopt1) = eval_cexpr env eenv e1
	      and te1 = get_cexpr env eenv e1
	      in
	      let (c0, c1, r) = Tent_op.bin_rel_typ te0 te1
	      in
	      let v0 = convert v0 c0
	      and v1 = convert v1 c1
	      in
	      let v = match bop with
		| S.Eq -> CA.rel_cval CA.EQ v0 v1
		| S.Ne -> CA.rel_cval CA.NE v0 v1
		| S.Lt -> CA.rel_cval CA.LT v0 v1
		| S.Gt -> CA.rel_cval CA.GT v0 v1
		| S.Le -> CA.rel_cval CA.LE v0 v1
		| S.Ge -> CA.rel_cval CA.GE v0 v1
	      in CA.cval_ext_of_cval v
	    with
	      | LinkConst e1 -> assert false
	  with
	    | LinkConst e0 -> assert false
	end    

    | CBinary_arithm (bop, e0, e1) ->
	begin
	  try
	    let te0 = get_cexpr env eenv e0
	    and te1 = get_cexpr env eenv e1
	    in
	    let (c0, c1, r) = Tent_op.bin_arithm_typ bop te0 te1
	    in
	    try
	      let (v0, s0, sopt0) = eval_cexpr env eenv e0
	      in
	      try
		let (v1, s1, sopt1) = eval_cexpr env eenv e1
		in
		let v0 = convert v0 c0
		and v1 = convert v1 c1
		in
		let v = match bop with
		  | S.Add -> CA.bin_cval CA.ADD v0 v1
		  | S.Sub -> CA.bin_cval CA.SUB v0 v1
		  | S.Mul -> CA.bin_cval CA.MUL v0 v1
		  | S.Div -> CA.bin_cval CA.DIV v0 v1
		  | S.Shl -> CA.bin_cval CA.SHL v0 v1
		  | S.Shr -> CA.bin_cval CA.SHR v0 v1
		  | S.Bor -> CA.bin_cval CA.BOR v0 v1
		  | S.Bxor -> CA.bin_cval CA.BXOR v0 v1
		  | S.Band -> CA.bin_cval CA.BAND v0 v1
		  | S.Mod -> CA.bin_cval CA.MOD v0 v1
		in CA.cval_ext_of_cval v
	      with
		| LinkConst e1 ->
		    let v0 = convert v0 c0
		    in 
		    raise 
		      (CResult
			(CBinary_arithm (bop, mk_cconst v0 c0 te0, e1)))
	    with
	      | LinkConst e0 ->
		  begin
		    try
		      let (v1,s1,sopt1) = eval_cexpr env eenv e1
		      in
		      let v1 = convert v1 c1
		      in 
		      raise 
			(CResult
			  (CBinary_arithm 
			    (bop, e0, mk_cconst v1 c1 te1)))
		    with
		      | LinkConst e1 ->
			  raise 
			    (CResult
			      (CBinary_arithm (bop, e0, e1)))
				
		  end
	  with
	    | CResult e -> 
		raise (LinkConst (put_cexpr eenv e te))
	end
	  
    | CUnary_arithm (uop, e) ->
	begin
	  try
	    match uop with
	      | S.Neg ->
		  let (v, s, sopt) = eval_cexpr env eenv e
		  in
		  CA.cval_ext_of_cval (CA.una_cval CA.NEG v)
	      | S.Bnot -> 
		  let (v, s, sopt) = eval_cexpr env eenv e
		  in
		  (* why? DEBUG? *)
		  (*camlp4_macro_warning "bitwise logic negation";*)
		  CA.cval_ext_of_cval (CA.una_cval CA.BNOT v)
	  with
	    | LinkConst e ->
		raise 
		  (LinkConst 
		    (CUnary_arithm (uop, e)))
	end
	  
    | CQuestion (condition, e0, e1) ->
	begin
	  try
	    let (cv,cs, csopt) = eval_cexpr env eenv condition
	    in
	    try
	      let v0 = eval_cexpr env eenv e0
	      in
	      try
		let v1 = eval_cexpr env eenv e1
		in
		let cvint = match CA.cval_to_CINT cv with
		  | CA.CINT v -> v
		  | _ -> assert false
		in
		if CA.cint_is_true cvint then v0 else v1
	      with
		| LinkConst e1 -> assert false
	    with
	      | LinkConst e0 -> assert false
	  with
	    | LinkConst cond -> assert false
	end
	  
    | CCast (bt, e) ->
	begin
	  try
	    let bt = 
	      if Tent_op.is_ptr_typ bt then
		env.EO.cptr_cuint_typ
	      else
		Tent_op.unqualified_typ bt
	    in
	    let (_, tid) = bt
	    in
	    let (v,s,sopt) = eval_cexpr env eenv e
	    and te = get_cexpr env eenv e
	    in
	    let (_, vid) = 
	      if Tent_op.is_ptr_typ te then
		env.EO.cptr_cuint_typ
	      else
		te
	    in
	    let converter = Converter.get vid tid
	    in
	    let v = converter v
	    in CA.cval_ext_of_cval v
	  with
	    | LinkConst e ->
		raise (LinkConst (CCast (bt, e)))
	end
	  
    | Cconst c -> snd c
    | Csizeof (ty, c) -> snd c
    | Calignof (ty, c) -> snd c
    | Cvct str ->
	raise (LinkConst (Cvct str))
	  
    | Cvar_lbl str ->
	raise (LinkConst (Cvar_lbl str))
	  
    | Cfun_lbl str ->
	raise (LinkConst (Cfun_lbl str))

    | Ccode_lbl str ->
	raise (LinkConst (Ccode_lbl str))
