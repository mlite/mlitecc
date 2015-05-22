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

module C99 = Ast_aa_gram
module I = Ast_ea_expr
module O = Ast_eb_expr
module T = Tent
module TO = Tent_op
module CA = Const_folding
module S = C_semantics_symbol
module E = Senv
module EO = Senv_op
module TM = Typ_mem
module QN = Qual_name
module AO = Ast_eb_expr_op
module AEE = Ast_ea_expr_env
module OP = Ast_eb_expr_printer

open Safe_list

let user_opts = []
  
exception LinkConst of O.cexpr
exception CResult of O.cexpr
exception Found of int

let ibit_to_int env sign ibit =
  let size = match ibit with
    | O.I8 -> 1L
    | O.I16 -> 2L
    | O.I32 -> 4L
    | O.I64 -> 8L
  in
  try
    List.iter
      (fun (id, kind, t_size, align, name, _, _) ->
	if kind = sign & t_size = size then
	  raise (Found id)
      ) Mach.builtin_type_table;
    assert false
  with
    | Found id -> 
	(env.E.te_tbl, id)


let fbit_to_float env ibit =
  let size = match ibit with
      | O.F32 -> 4L
      | O.F64 -> 8L
      | O.F80 -> 12L
  in
  try
    List.iter
      (fun (id, kind, t_size, align, name, _, _) ->
	if kind = Mach.F & t_size = size then
	  raise (Found id)
      ) Mach.builtin_type_table;
    assert false
  with
    | Found id -> 
	(env.E.te_tbl, id)




let put_lval eenv lval te = 
  let _ = O.LvalHashtbl.add eenv.O.lval_env lval te
  in lval

let put_rval eenv rval te = 
  let _ = O.RvalHashtbl.add eenv.O.rval_env rval te
  in rval

let put_rexpr eenv rexpr te = 
  let _ = O.RexprHashtbl.add eenv.O.rexpr_env rexpr te
  in rexpr

let put_cexpr eenv cexpr te = 
  let _ = O.CexprHashtbl.add eenv.O.cexpr_env cexpr te
  in cexpr


let get_lval env eenv lval = 
  try
    O.LvalHashtbl.find eenv.O.lval_env lval 
  with
    | Not_found ->
	let te = match lval with
	  | O.Lladdr (O.Nlbl ce) -> 
	      let ce_info = Cent_op.ce_info_of ce
	      in  ce_info.Cent.ce_te
		    
	  | O.Lladdr (O.Nctn ce) ->
	      let ce_info = Cent_op.ce_info_of ce
	      in  Tent_op.elmt_of (ce_info.Cent.ce_te)
		    
	  | O.Lreg ce -> 
	      let ce_info = Cent_op.ce_info_of ce
	      in  ce_info.Cent.ce_te
	in 
	O.LvalHashtbl.add eenv.O.lval_env lval te;
	te


let rec get_cexpr env eenv cexpr = 
  match cexpr with
| O.Cconst (t, _)
| O.CCast (t, _) -> t
| O.Csizeof (t, _) 
| O.Calignof (t, _) -> assert false
| O.Cvct ce ->
    let ce_info = Cent_op.ce_info_of ce 
    in Tent_op.ptr_of (Tent_op.elmt_of ce_info.Cent.ce_te)
| O.Cvar_lbl ce ->
    let ce_info = Cent_op.ce_info_of ce
    in Tent_op.ptr_of ce_info.Cent.ce_te
| O.Cfun_lbl _ -> (env.E.te_tbl, Mach.cptr_uint_id)
| _ -> 
    try
    O.CexprHashtbl.find eenv.O.cexpr_env cexpr
  with
    | Not_found ->
	let te_tbl = env.E.te_tbl
	in
	let te = match cexpr with
	  | O.CLogic_not cexpr -> get_cexpr env eenv cexpr
	      
	  (* convertion *)
	  | O.CPTR_TO_PTR (t, _) -> t
	  | O.CPTR_TO_UI cexpr -> (te_tbl, Mach.cptr_uint_id)
	      
	  (* floating point types to integer types *)
	  | O.CF_TO_SI (cexpr, fbits, ibits) -> (te_tbl, AO.ibits_to_int ibits true)
	  | O.CF_TO_UI (cexpr, fbits, ibits) -> (te_tbl, AO.ibits_to_int ibits false)
	      
	  (* bool type to integer type *)
	  | O.CB_TO_SI (cexpr, ibits1, ibits2) -> (te_tbl, AO.ibits_to_int ibits2 true)
	  | O.CB_TO_UI (cexpr, ibits1, ibits2) -> (te_tbl, AO.ibits_to_int ibits2 false)
	      
	  (* integer types to floating point types *)
	  | O.CSI_TO_F (cexpr, ibits, fbits) -> (te_tbl, AO.fbits_to_float fbits)
	  | O.CUI_TO_F (cexpr, ibits, fbits) -> (te_tbl, AO.fbits_to_float fbits)
	      
	  (* floating point types to floating point types *)
	  | O.CF_TO_F (cexpr, fbits1, fbits2) -> (te_tbl, AO.fbits_to_float fbits2)
	  | O.CSXI (cexpr, ibits1, ibits2) -> (te_tbl, AO.ibits_to_int ibits2 true)
	  | O.CZXI (cexpr, ibits1, ibits2) -> (te_tbl, AO.ibits_to_int ibits2 false)
	  | O.CLOBITS (cexpr, ibits1, ibits2) -> (te_tbl, AO.ibits_to_int ibits2 false)
	      
	  | O.CFneg (fbits, cexpr) -> (te_tbl, AO.fbits_to_float fbits)
	  | O.CFbin (fbin_op, fbits, cexpr1, cexpr2) -> (te_tbl, AO.fbits_to_float fbits)
	  | O.CIneg (ibits, cexpr) -> (te_tbl, AO.ibits_to_int ibits true)
	  | O.CIbnot (ibits, cexpr) -> (te_tbl, AO.ibits_to_int ibits false)
	  | O.CIbin (ibin_op, ibits, cexpr1, cexpr2) -> 
	      (te_tbl, AO.ibits_to_int ibits (AO.ibin_op_is_signed ibin_op))
	  | O.CFrel _
	  | O.CIrel _
	  | O.CLogic _ -> (te_tbl, Mach.cbool_id)
	  | O.CQuestion (cexpr0, cexpr1, cexpr2) -> get_cexpr env eenv cexpr2
	  | _ -> 
	      let str = camlp4_macro_str_pp_print
    		(fun fm -> OP.pp_print_cexpr fm ~need_paren:false cexpr)
	      in camlp4_macro_exception "cannot find %s in cexpr_env" str
	in
	O.CexprHashtbl.add eenv.O.cexpr_env cexpr te;
	te



let get_rval env eenv rval = 
  try
    O.RvalHashtbl.find eenv.O.rval_env rval
  with 
    | Not_found ->
	let te = match rval with
	  | O.Rreg ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in ce_info.Cent.ce_te 
		   
	  | O.Rindir ce ->
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.elmt_of ce_info.Cent.ce_te
		   
	  | O.Rfun ce -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of ce_info.Cent.ce_te 
		   
	  | O.Rvct ce ->
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of (Tent_op.elmt_of ce_info.Cent.ce_te)
		   
	  | O.Rladdr (O.Nlbl ce) -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in Tent_op.ptr_of ce_info.Cent.ce_te

	  | O.Rladdr (O.Nctn ce) -> 
	      let ce_info = Cent_op.ce_info_of ce 
	      in ce_info.Cent.ce_te
		   
	  | O.Rconst (te, _) -> te
	  | O.Rsizeof (te, const) -> env.Senv.size_typ
	  | O.Ralignof (te, const) -> env.Senv.size_typ
	  | O.Rvoid -> env.Senv.void_typ
	  | O.Rcode_label string
	  | O.Rbyte_value string -> assert false
	  | O.Rcexpr cexpr -> get_cexpr env eenv cexpr
	in
	O.RvalHashtbl.add eenv.O.rval_env rval te;
	te
	  
let get_rexpr env eenv rexpr = 
  try
    O.RexprHashtbl.find eenv.O.rexpr_env rexpr
  with 
    | Not_found -> 
	let te = match rexpr with
	  | O.Rval_cast (te, _) -> te
	      
	  | O.Rval rval -> get_rval env eenv rval
	  | O.Logic_not _ -> env.E.int_typ
	      
	  (* convertion *)
	  | O.PTR_TO_PTR (te, _) -> te
	  | O.PTR_TO_UI rval -> env.E.cptr_cuint_typ
	      
	  (* floating point types to integer types *)
	  | O.F_TO_SI (_,_,ibits) -> ibit_to_int env Mach.SI ibits
	  | O.F_TO_UI (_,_,ibits) -> ibit_to_int env Mach.UI ibits
	      
	  (* bool type to integer type *)
	  | O.B_TO_SI (_,_,ibits) -> ibit_to_int env Mach.SI ibits
	  | O.B_TO_UI (_,_,ibits) -> ibit_to_int env Mach.UI ibits
	      
	  (* integer types to floating point types *)
	  | O.SI_TO_F (_,_,fbits) -> fbit_to_float env fbits
	  | O.UI_TO_F (_,_,fbits) -> fbit_to_float env fbits
	      
	  (* floating point types to floating point types *)
	  | O.F_TO_F (_,_,fbits) -> fbit_to_float env fbits
	  | O.SXI (_,_,ibits) -> ibit_to_int env Mach.SI ibits
	  | O.ZXI (_,_,ibits) -> ibit_to_int env Mach.UI ibits
	  | O.LOBITS (_,_,ibits) -> ibit_to_int env Mach.UI ibits
	      
	  | O.Fneg (fbits, _) -> fbit_to_float env fbits
	  | O.Fbin (_, fbits, _,_) -> fbit_to_float env fbits
	  | O.Ineg (ibits,_) -> ibit_to_int env Mach.SI ibits
	  | O.Ibnot (ibits, _) -> ibit_to_int env Mach.UI ibits
	  | O.Ibin (bop, ibits,_,_) -> 
	      begin
		let sign = match bop with
		  | O.ADD 
		  | O.SUB
		  | O.MUL 
		  | O.DIV 
		  | O.MOD -> Mach.SI
		  | O.BAND
		  | O.BOR
		  | O.BXOR
		  | O.SHL 
		  | O.SHR 
		  | O.MULU 
		  | O.DIVU 
		  | O.MODU 
		  | O.SHRU -> Mach.UI
		in ibit_to_int env sign ibits
	      end 
	  | O.Frel _ -> env.E.int_typ
	  | O.Irel _ -> env.E.int_typ
	  | O.Logic _ -> env.E.int_typ
	in 
	O.RexprHashtbl.add eenv.O.rexpr_env rexpr te;
	te
	




type cmpstate = 
    {
      env: Senv.env;
      ienv: I.expr_env;
      oenv: O.expr_env;
    }

let rec compile_c_init_expression cmpstate expr = 
  match expr with
    | I.Static_init_none -> O.Static_init_none
    | I.Static_init cexpr -> O.Static_init (compile_cexpr cmpstate cexpr)
	
and compile_c_initializer cmpstate c_initializer =
  Typ_mem_op.compile (compile_c_init_expression cmpstate) c_initializer
    
and mk_rexpr rval = O.Rval rval

and c2s s = O.SESE [s]
  
and rexpr_to_rval cmpstate (rexpr:O.rexpr) = 
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  match rexpr with
    | O.Rval v -> ([], v)
    | _ -> 
	let rexpr_O_re_typ = get_rexpr env eenv rexpr
	in
	if TO.is_void_typ rexpr_O_re_typ then
	  ([], O.Rvoid)
	else
	  let var = EO.get_tmp_ce env rexpr_O_re_typ
	  in
	  ([c2s (O.Assign 
	    (O.Lladdr (O.Nlbl var), rexpr))], O.Rladdr (O.Nctn var))

and mk_cexpr_convertion cmpstate rexpr dest_type = 
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  let src_type = get_cexpr env eenv rexpr
  in
  let (_, srcid) = TO.convert_to_builtin_typ src_type
  and (_, destid) = TO.convert_to_builtin_typ dest_type
  in
  let (sid,src_tk,src_size,_,_,_,_) = 
    Mach.get_type_matrix srcid
  and (did,dest_tk,dest_size,_,_,_,_) = 
    Mach.get_type_matrix destid
  in
  assert (sid == srcid);
  let src_size = Int64.to_int src_size
  and dest_size = Int64.to_int dest_size
  in
  let ibits size = 
    if size = 1 then
      O.I8
    else if size = 2 then
      O.I16
    else if size = 4 then
      O.I32
    else if size = 8 then
      O.I64
    else assert false
      
  and fbits size = 
    if size = 4 then
      O.F32
    else if size = 8 then
      O.F64
    else if size = 12 then
      O.F80
    else
      assert false
  in
  let ret = match src_tk, dest_tk with
    | Mach.SI, Mach.SI -> (* SI_2_SI *)
	begin
	  if src_size > dest_size then
	    put_cexpr eenv 
	      (O.CLOBITS (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else if src_size < dest_size then
	    put_cexpr eenv
	      (O.CSXI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else
	    rexpr
	end

    | Mach.SI, Mach.UI -> (* SI_2_UI *)
	begin
	  if src_size > dest_size then
	    put_cexpr eenv
	      (O.CLOBITS (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else if src_size < dest_size then
	    put_cexpr eenv
	      (O.CSXI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else
	    rexpr
	end
	  
    | Mach.SI, Mach.F -> (* SI_2_F *)
	begin
	  let srcbits = ibits src_size
	  in
	  if dest_size = 4 then
	    put_cexpr eenv 
	      (O.CSI_TO_F (rexpr, srcbits, O.F32)) dest_type
	  else if dest_size = 8 then
	    put_cexpr eenv
	      (O.CSI_TO_F (rexpr, srcbits, O.F64)) dest_type
	  else if dest_size = 12 then
	    put_cexpr eenv
	      (O.CSI_TO_F (rexpr, srcbits, O.F80)) dest_type
	  else
	    assert false
	end
	  
    | Mach.SI, Mach.BI -> assert false
    | Mach.SI, Mach.PTR
    | Mach.SI, Mach.NIF
    | Mach.SI, Mach.CI 
    | Mach.SI, Mach.CF -> assert false
	
    | Mach.UI, Mach.SI -> (* UI_2_SI *)
	begin
	  let srcbits = ibits src_size
	  and destbits = ibits dest_size
	  in
	  if src_size > dest_size then
	    put_cexpr eenv
	      (O.CLOBITS (rexpr, srcbits, destbits)) dest_type
	  else if src_size < dest_size then 
	    put_cexpr eenv
	      (O.CZXI (rexpr, srcbits, destbits)) dest_type
	  else
	    rexpr
	end
    | Mach.UI, Mach.UI -> (* UI_2_UI *)
	begin
	  let srcbits = ibits src_size
	  and destbits = ibits dest_size
	  in
	  if src_size > dest_size then
	    put_cexpr eenv
	      (O.CLOBITS (rexpr, srcbits, destbits)) dest_type
	  else if src_size < dest_size then
	    put_cexpr eenv
	      (O.CZXI (rexpr, srcbits, destbits)) dest_type
	  else
	    rexpr
	end
	  
    | Mach.UI, Mach.F -> (* UI_2_F *)
	begin
	  let srcbits = ibits src_size
	  in
	  if dest_size = 4 then
	    put_cexpr eenv 
	      (O.CUI_TO_F (rexpr, srcbits, O.F32)) dest_type
	  else if dest_size = 8 then
	    put_cexpr eenv
	      (O.CUI_TO_F (rexpr, srcbits, O.F64)) dest_type
	  else if dest_size = 12 then
	    put_cexpr eenv
	      (O.CUI_TO_F (rexpr, srcbits, O.F80)) dest_type
	  else
	    assert false
	end
    | Mach.UI, Mach.BI -> assert false
    | Mach.UI, Mach.PTR 
    | Mach.UI, Mach.NIF
    | Mach.UI, Mach.CI 
    | Mach.UI, Mach.CF -> assert false

    | Mach.F, Mach.SI ->
	let srcbits = fbits src_size
	in
	put_cexpr eenv
	  (O.CF_TO_SI (rexpr, srcbits, ibits dest_size)) dest_type
	  
    | Mach.F, Mach.UI ->
	put_cexpr eenv
	  (O.CF_TO_UI (rexpr, fbits src_size, ibits dest_size)) dest_type

    | Mach.F, Mach.F ->
	if src_size <> dest_size then
	  put_cexpr eenv
	    (O.CF_TO_F (rexpr, fbits src_size, fbits dest_size)) dest_type
	else rexpr
	  
    | Mach.F, Mach.BI -> assert false
    | Mach.F, Mach.PTR 
    | Mach.F, Mach.NIF
    | Mach.F, Mach.CI 
    | Mach.F, Mach.CF -> assert false
	
    | Mach.PTR, Mach.UI ->
	if src_size = dest_size then
	  put_cexpr eenv (O.CPTR_TO_UI rexpr) dest_type
	else
	  assert false
	  
    | Mach.PTR, Mach.BI -> assert false
    | Mach.PTR, Mach.SI 
    | Mach.PTR, Mach.F 
    | Mach.PTR, Mach.PTR 
    | Mach.PTR, Mach.NIF
    | Mach.PTR, Mach.CI 
    | Mach.PTR, Mach.CF -> assert false


    | Mach.BI, Mach.SI -> (* BI_2_SI *)
	let destbits = ibits dest_size
	in
	put_cexpr eenv 
	  (O.CB_TO_SI (rexpr, ibits src_size, ibits dest_size)) dest_type

    | Mach.BI, Mach.UI -> (* BI_2_UI *)
	let destbits = ibits dest_size
	in
	put_cexpr eenv 
	  (O.CB_TO_UI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  
    | Mach.BI, _ -> assert false
	
    | Mach.NIF, _
    | Mach.CI, _ 
    | Mach.CF, _ -> assert false
  in ret

and mk_convertion cmpstate rexpr dest_type = 
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  let src_type = get_rval env eenv rexpr
  in
  let (_, srcid) = TO.convert_to_builtin_typ src_type
  and (_, destid) = TO.convert_to_builtin_typ dest_type
  in
  let (sid,src_tk,src_size,_,_,_,_) = 
    Mach.get_type_matrix srcid
  and (did,dest_tk,dest_size,_,_,_,_) = 
    Mach.get_type_matrix destid
  in
  assert (sid == srcid);
  let src_size = Int64.to_int src_size
  and dest_size = Int64.to_int dest_size
  in
  let ibits size = 
    if size = 1 then
      O.I8
    else if size = 2 then
      O.I16
    else if size = 4 then
      O.I32
    else if size = 8 then
      O.I64
    else assert false
      
  and fbits size = 
    if size = 4 then
      O.F32
    else if size = 8 then
      O.F64
    else if size = 12 then
      O.F80
    else
      assert false
  in
  let ret = match src_tk, dest_tk with
    | Mach.SI, Mach.SI -> (* SI_2_SI *)
	begin
	  if src_size > dest_size then
	    put_rexpr eenv 
	      (O.LOBITS (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else if src_size < dest_size then
	    put_rexpr eenv
	      (O.SXI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else
	    mk_rexpr rexpr
	end

    | Mach.SI, Mach.UI -> (* SI_2_UI *)
	begin
	  if src_size > dest_size then
	    put_rexpr eenv
	      (O.LOBITS (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else if src_size < dest_size then
	    put_rexpr eenv
	      (O.SXI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  else
	    mk_rexpr rexpr
	end
	  
    | Mach.SI, Mach.F -> (* SI_2_F *)
	begin
	  let srcbits = ibits src_size
	  in
	  if dest_size = 4 then
	    put_rexpr eenv
	      (O.SI_TO_F (rexpr, srcbits, O.F32)) dest_type
	  else if dest_size = 8 then
	    put_rexpr eenv
	      (O.SI_TO_F (rexpr, srcbits, O.F64)) dest_type
	  else if dest_size = 12 then
	    put_rexpr eenv
	      (O.SI_TO_F (rexpr, srcbits, O.F80)) dest_type
	  else
	    assert false
	end
	  
    | Mach.SI, Mach.BI -> assert false
    | Mach.SI, Mach.PTR
    | Mach.SI, Mach.NIF
    | Mach.SI, Mach.CI 
    | Mach.SI, Mach.CF -> assert false
	
    | Mach.UI, Mach.SI -> (* UI_2_SI *)
	begin
	  let srcbits = ibits src_size
	  and destbits = ibits dest_size
	  in
	  if src_size > dest_size then
	    put_rexpr eenv 
	      (O.LOBITS (rexpr, srcbits, destbits)) dest_type
	  else if src_size < dest_size then 
	    put_rexpr eenv
	      (O.ZXI (rexpr, srcbits, destbits)) dest_type
	  else
	    mk_rexpr rexpr
	end
    | Mach.UI, Mach.UI -> (* UI_2_UI *)
	begin
	  let srcbits = ibits src_size
	  and destbits = ibits dest_size
	  in
	  if src_size > dest_size then
	    put_rexpr eenv
	      (O.LOBITS (rexpr, srcbits, destbits)) dest_type
	  else if src_size < dest_size then
	    put_rexpr eenv
	      (O.ZXI (rexpr, srcbits, destbits)) dest_type
	  else
	    mk_rexpr rexpr
	end
	  
    | Mach.UI, Mach.F -> (* UI_2_F *)
	begin
	  let srcbits = ibits src_size
	  in
	  if dest_size = 4 then
	    put_rexpr eenv
	      (O.UI_TO_F (rexpr, srcbits, O.F32)) dest_type
	  else if dest_size = 8 then
	    put_rexpr eenv
	      (O.UI_TO_F (rexpr, srcbits, O.F64)) dest_type
	  else if dest_size = 12 then
	    put_rexpr eenv
	      (O.UI_TO_F (rexpr, srcbits, O.F80)) dest_type
	  else
	    assert false
	end
    | Mach.UI, Mach.BI -> assert false
    | Mach.UI, Mach.PTR 
    | Mach.UI, Mach.NIF
    | Mach.UI, Mach.CI 
    | Mach.UI, Mach.CF -> assert false

    | Mach.F, Mach.SI ->
	let srcbits = fbits src_size
	in
	put_rexpr eenv
	  (O.F_TO_SI (rexpr, srcbits, ibits dest_size)) dest_type
	  
    | Mach.F, Mach.UI ->
	put_rexpr eenv
	  (O.F_TO_UI (rexpr, fbits src_size, ibits dest_size)) dest_type
	  
    | Mach.F, Mach.F ->
	if src_size <> dest_size then
	  put_rexpr eenv
	    (O.F_TO_F (rexpr, fbits src_size, fbits dest_size)) dest_type
	else mk_rexpr rexpr
	  
    | Mach.F, Mach.BI -> assert false
    | Mach.F, Mach.PTR 
    | Mach.F, Mach.NIF
    | Mach.F, Mach.CI 
    | Mach.F, Mach.CF -> assert false
	
    | Mach.PTR, Mach.UI ->
	if src_size = dest_size then
	  put_rexpr eenv (O.PTR_TO_UI rexpr) dest_type
	else
	  assert false
	  
    | Mach.PTR, Mach.BI -> assert false
    | Mach.PTR, Mach.SI 
    | Mach.PTR, Mach.F 
    | Mach.PTR, Mach.PTR 
    | Mach.PTR, Mach.NIF
    | Mach.PTR, Mach.CI 
    | Mach.PTR, Mach.CF -> assert false


    | Mach.BI, Mach.SI -> (* BI_2_SI *)
	let destbits = ibits dest_size
	in
	put_rexpr eenv
	  (O.B_TO_SI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  
    | Mach.BI, Mach.UI -> (* BI_2_UI *)
	let destbits = ibits dest_size
	in
	put_rexpr eenv
	  (O.B_TO_UI (rexpr, ibits src_size, ibits dest_size)) dest_type
	  
    | Mach.BI, _ -> assert false
	
    | Mach.NIF, _
    | Mach.CI, _ 
    | Mach.CF, _ -> assert false
  in rexpr_to_rval cmpstate ret

and type_to_ibits ty =
  if TO.is_integer_typ ty || TO.is_ptr_typ ty then
    begin
      let csize = TO.static_sizeof ty
      in
      match csize with
	| 1L -> O.I8
	| 2L -> O.I16
	| 4L -> O.I32
	| 8L -> O.I64
	| _ -> assert false
    end
  else
    assert false

and type_to_fbits ty =
  if TO.is_real_typ ty then
    begin
      let csize = TO.static_sizeof ty
      in
      match csize with
	| 4L -> O.F32
	| 8L -> O.F64
	| 12L -> O.F80
	| _ -> assert false
    end
  else
    assert false

and mk_cexpr_bin_arithm cmpstate bop e0 e1 =
  begin
    let env = cmpstate.env
    and eenv = cmpstate.oenv
    in
    let e0_te = get_cexpr env eenv e0
    and e1_te = get_cexpr env eenv e1
    in
    let (_, _, dest_typ) = 
      TO.bin_arithm_typ bop e0_te e1_te
    in
    let e0 = mk_cexpr_convertion cmpstate e0 dest_typ
    and e1 = 
      match bop with
	| S.Add ->
	    begin
	      if TO.is_ptr_typ dest_typ then
		e1
	      else
		mk_cexpr_convertion cmpstate e1 dest_typ
	    end
	| S.Sub ->
	    begin
	      if TO.is_ptr_typ dest_typ then
		e1
	      else
		mk_cexpr_convertion cmpstate e1 dest_typ
	    end
	| S.Mul
	| S.Band
	| S.Bor
	| S.Bxor
	| S.Div -> mk_cexpr_convertion cmpstate e1 dest_typ
	| S.Mod -> mk_cexpr_convertion cmpstate e1 dest_typ
	| S.Shl
	| S.Shr -> mk_cexpr_convertion cmpstate e1 dest_typ (*env.E.int_typ*)
    in 
    let is_real = TO.is_real_typ dest_typ
    and is_signed =  TO.is_signed_integer_typ dest_typ
    and is_ptr = TO.is_ptr_typ dest_typ
    and e0_is_signed = TO.is_signed_integer_typ e0_te
    in
    let expr = 
      if is_real then
	begin
	  match bop with
	    | S.Add -> 
		O.CFbin (O.F_ADD, type_to_fbits dest_typ, e0, e1)
	    | S.Sub ->
		O.CFbin (O.F_SUB, type_to_fbits dest_typ, e0, e1)
	    | S.Mul ->
		O.CFbin (O.F_MUL, type_to_fbits dest_typ, e0, e1)
	    | S.Div ->
		O.CFbin (O.F_DIV, type_to_fbits dest_typ, e0, e1)
	    | _ -> assert false
	end
      else if is_ptr then
	begin
	  match bop with
	    | S.Add -> 
		O.CIbin (O.ADD, type_to_ibits dest_typ, e0, e1)
	    | S.Sub ->
		O.CIbin (O.SUB, type_to_ibits dest_typ, e0, e1)
	    | _ -> assert false
	end
      else 
	begin
	  let bop = 
	    match bop with
	      | S.Add -> O.ADD
	      | S.Sub -> O.SUB
	      | S.Mul -> O.MUL (*if is_signed then O.MUL else O.MULU*)
 	      | S.Div -> if is_signed then O.DIV else O.DIVU
	      | S.Mod -> if is_signed then O.MOD else O.MODU
	      | S.Band -> O.BAND
	      | S.Bor -> O.BOR
	      | S.Bxor -> O.BXOR
	      | S.Shl -> O.SHL
	      | S.Shr -> if e0_is_signed then O.SHR else O.SHRU
	  in O.CIbin (bop, type_to_ibits dest_typ, e0, e1)
	end
    in expr
  end
    
and mk_cexpr_bin_rel cmpstate bop e0 e1 =
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  let e0_te = get_cexpr env eenv e0
  and e1_te = get_cexpr env eenv e1
  in
  let (_, _, dest_ty) = TO.bin_rel_typ e0_te e1_te
  in
  let e0 = mk_cexpr_convertion cmpstate e0 dest_ty
  and e1 = mk_cexpr_convertion cmpstate e1 dest_ty
  in
  let is_real = TO.is_real_typ dest_ty
  and is_signed = 
    TO.is_signed_integer_typ e0_te & TO.is_signed_integer_typ e1_te
  in
  let ret = match bop with
    | S.Eq ->
	begin
	  if is_real then
	    O.CFrel (O.F_EQ, type_to_fbits dest_ty, e0, e1)
	  else
	    O.CIrel (O.EQ, type_to_ibits dest_ty, e0, e1)
	end
    | S.Ne ->
	begin
	  if is_real then
	    O.CFrel (O.F_NE, type_to_fbits dest_ty, e0, e1)
	  else
	    O.CIrel (O.NE, type_to_ibits dest_ty, e0, e1)
	end
    | S.Lt ->
	begin
	  if is_real then
	    O.CFrel (O.F_LT, type_to_fbits dest_ty, e0, e1)
	  else if is_signed then
	    O.CIrel (O.LT, type_to_ibits dest_ty, e0, e1)
	  else
	    O.CIrel (O.LTU, type_to_ibits dest_ty, e0, e1)
	end
    | S.Gt ->
	begin
	  if is_real then
	    O.CFrel (O.F_GT, type_to_fbits dest_ty, e0, e1)
	  else if is_signed then
	    O.CIrel (O.GT, type_to_ibits dest_ty, e0, e1)
	  else
	    O.CIrel (O.GTU, type_to_ibits dest_ty, e0, e1)
	end
    | S.Le ->
	begin
	  if is_real then
	    O.CFrel (O.F_LE, type_to_fbits dest_ty, e0, e1)
	  else if is_signed then
	    O.CIrel (O.LE, type_to_ibits dest_ty, e0, e1)
	  else
	    O.CIrel (O.LEU, type_to_ibits dest_ty, e0, e1)
	end
    | S.Ge ->
	begin
	  if is_real then
	    O.CFrel (O.F_GE, type_to_fbits dest_ty, e0, e1)
	  else if is_signed then
	    O.CIrel (O.GE, type_to_ibits dest_ty, e0, e1)
	  else
	    O.CIrel (O.GEU, type_to_ibits dest_ty, e0, e1)
	end
  in ret
       
and mk_bin_arithm cmpstate bop e0 e1 =
  begin
    let env = cmpstate.env
    and eenv = cmpstate.oenv
    in
    let e0_te = get_rval env eenv e0
    and e1_te = get_rval env eenv e1
    in
    let (dest_e0, dest_e1, dest_typ) = 
      match TO.bin_arithm_typ bop e0_te e1_te with
	| (Some (dest_e0, _), Some (dest_e1, _), dest_typ) -> 
	    (dest_e0, dest_e1, dest_typ)
	| (None, Some (dest_e1, _), dest_typ) ->
	    (dest_typ, dest_e1, dest_typ)
	| (Some (dest_e0, _), None, dest_typ) ->
	    (dest_e0, dest_typ, dest_typ)
	| (None, None, dest_typ) ->
	    (dest_typ, dest_typ, dest_typ)
    in
    let (l0, e0') = mk_convertion cmpstate e0 dest_e0
    and (l1, e1') = 
      match bop with
	| S.Add ->
	    begin
	      if TO.is_ptr_typ dest_e1 then
		([], e1)
	      else
		mk_convertion cmpstate e1 dest_e1
	    end
	| S.Sub ->
	    begin
	      if TO.is_ptr_typ dest_e1 then
		([], e1)
	      else
		mk_convertion cmpstate e1 dest_e1
	    end
	| S.Mul
	| S.Band
	| S.Bor
	| S.Bxor
	| S.Div -> mk_convertion cmpstate e1 dest_e1
	| S.Mod -> mk_convertion cmpstate e1 dest_e1
	| S.Shl
	| S.Shr -> mk_convertion cmpstate e1 dest_e1
    in 
    let is_real = TO.is_real_typ dest_typ
    and is_signed =  TO.is_signed_integer_typ dest_typ
    and is_ptr = TO.is_ptr_typ dest_typ
    and e0_is_signed = TO.is_signed_integer_typ e0_te
    in
    let expr = 
      if is_real then
	begin
	  match bop with
	    | S.Add -> 
		O.Fbin (O.F_ADD, type_to_fbits dest_typ, e0', e1')
	    | S.Sub ->
		O.Fbin (O.F_SUB, type_to_fbits dest_typ, e0', e1')
	    | S.Mul ->
		O.Fbin (O.F_MUL, type_to_fbits dest_typ, e0', e1')
	    | S.Div ->
		O.Fbin (O.F_DIV, type_to_fbits dest_typ, e0', e1')
	    | _ -> assert false
	end
      else if is_ptr then
	begin
	  match bop with
	    | S.Add -> 
		O.Ibin (O.ADD, type_to_ibits dest_typ, e0', e1')
	    | S.Sub ->
		O.Ibin (O.SUB, type_to_ibits dest_typ, e0', e1')
	    | _ -> assert false
	end
      else 
	begin
	  let bop = 
	    match bop with
	      | S.Add -> O.ADD
	      | S.Sub -> O.SUB
	      | S.Mul -> O.MUL (*if is_signed then O.MUL else O.MULU*)
 	      | S.Div -> if is_signed then O.DIV else O.DIVU
	      | S.Mod -> if is_signed then O.MOD else O.MODU
	      | S.Band -> O.BAND
	      | S.Bor -> O.BOR
	      | S.Bxor -> O.BXOR
	      | S.Shl -> O.SHL
	      | S.Shr -> if e0_is_signed then O.SHR else O.SHRU
	  in O.Ibin (bop, type_to_ibits dest_typ, e0', e1')
	end
    in
    (l0 @ l1, expr)
  end
    
and mk_una_expr cmpstate uop e0 =
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  let e0_te = get_rval env eenv e0
  in
  let dest_ty = e0_te
  in
  let is_real = TO.is_real_typ e0_te
  in
  match uop with
    | S.Neg ->
	begin
	  if is_real then
	    O.Fneg (type_to_fbits dest_ty, e0)
	  else
	    O.Ineg (type_to_ibits dest_ty, e0)
	end
    | S.Bnot ->
	begin
	  if is_real then
	    assert false 
	  else
	    O.Ibnot (type_to_ibits dest_ty, e0)
	end

and mk_bin_rel cmpstate bop e0 e1 =
  let env = cmpstate.env
  and eenv = cmpstate.oenv
  in
  let e0_te = get_rval env eenv e0
  and e1_te = get_rval env eenv e1
  in
  let (_, _, dest_ty) = TO.bin_rel_typ e0_te e1_te
  in
  let (l0, e0') = mk_convertion cmpstate e0 dest_ty
  and (l1, e1') = mk_convertion cmpstate e1 dest_ty
  in
  let is_real = TO.is_real_typ dest_ty
  and is_signed = 
    TO.is_signed_integer_typ e0_te & TO.is_signed_integer_typ e1_te
  in
  let ret = match bop with
    | S.Eq ->
	begin
	  if is_real then
	    O.Frel (O.F_EQ, type_to_fbits dest_ty, e0', e1')
	  else
	    O.Irel (O.EQ, type_to_ibits dest_ty, e0', e1')
	end
    | S.Ne ->
	begin
	  if is_real then
	    O.Frel (O.F_NE, type_to_fbits dest_ty, e0', e1')
	  else
	    O.Irel (O.NE, type_to_ibits dest_ty, e0', e1')
	end
    | S.Lt ->
	begin
	  if is_real then
	    O.Frel (O.F_LT, type_to_fbits dest_ty, e0', e1')
	  else if is_signed then
	    O.Irel (O.LT, type_to_ibits dest_ty, e0', e1')
	  else
	    O.Irel (O.LTU, type_to_ibits dest_ty, e0', e1')
	end
    | S.Gt ->
	begin
	  if is_real then
	    O.Frel (O.F_GT, type_to_fbits dest_ty, e0', e1')
	  else if is_signed then
	    O.Irel (O.GT, type_to_ibits dest_ty, e0', e1')
	  else
	    O.Irel (O.GTU, type_to_ibits dest_ty, e0', e1')
	end
    | S.Le ->
	begin
	  if is_real then
	    O.Frel (O.F_LE, type_to_fbits dest_ty, e0', e1')
	  else if is_signed then
	    O.Irel (O.LE, type_to_ibits dest_ty, e0', e1')
	  else
	    O.Irel (O.LEU, type_to_ibits dest_ty, e0', e1')
	end
    | S.Ge ->
	begin
	  if is_real then
	    O.Frel (O.F_GE, type_to_fbits dest_ty, e0', e1')
	  else if is_signed then
	    O.Irel (O.GE, type_to_ibits dest_ty, e0', e1')
	  else
	    O.Irel (O.GEU, type_to_ibits dest_ty, e0', e1')
	end
  in (l0 @ l1, ret)

and mk_bin_logic bop e0 e1 =
  match bop with
    | S.And -> O.Logic (O.AND, e0, e1)
    | S.Or  -> O.Logic (O.OR, e0, e1)
	
and compile_const (t, v) = (t, v)

and compile_rval_ cmpstate e =
  match e with
    | I.Rladdr (I.Nlbl ce) -> O.Rladdr (O.Nlbl ce)
    | I.Rladdr (I.Nctn ce) -> O.Rladdr (O.Nctn ce) (*laddr -> assert false*)
    | I.Rreg s -> O.Rreg s (*assert false*)
    | I.Rindir s -> O.Rindir s
    | I.Rfun s -> O.Rfun s
    | I.Rvct s -> O.Rvct s
    | I.Rbyte_value s -> O.Rbyte_value s
    | I.Rcode_label s -> O.Rcode_label s
    | I.Rconst c_const040 -> O.Rconst (compile_const c_const040)
    | I.Rsizeof (c_type, c_const040) ->
	O.Rsizeof(c_type, compile_const c_const040)
    | I.Ralignof (c_type, c_const040) ->
	O.Ralignof (c_type, compile_const c_const040)
    | I.Rvoid -> O.Rvoid
    | I.Rcexpr expr -> 
	O.Rcexpr (compile_cexpr cmpstate expr)
	  
and compile_rval cmpstate (expr:I.rval) = 
  compile_rval_ cmpstate expr

and compile_lval cmpstate e = 
  match e with
    | I.Lladdr (I.Nlbl ce) -> O.Lladdr (O.Nlbl ce)
    | I.Lladdr (I.Nctn ce) -> O.Lladdr (O.Nctn ce)
    | I.Lreg ce -> O.Lreg ce
	

and compile_cexpr cmpstate (expr:I.cexpr) =
  compile_cexpr_ cmpstate expr
    
and mk_cexpr_rval env eenv e = I.Rcexpr e

and compile_cexpr_ cmpstate (expr:I.cexpr) = 
  match expr with
    | I.CBinary_arithm (bop, expr0, expr1) ->
	let expr0 = compile_cexpr cmpstate expr0
	and expr1 = compile_cexpr cmpstate expr1
	in (mk_cexpr_bin_arithm cmpstate bop expr0 expr1)
	     
    | I.CBinary_predicate (binary_predicate, expr0, expr1) ->
	let expr0 = compile_cexpr cmpstate expr0
	and expr1 = compile_cexpr cmpstate expr1
	in (mk_cexpr_bin_rel cmpstate binary_predicate expr0 expr1)
	     
    | I.CBinary_logic (binary_logic_connect, expr0, expr1) ->
	let expr0 = compile_cexpr cmpstate expr0
	and expr1 = compile_cexpr cmpstate expr1
	in 
	Ast_eb_expr_op.rexpr__to_cexpr_
	  (mk_bin_logic binary_logic_connect 
	    (AO.cexpr_to_rval expr0) (AO.cexpr_to_rval expr1))
	  
    | I.CUnary_arithm (uop, expr) ->
	let expr = compile_cexpr cmpstate expr
	in 
	Ast_eb_expr_op.rexpr__to_cexpr_
	  (mk_una_expr cmpstate uop (AO.cexpr_to_rval expr))
	  
    | I.CLogic_not expr -> 
	let expr = compile_cexpr cmpstate expr
	in O.CLogic_not expr
	     
    | I.CCast (c_type_name, expr) -> 
	let expr = compile_cexpr cmpstate expr
	in O.CCast (c_type_name, expr)

    | I.CQuestion (cond, expr0, expr1) -> 
	O.CQuestion (compile_cexpr cmpstate cond, compile_cexpr cmpstate expr0,
	compile_cexpr cmpstate expr1)
	  
    | I.Cconst c_const040  ->
	O.Cconst (compile_const c_const040)
	  
    | I.Csizeof (t, c_const040) ->
	O.Csizeof (t, compile_const c_const040)
	  
    | I.Calignof (t, c_const040) ->
	O.Calignof (t, compile_const c_const040)
	  
    | I.Cvct str ->  O.Cvct str
    | I.Cvar_lbl str -> O.Cvar_lbl str
    | I.Ccode_lbl str -> O.Ccode_lbl str
    | I.Cfun_lbl str -> O.Cfun_lbl str
	

and compile_rexpr cmpstate (expr:I.rexpr) = 
  compile_rexpr_ cmpstate expr

and compile_rexpr_ cmpstate (expr:I.rexpr) = 
  let env = cmpstate.env
  in
  match expr with
    | I.Rval rval ->
	([], O.Rval (compile_rval cmpstate rval))

    | I.Binary_arithm (bop, expr0, expr1) ->
	let expr0 = compile_rval cmpstate expr0
	and expr1 = compile_rval cmpstate expr1
	in 
	mk_bin_arithm cmpstate bop expr0 expr1
	  
    | I.Binary_predicate (bop, expr0, expr1) ->
	let expr0 = compile_rval cmpstate expr0
	and expr1 = compile_rval cmpstate expr1
	in 
	mk_bin_rel cmpstate bop expr0 expr1
	  
    | I.Binary_logic (bop, expr0, expr1) ->
	let expr0 = compile_rval cmpstate expr0
	and expr1 = compile_rval cmpstate expr1
	in 
	let (l0, expr0') = mk_convertion cmpstate expr0 env.E.int_typ
	and (l1, expr1') = mk_convertion cmpstate expr1 env.E.int_typ
	in
	(l0 @ l1, mk_bin_logic bop expr0' expr1')
	  
    | I.Unary_arithm (uop, expr) ->
	let expr = compile_rval cmpstate expr
	in ([], mk_una_expr cmpstate uop expr)
	     
    | I.Logic_not expr -> 
	let expr = compile_rval cmpstate expr
	in ([], O.Logic_not expr)
	     
    | I.Cast (c_type, rexpr) ->
	let rval = compile_rval cmpstate rexpr
	in 
	let (l, rval) = mk_convertion cmpstate rval c_type
	in (l, O.Rval_cast (c_type, rval))

    | I.Ne_cast (c_type, v0, v1) ->
	let (l, rexpr_) = compile_rexpr_ cmpstate
	  (I.Binary_predicate (C_semantics_symbol.Ne, v0, v1))
	in
	let rexpr = rexpr_
	in
	let (l1, rval) = rexpr_to_rval cmpstate rexpr_
	in
	let (l0, rval) = mk_convertion cmpstate rval c_type
	in (l @ l1 @ l0, O.Rval_cast (c_type, rval))

and compile_expr cmpstate (expr:I.expr) =
  let env = cmpstate.env
  and eenv = cmpstate.ienv
  in
  match expr with
    | I.Rexpr expr -> 
	let (l, expr) = compile_rexpr cmpstate expr
	in (l, O.Rexpr expr)
	     
    | I.Macro_va_start (expr0, expr1) -> 
	let expr0 = compile_rval cmpstate expr0
	and expr1 = compile_rval cmpstate expr1
	in ([], O.Macro_va_start (expr0, expr1))
	     
    | I.Macro_va_end expr0 -> 
	let expr0 = compile_rval cmpstate expr0
	in ([], O.Macro_va_end expr0)
	     
    | I.Macro_va_arg (lval, expr0, c_type) ->
	let expr0 = compile_rval cmpstate expr0
	in 
	([], O.Macro_va_arg 
	  (compile_lval cmpstate lval, expr0, c_type))
	  
    | I.Assign (expr0, expr1) -> 
	let t0 = AEE.get_lval env eenv expr0
	and t1 = AEE.get_rexpr env eenv expr1
	in
	let t0 = TO.normalize_bit_typ t0
	and t1 = TO.normalize_bit_typ t1
	in
	let s0 = TO.static_sizeof t0
	and s1 = TO.static_sizeof t1
	in
	assert (s0 = s1);
	if (TO.is_native_assign_supported_typ t0) || 
	  (TO.is_native_assign_supported_typ t1) then
	    let expr0 = compile_lval cmpstate expr0
	    and (l, expr1) = compile_rexpr cmpstate expr1
	    in (l, O.Assign (expr0, expr1))
	else 
	  assert false
	    
    | I.Memcpy (lhs, rhs, size) ->
	let lhs = compile_rval cmpstate lhs
	and rhs = compile_rval cmpstate rhs
	and size = compile_cexpr cmpstate size
	in ([], O.Memcpy (lhs, rhs, size))

    | I.Alloca (lval, te, rexpr) ->
	let lval = compile_lval cmpstate lval
	and (l, rexpr) = compile_rexpr cmpstate rexpr
	in (l, O.Alloca (lval, te, rexpr))
	     
and addrof_lval e:I.rexpr = 
  let v =  match e with
    | I.Lladdr laddr -> assert false
    | I.Lreg ce -> assert false
  in I.Rval v

and addrof_rexpr e = 
  let v = match e with
    | I.Rval rval -> I.Rval (addrof_rval rval)
    | I.Cast (t, rval) -> I.Rval (addrof_rval rval)
    | _ -> assert false
  in v

and addrof_rval e =
  let v = match e with
    | I.Rindir s -> assert false
    | _ -> assert false
  in v

and compile_c_constant_expression cmpstate
    (expr:I.c_constant_expression) :O.c_constant_expression = 
  compile_cexpr cmpstate expr
    
(** the following are standard copy translation **)

and compile_linkage e =
  match e with
    | I.Default_extern -> O.Default_extern
    | I.Default_storage -> O.Default_storage
    | I.Extern -> O.Extern
    | I.Extern_Inline -> O.Extern_Inline
    | I.Auto -> O.Auto
    | I.Static -> O.Static
    | I.Static_Inline -> O.Static_Inline
    | I.Register -> O.Register
    | I.Inline -> O.Inline
    | I.Type_alias -> O.Type_alias
    | I.Thread -> O.Thread
    | I.Extern_Thread -> O.Extern_Thread
    | I.Static_Thread -> O.Static_Thread
	
and compile_c_declaration cmpstate (expr:I.c_declaration) :O.c_declaration = 
  match expr with
    | I.Str_decl_init (linkage, ce, str_literal) ->
	O.Str_decl_init (compile_linkage linkage, ce, str_literal)
	  
    | I.Obj_decl (linkage, ce) -> O.Obj_decl (compile_linkage linkage, ce)
	
    | I.Obj_decl_init (linkage, ce, c_initializer) ->
	O.Obj_decl_init (compile_linkage linkage, ce, 
	compile_c_initializer cmpstate c_initializer)
	  
    | I.Type_def c_type -> O.Type_def c_type
    | I.Type_decl c_type -> O.Type_decl c_type
	
    | I.Type_only c_type -> O.Type_only c_type
	
and compile_c_local_declaration cmpstate 
    (expr:I.c_local_declaration) :O.c_local_declaration = 
  match expr with
    | I.Local_obj_decl (linkage, ce) ->
	O.Local_obj_decl (compile_linkage linkage, ce)
	  
    | I.Local_obj_decl_init (linkage, ce, c_initializer) ->
	O.Local_obj_decl_init (compile_linkage linkage, 
	ce, compile_c_initializer cmpstate c_initializer)
	  
    | I.Local_type_def c_type -> O.Local_type_def c_type
	
    | I.Local_type_decl c_type -> O.Local_type_decl c_type
	
    | I.Local_type_only c_type -> O.Local_type_only c_type
	
    | I.Local_register ce -> O.Local_register ce
	
and expr_has_side_effect (expr:I.expr) = 
  match expr with
    | I.Rexpr expr -> false
    | I.Macro_va_start (expr0, expr1) -> true
    | I.Macro_va_end expr0 -> true
    | I.Macro_va_arg (lval, expr0, c_type) -> true
    | I.Assign (expr0, expr1) -> true
    | I.Memcpy _ -> true
    | I.Alloca (expr0, c_type, expr1) -> true
	
and compile_true_cond cmpstate = function
  | I.NEQ_ZERO rval -> ([], O.NEQ_ZERO (compile_rval cmpstate rval))
  | I.EQ_ZERO rval -> ([], O.EQ_ZERO (compile_rval cmpstate rval))
  | I.PRED (brel, v0, v1) -> 
      begin
	let expr0 = compile_rval cmpstate v0
	and expr1 = compile_rval cmpstate v1
	in 
	let (l, xrel) = mk_bin_rel cmpstate brel expr0 expr1
	in
	let pred = match xrel with
	  | O.Irel (rel, bits, v0, v1) ->
	      O.IPRED (rel, bits, v0, v1)
	  | O.Frel (rel, bits, v0, v1) ->
	      O.FPRED (rel, bits, v0, v1)
	  | _ -> assert false
	in (l, pred)
      end
	
and compile_c_stmt010 cmpstate (expr:I.c_stmt010) :O.c_stmt010 =
  match expr with
    | I.STMT_SPAN (str, stmt) -> 
	O.STMT_SPAN (str, compile_c_stmt010 cmpstate stmt)
	  
    | I.STMT_AT (coord, stmt) -> 
	O.STMT_AT (coord, compile_c_stmt010 cmpstate stmt)
	  
    | I.NOP -> O.NOP
	
    | I.COMPUTATION (c_expression) ->
	if (expr_has_side_effect c_expression) then
	  let (l, e) = compile_expr cmpstate c_expression
	  in O.SEQUENCE (None, l @ [O.SESE [e]])
	else
	  O.NOP
	    
    | I.SESE exprs ->
	let l = 
	  List.fold_left 
	    (fun l v -> 
	      let (l0, e) = compile_expr cmpstate v
	      in O.SEQUENCE (None, l0 @ [O.SESE [e]])::l
	    ) [] exprs
	in O.SEQUENCE (None, Safe_list.rev l)
	     
    | I.SEQUENCE (txt_opt, c_stmt010_list) ->
	O.SEQUENCE (txt_opt, Safe_list.map 
	  (compile_c_stmt010 cmpstate) c_stmt010_list)
	  
    | I.COMPOUND (txt_opt, c_compound_stmt010) ->
	O.COMPOUND 
	  (txt_opt, compile_c_compound_stmt010 cmpstate c_compound_stmt010)
	  
    | I.IF (expr0, then_c_stmt010, else_c_stmt010) ->
	begin
	  let (l, expr0) = compile_true_cond cmpstate expr0
	  in
	  let then_c_stmt010 = compile_c_stmt010 cmpstate then_c_stmt010
	  in
	  let else_c_stmt010 = compile_c_stmt010 cmpstate else_c_stmt010
	  in 
	  let if_stmt = O.IF (expr0, then_c_stmt010, else_c_stmt010)
	  in
	  match l with
	    | [] -> if_stmt 
	    | _ -> O.SEQUENCE (None, l @ [if_stmt])
	end
	  
    | I.WHILE (expr0, c_stmt010) -> 
	begin
	  let (l, expr0) = compile_true_cond cmpstate expr0
	  in
	  let body_stmt = compile_c_stmt010 cmpstate c_stmt010
	  in
	  match l with
	    | [] -> O.WHILE (expr0, body_stmt)
	    | _ -> 
		O.LOOP (O.COMPOUND 
		  (None, O.BLOCK ([], [], 
		  [O.SEQUENCE (None, l @$ 
		    [O.IF (expr0, body_stmt, O.BREAK)])])))
	end
	  
    | I.LOOP (c_stmt010) -> 
	O.LOOP (compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.BREAK -> O.BREAK
    | I.CONTINUE -> O.CONTINUE
    | I.RETURN_VALUE (c_expression) -> 
	let (l, rexpr) = compile_rexpr cmpstate c_expression
	in O.SEQUENCE (None, l @ [O.RETURN_VALUE rexpr])
	     
    | I.RETURN -> O.RETURN
	
    | I.EPI str_opt -> 
	let str_opt = Mapping.map_opt (compile_rval cmpstate) str_opt
	in O.EPI str_opt
	     
    | I.SWITCH (c_expression, c_stmt010) ->
	O.SWITCH (compile_rval cmpstate c_expression, 
	compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.CASE (c_constant_expression, c_stmt010) ->
	O.CASE 
	  (compile_c_constant_expression cmpstate c_constant_expression, 
	  compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.CASE_RANGE (e0, e1, c_stmt010) ->
	O.CASE_RANGE 
	  (compile_c_constant_expression cmpstate e0,
	  compile_c_constant_expression cmpstate e1,
	  compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.DEFAULT (c_stmt010) ->
	O.DEFAULT (compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.LABEL (string, c_stmt010) ->
	O.LABEL (string, compile_c_stmt010 cmpstate c_stmt010)
	  
    | I.GOTO (string) -> 
	O.GOTO (string)

    | I.GCC_GOTO expr ->
	O.GCC_GOTO (compile_rval cmpstate expr)

    | I.ASM (str_list, asm_details_opt) ->
	let asm_details_opt = match asm_details_opt with
	  | Some asm_details ->
	      let fl (so, s, e) = 
		(so, s, compile_lval cmpstate e)
	      and fv (so, s, e) = 
		(so, s, compile_rval cmpstate e)
	      in
	      Some 
		({
		  O.asm_outputs = Safe_list.map fl asm_details.I.asm_outputs;
		  O.asm_inputs = Safe_list.map fv asm_details.I.asm_inputs;
		  O.asm_clobbers = asm_details.I.asm_clobbers;
		})
	  | None -> None
	in O.ASM (str_list, asm_details_opt)
	     
    | I.CALL (lval_opt, expr0, expr_list) -> 
	let expr0 = compile_rval cmpstate expr0
	and expr_list = Safe_list.map (compile_rval cmpstate) expr_list
	in O.CALL 
	     (Mapping.map_opt (compile_lval cmpstate) lval_opt, expr0, expr_list)

and compile_c_compound_stmt010 cmpstate (expr:I.c_compound_stmt010)
    :O.c_compound_stmt010 = 
  let I.BLOCK (labels, decls, stmts) = expr
  in
  let env = cmpstate.env
  in
  let _ = EO.begin_block env
  in
  let decls = compile_c_local_declaration_list cmpstate decls
  and stmts = Safe_list.map (compile_c_stmt010 cmpstate) stmts
  in      
  let (new_tmp_syms, new_typs) = EO.end_block env
  in
  let new_decls = 
    Safe_list.map (fun (t, s) -> O.Local_register s) new_tmp_syms
  and new_ty_decls = 
    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
  in O.BLOCK (labels, decls @ new_ty_decls @ new_decls , stmts)

and compile_c_translation_unit env (c_translation_unit:I.c_translation_unit)
    :O.c_translation_unit = 
  let expr_env = 
      {
	O.rexpr_env = O.RexprHashtbl.create 17;
	O.cexpr_env = O.CexprHashtbl.create 17;
	O.rval_env = O.RvalHashtbl.create 17;
	O.lval_env = O.LvalHashtbl.create 17;
      }
  in  
  match c_translation_unit with
    | I.Translation_unit (l,eenv) ->
	let cmpstate = 
	  {
	    env = env;
	    ienv = eenv;
	    oenv = expr_env;
	  }
	in
	O.Translation_unit 
	  ((Safe_list.map
	    (fun external_declaration -> 
	      compile_c_external_declaration cmpstate external_declaration
	    ) l), expr_env)
	  
and compile_c_external_declaration cmpstate (expr:I.c_external_declaration) 
    :O.c_external_declaration =
  match expr with
    | I.External_declaration_at (coord, expr) -> 
	O.External_declaration_at 
	  (coord, compile_c_external_declaration cmpstate expr)
    | I.External_declaration_1 (c_function_definition) ->
	O.External_declaration_1 
	  (compile_c_function_definition cmpstate c_function_definition)
    | I.External_declaration_2 (c_declaration) ->
	O.External_declaration_2 
	  (Safe_list.map (compile_c_declaration cmpstate) c_declaration)

and compile_c_function_definition cmpstate (expr:I.c_function_definition)
    :O.c_function_definition =
  let env = cmpstate.env
  in
  match expr with
    | I.Function_definition (linkage, fun_type, fun_name, c_compound_stmt010) 
      ->
	let _ = EO.begin_function "_mlitecc_eb_" env fun_name.QN.qn_sname 
	in
	let c_compound_stmt010 = 
	  compile_c_compound_stmt010 cmpstate c_compound_stmt010
	in
	let _ = EO.end_function env
	in
	O.Function_definition 
	  (compile_linkage linkage, fun_type, fun_name, c_compound_stmt010)
	  
and compile_c_local_declaration_list cmpstate 
    (c_declaration_list:I.c_local_declaration list)
    :O.c_local_declaration list = 
  Safe_list.map 
    (compile_c_local_declaration cmpstate) c_declaration_list
       
and compile: string -> I.c_translation_unit -> E.env -> 
  O.c_translation_unit =
  fun basename c_translation_unit env ->    
    let _ = EO.begin_file env basename   
    in
    let c_translation_unit = 
      compile_c_translation_unit env c_translation_unit
    in
    let (new_tmp_syms, new_typs) = EO.end_file env
    in c_translation_unit 
	 
