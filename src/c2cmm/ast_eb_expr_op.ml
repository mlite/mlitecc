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

open Ast_eb_expr

let const040_map f (t, v) = (f t, v)
  
let ce_map_te = Cent_op.ce_map_te


let rec cexpr_map ft fc (expr:cexpr) = 
  match expr with
    | CLogic_not expr -> 
	let expr = cexpr_map ft fc expr
	in CLogic_not expr
	     
    | CCast (te, expr) -> 
	let expr = cexpr_map ft fc expr
	in (CCast (ft te, expr))
	     
    | CQuestion (cond, expr0, expr1) -> 
	CQuestion (cexpr_map ft fc cond, cexpr_map ft fc expr0,
	cexpr_map ft fc expr1)
	  
    | Cconst c_const040 -> 
	Cconst (const040_map ft c_const040)
	  
    | Csizeof (te, c_const040) -> 
	Csizeof (ft te, const040_map ft c_const040)
	  
    | Calignof (te, c_const040) ->
	Calignof (ft te, const040_map ft c_const040)
	  
    | Cvct ce -> Cvct (fc ce)
	
    | Cvar_lbl ce -> Cvar_lbl (fc ce)
	
    | Ccode_lbl str -> Ccode_lbl str
	
    | Cfun_lbl ce -> Cfun_lbl (fc ce)

    | CLogic (op, e0, e1) ->
	CLogic (op, cexpr_map ft fc e0, cexpr_map ft fc e1)
	  
    | CFrel (rel, fbits, e0, e1) -> 
	CFrel (rel, fbits, cexpr_map ft fc e0, cexpr_map ft fc e1)
	  
    | CIrel (rel, ibits, e0, e1) ->
	CIrel (rel, ibits, cexpr_map ft fc e0, cexpr_map ft fc e1)
	  
    | CFneg (fbits, e0) ->
	CFneg (fbits, cexpr_map ft fc e0)
	  
    | CFbin (fbin_op, fbits, e0, e1) ->
	CFbin (fbin_op, fbits, cexpr_map ft fc e0, cexpr_map ft fc e1)
	  
    | CIneg (ibits, e0) ->
	CIneg (ibits, cexpr_map ft fc e0)
	  
    | CIbnot (ibits, e0) ->
	CIbnot (ibits, cexpr_map ft fc e0)
	  
    | CIbin (ibin_op, ibits, e0, e1) ->
	CIbin (ibin_op, ibits, cexpr_map ft fc e0, cexpr_map ft fc e1)
	  
    | CPTR_TO_PTR (c_type, rexpr) ->
	CPTR_TO_PTR (ft c_type, cexpr_map ft fc rexpr)
	  
    | CPTR_TO_UI rexpr ->
	CPTR_TO_UI (cexpr_map ft fc rexpr)
	  
    | CF_TO_SI (rexpr,fbits, ibits) ->
	CF_TO_SI (cexpr_map ft fc rexpr, fbits, ibits)
	  
    | CF_TO_UI (rexpr, fbits, ibits) ->
	CF_TO_UI (cexpr_map ft fc rexpr, fbits, ibits)
	  
    | CB_TO_SI (rexpr, fbits, ibits) ->
	CB_TO_SI (cexpr_map ft fc rexpr, fbits, ibits)
	  
    | CB_TO_UI (rexpr, fbits, ibits) ->
	CB_TO_UI (cexpr_map ft fc rexpr, fbits, ibits)
	  
    (* integer types to floating point types *)
    | CSI_TO_F (rexpr, ibits, fbits) ->
	CSI_TO_F (cexpr_map ft fc rexpr, ibits, fbits)
	  
    | CUI_TO_F (rexpr, ibits, fbits) ->
	CUI_TO_F (cexpr_map ft fc rexpr, ibits, fbits)
	  
    | CF_TO_F (rexpr, f0, f1) ->
	CF_TO_F (cexpr_map ft fc rexpr, f0, f1)
	  
    | CSXI (rexpr, srcbits, destbits) ->
	CSXI (cexpr_map ft fc rexpr, srcbits, destbits)
	  
    (* zero extension *)
    | CZXI (rexpr, srcbits, destbits) ->
	CZXI (cexpr_map ft fc rexpr, srcbits, destbits)
	  
    (* truncate *)
    | CLOBITS (rexpr, srcbits, destbits) ->
	CLOBITS (cexpr_map ft fc rexpr, srcbits, destbits)

    

let rval_map ft fc e =
  match e with
    | Rladdr (Nlbl ce) -> Rladdr (Nlbl (fc ce))
    | Rladdr (Nctn ce) -> Rladdr (Nctn (fc ce))
    | Rreg id -> Rreg (fc id)
    | Rindir id -> Rindir (fc id)
    | Rfun id -> Rfun (fc id)
    | Rvct id -> Rvct (fc id)
    (*| Rlbl id -> Rlbl (fc id)*)
    | Rcode_label _ -> e
    | Rconst c_const040 -> Rconst (const040_map ft c_const040)
    | Rsizeof (c_type, c_const040) ->
	Rsizeof(ft c_type, const040_map ft c_const040)
    | Ralignof (c_type, c_const040) ->
	Ralignof (ft c_type, const040_map ft c_const040)
    | Rvoid -> Rvoid
    | Rbyte_value str -> Rbyte_value str
    | Rcexpr expr -> Rcexpr (cexpr_map ft fc expr)


let true_cond_map ft fc e = 
  match e with
    | NEQ_ZERO v -> NEQ_ZERO (rval_map ft fc v)
    | EQ_ZERO v -> EQ_ZERO (rval_map ft fc v)
    | FPRED (brel, bits, v0, v1) -> 
	FPRED (brel, bits, rval_map ft fc v0, rval_map ft fc v1)
    | FNOTPRED (brel, bits, v0, v1) -> 
	FNOTPRED (brel, bits, rval_map ft fc v0, rval_map ft fc v1)
    | IPRED (brel, bits, v0, v1) -> 
	IPRED (brel, bits, rval_map ft fc v0, rval_map ft fc v1)
    
let lval_map ft fc e = 
  match e with
    | Lreg id -> Lreg (fc id)
    | Lladdr (Nlbl ce) -> Lladdr (Nlbl (fc ce))
    | Lladdr (Nctn ce) -> Lladdr (Nctn (fc ce))

let rec rexpr_map ft fc (expr:rexpr) = 
  match expr with
    | Rval_cast (t, v) -> Rval_cast (ft t, rval_map ft fc v)
    | Rval rval -> Rval (rval_map ft fc rval)
	
    | Logic (op, e0, e1) ->
	Logic (op, rval_map ft fc e0, rval_map ft fc e1)
	  
    | Logic_not expr -> 
	let expr = rval_map ft fc expr
	in Logic_not expr
	     
    | Frel (rel, fbits, e0, e1) -> 
	Frel (rel, fbits, rval_map ft fc e0, rval_map ft fc e1)
	  
    | Irel (rel, ibits, e0, e1) ->
	Irel (rel, ibits, rval_map ft fc e0, rval_map ft fc e1)

    | Fneg (fbits, e0) ->
	Fneg (fbits, rval_map ft fc e0)
	  
    | Fbin (fbin_op, fbits, e0, e1) ->
	Fbin (fbin_op, fbits, rval_map ft fc e0, rval_map ft fc e1)
	  
    | Ineg (ibits, e0) ->
	Ineg (ibits, rval_map ft fc e0)
	  
    | Ibnot (ibits, e0) ->
	Ibnot (ibits, rval_map ft fc e0)
	  
    | Ibin (ibin_op, ibits, e0, e1) ->
	Ibin (ibin_op, ibits, rval_map ft fc e0, rval_map ft fc e1)

    | PTR_TO_PTR (c_type, rexpr) ->
	PTR_TO_PTR (ft c_type, rval_map ft fc rexpr)

    | PTR_TO_UI rexpr ->
	PTR_TO_UI (rval_map ft fc rexpr)
	  
    | F_TO_SI (rexpr,fbits, ibits) ->
	F_TO_SI (rval_map ft fc rexpr, fbits, ibits)
	  
    | F_TO_UI (rexpr, fbits, ibits) ->
	F_TO_UI (rval_map ft fc rexpr, fbits, ibits)

    | B_TO_SI (rexpr, fbits, ibits) ->
	B_TO_SI (rval_map ft fc rexpr, fbits, ibits)

    | B_TO_UI (rexpr, fbits, ibits) ->
	B_TO_UI (rval_map ft fc rexpr, fbits, ibits)
	  
    (* integer types to floating point types *)
    | SI_TO_F (rexpr, ibits, fbits) ->
	SI_TO_F (rval_map ft fc rexpr, ibits, fbits)
	  
    | UI_TO_F (rexpr, ibits, fbits) ->
	UI_TO_F (rval_map ft fc rexpr, ibits, fbits)
	  
    | F_TO_F (rexpr, f0, f1) ->
	F_TO_F (rval_map ft fc rexpr, f0, f1)
	  
    | SXI (rexpr, srcbits, destbits) ->
	SXI (rval_map ft fc rexpr, srcbits, destbits)

    (* zero extension *)
    | ZXI (rexpr, srcbits, destbits) ->
	ZXI (rval_map ft fc rexpr, srcbits, destbits)
	  
    (* truncate *)
    | LOBITS (rexpr, srcbits, destbits) ->
	LOBITS (rval_map ft fc rexpr, srcbits, destbits)


let expr_map ft fc e = 
  match e with
    | Rexpr expr -> Rexpr (rexpr_map ft fc expr)
	
    | Macro_va_start (expr0, expr1) -> 
	let expr0 = rval_map ft fc expr0
	and expr1 = rval_map ft fc expr1
	in (Macro_va_start (expr0, expr1))
	     
    | Macro_va_end expr0 -> 
	let expr0 = rval_map ft fc expr0
	in (Macro_va_end expr0)	
	     
    | Macro_va_arg (lval, expr0, c_type) ->
	let expr0 = rval_map ft fc expr0
	in (Macro_va_arg (lval_map ft fc lval, expr0, ft c_type))
	     
    | Assign (expr0, expr1) -> 
	let expr0 = lval_map ft fc expr0
	and expr1 = rexpr_map ft fc expr1
	in (Assign (expr0, expr1))

    | Memcpy (expr0, expr1, csize) -> 
	let expr0 = rval_map ft fc expr0
	and expr1 = rval_map ft fc expr1
	and csize = cexpr_map ft fc csize
	in (Memcpy (expr0, expr1, csize))

    | Alloca (lval, te, expr) ->
	let lval = lval_map ft fc lval
	and te = ft te
	and expr = rexpr_map ft fc expr
	in Alloca (lval, te, expr)

let call_ctrl_map ft fc (lval_opt, e, args) =
  (Mapping.map_opt (lval_map ft fc) lval_opt, rval_map ft fc e, 
  List.map (rval_map ft fc) args)

let c_init_expression_map ft fc = function
  | Static_init e -> Static_init (cexpr_map ft fc e)
  | Static_init_none -> Static_init_none
      
let c_initializer_map ft fc e =
  Typ_mem_op.compile_ex ft (c_init_expression_map ft fc) e
      
let c_declaration_map ft fc = function
  | Str_decl_init (linkage, ce, value) ->
      Str_decl_init (linkage, fc ce, value)

  | Obj_decl (linkage, ce) -> Obj_decl (linkage, fc ce)
	
  | Type_def te -> Type_def (ft te)
	
  | Type_decl te -> Type_decl (ft te)
	
  | Type_only te -> Type_only (ft te)

  | Obj_decl_init (linkage, ce, c_initializer) ->
      Obj_decl_init (linkage, fc ce, c_initializer_map ft fc c_initializer)
	

let c_local_declaration_map ft fc = function
  | Local_obj_decl (linkage, ce) ->
      Local_obj_decl (linkage, fc ce)
	
  | Local_type_def c_type ->
      Local_type_def (ft c_type)
	
  | Local_type_decl c_type ->
      Local_type_decl (ft c_type)
	
  | Local_type_only c_type ->
      Local_type_only (ft c_type)

  | Local_obj_decl_init (linkage, ce, c_initializer) ->
      Local_obj_decl_init 
	(linkage, fc ce, c_initializer_map ft fc c_initializer)
	
  | Local_register ce -> 
      Local_register (fc ce)


let cexpr_to_rval e =
  Rcexpr e

let rval_to_cexpr e = 
  match e with
    | Rcexpr cexpr -> cexpr
    | _ -> assert false
    
let cexpr__to_rexpr_ e = 
  match e with
    | CFbin (fbin_op, fbits, e0, e1) ->
	Fbin (fbin_op, fbits, cexpr_to_rval e0, cexpr_to_rval e1)
	  
    | CIbin (ibin_op, ibits, e0, e1) ->
	Ibin (ibin_op, ibits, cexpr_to_rval e0, cexpr_to_rval e1)
	  
    | CFrel (fbin_op, fbits, e0, e1) ->
	Frel (fbin_op, fbits, cexpr_to_rval e0, cexpr_to_rval e1)
	  
    | CIrel (ibin_op, ibits, e0, e1) ->
	Irel (ibin_op, ibits, cexpr_to_rval e0, cexpr_to_rval e1)
	  
    | CLogic (lop, e0, e1) ->
	Logic (lop, cexpr_to_rval e0, cexpr_to_rval e1)
	  
    | CFneg (fbits, e) -> Fneg (fbits, cexpr_to_rval e)
    | CIneg (ibits, e) -> Ineg (ibits, cexpr_to_rval e)
    | CIbnot (ibits, e) -> Ibnot (ibits, cexpr_to_rval e)
    | _ -> assert false


let rexpr__to_cexpr_ e = 
  match e with
    | Fbin (fbin_op, fbits, e0, e1) ->
	CFbin (fbin_op, fbits, rval_to_cexpr e0, rval_to_cexpr e1)
	  
    | Ibin (ibin_op, ibits, e0, e1) ->
	CIbin (ibin_op, ibits, rval_to_cexpr e0, rval_to_cexpr e1)
	  
    | Frel (fbin_op, fbits, e0, e1) ->
	CFrel (fbin_op, fbits, rval_to_cexpr e0, rval_to_cexpr e1)
	  
    | Irel (ibin_op, ibits, e0, e1) ->
	CIrel (ibin_op, ibits, rval_to_cexpr e0, rval_to_cexpr e1)
	  
    | Logic (lop, e0, e1) ->
	CLogic (lop, rval_to_cexpr e0, rval_to_cexpr e1)
	  
    | Fneg (fbits, e) -> CFneg (fbits, rval_to_cexpr e)
    | Ineg (ibits, e) -> CIneg (ibits, rval_to_cexpr e)
    | Ibnot (ibits, e) -> CIbnot (ibits, rval_to_cexpr e)
    | _ -> assert false	

let fbits_to_float e =
  match e with
    | F32 -> Mach.cfloat_id
    | F64 -> Mach.cdouble_id
    | F80 -> Mach.cldouble_id

let ibits_to_int e signed = 
  match e with
    | I8 -> if signed then Mach.cschar_id else Mach.cuchar_id
    | I16 -> if signed then Mach.cshort_id else Mach.cushort_id
    | I32 -> if signed then Mach.int32_id else Mach.uint32_id
    | I64 -> if signed then Mach.int64_id else Mach.uint64_id

let ibin_op_is_signed e =
  match e with
    | ADD -> true
    | SUB -> true
    | MUL -> true
    | MULU -> false
    | DIV -> true
    | DIVU -> false
    | MOD -> true
    | MODU -> false
    | BAND -> true
    | BOR -> true
    | BXOR -> true
    | SHL -> true
    | SHR -> true
    | SHRU -> true
