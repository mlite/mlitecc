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

let const040_map_id f (t, v) = (t, v)

let rec cexpr_map_id f (expr:cexpr) = 
  match expr with
    | CBinary_arithm (binary_arithmatic, expr0, expr1) ->
	let expr0 = cexpr_map_id f expr0
	and expr1 = cexpr_map_id f expr1
	in (CBinary_arithm (binary_arithmatic, expr0, expr1))
	     
    | CBinary_predicate (binary_predicate, expr0, expr1) ->
	let expr0 = cexpr_map_id f expr0
	and expr1 = cexpr_map_id f expr1
	in (CBinary_predicate (binary_predicate, expr0, expr1))
	     
    | CBinary_logic (binary_logic_connect, expr0, expr1) ->
	let expr0 = cexpr_map_id f expr0
	and expr1 = cexpr_map_id f expr1
	in CBinary_logic (binary_logic_connect, expr0, expr1)
	     
    | CUnary_arithm (unary_arithmatic, expr) ->
	let expr = cexpr_map_id f expr
	in (CUnary_arithm (unary_arithmatic, expr))
	     
    | CLogic_not expr -> 
	let expr = cexpr_map_id f expr
	in CLogic_not expr
	     
    | CCast (c_type_name, expr) -> 
	let expr = cexpr_map_id f expr
	in (CCast (c_type_name, expr))
	     
    | CQuestion (cond, expr0, expr1) -> 
	CQuestion (cexpr_map_id f cond, cexpr_map_id f expr0,
	cexpr_map_id f expr1)

    | Cconst c_const040 -> 
	Cconst (const040_map_id f c_const040)
	  
    | Csizeof (ty, c_const040) -> 
	Csizeof (ty, const040_map_id f c_const040)
	  
    | Calignof (ty, c_const040) ->
	Calignof (ty, const040_map_id f c_const040)
	  
    | Cvct str -> Cvct str
	
    | Cvar_lbl str -> Cvar_lbl str
	
    | Ccode_lbl str -> Ccode_lbl str
	
    | Cfun_lbl str -> Cfun_lbl str

	
let rval_map_id f e =
  match e with
    | Rladdr (Nlbl id) -> Rladdr (Nlbl (f id))
    | Rladdr (Nctn id) -> Rladdr (Nctn (f id))
    | Rreg id -> Rreg (f id)
    | Rindir id -> Rindir (f id)
    | Rfun id -> Rfun (f id)
    | Rvct id -> Rvct (f id)
    | Rcode_label _ -> e
    | Rconst c_const040 -> Rconst (const040_map_id f c_const040)
    | Rsizeof (c_type, c_const040) -> e
    | Ralignof (c_type, c_const040) -> e
    | Rvoid -> e
    | Rbyte_value str -> e
    | Rcexpr cexpr ->
	Rcexpr (cexpr_map_id f cexpr)	  
      
	  
let lval_map_id f e = 
  match e with	
    | Lreg ce -> Lreg (f ce)
    | Lladdr (Nlbl id) -> Lladdr (Nlbl (f id))
    | Lladdr (Nctn id) -> Lladdr (Nctn (f id))
	
	
let rec rexpr_map_id f (expr:rexpr) = 
  match expr with
    | Rval rval -> Rval (rval_map_id f rval)
    | Cast (t, v) -> Cast (t, rval_map_id f v)
	
    | Ne_cast (t, v0, v1) -> 
	Ne_cast (t, rval_map_id f v0, rval_map_id f v1)
	
    | Binary_arithm (binary_arithmatic, expr0, expr1) ->
	let expr0 = rval_map_id f expr0
	and expr1 = rval_map_id f expr1
	in (Binary_arithm (binary_arithmatic, expr0, expr1))
	     
    | Binary_predicate (binary_predicate, expr0, expr1) ->
	let expr0 = rval_map_id f expr0
	and expr1 = rval_map_id f expr1
	in (Binary_predicate (binary_predicate, expr0, expr1))
	     
    | Binary_logic (binary_logic_connect, expr0, expr1) ->
	let expr0 = rval_map_id f expr0
	and expr1 = rval_map_id f expr1
	in Binary_logic (binary_logic_connect, expr0, expr1)
	     
    | Unary_arithm (unary_arithmatic, expr) ->
	let expr = rval_map_id f expr
	in (Unary_arithm (unary_arithmatic, expr))
	     
    | Logic_not expr -> 
	let expr = rval_map_id f expr
	in Logic_not expr
	     

let expr_map_id f = function 
  | Rexpr expr -> Rexpr (rexpr_map_id f expr)
      
  | Macro_va_start (expr0, expr1) -> 
      let expr0 = rval_map_id f expr0
      and expr1 = rval_map_id f expr1
      in (Macro_va_start (expr0, expr1))
	   
  | Macro_va_end expr0 -> 
      let expr0 = rval_map_id f expr0
      in (Macro_va_end expr0)	
	   
  | Macro_va_arg (lval, expr0, c_type) ->
      let expr0 = rval_map_id f expr0
      in (Macro_va_arg (lval_map_id f lval, expr0, c_type))
	   
  | Assign (expr0, expr1) -> 
      let expr0 = lval_map_id f expr0
      and expr1 = rexpr_map_id f expr1
      in (Assign (expr0, expr1))

  | Memcpy (lval, expr, cexpr) -> 
      let lval = rval_map_id f lval
      and expr = rval_map_id f expr
      and cexpr = cexpr_map_id f cexpr
      in Memcpy (lval, expr, cexpr)

  | Alloca (expr0, te, expr1) ->
      let expr0 = lval_map_id f expr0
      and expr1 = rexpr_map_id f expr1
      in (Alloca (expr0, te, expr1))

let call_ctrl_map_id f  (lval_opt, e, args) =
  (Mapping.map_opt (lval_map_id f) lval_opt, rval_map_id f e, 
  Safe_list.map (rval_map_id f) args)

let c_init_expression_map_id f = function
  | Static_init e -> Static_init (cexpr_map_id f e)
  | Static_init_none -> Static_init_none
      
let c_initializer_map_id f e =
  Typ_mem_op.compile (c_init_expression_map_id f) e
      
let c_declaration_map_id f = function
  | Str_decl_init (linkage, ce, value) ->
      Str_decl_init (linkage, ce, value)
	
  | Obj_decl (linkage, ce) -> Obj_decl (linkage, ce)
      
  | Obj_decl_init (linkage, ce, c_initializer) ->
      let c_initializer = c_initializer_map_id f c_initializer
      in Obj_decl_init (linkage, ce, c_initializer)
	   
  | Type_def c_type ->
      Type_def (f c_type)
	
  | Type_decl c_type ->
      Type_decl (f c_type)
	
  | Type_only c_type ->
      Type_only (f c_type)


let c_local_declaration_map_id f = function
  | Local_obj_decl (linkage, ce) ->
      Local_obj_decl (linkage, ce)
	
  | Local_obj_decl_init 
      (linkage, ce, c_initializer) ->
      let c_initializer = c_initializer_map_id f c_initializer
      in Local_obj_decl_init (linkage, ce, c_initializer)
	   
  | Local_type_def c_type ->
      Local_type_def (f c_type)
	
  | Local_type_decl c_type ->
      Local_type_decl (f c_type)
	
  | Local_type_only c_type ->
      Local_type_only (f c_type)

  | Local_register ce ->
      Local_register ce



let true_cond_map_id f = function
  | NEQ_ZERO rval -> NEQ_ZERO (rval_map_id f rval)
  | EQ_ZERO rval -> EQ_ZERO (rval_map_id f rval)
  | PRED (brel, v0, v1) -> PRED (brel, rval_map_id f v0, rval_map_id f v1)


let rec c_compound_stmt_map_id f s =
  let BLOCK (strs, c_local_declarations, stmts) = s
  in
  BLOCK (strs, c_local_declarations, 
  Safe_list.map (fun v -> stmt_map_id f v) stmts)
    
and stmt_map_id f = function
  | STMT_SPAN (str, s) -> STMT_SPAN (str, stmt_map_id f s)
  | STMT_AT (cord, s) -> STMT_AT (cord, stmt_map_id f s)
  | NOP -> NOP
  | COMPUTATION expr -> COMPUTATION (expr_map_id f expr)
  | SESE exprs -> SESE (Safe_list.map (fun e -> expr_map_id f e) exprs)
  | SEQUENCE (str_opt, lst) ->
      SEQUENCE (str_opt, Safe_list.map (fun s -> stmt_map_id f s) lst)
	
  | COMPOUND (str_opt, s) ->
      COMPOUND (str_opt, c_compound_stmt_map_id f s)
	
  | IF (rval, s0, s1) -> IF (true_cond_map_id f rval, stmt_map_id f s0, stmt_map_id f s1)
  | WHILE (rval, s0) -> WHILE (true_cond_map_id f rval, stmt_map_id f s0)
  | LOOP s -> LOOP (stmt_map_id f s)
  | BREAK -> BREAK
  | CONTINUE -> CONTINUE
  | RETURN_VALUE e -> RETURN_VALUE (rexpr_map_id f e)
  | RETURN -> RETURN
  | EPI (rval_opt) -> EPI (Mapping.map_opt (fun v -> rval_map_id f v) rval_opt)
  | SWITCH (rval, s) -> SWITCH (rval_map_id f rval, stmt_map_id f s)
  | CASE (ce, s) -> CASE (cexpr_map_id f ce, stmt_map_id f s)
  | CASE_RANGE (ce0, ce1, s) -> 
      CASE_RANGE (cexpr_map_id f ce0, cexpr_map_id f ce1, stmt_map_id f s)
	
  | DEFAULT s -> DEFAULT (stmt_map_id f s)
  | LABEL (str, s) -> LABEL (str, stmt_map_id f s)
  | GOTO str -> GOTO str
  | GCC_GOTO rval -> GCC_GOTO (rval_map_id f rval)
  | ASM (strs, asm_details_opt) -> ASM (strs, asm_details_opt)
  | CALL call_transfer -> CALL call_transfer
