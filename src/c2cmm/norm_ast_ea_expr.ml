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
module O = Ast_ea_expr
module TO = Tent_op
module CEO = Cent_op
module CE = Cent
module E = Senv
module S = C_semantics_symbol

open Ast_ea_expr

let disable_normalization = ref false
let enable_long_long = ref false
let enable_long_double = ref false

let user_opts = 
[
  ("--ast-ea-expr-disable-norm",
  Arg.Set disable_normalization,
  "disable normalization of ast-ea-expr which eliminates typedef");
  ("--long-long", Arg.Unit (fun () -> enable_long_long := true), 
  "Allow long long int");
  ("--long-double", Arg.Unit (fun () -> enable_long_double := true), 
  "Allow long double");
]

let check_type te = 
  if (not !enable_long_long) & (TO.is_llong_te te or TO.is_ullong_te te) then
    camlp4_macro_exception "long long int is not supported\n";
  if (not !enable_long_double) & TO.is_long_double_typ te then
    camlp4_macro_exception "long double is not supported\n"
      
    
exception VoidExpr

let mk_rexpr rval = O.Rval rval
    
let normalize (env:Senv.env) (c_translation_unit:c_translation_unit):c_translation_unit = 
  let rec normalize_const (t, v) = 
    (normalize_c_type t, v)
      
  and normalize_rval_ e =
    match e with
      | Rladdr laddr -> e
      | Rreg s -> e
      | Rindir _
      | Rfun _
      | Rvct _	  
      | Rbyte_value _ 
      | Rcode_label _ -> e
      | Rconst c_const040 -> Rconst (normalize_const c_const040)
      | Rsizeof (c_type, c_const040) ->
	  Rsizeof(normalize_c_type c_type, normalize_const c_const040)
      | Ralignof (c_type, c_const040) ->
	  Ralignof (normalize_c_type c_type, normalize_const c_const040)
      | Rcexpr cexpr ->
	  Rcexpr (normalize_cexpr cexpr)
      | Rvoid -> 
	  raise VoidExpr
	    
  and normalize_rval (expr:rval) = 
    normalize_rval_ expr

  and normalize_lval (expr:lval) = 
    expr
      
  and normalize_cexpr (expr:cexpr) = 
    normalize_cexpr_ expr
      
  and normalize_cexpr_ (expr:cexpr) = 
    match expr with
      | CBinary_arithm (binary_arithmatic, expr0, expr1) ->
	  let expr0 = normalize_cexpr expr0
	  and expr1 = normalize_cexpr expr1
	  in CBinary_arithm (binary_arithmatic, expr0, expr1)
	       
      | CBinary_predicate (binary_predicate, expr0, expr1) ->
	  let expr0 = normalize_cexpr expr0
	  and expr1 = normalize_cexpr expr1
	  in CBinary_predicate (binary_predicate, expr0, expr1)
	       
      | CBinary_logic (binary_logic_connect, expr0, expr1) ->
	  let expr0 = normalize_cexpr expr0
	  and expr1 = normalize_cexpr expr1
	  in CBinary_logic (binary_logic_connect, expr0, expr1)
	       
      | CUnary_arithm (unary_arithmatic, expr) ->
	  let expr = normalize_cexpr expr
	  in CUnary_arithm (unary_arithmatic, expr)
	       
      | CLogic_not expr -> 
	  let expr = normalize_cexpr expr
	  in CLogic_not expr
	       
      | CCast (te, expr) -> 
	  let expr = normalize_cexpr expr
	  in CCast (normalize_c_type te, expr)

      | CQuestion (cond, expr0, expr1) -> 
	  CQuestion (normalize_cexpr cond, normalize_cexpr expr0,
	  normalize_cexpr expr1)
	    
      | Cconst c_const040  ->
	  Cconst (normalize_const c_const040)
	    
      | Csizeof (t, c_const040) ->
	  Csizeof (normalize_c_type t, normalize_const c_const040)
	    
      | Calignof (t, c_const040) ->
	  Calignof (normalize_c_type t, normalize_const c_const040)
	    
      | Cvct str ->  Cvct str
      | Cvar_lbl str -> Cvar_lbl str
      | Ccode_lbl str -> Ccode_lbl str
      | Cfun_lbl str -> Cfun_lbl str
	  

  and normalize_rexpr (expr:rexpr) = 
    normalize_rexpr_ expr

  and normalize_rexpr_ (expr:rexpr) = 
    match expr with
      | Rval rval ->
	  Rval (normalize_rval rval)

      | Binary_arithm (binary_arithmatic, expr0, expr1) ->
	  let expr0 = normalize_rval expr0
	  and expr1 = normalize_rval expr1
	  in Binary_arithm (binary_arithmatic, expr0, expr1)
	       
      | Binary_predicate (binary_predicate, expr0, expr1) ->
	  let expr0 = normalize_rval expr0
	  and expr1 = normalize_rval expr1
	  in Binary_predicate (binary_predicate, expr0, expr1)
	       
      | Binary_logic (binary_logic_connect, expr0, expr1) ->
	  let expr0 = normalize_rval expr0
	  and expr1 = normalize_rval expr1
	  in Binary_logic (binary_logic_connect, expr0, expr1)
	       
      | Unary_arithm (unary_arithmatic, expr) ->
	  let expr = normalize_rval expr
	  in (Unary_arithm (unary_arithmatic, expr))
	       
      | Logic_not expr -> 
	  let expr = normalize_rval expr
	  in Logic_not expr
	       
      | Cast (te, rexpr) ->
	  Cast (normalize_c_type te, normalize_rval rexpr)

      | Ne_cast (te, e0, e1) ->
	  Ne_cast (normalize_c_type te, 
	  normalize_rval e0, normalize_rval e1)
	    
  and normalize_expr (expr:expr) = 
    match expr with
      | Rexpr expr -> Rexpr (normalize_rexpr expr)
	  
      | Macro_va_start (expr0, expr1) -> 
	  let expr0 = normalize_rval expr0
	  and expr1 = normalize_rval expr1
	  in Macro_va_start (expr0, expr1)
	       
      | Macro_va_end expr0 -> 
	  let expr0 = normalize_rval expr0
	  in Macro_va_end expr0
	       
      | Macro_va_arg (lval, expr0, c_type) ->
	  let expr0 = normalize_rval expr0
	  in
	  Macro_va_arg 
	    (normalize_lval lval, expr0, 
	    normalize_c_type c_type)
	    
      | Assign (expr0, expr1) -> 
	  let expr0 = normalize_lval expr0
	  and expr1 = normalize_rexpr expr1
	  in Assign (expr0, expr1)

      | Memcpy (expr0, expr1, cexpr) -> 
	  let expr0 = normalize_rval expr0
	  and expr1 = normalize_rval expr1
	  and cexpr = normalize_cexpr cexpr
	  in Memcpy (expr0, expr1, cexpr)

      | Alloca (expr0, c_type, expr1) -> 
	  let expr0 = normalize_lval expr0
	  and expr1 = normalize_rexpr expr1
	  in Alloca (expr0, normalize_c_type c_type, expr1)

  and expr_has_side_effect (expr:expr) = 
    match expr with
      | Rexpr expr -> false
      | Macro_va_start (expr0, expr1) -> true
      | Macro_va_end expr0 -> true
      | Macro_va_arg (lval, expr0, c_type) -> true
      | Assign (expr0, expr1) -> true
      | Memcpy _ -> true
      | Alloca (expr0, c_type, expr1) -> true

  and normalize_c_constant_expression: c_constant_expression -> 
  c_constant_expression = 
    fun expr -> normalize_cexpr expr
      
  (** the following are standard copy translation **)

  and normalize_c_type te = 
    let te = TO.normalize_decl_typ te
    in TO.norm_ptr_bit (TO.stripoff_const_qualifier te)
	 
  and normalize_decl_type te = 
    let te = TO.normalize_decl_typ te
    in TO.stripoff_const_qualifier te
	 
  and normalize_ce ce = 
    let ce_info = CEO.ce_info_of ce
    in ce_info.CE.ce_te <- normalize_c_type ce_info.CE.ce_te;
    ce

  and normalize_decl_ce ce = 
    let ce_info = CEO.ce_info_of ce
    in ce_info.CE.ce_te <- normalize_decl_type ce_info.CE.ce_te;
    ce
      
  and normalize_c_declaration: c_declaration -> c_declaration list = 
    fun expr -> 
      match expr with
	| Str_decl_init (linkage, ce, str_literal) ->
	    [Str_decl_init (linkage, normalize_decl_ce ce, str_literal)]
	      
	| Obj_decl (linkage, ce) ->
	    [Obj_decl (linkage, normalize_decl_ce ce)]
	      
	| Obj_decl_init (linkage, ce, c_initializer) 
	  ->
	    begin
	      let ce = normalize_ce ce
	      in
	      let c_initializer = normalize_c_initializer c_initializer
	      in
	      [Obj_decl_init (linkage, ce, c_initializer)]
	    end

	| Type_def c_type ->
	    []
	      
	| Type_decl c_type ->
	    [Type_decl (normalize_c_type c_type)]
	      
	| Type_only c_type ->
	    [Type_only (normalize_c_type c_type)]


  and normalize_c_local_declaration: c_local_declaration -> c_local_declaration list = 
    fun expr -> 
      match expr with
	| Local_obj_decl (linkage, ce) ->
	    [Local_obj_decl (linkage, normalize_decl_ce ce)]

	| Local_obj_decl_init (linkage, ce, c_initializer) 
	  ->
	    let ce = normalize_decl_ce ce
	    in
	    let c_initializer = normalize_c_initializer c_initializer
	    in [Local_obj_decl_init (linkage, ce, c_initializer)]
		 
	| Local_type_def c_type ->
	    []
	      
	| Local_type_decl c_type ->
	    [Local_type_decl (normalize_c_type c_type)]
	      
	| Local_type_only c_type ->
	    [Local_type_only (normalize_c_type c_type)]

	| Local_register ce ->
	    [Local_register (normalize_decl_ce ce)]

  and normalize_c_init_expression: c_init_expression -> c_init_expression = 
    fun expr ->
      match expr with
	| Static_init e -> 
	    Static_init (normalize_c_constant_expression e)
	| Static_init_none -> expr

  and normalize_c_initializer: c_initializer -> c_initializer = 
    fun expr ->
      Typ_mem_op.compile_ex 
	normalize_c_type 
	normalize_c_init_expression 
	expr

  and normalize_true_cond = function
    | NEQ_ZERO rval -> NEQ_ZERO (normalize_rval rval)
    | EQ_ZERO rval -> EQ_ZERO (normalize_rval rval)
    | PRED (rel, v0, v1) -> PRED (rel, normalize_rval v0, normalize_rval v1)
	
  and normalize_c_stmt010: c_stmt010 -> c_stmt010 =
    fun expr ->
      match expr with
	| STMT_SPAN (str, stmt) -> STMT_SPAN (str, normalize_c_stmt010 stmt)
	| STMT_AT (coord, stmt) -> STMT_AT (coord, normalize_c_stmt010 stmt)
	| NOP -> NOP
	    
	| COMPUTATION (c_expression) ->
	    if (expr_has_side_effect c_expression) then
	      SESE [normalize_expr c_expression]
	    else
	      NOP
		
	| SESE exprs ->
	    SESE (List.map normalize_expr exprs)
	      
	| SEQUENCE (txt_opt, c_stmt010_list) ->
	    SEQUENCE (txt_opt, Safe_list.map normalize_c_stmt010 c_stmt010_list)
	      
	| COMPOUND (txt_opt, c_compound_stmt010) ->
	    COMPOUND 
	      (txt_opt, normalize_c_compound_stmt010 c_compound_stmt010)
	      
	| IF (expr0, then_c_stmt010, else_c_stmt010) ->
	    let expr0 = normalize_true_cond expr0
	    in
	    let then_c_stmt010 = normalize_c_stmt010 then_c_stmt010
	    in
	    let else_c_stmt010 = normalize_c_stmt010 else_c_stmt010
	    in IF (expr0, then_c_stmt010, else_c_stmt010)
		 
	| WHILE (c_expression, c_stmt010) -> 
	    WHILE (normalize_true_cond c_expression, normalize_c_stmt010 c_stmt010)
	      
	| LOOP (c_stmt010) -> 
	    LOOP (normalize_c_stmt010 c_stmt010)
	      
	| BREAK -> BREAK
	| CONTINUE -> CONTINUE
	| RETURN_VALUE (c_expression) -> 
	    RETURN_VALUE (normalize_rexpr c_expression)

	| RETURN -> RETURN
	    
	| EPI str_opt -> EPI str_opt
	    
	| SWITCH (c_expression, c_stmt010) ->
	    SWITCH (normalize_rval c_expression, 
	    normalize_c_stmt010 c_stmt010)

	| CASE (c_constant_expression, c_stmt010) ->
	    CASE 
	      (normalize_c_constant_expression c_constant_expression, 
	      normalize_c_stmt010 c_stmt010)
	      
	| CASE_RANGE (e0, e1, c_stmt010) ->
	    CASE_RANGE 
	      (normalize_c_constant_expression e0,
	      normalize_c_constant_expression e1,
	      normalize_c_stmt010 c_stmt010)
	      
	| DEFAULT (c_stmt010) ->
	    DEFAULT (normalize_c_stmt010 c_stmt010)
	      
	| LABEL (string, c_stmt010) ->
	    LABEL (string, normalize_c_stmt010 c_stmt010)
	      
	| GOTO (string) -> 
	    GOTO (string)

	| GCC_GOTO expr ->
	    GCC_GOTO (normalize_rval expr)

	| ASM (str_list, asm_details_opt) ->
	    let asm_details_opt = match asm_details_opt with
	      | Some asm_details ->
		  let fl (so, s, e) = 
		    (so, s, normalize_lval e)
		  and fv (so, s, e) = 
		    (so, s, normalize_rval e)
		  in
		  Some 
		    ({
		      asm_outputs = Safe_list.map fl asm_details.asm_outputs;
		      asm_inputs = Safe_list.map fv asm_details.asm_inputs;
		      asm_clobbers = asm_details.asm_clobbers;
		    })
	      | None -> None
	    in ASM (str_list, asm_details_opt)
		 
	| CALL (lval_opt, expr0, expr_list) -> 
	    let expr0 = normalize_rval expr0
	    and expr_list = Safe_list.map (normalize_rval) expr_list
	    in
	    (CALL (Mapping.map_opt (normalize_lval) lval_opt, 
	    expr0, expr_list))

  and normalize_c_compound_stmt010: c_compound_stmt010 -> 
  c_compound_stmt010 = 
    fun expr ->
      let BLOCK (labels, decls, stmts) = expr
      in
      let v = 
	BLOCK 
	  (labels, normalize_c_local_declaration_list decls, 
	  Safe_list.map (normalize_c_stmt010) stmts)
      in v

  and normalize_c_translation_unit: c_translation_unit -> 
  c_translation_unit = 
    fun c_translation_unit ->
      match c_translation_unit with
	| Translation_unit (l,eenv) ->
	    Translation_unit 
	      (Safe_list.map
		(fun external_declaration -> 
		  normalize_c_external_declaration external_declaration
		) l, eenv)
	      
  and normalize_c_external_declaration: c_external_declaration -> 
  c_external_declaration =
    fun expr ->
      match expr with
	| External_declaration_at (coord, expr) -> 
	    External_declaration_at 
	      (coord, normalize_c_external_declaration expr)
	| External_declaration_1 (c_function_definition) ->
	    External_declaration_1 
	      (normalize_c_function_definition 
		c_function_definition)
	| External_declaration_2 (c_declaration) ->
	    External_declaration_2 
	      (Safe_list.flatten (Safe_list.map normalize_c_declaration c_declaration))

  and normalize_c_function_definition 
      (Function_definition (linkage, fun_type, fun_name, c_compound_stmt010)):
      c_function_definition =
    Function_definition 
      (linkage, normalize_c_type fun_type, 
      fun_name, normalize_c_compound_stmt010 c_compound_stmt010)
	      
  and normalize_c_local_declaration_list: c_local_declaration list -> 
  c_local_declaration list = 
    fun c_declaration_list ->
      let v = Safe_list.map 
	(normalize_c_local_declaration) c_declaration_list
      in
      (Safe_list.flatten v)
  in
  if !disable_normalization then
    c_translation_unit
  else
    let _ = TO.canonicalize env.E.te_tbl
    in normalize_c_translation_unit c_translation_unit	  	   
	   
