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

open Ast_ba_stmt

let disable_normalization = ref false

let user_opts = [
  ("--ast-ba-stmt-disable-norm",
   Arg.Set disable_normalization,
   "disable normalization of stmt010 syntax tree which eliminate unnecessary compound statement");
]


let rec normalize_c_stmt010 ~(must_be_compound:bool) (expr:c_stmt010):c_stmt010 =
  match expr with
    | STMT_AT (coord, stmt) ->
	STMT_AT (coord, normalize_c_stmt010 ~must_be_compound stmt)
    | NOP -> NOP
    | COMPUTATION (c_expression) ->
	COMPUTATION (c_expression)
    | SEQUENCE (txt_opt, c_stmt010_list) ->
	SEQUENCE (txt_opt, 
	Safe_list.map (normalize_c_stmt010 ~must_be_compound:false) 
	  c_stmt010_list)
	  
    | COMPOUND (txt_opt, c_compound_stmt010) ->
	normalize_c_compound_stmt010 
	  ~must_be_compound (txt_opt, c_compound_stmt010)
	  
    | IF (c_expression, then_c_stmt010, else_c_stmt010) ->
	IF (c_expression, 
	normalize_c_stmt010 ~must_be_compound:true then_c_stmt010,
	normalize_c_stmt010 ~must_be_compound:true else_c_stmt010)
	  
    | WHILE (c_expression, c_stmt010) -> 
	WHILE (c_expression,
	normalize_c_stmt010 ~must_be_compound:true c_stmt010)

    | LOOP (c_stmt010) -> 
	LOOP (normalize_c_stmt010 ~must_be_compound:true c_stmt010)
	  
    | BREAK -> BREAK 
    | CONTINUE -> CONTINUE
    | RETURN_VALUE (c_expression) -> 
	RETURN_VALUE (c_expression)

    | RETURN -> RETURN
    | SWITCH (c_expression, c_stmt010) ->
	SWITCH (c_expression, 
	normalize_c_stmt010 ~must_be_compound:true c_stmt010)

    | CASE (c_constant_expression, c_stmt010) ->
	CASE (c_constant_expression, 
	normalize_c_stmt010 ~must_be_compound:false c_stmt010)
	  
    | CASE_RANGE (e0, e1, c_stmt010) ->
	CASE_RANGE (e0, e1,
	normalize_c_stmt010 ~must_be_compound:false c_stmt010)

    | DEFAULT (c_stmt010) ->
	DEFAULT (normalize_c_stmt010 ~must_be_compound:false c_stmt010)

    | LABEL (string, c_stmt010) ->
	LABEL (string, normalize_c_stmt010 
	  ~must_be_compound:false c_stmt010)

    | GOTO (string) -> 
	GOTO (string)
    | GCC_GOTO expr ->
	GCC_GOTO expr
	  
    | ASM (str_list, asm_details_opt) ->
	ASM (str_list, asm_details_opt)
	  
and normalize_c_compound_stmt010 ~(must_be_compound:bool) 
    ((txt_opt,expr):(string option * c_compound_stmt010)) :c_stmt010 = 
  let BLOCK (labels, decls, stmts) = expr
  in
  match decls with
    | [] ->
	begin
	  if must_be_compound then 
	    COMPOUND (txt_opt, 
	    BLOCK (labels, decls, 
	    Safe_list.map (normalize_c_stmt010 ~must_be_compound:false) stmts))
	  else 
	    SEQUENCE 
	      (txt_opt, Safe_list.map (normalize_c_stmt010 ~must_be_compound:false) stmts)
	end
    | _ ->
	COMPOUND 
	  (txt_opt, 
	  BLOCK (labels, decls, 
	  Safe_list.map (normalize_c_stmt010 ~must_be_compound:false) stmts))

and normalize_c_translation_unit = function
  | Translation_unit l ->
      Translation_unit 
	(Safe_list.map
	  (fun external_declaration -> 
	    normalize_c_external_declaration external_declaration
	  ) l
	)
	
and normalize_c_external_declaration expr = 
  match expr with
    | External_declaration_at (coord, expr) ->
	External_declaration_at (coord, normalize_c_external_declaration expr)
    | External_declaration_1 (c_function_definition) ->
	External_declaration_1 (normalize_c_function_definition c_function_definition)
    | External_declaration_2 (c_declaration) -> expr
	
and normalize_c_function_definition = function
  |  Function_definition (c_declaration_specifiers,
     c_declarator,
     c_declaration_list_opt,
     c_compound_stmt010) 
    ->
       let stmt = normalize_c_compound_stmt010 
	 ~must_be_compound:true (None, c_compound_stmt010)
       in
       match stmt with
	 | COMPOUND (_, stmts) ->
	     Function_definition (c_declaration_specifiers,
	     c_declarator,
	     c_declaration_list_opt,
	     stmts)
	 | _ -> assert false
	     
and normalize: c_translation_unit -> c_translation_unit = 
  fun c_translation_unit ->
    if !disable_normalization then
      c_translation_unit
    else
      normalize_c_translation_unit c_translation_unit
