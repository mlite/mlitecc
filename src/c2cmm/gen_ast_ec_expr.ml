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
module I = Ast_eb_expr
module O = Ast_ec_expr
module T = Tent
module TO = Tent_op
module CA = Const_folding
module S = C_semantics_symbol
module E = Senv
module EO = Senv_op
module TM = Typ_mem
module QN = Qual_name

open Safe_list

let user_opts = []
  
let compile (basename:string) (c_translation_unit:I.c_translation_unit) 
    (env:Senv.env):O.c_translation_unit =
  let local_decls = ref []
  and static_decls = ref []
  in 
  let rec compile_c_stmt010 (expr:I.c_stmt010) :O.c_stmt010 =
    match expr with
      | I.STMT_SPAN (str, stmt) -> 
	  O.STMT_SPAN (str, compile_c_stmt010 stmt)
	    
      | I.STMT_AT (coord, stmt) -> 
	  O.STMT_AT (coord, compile_c_stmt010 stmt)
	    
      | I.NOP -> O.NOP
	  
      | I.COMPUTATION expr -> O.COMPUTATION expr
	  
      | I.SESE exprs -> O.SESE exprs
	  
      | I.SEQUENCE (txt_opt, c_stmt010_list) ->
	  O.SEQUENCE (txt_opt, Safe_list.map compile_c_stmt010 c_stmt010_list)
	    
      | I.COMPOUND (txt_opt, c_compound_stmt010) ->
	  let lst = compile_c_compound_stmt010 c_compound_stmt010
	  in
	  let txt_opt = match txt_opt with
	    | Some str -> Some str
	    | None -> Some ":"
	  in O.COMPOUND (txt_opt, lst)
	       
      | I.IF (expr0, then_c_stmt010, else_c_stmt010) ->
	  let then_c_stmt010 = compile_c_stmt010 then_c_stmt010
	  and else_c_stmt010 = compile_c_stmt010 else_c_stmt010
	  in 
	  O.IF (expr0, then_c_stmt010, else_c_stmt010)
	    
      | I.WHILE (expr, c_stmt010) -> O.WHILE (expr, compile_c_stmt010 c_stmt010)
	  
      | I.LOOP (c_stmt010) -> O.LOOP (compile_c_stmt010 c_stmt010)
	  
      | I.BREAK -> O.BREAK
      | I.CONTINUE -> O.CONTINUE
      | I.RETURN_VALUE expr -> O.RETURN_VALUE expr
	  
      | I.RETURN -> O.RETURN
	  
      | I.EPI str_opt -> O.EPI str_opt
	  
      | I.SWITCH (expr, c_stmt010) ->
	  O.SWITCH (expr, compile_c_stmt010 c_stmt010)
	    
      | I.CASE (expr, c_stmt010) ->
	  O.CASE (expr, compile_c_stmt010 c_stmt010)
	    
      | I.CASE_RANGE (e0, e1, c_stmt010) ->
	  O.CASE_RANGE (e0, e1, compile_c_stmt010 c_stmt010)
	    
      | I.DEFAULT (c_stmt010) ->
	  O.DEFAULT (compile_c_stmt010 c_stmt010)
	    
      | I.LABEL (string, c_stmt010) ->
	  O.LABEL (string, compile_c_stmt010 c_stmt010)
	    
      | I.GOTO (string) -> O.GOTO (string)
	  
      | I.GCC_GOTO expr -> O.GCC_GOTO expr
	  
      | I.ASM (str_list, asm_details_opt) ->
	  let asm_details_opt = match asm_details_opt with
	    | Some asm_details ->
		let fl (so, s, e) = 
		  (so, s, e)
		and fv (so, s, e) = 
		  (so, s, e)
		in
		Some 
		  ({
		    I.asm_outputs = Safe_list.map fl asm_details.I.asm_outputs;
		    I.asm_inputs = Safe_list.map fv asm_details.I.asm_inputs;
		    I.asm_clobbers = asm_details.I.asm_clobbers;
		  })
	    | None -> None
	  in O.ASM (str_list, asm_details_opt)
	       
      | I.CALL (lval_opt, expr0, expr_list) -> 
	  O.CALL (lval_opt, expr0, expr_list)
	    
  and compile_c_compound_stmt010 (expr:I.c_compound_stmt010) : O.c_stmt010 list =
    let I.BLOCK (labels, decls, stmts) = expr
    in
    let _ = compile_c_local_declaration_list decls
    in
    let l = Safe_list.map (compile_c_stmt010) stmts
    in l
	 
  and compile_c_translation_unit (I.Translation_unit (l, eenv)) =
    O.Translation_unit 
      (Safe_list.flatten
	(Safe_list.map
	  (fun external_declaration -> 
	    compile_c_external_declaration external_declaration
	  ) l), eenv)
      
  and compile_c_external_declaration (expr: I.c_external_declaration)
      :O.c_external_declaration list =
    match expr with
      | I.External_declaration_at (coord, expr) ->
	  let decls = compile_c_external_declaration expr
	  in
	  Safe_list.map
	    (fun decl -> O.External_declaration_at (coord, decl)) decls
	    
      | I.External_declaration_1 (c_function_definition) ->
	  let (static_decls, fun_def) = 
	    compile_c_function_definition c_function_definition
	  in 
	  [O.External_declaration_2 static_decls;O.External_declaration_1 fun_def]
	       
      | I.External_declaration_2 (c_declaration) ->
	  [O.External_declaration_2 c_declaration]
	    
  and compile_c_function_definition (expr:I.c_function_definition):
      (O.c_declaration list * O.c_function_definition) =
    let I.Function_definition 
	(linkage, fun_type, fun_name, c_compound_stmt010) = expr
    in
    let c_compound_stmt010 = 
      compile_c_compound_stmt010 c_compound_stmt010
    in
    let decls = !local_decls
    and s_decls = !static_decls
    in 
    local_decls := [];
    static_decls := [];
    (s_decls, O.Function_definition (linkage, fun_type, fun_name, 
    O.BLOCK ([], decls, c_compound_stmt010)))
      
  and compile_c_local_declaration_list c_declaration_list =
    let (s_decls, l_decls) = 
      List.fold_left
	(fun (s_decls, l_decls) decl ->
	  match decl with
	    | I.Local_obj_decl (linkage, ce) ->
		begin
		  match linkage with
		    | I.Default_extern
		    | I.Default_storage 
		    | I.Extern
		    | I.Extern_Inline
		    | I.Auto		      
		    | I.Register
		    | I.Inline
		    | I.Type_alias
		    | I.Thread 
		    | I.Extern_Thread -> (s_decls, decl::l_decls) 
		    | I.Static
		    | I.Static_Inline
		    | I.Static_Thread -> ((I.Obj_decl (linkage, ce))::s_decls, l_decls)
		end
	    | I.Local_obj_decl_init (linkage, ce, init) ->
		begin
		  match linkage with
		    | I.Default_extern
		    | I.Default_storage 
		    | I.Extern
		    | I.Extern_Inline
		    | I.Auto		      
		    | I.Register
		    | I.Inline
		    | I.Type_alias
		    | I.Thread 
		    | I.Extern_Thread -> (s_decls, decl::l_decls) 
		    | I.Static
		    | I.Static_Inline
		    | I.Static_Thread -> 
			((I.Obj_decl_init (linkage, ce, init))::s_decls, l_decls)
		end
	    | I.Local_type_def _ 
	    | I.Local_type_decl _
	    | I.Local_type_only _  
	    | I.Local_register _ -> (s_decls, decl::l_decls)
	) ([], []) c_declaration_list
    in
    local_decls := !local_decls @$ (List.rev l_decls);
    static_decls := !static_decls @$ (List.rev s_decls);
    
  in compile_c_translation_unit c_translation_unit
