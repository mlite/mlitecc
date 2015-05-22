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
module I = Ast_ca_expr
module O = Ast_da_type
open Mapping
open Safe_list

exception Error 

let current_coord = ref ("", 0, 0)

let keep_gnu_dev_all = ref false

let user_opts = 
  [
    ("--keep-gnu-dev-all", Arg.Set keep_gnu_dev_all, "keep gnu_dev_* function declarations");
  ]

let compile (basename:string) (c_translation_unit:I.c_translation_unit):O.c_translation_unit = 
  let anonymous_type_count = ref 0
  and prefix = Random_prefix.get ("_" ^ (C_str.c_identifierize basename) ^ "_da_")
  and cstr_lit_hashtbl = C_syntax_symbol_op.CStrLitHashtbl.create 7;
  in
  let alloc_anonymous_type_name () =
    begin
      incr anonymous_type_count;
      prefix ^ (string_of_int !anonymous_type_count)
    end
  in
  let find_cstr_lit cstr =
    let char_da_type = function 
      | C_syntax_symbol.String_literal _ ->
	  O.Xarray (O.Primitive_type O.Char)
      | C_syntax_symbol.WString_literal _ ->
	  O.Xarray (O.Primitive_type O.WChar)
    in
    try
      let (_, cstr_id) = 
	C_syntax_symbol_op.CStrLitHashtbl.find cstr_lit_hashtbl cstr
      in cstr_id
    with
	Not_found->
	  begin
	    let cstr_id = "str_" ^ (alloc_anonymous_type_name())
	    in
	    let decl = 
	      O.External_declaration_string
		(char_da_type cstr, cstr_id, cstr)
	    in
	    C_syntax_symbol_op.CStrLitHashtbl.add cstr_lit_hashtbl
	      cstr (decl, cstr_id);
	    cstr_id
	  end
  and export_strs () =
    let l = ref []
    in
    let _ = 
      C_syntax_symbol_op.CStrLitHashtbl.iter
	(fun k v -> l := (fst v)::!l) cstr_lit_hashtbl
    in
    (List.rev !l)
  in
  (** the following are standard copy translation **)
  let rec compile_gnu_attribute (str, expr_list) = 
    (str, List.map compile_c_expression expr_list)
      
  and compile_c_expr020 ~is_init = function
    | I.Comma (expr0, expr1) -> 
	O.Comma 
	  (compile_c_expr020 ~is_init expr0, 
	  compile_c_expr020 ~is_init expr1)
	  
    | I.Constant c_constant -> 
	O.Constant c_constant
	  
    | I.String c_string_literal -> 
	if not is_init then
	  let id = find_cstr_lit c_string_literal
	  in O.Variable id
	else
	  O.String c_string_literal

    | I.Call (expr0, expr_list) -> 
	O.Call 
	  (compile_c_expr020 ~is_init expr0, 
	  Safe_list.map (compile_c_expr020 ~is_init) expr_list)
	  
    | I.Macro_va_start (expr0, expr1) -> 
	O.Macro_va_start
	  (compile_c_expr020 ~is_init expr0, compile_c_expr020 ~is_init expr1)
    | I.Macro_va_arg (expr0, c_type_name) ->
	let (type_decls, c_type) = compile_c_type_name c_type_name
	in
	assert (type_decls = []);
	O.Macro_va_arg (compile_c_expr020 ~is_init expr0, c_type)

    | I.Builtin_types_compatible (t0, t1) ->
	let (type_decls0, c_type0) = compile_c_type_name t0
	and (type_decls1, c_type1) = compile_c_type_name t1
	in
	assert (type_decls0 = []);
	assert (type_decls1 = []);
	O.Builtin_types_compatible (c_type0, c_type1)

    | I.Builtin_constant_p (e0) ->
	O.Builtin_constant_p (compile_c_expr020 ~is_init e0)

    | I.Builtin_expect (e0, e1) ->
	O.Builtin_expect (compile_c_expr020 ~is_init e0, compile_c_expr020 ~is_init e1)
	  
    | I.Gnu_block block ->
	O.Gnu_block (compile_c_compound_stmt010 block)

    | I.Gnu_labeladdr string -> 
	O.Gnu_labeladdr string
	  
    | I.Variable string -> 
	O.Variable string
	  
    | I.Macro_va_end expr0 -> 
	O.Macro_va_end (compile_c_expr020 ~is_init expr0)
	  
    | I.Memberof (expr, string) -> 
	O.Memberof (compile_c_expr020 ~is_init expr, string)
    | I.Memberof_ptr (expr, string) -> 
	O.Memberof_ptr (compile_c_expr020 ~is_init expr, string)
    | I.Indexof (expr0, expr1) -> 
	O.Indexof (compile_c_expr020 ~is_init expr0, compile_c_expr020 ~is_init expr1)
    | I.Binary_arithm (binary_arithmatic, expr0, expr1) ->
	O.Binary_arithm (binary_arithmatic,
	compile_c_expr020 ~is_init expr0, compile_c_expr020 ~is_init expr1)
	  
    | I.Binary_predicate (binary_predicate, expr0, expr1) ->
	O.Binary_predicate 
	  (binary_predicate, 
	  compile_c_expr020 ~is_init expr0, 
	  compile_c_expr020 ~is_init expr1)

    | I.Binary_logic (binary_logic_connect, expr0, expr1) ->
	O.Binary_logic 
	  (binary_logic_connect,
	  compile_c_expr020 ~is_init expr0,
	  compile_c_expr020 ~is_init expr1)
	  
    | I.Unary_arithm (unary_arithmatic, expr) ->
	O.Unary_arithm (unary_arithmatic, compile_c_expr020 ~is_init expr)
	  
    | I.Logic_not expr -> O.Logic_not (compile_c_expr020 ~is_init expr)
	
    | I.Sizeof_expr expr -> O.Sizeof_expr (compile_c_expr020 ~is_init expr)
    | I.Sizeof_type c_type_name -> 
	begin
	  let (type_decls, c_type) = compile_c_type_name c_type_name
	  in
	  match type_decls with
	    | [O.Type_decl c_type] -> 
		O.Sizeof_type (c_type, true)
	    | [] -> 
		O.Sizeof_type (c_type, false)
	    | _ -> assert false
	end
	  
    | I.Alignof_expr expr -> O.Alignof_expr (compile_c_expr020 ~is_init expr)
    | I.Alignof_type c_type_name -> 
	begin
	  let (type_decls, c_type) = compile_c_type_name c_type_name
	  in
	  match type_decls with
	    | [O.Type_decl c_type] -> 
		O.Alignof_type (c_type, true)
	    | [] -> 
		O.Alignof_type (c_type, false)
	    | _ -> assert false
	end
	  
    | I.Cast (c_type_name, expr) -> 
	let (type_decls, c_type) = compile_c_type_name c_type_name
	in
	assert (type_decls = []);
	O.Cast (c_type, compile_c_expr020 ~is_init expr)
    | I.Assign (expr0, expr1) -> 
	O.Assign (compile_c_expr020 ~is_init expr0, compile_c_expr020 ~is_init expr1)
    | I.Post_decr expr -> O.Post_decr (compile_c_expr020 ~is_init expr)
    | I.Post_incr expr -> O.Post_incr (compile_c_expr020 ~is_init expr)
    | I.Pre_decr expr -> O.Pre_decr (compile_c_expr020 ~is_init expr)
    | I.Pre_incr expr -> O.Pre_incr (compile_c_expr020 ~is_init expr)
    | I.Assign_arithm (binary_arithmatic, expr0, expr1) ->
	O.Assign_arithm 
	  (binary_arithmatic,
	  compile_c_expr020 ~is_init expr0, 
	  compile_c_expr020 ~is_init expr1)

    | I.Cast_init (c_type_name, c_initializer_list) ->
	let (type_decls, c_type) = compile_c_type_name c_type_name
	in
	assert (type_decls = []);
	O.Cast_init (c_type,
	compile_c_initializer_list c_initializer_list)

    | I.Memof expr -> O.Memof (compile_c_expr020 ~is_init expr)
    | I.Addrof expr -> O.Addrof (compile_c_expr020 ~is_init:false expr)
    | I.Question (expr0, expr1, expr2) ->
	O.Question (compile_c_expr020 ~is_init expr0, 
	Mapping.map_opt (compile_c_expr020 ~is_init) expr1,
	compile_c_expr020 ~is_init expr2)
	  
  and compile_c_assignment_expression ~is_init expr = 
    compile_c_expr020 ~is_init expr
      
  and compile_c_expression expr = compile_c_expr020 false expr
    
  and compile_c_constant_expression expr = compile_c_expr020 false expr

  and mk_gnu_attribute_type t atts = 
    if atts = [] then t
    else O.GCC_attribute_type (t, atts)

  and mk_aggregate_gnu_attribute_type t atts = 
    let attach_atts lst = 
      if atts <> [] then
	List.map
	  (fun field -> 
	    match field with
	      | O.Regular (O.GCC_attribute_type (t, atts0), str) ->
		  O.Regular (O.GCC_attribute_type (t, atts0 @ atts), str)
	      | O.Regular (t, str) ->
		  O.Regular (O.GCC_attribute_type (t, atts), str)
	      | O.Bits (O.GCC_attribute_type (t, atts0), expr, str_opt) ->
		  O.Bits (O.GCC_attribute_type (t, atts0 @ atts), expr, str_opt)
	      | O.Bits (t, expr, str_opt) ->
		  O.Bits (O.GCC_attribute_type (t, atts), expr, str_opt)
	  ) lst
      else
	lst
    in    
    match t with
      | O.Enum_type (s, lst) ->
	  if atts <> [] then
	    assert false
	  else
	    O.Enum_type (s, lst)
	      
      | O.Union_type (s, lst) ->
	  let lst = attach_atts lst
	  in O.Union_type (s, lst)
	       
      | O.Struct_type (s, lst, layout) ->
	  begin
	    match atts, layout with
	      | [("__attribute__", [O.Variable "__gcc_struct__"])], O.C_struct ->
		  O.Struct_type (s, lst, O.GCC_struct)
	      | [("__attribute__", [O.Variable "__ms_struct__"])], O.C_struct ->
		  O.Struct_type (s, lst, O.MS_struct)
	      | _,_ ->
		  let lst = attach_atts lst
		  in O.Struct_type (s, lst, O.C_struct)
	  end
	       
      | _ -> assert false
	       
  and compile_c_declaration (is_global:bool) (expr:I.c_declaration):
      O.c_declaration_ex list = 
    let lst = match expr with
      | I.Declaration 
	  (c_declaration_specifiers,  c_init_declarator_list_opt) 
	->
	  begin
	    let (storage_class_opt, type_decls, c_type_opt) = 
	      compile_c_declaration_specifiers c_declaration_specifiers
	    in
	    let type_decls = ref type_decls
	    in
	    let _ = match c_type_opt with
	      | Some c_type' -> 
		  begin
		    let rec extract_type_decl c_typ atts = 
		      match c_typ with
			| O.Enum_type (s, _) ->
			    begin
			      type_decls := !type_decls @$ 
				[O.Type_decl (mk_aggregate_gnu_attribute_type c_typ atts)]; 
			      (O.Enum_type_name s, true)
			    end
			| O.Union_type (s, _) -> 
			    begin
			      type_decls := !type_decls @$ 
				[O.Type_decl (mk_aggregate_gnu_attribute_type c_typ atts)]; 
			      (O.Union_type_name s, true)
			    end
			| O.Struct_type (s, _,_) -> 
			    begin 
			      type_decls := !type_decls @$ 
				[O.Type_decl (mk_aggregate_gnu_attribute_type c_typ atts)]; 
			      (O.Struct_type_name s, true)
			    end
			| O.Qualified_type (q, c_typ') -> 
			    begin
			      let (c_typ, declarated) = extract_type_decl
				c_typ' []
			      in
			      (O.Qualified_type (q, c_typ), declarated)
			    end
			| O.GCC_attribute_type (c_typ', atts) -> 
			    begin
			      let (c_typ, declarated) = extract_type_decl
				c_typ' atts
			      in
			      (c_typ, declarated)
			    end
			| O.Incomplete_qualified_type (c_typ_opt, q) ->
			    begin
			      match c_typ_opt with
				| Some c_typ -> 
				    let (c_typ, declarated) = 
				      extract_type_decl c_typ []
				    in
				    (O.Incomplete_qualified_type 
				      (Some c_typ, q), declarated) 
				| None -> (* default is int *)
				    begin
				      (O.Incomplete_qualified_type
					(Some (O.Primitive_type O.Int), q), false)
				    end
			    end
			| O.Incomplete_gnu_attribute_type (c_typ_opt, q) ->
			    begin
			      match c_typ_opt with
				| Some c_typ -> 
				    begin
				      let (c_typ, declarated) = 
					extract_type_decl c_typ q
				      in
				      if declarated then
					(c_typ, declarated)
				      else
					(O.GCC_attribute_type
					  (c_typ, q), declarated)
				    end
				| None -> assert false
			    end
			| _ -> (c_typ, false)
		    in
		    let (c_type, type_declarated) = 
		      extract_type_decl c_type' []
		    in
		    let storage_class = 
		      match storage_class_opt with
			| Some v -> v
			| None -> 
			    if is_global 
			    then O.Default_extern 
			    else O.Default
		    in
		    let init_declarators_opt = 
		      map_list_opt (compile_c_init_declarator
			c_type) c_init_declarator_list_opt
		    in
		    match init_declarators_opt with
		      | Some init_declarators ->
			  type_decls := !type_decls @$ 
			    (Safe_list.map 
			      (fun (typ', id, init_opt) -> 
				let typ = match typ' with
				  | O.Function_type function_type ->
				      begin
					assert 
					  (function_type.O.func_name = None);
					O.Function_type 
					  ({ 
					    O.func_name = Some id;
					    O.param_type 
					    = function_type.O.param_type;
					    O.formal_param 
					    = function_type.O.formal_param;
					    O.return_type 
					    = function_type.O.return_type; 
					    O.func_attributes = [];
					  })
				      end
				  | O.Function_type_ex (function_type, decls) ->
				      begin
					assert 
					  (function_type.O.func_name = None);
					O.Function_type_ex 
					  ({O.func_name = Some id;
					  O.param_type 
					  = function_type.O.param_type;
					  O.formal_param 
					  = function_type.O.formal_param;
					  O.return_type 
					  = function_type.O.return_type; 
					  O.func_attributes = [];
					  },
					  decls)
				      end
				  | _ -> typ'
				in
				match storage_class with
				  | O.Type_alias -> O.Typedef (typ, id)
				  | O.Extern -> O.Obj_decl (O.Extern, typ, id)
				  | O.Default_extern -> 
				      begin
					match init_opt with
					  | Some expr020_list -> 
					      O.Obj_decl_init 
						(O.Default_extern, 
						typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl 
						(O.Default_extern, typ, id)
				      end
				  | O.Default -> 
				      begin
					match init_opt with
					  | Some expr020_list -> 
					      O.Obj_decl_init 
						(O.Default, 
						typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl 
						(O.Default, typ, id)
				      end
				  | O.Static -> 
				      begin
					match init_opt with
					  | Some expr020_list -> 
					      O.Obj_decl_init 
						(O.Static, typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl 
						(O.Static, typ, id)
				      end
				  | O.Auto -> 
				      begin
					match init_opt with
					  | Some expr020_list -> 
					      O.Obj_decl_init 
						(O.Auto, typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl (O.Auto, typ, id)
				      end
				  | O.Register -> 
				      begin
					match init_opt with
					  | Some expr020_list ->
					      O.Obj_decl_init 
						(O.Register, typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl (O.Register, typ, id)
				      end
				  | O.Inline -> 
				      begin
					match init_opt with
					  | Some _ -> assert false
					  | None -> 
					      O.Obj_decl (O.Inline, typ, id)
				      end
				  | O.Extern_Inline -> 
				      O.Obj_decl (O.Extern_Inline, typ, id)
				  | O.Static_Inline -> 
				      O.Obj_decl (O.Static_Inline, typ, id)
				  | O.Thread -> 
				      begin
					match init_opt with
					  | Some expr020_list ->
					      O.Obj_decl_init
						(O.Thread, typ, id, expr020_list)
					  | None -> 
					      O.Obj_decl (O.Thread, typ, id)
				      end
				  | O.Extern_Thread -> 
				      O.Obj_decl (O.Extern_Thread, typ, id)
				  | O.Static_Thread -> 
				      O.Obj_decl (O.Static_Thread, typ, id)
			      ) init_declarators
			    )
		      | None -> (** user defined type declaration  **)
			  begin
			    assert (storage_class_opt = None);
			    if not type_declarated then
			      type_decls := !type_decls @$ [O.Type_only c_type]
			  end
		  end
	      | None -> assert false 
	    in
	    !type_decls
	  end
    in
    Safe_list.map (fun v -> (v, ref false)) lst 
      
  and compile_c_storage_class_specifier = function
    | C_syntax_symbol.Storage_class_specifier_TYPEDEF -> O.Type_alias
    | C_syntax_symbol.Storage_class_specifier_EXTERN -> O.Extern
    | C_syntax_symbol.Storage_class_specifier_STATIC -> O.Static
    | C_syntax_symbol.Storage_class_specifier_AUTO -> O.Auto
    | C_syntax_symbol.Storage_class_specifier_REGISTER -> O.Register
    | C_syntax_symbol.Storage_class_specifier_THREAD -> O.Thread


  and compile_c_storage_class_specifier_for_thread = function
    | C_syntax_symbol.Storage_class_specifier_TYPEDEF -> assert false
    | C_syntax_symbol.Storage_class_specifier_EXTERN -> O.Extern_Thread
    | C_syntax_symbol.Storage_class_specifier_STATIC -> O.Static_Thread
    | C_syntax_symbol.Storage_class_specifier_AUTO -> O.Thread
    | C_syntax_symbol.Storage_class_specifier_REGISTER -> assert false
    | C_syntax_symbol.Storage_class_specifier_THREAD -> assert false

  and compile_c_storage_class_specifier_for_inline = function
    | C_syntax_symbol.Storage_class_specifier_TYPEDEF -> assert false
    | C_syntax_symbol.Storage_class_specifier_EXTERN -> O.Extern_Inline
    | C_syntax_symbol.Storage_class_specifier_STATIC -> O.Static_Inline
    | C_syntax_symbol.Storage_class_specifier_AUTO -> O.Inline
    | C_syntax_symbol.Storage_class_specifier_REGISTER -> assert false
    | C_syntax_symbol.Storage_class_specifier_THREAD -> assert false
	
	
  and compile_c_declaration_specifiers (expr:I.c_declaration_specifiers)
      :O.linkage option * O.c_declaration list * O.c_type option =
    match expr with
      | I.Declaration_specifiers_1 
	  (c_storage_class_specifier, c_declaration_specifiers_opt) ->
	  begin
	    let v = 
	      map_opt compile_c_declaration_specifiers
		c_declaration_specifiers_opt
	    in
	    match v with
	      | Some (None, l, t) -> 
		  (Some 
		    (compile_c_storage_class_specifier 
		      c_storage_class_specifier), l, t)
	      | Some (Some O.Inline, l, t) ->
		  (Some 
		    (compile_c_storage_class_specifier_for_inline 
		      c_storage_class_specifier), l, t)
	      | Some (Some O.Thread, l, t) ->
		  (Some 
		    (compile_c_storage_class_specifier_for_thread
		      c_storage_class_specifier), l, t)
	      | None -> 
		  (** extern  f () **)
		  (Some 
		    (compile_c_storage_class_specifier 
		      c_storage_class_specifier), 
		  [], Some (O.Primitive_type O.Int))
	      | _ -> assert false
	  end
	    
      | I.Declaration_specifiers_2 
	  (c_type_specifier, c_declaration_specifiers_opt) 
	->
	  begin
	    let v = 
	      map_opt compile_c_declaration_specifiers
		c_declaration_specifiers_opt
	    in
	    match v with
	      | Some (s, l, t) -> 
		  let (type_decls, t') = 
		    compile_c_type_specifier t c_type_specifier
		  in
		  (s, l @$ type_decls, Some t')
	      | None -> 
		  let (type_decls, t') = 
		    compile_c_type_specifier None c_type_specifier
		  in
		  (None, type_decls, Some t')
	  end

      | I.Declaration_specifiers_3 
	  (c_type_qualifier, c_declaration_specifiers_opt) 
	->
	  begin
	    let v = 
	      map_opt compile_c_declaration_specifiers
		c_declaration_specifiers_opt
	    in
	    match v with
	      | Some (s, l, t) -> 
		  (s, l, Some (compile_c_type_qualifier t c_type_qualifier))
	      | None -> 
		  (None, [], Some (compile_c_type_qualifier None c_type_qualifier))
	  end
	    
      | I.Declaration_specifiers_4 
	  (c_function_specifier, c_declaration_specifiers_opt) 
	->
	  begin
	    let v = 
	      map_opt compile_c_declaration_specifiers
		c_declaration_specifiers_opt
	    in
	    match v  with
	      | Some (s, l, Some t) -> 
		  (Some O.Inline, l, Some t)
	      | None ->
		  (Some O.Inline, [], None)
	      | _ -> assert false
	  end

      | I.Declaration_specifiers_GNU
	  (attribute, c_declaration_specifiers_opt) -> 
	  begin
	    let v = 
	      map_opt compile_c_declaration_specifiers
		c_declaration_specifiers_opt
	    and att = compile_gnu_attribute attribute
	    in
	    match v with
	      | Some (s, l, Some (O.GCC_attribute_type (t, atts))) ->
		  (s, l, Some (O.GCC_attribute_type (t, atts @ [att])))
	      | Some (s, l, Some t) ->
		  (s, l, Some (O.GCC_attribute_type (t, [att])))
	      | Some (s, l, None) ->
		  (s, l, Some (O.Incomplete_gnu_attribute_type (None, [att])))
	      | None -> 
		  (None, [], Some (O.Incomplete_gnu_attribute_type (None, [att])))
	  end
	    
  and compile_c_init_declarator: O.c_type -> I.c_init_declarator 
  -> O.c_type * string * O.c_initializer option = 
    fun c_type expr ->
      match expr with
	| I.Init_declarator_1 c_declarator ->
	    let (typ, id) = compile_c_declarator c_type c_declarator
	    in
	    (typ, id, None)
	      
	| I.Init_declarator_2 (c_declarator, c_initializer) ->
	    let (typ, id) = compile_c_declarator c_type c_declarator
	    and c_initializer = compile_c_initializer c_initializer
	    in
	    (typ, id, Some c_initializer)
	      
	      
  and compile_c_type_specifier: O.c_type option -> I.c_type_specifier -> 
  O.c_declaration list * O.c_type = 
    fun c_type expr ->
      match c_type with
	| Some (O.Incomplete_qualified_type (t, q)) ->
	    let (decls, t) = compile_c_type_specifier t expr
	    in
	    (decls, O.Qualified_type (q, t))
	| Some (O.Incomplete_gnu_attribute_type (t, atts)) ->
	    let (decls, t) = compile_c_type_specifier t expr
	    in (decls, O.GCC_attribute_type (t, atts))
	| _ ->
	    begin
	      match expr with
		| I.Type_builtin builtin_type ->
		    begin
		      let (l, t) = 
			match builtin_type with
			  | C_syntax_symbol.Type_specifier_VOID -> 
			      begin
				match c_type with 
				  | None 
				  | Some(O.Primitive_type O.Default_int) -> 
				      ([], O.Primitive_type O.Void)
				  | _ -> assert false
			      end
			  | C_syntax_symbol.Type_specifier_CHAR ->
			      begin
				let rec f c_type = 
				  match c_type with
				    | None 
				    | Some (O.Primitive_type O.Default_int) ->
					([], O.Primitive_type O.Char)
				    | Some (O.Primitive_type O.Signed) -> 
					([], O.Primitive_type O.Signed_Char)
				    | Some (O.Primitive_type O.Unsigned) -> 
					([], O.Primitive_type O.Unsigned_Char)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | _ -> assert false
				in f c_type
			      end
			  | C_syntax_symbol.Type_specifier_SHORT -> 
			      begin
				let rec f c_type = 
				  match c_type with
				    | None 
				    | Some (O.Primitive_type O.Default_int) ->
					([], O.Primitive_type O.Short_Int)
				    | Some (O.Primitive_type O.Signed) -> 
					([], O.Primitive_type O.Signed_Short_Int)
				    | Some (O.Primitive_type O.Int) -> 
					([], O.Primitive_type O.Short_Int)
				    | Some (O.Primitive_type O.Unsigned) -> 
					([], O.Primitive_type O.Unsigned_Short_Int)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | _ -> assert false
				in f c_type				
			      end
			  | C_syntax_symbol.Type_specifier_INT -> 
			      begin
				let rec f c_type = 
				  match c_type with
				    | None
				    | Some (O.Primitive_type O.Default_int) ->
					([], O.Primitive_type O.Int)
				    | Some (O.Primitive_type O.Signed) -> 
					([], O.Primitive_type O.Signed_Int)
				    | Some (O.Primitive_type O.Unsigned) -> 
					([], O.Primitive_type O.Unsigned_Int)
				    | Some (O.Primitive_type O.Short_Int) ->
					([], O.Primitive_type O.Short_Int)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | _ -> assert false
				in f c_type
			      end
			  | C_syntax_symbol.Type_specifier_LONG -> 
			      begin
				let rec f c_type = 
				  match c_type with
				    | None 
				    | Some (O.Primitive_type O.Default_int) ->
					([], O.Primitive_type O.Long_Int)
				    | Some (O.Primitive_type O.Signed) -> 
					([], O.Primitive_type O.Signed_Long_Int)
				    | Some (O.Primitive_type O.Unsigned) -> 
					([], O.Primitive_type O.Unsigned_Long_Int)
				    | Some (O.Primitive_type O.Int) -> 
					([], O.Primitive_type O.Long_Int)
				    | Some (O.Primitive_type O.Unsigned_Int) -> 
					([], O.Primitive_type O.Unsigned_Long_Int)
				    | Some (O.Primitive_type O.Long_Int) ->
					([], O.Primitive_type O.Long_Long_Int)
				    | Some (O.Primitive_type O.Signed_Long_Int) ->
					([], O.Primitive_type O.Signed_Long_Long_Int)
				    | Some (O.Primitive_type O.Unsigned_Long_Int) ->
					([], O.Primitive_type O.Unsigned_Long_Long_Int)
				    | Some (O.Primitive_type O.Double) ->
					([], O.Primitive_type O.Long_Double)
				    | Some (O.Primitive_type O.Double_Complex) ->
					([], O.Primitive_type O.Long_Double_Complex) 
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some(O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | _ -> assert false
				in f c_type
			      end
			  | C_syntax_symbol.Type_specifier_FLOAT -> 
			      begin
				match c_type with
				  | None 
				  | Some(O.Primitive_type O.Default_int) -> 
				      ([], O.Primitive_type O.Float)
				  | _ -> 
				      ([], O.Primitive_type O.Float_Complex)
			      end
			  | C_syntax_symbol.Type_specifier_DOUBLE -> 
			      begin
				match c_type with
				  | None 
				  | Some(O.Primitive_type O.Default_int) -> 
				      ([], O.Primitive_type O.Double)
				  | _ -> 
				      ([], O.Primitive_type O.Double_Complex)
			      end
			  | C_syntax_symbol.Type_specifier_BOOL -> 
			      begin
				match c_type with
				  | None 
				  | Some(O.Primitive_type O.Default_int) -> 
				      ([], O.Primitive_type O.Bool)
				  | _ -> assert false
			      end
			  | C_syntax_symbol.Type_specifier_COMPLEX -> 
			      begin
				let rec f c_type =
				  match c_type with
				    | None 
				    | Some(O.Primitive_type O.Default_int) -> 
					([], O.Primitive_type O.Complex)
				    | Some (O.Primitive_type O.Double) ->
					([], O.Primitive_type O.Double_Complex)
				    | Some (O.Primitive_type O.Float) ->
					([], O.Primitive_type O.Float_Complex)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | _ -> assert false
				in f c_type
			      end
			  | C_syntax_symbol.Type_specifier_SIGNED -> 
			      begin
				let rec f c_type = 
				  match c_type with
				    | Some (O.Primitive_type O.Char) -> 
					([], O.Primitive_type O.Signed_Char)
				    | Some (O.Primitive_type O.Short_Int) -> 
					([], O.Primitive_type O.Signed_Short_Int)
				    | Some (O.Primitive_type O.Int) -> 
					([], O.Primitive_type O.Signed_Int)
				    | Some (O.Primitive_type O.Long_Int) -> 
					([], O.Primitive_type O.Signed_Long_Int)
				    | Some (O.Primitive_type O.Long_Long_Int) -> 
					([], O.Primitive_type O.Signed_Long_Long_Int)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					let (l, t) = f (Some t)
					in
					(l, O.GCC_attribute_type (t, atts))
				    | None 
				    | Some(O.Primitive_type O.Default_int) -> 
					([], O.Primitive_type O.Signed)
				    | _ -> assert false
				in f c_type
			      end
				
			  | C_syntax_symbol.Type_specifier_UNSIGNED -> 
			      begin
				let rec f c_type = 
				  match c_type with
				    | Some (O.Primitive_type O.Char) -> 
					([], O.Primitive_type O.Unsigned_Char)
				    | Some (O.Primitive_type O.Short_Int) -> 
					([], O.Primitive_type O.Unsigned_Short_Int)
				    | Some (O.Primitive_type O.Int) -> 
					([], O.Primitive_type O.Unsigned_Int)
				    | Some (O.Primitive_type O.Long_Int) -> 
					([], O.Primitive_type O.Unsigned_Long_Int)
				    | Some (O.Primitive_type O.Long_Long_Int) -> 
					([], O.Primitive_type O.Unsigned_Long_Long_Int)
				    | None 
				    | Some(O.Primitive_type O.Default_int) -> 
					([], O.Primitive_type O.Unsigned)
				    | Some (O.Qualified_type (q, t)) ->
					let (l, t) = f (Some t)
					in
					(l, O.Qualified_type (q, t))
				    | Some (O.GCC_attribute_type (t, atts)) ->
					begin
					  let (l, typ) = f (Some t)
					  in
					  (l, O.GCC_attribute_type (typ, atts))
					end
				    | _ -> assert false
				in f c_type
			      end
		      in
		      (l, t)
		    end
		      
		| I.Type_specifier_STRUCT_OR_UNION c_struct_or_union_specifier ->
		    begin
		      match c_type with 
			| None 
			| Some (O.Primitive_type O.Default_int) 
			  -> compile_c_struct_or_union_specifier c_struct_or_union_specifier 
			| _ -> assert false
		    end
		      
		| I.Type_specifier_ENUM c_enum_specifier ->
		    begin
		      match c_type with
			| None 
			| Some (O.Primitive_type O.Default_int) 
			  -> compile_c_enum_specifier c_enum_specifier
			| _ -> assert false
		    end
		      
		| I.Type_specifier_TYPENAME (I.Typedef_name id) -> 
		    begin
		      match c_type with
			| None 
			| Some (O.Primitive_type O.Default_int) -> 
			    ([], O.Typename id)
			| _ -> assert false
		    end
		      
		| I.Type_specifier_GCC_TYPEOF_E (c_expr020) -> 
		    begin
		      match c_type with
			| None 
			| Some (O.Primitive_type O.Default_int) 
			  -> ([], O.Typeof (compile_c_expr020 false c_expr020))
			| _ -> assert false
		    end
		      
		| I.Type_specifier_GCC_TYPEOF_T (c_type_name) -> 
		    begin
		      match c_type with
			| None 
			| Some (O.Primitive_type O.Default_int) 
			  -> compile_c_type_name c_type_name
			| _ -> assert false
		    end
	    end

	      
  and compile_c_struct_or_union_specifier: I.c_struct_or_union_specifier ->
  O.c_declaration list * O.c_type = 
    fun expr ->
      match expr with
	| I.Struct_or_union_specifier_1 (c_struct_or_union, c_identifier_opt, 
	  c_struct_declaration_list) 
	  ->
	    begin
	      match c_struct_or_union with
		| C_syntax_symbol.Struct_or_union_STRUCT ->  
		    begin
		      let (is_anonymous, id) = match c_identifier_opt with
			| Some s -> (false, s)
			| None -> (true, alloc_anonymous_type_name ())
		      in
		      let (type_decls, fields) = 
			compile_c_struct_declaration_list 
			  c_struct_declaration_list
		      in
		      (type_decls, O.Struct_type (id, fields, O.C_struct))
		    end
		      
		| C_syntax_symbol.Struct_or_union_UNION ->  
		    begin
		      let (is_anonymous, id) = match c_identifier_opt with
			| Some s -> (false, s)
			| None -> (true, alloc_anonymous_type_name ())
		      in
		      let (type_decls, fields) = 
			compile_c_struct_declaration_list 
			  c_struct_declaration_list
		      in
		      (type_decls, O.Union_type (id, fields))
		    end
	    end
	| I.Struct_or_union_specifier_2 (c_struct_or_union, id) -> 
	    begin
	      match c_struct_or_union with
		| C_syntax_symbol.Struct_or_union_STRUCT ->
		    ([], O.Struct_type_name id)
		| C_syntax_symbol.Struct_or_union_UNION ->
		    ([], O.Union_type_name id)
	    end
	      
  and compile_c_struct_declaration_list: I.c_struct_declaration list -> 
  O.c_declaration list * O.field list = 
    fun expr ->
      let (decls, fields) = 
	Safe_list.split (Safe_list.map compile_c_struct_declaration expr)
      in
      (Safe_list.flatten decls, Safe_list.flatten fields)
	
	
  and compile_c_struct_declaration: I.c_struct_declaration -> 
  O.c_declaration list * O.field list = 
    fun expr ->
      match expr with
	| I.Struct_declaration (c_specifier_qualifier_list, c_struct_declarator_list) 
	  ->
	    begin
	      let (l, c_type) = 
		compile_c_specifier_qualifier_list 
		  c_specifier_qualifier_list
	      in
	      let field_lst = 
		match c_struct_declarator_list with
		  | h::t ->
		      begin
			match c_type with
			  | O.Struct_type (id, fields, layout) -> 
			      begin
				(compile_c_struct_declarator 
				  (O.Struct_type (id, fields, layout)) h)
				::(Safe_list.map 
				  (compile_c_struct_declarator 
				    (O.Struct_type_name id)) t)
			      end
			  | O.Union_type (id, fields) ->
			      begin
				(compile_c_struct_declarator 
				  (O.Union_type (id, fields)) h)
				::(Safe_list.map 
				  (compile_c_struct_declarator 
				    (O.Union_type_name id)) t)
			      end
			  | O.Enum_type (id, fields) ->
			      begin
				(compile_c_struct_declarator 
				  (O.Enum_type (id, fields)) h)
				::(Safe_list.map 
				  (compile_c_struct_declarator 
				    (O.Enum_type_name id)) t)
			      end
			  | _ -> 
			      Safe_list.map 
				(compile_c_struct_declarator c_type) 
				c_struct_declarator_list
		      end
		  | [] -> []
	      in
	      (l, field_lst)
	    end
	      

  and compile_c_specifier_qualifier_list: I.c_specifier_qualifier_list -> 
  O.c_declaration list * O.c_type = 
    fun expr ->
      match expr with
	| I.Specifier_qualifier_list_1 
	    (c_type_specifier, c_specifier_qualifier_list_opt) 
	  ->
	    begin
	      let (l, c_type_opt) = 
		compile_c_specifier_qualifier_list_opt
		  c_specifier_qualifier_list_opt
	      in
	      let (ll, c_type) = 
		compile_c_type_specifier c_type_opt c_type_specifier
	      in (l @$ ll, c_type)
	    end
	      
	| I.Specifier_qualifier_list_2 
	    (c_type_qualifier, c_specifier_qualifier_list_opt) 
	  ->
	    begin
	      let (l, c_type_opt) = 
		compile_c_specifier_qualifier_list_opt 
		  c_specifier_qualifier_list_opt
	      in
	      let c_type = 
		compile_c_type_qualifier c_type_opt c_type_qualifier
	      in (l, c_type)
	    end

	| I.Specifier_qualifier_list_GNU 
	    (gnu_attribute, c_specifier_qualifier_list_opt) 
	  ->
	    begin
	      let (l, c_type_opt) = 
		compile_c_specifier_qualifier_list_opt 
		  c_specifier_qualifier_list_opt
	      in
	      let c_type =
		compile_c_type_opt_gnu_attribute c_type_opt gnu_attribute
	      in (l, c_type)
	    end

	      
  and compile_c_struct_declarator: O.c_type -> I.c_struct_declarator -> 
  O.field = 
    fun c_type expr ->
      match expr with
	| I.Struct_declarator_1 c_declarator ->
	    let (typ, id) = compile_c_declarator c_type c_declarator
	    in
	    O.Regular (typ, id)

	| I.Struct_declarator_2 (c_declarator_opt, expr) ->
	    begin
	      match map_opt (compile_c_declarator c_type) c_declarator_opt with
		| Some (typ, id) -> 
		    O.Bits (typ, compile_c_constant_expression expr, Some id)
		| None ->
		    O.Bits (c_type, compile_c_constant_expression expr, None)
	    end
	      
	| I.Struct_declarator_GNU (c_struct_declarator, attributes)
          ->
	    assert false

	      
  and compile_c_enum_specifier: I.c_enum_specifier -> O.c_declaration list * O.c_type = 
    fun expr ->
      let atts = []
      in
      match expr with
	| I.Enum_specifier_1 (id_opt, c_enumerator_list) ->
	    let id = match id_opt with
	      | Some s -> s
	      | None -> alloc_anonymous_type_name ()
	    in
	    ([O.Type_decl 
	      (O.Enum_type 
		(id, Safe_list.map compile_c_enumerator c_enumerator_list))],
	    O.Enum_type_name id)
	| I.Enum_specifier_2 id -> 
	    assert (atts = []);
	    ([], O.Enum_type_name id)

	      
  and compile_c_enumerator = function
    | I.Enumerator_1 c_enumeration_constant -> 
	(c_enumeration_constant, None)
    | I.Enumerator_2 (c_enumeration_constant, c_constant_expression) ->
	(c_enumeration_constant, 
	Some (compile_c_constant_expression c_constant_expression))
	  
  and compile_c_type_qualifier (c_type_opt:O.c_type option) (expr:I.c_type_qualifier):O.c_type = 
    let qualifier = match expr with
      | C_syntax_symbol.Type_qualifier_CONST -> O.Const
      | C_syntax_symbol.Type_qualifier_RESTRICT -> O.Restrict
      | C_syntax_symbol.Type_qualifier_VOLATILE -> O.Volatile
    in
    match c_type_opt with
      | Some c_type ->
	  O.Qualified_type (qualifier, c_type)
      | None -> 
	  (* default is int*)
	  O.Incomplete_qualified_type (Some (O.Primitive_type O.Default_int), qualifier)

  and compile_c_type_opt_gnu_attribute (c_type_opt:O.c_type option)
      (expr:I.gnu_attribute) :O.c_type = 
    match c_type_opt with
      | Some c_type ->
	  O.GCC_attribute_type (c_type, [compile_gnu_attribute expr])
      | None -> 
	  O.Incomplete_gnu_attribute_type (None, [compile_gnu_attribute expr])
	    
  and compile_c_declarator: O.c_type -> I.c_declarator -> O.c_type * string = 
    fun c_type c_declarator ->
      match c_declarator with
	| I.Declarator (ptr_opt, c_direct_declarator) ->
	    begin
	      let c_type' = compile_c_pointer_opt c_type ptr_opt
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end
	      
	| I.Declarator_GNU (declarator, attributes) ->
	    begin
	      let c_type = O.GCC_attribute_type 
		(c_type, List.map compile_gnu_attribute attributes)
	      in compile_c_declarator c_type declarator
	    end

  and compile_c_direct_declarator: O.c_type -> I.c_direct_declarator -> 
  O.c_type * string = 
    fun c_type c_direct_declarator ->
      match c_direct_declarator with
	| I.Direct_declarator_1 c_identifier -> 
	    (c_type, c_identifier)
	| I.Direct_declarator_2 c_declarator -> 
	    compile_c_declarator c_type c_declarator
	| I.Direct_declarator_3 
	    (c_direct_declarator, 
	    c_type_qualifier_list_opt, 
	    c_assignment_expression_opt) 
	  ->
	    begin
	      let c_type' = 
		match c_assignment_expression_opt with
		  | Some e ->
		      O.Array 
			(c_type, compile_c_assignment_expression ~is_init:false e)
		  | None ->
		      O.Xarray c_type
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end
	| I.Direct_declarator_4_STATIC 
	    (c_direct_declarator, 
	    c_type_qualifier_list_opt,
	    c_assignment_expression) 
	  ->
	    begin
	      let c_type' =
		O.Array 
		  (c_type, compile_c_assignment_expression 
		    ~is_init:false c_assignment_expression)
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end
	      
	| I.Direct_declarator_5_STATIC 
	    (c_direct_declarator, c_type_qualifier_list,
	    c_assignment_expression)
	  ->
	    begin
	      let c_type' = 
		O.Array 
		  (c_type, compile_c_assignment_expression ~is_init:false 
		    c_assignment_expression)
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end
	      
	| I.Direct_declarator_6_STAR 
	    (c_direct_declarator, c_type_qualifier_list_opt) 
	  ->
	    begin
	      let c_type' = O.Xarray c_type
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end
	      
	| I.Direct_declarator_7 
	    (c_direct_declarator, c_parameter_type_list) 
	  ->
	    begin
	      let (param_type, ids) = 
		compile_c_parameter_type_list c_parameter_type_list
	      in
	      let c_type' = O.Function_type 
		{ 
		  O.func_name = None;
		  O.param_type = param_type;
		  O.formal_param = ids;
		  O.return_type = c_type;
		  O.func_attributes = [];
		}
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end

	| I.Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	    begin
	      let params = match c_identifier_list_opt with
		| Some c_identifier_list ->
		    begin
		      c_identifier_list
		    end
		| None -> []
	      in
	      let c_type' = 
		O.Function_type 
		  { 
		    O.func_name = None;
		    O.param_type = O.Param_list params;
		    O.formal_param = Safe_list.map (fun v -> Some v) params;
		    O.return_type = c_type;
		    O.func_attributes = [];
		  }
	      in
	      compile_c_direct_declarator c_type' c_direct_declarator
	    end


  and compose_pointer_type: O.c_type -> O.c_type = 
    fun c_type ->
      match c_type with
	| O.Incomplete_qualified_type (c_type_opt, type_qualifier) ->
	    begin
	      match c_type_opt with
		| Some c_type -> O.Qualified_type (type_qualifier, 
		  O.Pointer c_type)
		| None -> assert false
	    end
	| _ -> O.Pointer c_type
	    
  and compile_c_pointer: O.c_type -> I.c_pointer -> O.c_type = 
    fun c_type pointer ->
      let c_type' = compose_pointer_type c_type
      in
      match pointer with
	| I.Pointer_1 c_type_qualifier_list_opt -> 
	    compile_c_type_qualifier_list_opt c_type' c_type_qualifier_list_opt
	| I.Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
	    let c_type'' = 
	      compile_c_type_qualifier_list_opt c_type' 
		c_type_qualifier_list_opt
	    in
	    compile_c_pointer c_type'' c_pointer


  and compile_c_type_qualifier_list (c_type:O.c_type)
      (c_type_qualifier_list:I.c_type_qualifier list):O.c_type =
    List.fold_left 
      (fun c_type c_type_qualifier ->
	compile_c_type_qualifier (Some c_type)  c_type_qualifier) 
      c_type c_type_qualifier_list 
      
  and compile_c_parameter_type_list: I.c_parameter_type_list -> 
  O.param_type * string option list = 
    fun expr ->
      match expr with
	| I.Parameter_type_list_FIX c_parameter_list ->
	    let (type_list, arg_list) = 
	      compile_c_parameter_list c_parameter_list
	    in
	    (O.Param_type_fix (type_list), arg_list)
	      
	| I.Parameter_type_list_VAR c_parameter_list ->
	    let (type_list, arg_list) = 
	      compile_c_parameter_list c_parameter_list
	    in
	    (O.Param_type_va (type_list), arg_list)
	      
  and compile_c_parameter_list: I.c_parameter_list -> 
  O.c_type list * string option list = 
    fun expr ->
      match expr with
	| I.Parameter_list l -> 
	    Safe_list.split (Safe_list.map compile_c_parameter_declaration l)
	      

  and compile_c_parameter_declaration = function
    | I.Parameter_declaration_1 (c_declaration_specifiers, c_declarator) ->
	begin
	  let (_, type_decls, c_type) = 
	    compile_c_declaration_specifiers c_declaration_specifiers
	  in
	  assert (type_decls = []);
	  match c_type with
	    | Some c_type -> 
		begin
		  let (c_type, id) = 
		    compile_c_declarator c_type c_declarator
		  in
		  (c_type, Some id)
		end
	    | None -> assert false
	end
	  
    | I.Parameter_declaration_2 (c_declaration_specifiers, 
      c_abstract_declarator_opt) 
      ->
	begin
	  let (_, type_decls, c_type_opt) = 
	    compile_c_declaration_specifiers c_declaration_specifiers
	  in
	  assert (type_decls = []);
	  match c_type_opt with
	    | Some c_type -> 
		(compile_c_abstract_declarator_opt 
		  c_type c_abstract_declarator_opt, None)
	    | None -> assert false
	end


  and compile_c_type_name: I.c_type_name -> O.c_declaration list * O.c_type = 
    fun expr ->
      match expr with
	| I.Type_name (c_specifier_qualifier_list, c_abstract_declarator_opt) ->
	    begin
	      let (type_decls, c_type) = 
		compile_c_specifier_qualifier_list c_specifier_qualifier_list
	      in
	      (type_decls, 
	      compile_c_abstract_declarator_opt 
		c_type c_abstract_declarator_opt)
	    end
	      
  and compile_c_abstract_declarator: O.c_type -> I.c_abstract_declarator -> 
  O.c_type = 
    fun c_type expr ->
      match expr with
	| I.Abstract_declarator_1 c_pointer -> 
	    begin
	      compile_c_pointer c_type c_pointer
	    end
	| I.Abstract_declarator_2 
	    (c_pointer_opt, c_direct_abstract_declarator) 
	  ->
	    begin
	      let c_type = compile_c_pointer_opt c_type c_pointer_opt
	      in
	      compile_c_direct_abstract_declarator 
		c_type c_direct_abstract_declarator
	    end

  and compile_c_direct_abstract_declarator: O.c_type -> 
  I.c_direct_abstract_declarator -> 
  O.c_type = 
    fun c_type expr ->
      match expr with
	| I.Direct_abstract_declarator_error -> assert false
	| I.Direct_abstract_declarator_1 c_abstract_declarator ->
	    compile_c_abstract_declarator c_type c_abstract_declarator

	| I.Direct_abstract_declarator_2 
	    (c_direct_abstract_declarator_opt, 
	    c_assignment_expression_opt) 
	  ->
	    begin
	      let c_type = 
		match c_assignment_expression_opt with
		  | Some e -> 
		      O.Array 
			(c_type, compile_c_assignment_expression
			  ~is_init:false e)
		  | None -> O.Xarray c_type
	      in
	      compile_c_direct_abstract_declarator_opt 
		c_type c_direct_abstract_declarator_opt
	    end
	      
	| I.Direct_abstract_declarator_3_STAR 
	    c_direct_abstract_declarator_opt  
	  ->
	    begin
	      let c_type = O.Xarray (c_type)
	      in
	      compile_c_direct_abstract_declarator_opt 
		c_type c_direct_abstract_declarator_opt
	    end
	      
	| I.Direct_abstract_declarator_4 
	    (c_direct_abstract_declarator_opt, 
	    c_parameter_type_list_opt) 
	  ->
	    begin
	      let (param_type, ids) = 
		compile_c_parameter_type_list_opt c_parameter_type_list_opt 
	      in
	      let c_type = 
		O.Function_type 
		  { 
		    O.func_name = None; 
		    O.formal_param = ids;
		    O.param_type = param_type; 
		    O.return_type = c_type;
		    O.func_attributes = [];
		  }
	      in
	      compile_c_direct_abstract_declarator_opt 
		c_type c_direct_abstract_declarator_opt
	    end
	      
  and compile_c_initializer: I.c_initializer -> O.c_initializer = 
    fun expr ->
      match expr with
	| I.Initializer_1 c_assignment_expression ->
	    O.Initializer_1 
	      (compile_c_assignment_expression ~is_init:true 
		c_assignment_expression)
	      
	| I.Initializer_2 c_initializer_list  ->
	    O.Initializer_2 (compile_c_initializer_list c_initializer_list)
	      
  and compile_c_initializer_list: I.c_initializer_list -> 
  O.c_initializer_list = 
    fun expr ->
      let new_lst = 
	(Safe_list.map 
	  (fun (c_designator_list, c_initializer) -> 
	    let c_initializer = compile_c_initializer c_initializer
	    in
	    (Safe_list.map compile_c_designator c_designator_list,
	    c_initializer)
	  ) 
	  expr
	)
      in
      (new_lst)
	
  and compile_c_designator = function
    | I.Designator_1 c_constant_expression ->
	O.Designator_1 (compile_c_constant_expression c_constant_expression)

    | I.Designator_2 c_identifier ->
	O.Designator_2 (c_identifier)
	  
    | I.Designator_gnu_range (e0, e1) ->
	O.Designator_gnu_range 
	  (compile_c_constant_expression e0,
	  compile_c_constant_expression e1)

  and compile_c_specifier_qualifier_list_opt expr = 
    match expr with
      | Some c_specifier_qualifier_list ->
	  let (l, t) = 
	    compile_c_specifier_qualifier_list c_specifier_qualifier_list
	  in
	  (l, Some t)
      | None -> 
	  ([], None)

  and compile_c_abstract_declarator_opt c_type expr = 
    match expr with
      | Some c_abstract_declarator ->
	  compile_c_abstract_declarator c_type c_abstract_declarator
      | None -> 
	  c_type
	    
  and compile_c_type_qualifier_list_opt c_type expr = 
    match expr with
      | Some c_type_qualifier_list ->
	  compile_c_type_qualifier_list c_type c_type_qualifier_list
      | None -> 
	  c_type

  and compile_c_pointer_opt c_type expr = 
    match expr with
      | Some c_pointer -> 
	  compile_c_pointer c_type c_pointer
      | None -> 
	  c_type
	    
  and compile_c_direct_abstract_declarator_opt c_type expr = 
    match expr with
      | Some c_direct_abstract_declarator ->
	  compile_c_direct_abstract_declarator 
	    c_type c_direct_abstract_declarator
      | None -> 
	  c_type
	    
  and compile_c_parameter_type_list_opt expr = 
    match expr with
      | Some c_parameter_type_list ->
	  compile_c_parameter_type_list c_parameter_type_list
      | None -> 
	  (O.Param_type_fix [], [])

  and compile_asm (str_list, asm_details_opt) = 
    let asm_details_opt = match asm_details_opt with
      | Some asm_details ->
	  let f (so, s, e) = (so, s, compile_c_expression e)
	  in
	  Some 
	    ({
	      O.asm_outputs = Safe_list.map f asm_details.I.asm_outputs;
	      O.asm_inputs = Safe_list.map f asm_details.I.asm_inputs;
	      O.asm_clobbers = asm_details.I.asm_clobbers;
	    })
      | None -> None
    in
    O.ASM (str_list, asm_details_opt)

  and compile_c_stmt010 = function
    | I.STMT_AT (coord, stmt) -> O.STMT_AT (coord, compile_c_stmt010 stmt)
	
    | I.NOP -> O.NOP
	
    | I.SEQUENCE (txt_opt, c_stmt010_list) ->
	O.SEQUENCE (txt_opt, Safe_list.map compile_c_stmt010 c_stmt010_list)
	  
    | I.COMPUTATION (c_expression) ->
	O.COMPUTATION (compile_c_expression c_expression)
	  
    | I.COMPOUND (txt_opt, c_compound_stmt010) ->
	O.COMPOUND (txt_opt, compile_c_compound_stmt010 c_compound_stmt010)
	  
    | I.IF (c_expression, then_c_stmt010, else_c_stmt010) ->
	O.IF (compile_c_expression c_expression, 
	compile_c_stmt010 then_c_stmt010,
	compile_c_stmt010 else_c_stmt010)

    | I.WHILE (c_expression, c_stmt010) -> 
	O.WHILE (compile_c_expression c_expression,
	compile_c_stmt010 c_stmt010)

    | I.LOOP (c_stmt010) -> 
	O.LOOP (compile_c_stmt010 c_stmt010)

    | I.BREAK -> O.BREAK
    | I.CONTINUE -> O.CONTINUE
    | I.RETURN_VALUE (c_expression) -> 
	O.RETURN_VALUE (compile_c_expression c_expression)

    | I.RETURN -> O.RETURN
    | I.SWITCH (c_expression, c_stmt010) ->
	O.SWITCH (compile_c_expression c_expression, 
	compile_c_stmt010 c_stmt010)

    | I.CASE (c_constant_expression, c_stmt010) ->
	O.CASE (compile_c_constant_expression c_constant_expression, 
	compile_c_stmt010 c_stmt010)
	  
    | I.CASE_RANGE (e0, e1, c_statement) ->
	O.CASE_RANGE 
	  (compile_c_constant_expression e0,
	  compile_c_constant_expression e1,
	  compile_c_stmt010 c_statement)

    | I.DEFAULT (c_stmt010) ->
	O.DEFAULT (compile_c_stmt010 c_stmt010)

    | I.LABEL (string, c_stmt010) ->
	O.LABEL (string, compile_c_stmt010 c_stmt010)
	  
    | I.GOTO (string) -> 
	O.GOTO (string)

    | I.GCC_GOTO expr ->
	O.GCC_GOTO (compile_c_expression expr)
	  
    | I.ASM (u,v) ->
	compile_asm (u,v)
	  
  and compile_c_compound_stmt010 expr =       
    match expr with
      | I.BLOCK (labels, decls, stmts) ->
	  let c_decls = compile_c_declaration_list false decls
	  in
	  O.BLOCK (labels, c_decls, Safe_list.map compile_c_stmt010 stmts)

  and compile_c_translation_unit = function
    | I.Translation_unit l ->
	let l0 = 
	  (Safe_list.map
	    (fun external_declaration -> 
	      compile_c_external_declaration external_declaration
	    ) l
	  )
	and str_decls = export_strs ()
	in O.Translation_unit (Safe_list.concat str_decls l0)

  and hoist_attribute t = 
    match t with
      | O.Typeid _ -> assert false
      | O.GCC_attribute_type (t,atts) -> 
	  let (t, atts0) = hoist_attribute t
	  in (t, atts0 @ atts)
      | O.Primitive_type _ -> (t, [])
      | O.Pointer c_type -> 
	  let (t, atts) = hoist_attribute c_type
	  in (O.Pointer t, atts)
      | O.Array (c_type, c_expr020) -> 
	  let (t, atts) = hoist_attribute c_type
	  in (O.Array (t, c_expr020), atts)
      | O.Xarray c_type -> 
	  let (t, atts) = hoist_attribute c_type
	  in (O.Xarray t, atts)
      | O.Struct_type (string, fields,_) -> (t, [])
      | O.Struct_type_name string -> (t, [])
      | O.Union_type (string, fields) -> (t, [])
      | O.Union_type_name string -> (t, [])
      | O.Enum_type (string, _) -> (t, [])
      | O.Enum_type_name string -> (t, [])
      | O.Function_type function_type -> (t, [])
      | O.Function_type_ex (function_type, params) -> (t, [])
      | O.Typename string -> (t, [])
      | O.Typeof expr -> (t, [])
      | O.Qualified_type (q, c_type) ->
	  let (c_type, atts) = hoist_attribute c_type
	  in
	  (O.Qualified_type (q, c_type), atts)
	    
      | O.Incomplete_qualified_type (c_type_opt, q) ->
	  (t, [])
	    
      | O.Incomplete_gnu_attribute_type (c_type_opt, atts0) -> assert false

  and fixup_function_type t =
    let (ret_typ, atts) = hoist_attribute t.O.return_type
    in
    if atts = [] then
      t
    else
      {
	O.func_name = t.O.func_name;
	O.param_type = t.O.param_type;
	O.formal_param = t.O.formal_param;
	O.return_type = ret_typ;
	O.func_attributes = atts;
      }
	
  and fixup_c_declaration_ex c_declaration_ex =
    let (c_declaration, v) = c_declaration_ex
    in
    match c_declaration with
      | O.Obj_decl (linkage, c_type, str) ->
	  begin
	    let c_type = match c_type with
	      | O.Function_type t ->
		  O.Function_type (fixup_function_type t)
	      | O.Function_type_ex (function_type, lst) ->
		  O.Function_type_ex (fixup_function_type function_type, lst)
	      | _ -> c_type
	    in (O.Obj_decl (linkage, c_type, str), v)
	  end
      | O.Obj_decl_init _ -> c_declaration_ex
      | O.Typedef _ -> c_declaration_ex
      | O.Type_decl _ -> c_declaration_ex
      | O.Type_only _ -> c_declaration_ex

  and compile_c_external_declaration = function
    | I.External_declaration_at (coord, expr) ->
	O.External_declaration_at 
	  (coord, compile_c_external_declaration expr)
	  
    | I.External_declaration_1 (c_function_definition) ->
	
	  (compile_c_function_definition c_function_definition)
	  
    | I.External_declaration_2 (c_declaration) ->
	let decls = compile_c_declaration true c_declaration
	in
	let decls = List.map fixup_c_declaration_ex decls
	in O.External_declaration_2 decls
	     
  and compile_c_function_definition = function
    | I.Function_definition 
	(c_declaration_specifiers, c_declarator,
	c_declaration_list_opt, c_compound_stmt010) ->
	let (storage_class_opt, type_decls, ret_type_opt) = 
	  compile_c_declaration_specifiers c_declaration_specifiers
	in
	assert (type_decls = []);
	let storage_class = match storage_class_opt with
	  | Some v -> v
	  | None -> O.Default_extern
	in
	let ret_type = match ret_type_opt with
	  | Some ret_type -> ret_type
	  | None -> O.Primitive_type O.Int
	in
	let (fun_type, id) = compile_c_declarator ret_type c_declarator
	in
	let fun_type = match fun_type with
	  | O.Function_type v -> 
	      let v = fixup_function_type v
	      in compile_c_declaration_list_opt v c_declaration_list_opt 
	  | _ -> assert false
	in
	let linkage = match storage_class with
	  | O.Type_alias -> assert false
	  | O.Default_extern -> O.Default_extern
	  | O.Default -> O.Default
	  | O.Extern -> O.Extern
	  | O.Extern_Inline -> O.Extern_Inline
	  | O.Static -> O.Static
	  | O.Auto -> assert false
	  | O.Inline -> O.Inline
	  | O.Static_Inline -> O.Static_Inline
	  | O.Register -> assert false
	  | O.Thread -> assert false
	  | O.Extern_Thread -> assert false
	  | O.Static_Thread -> assert false
	in
	let fun_decl = 
	  O.Function_definition 
	    (linkage, fun_type, id, 
	    compile_c_compound_stmt010 c_compound_stmt010)
	in
	match id with
	  | "gnu_dev_major"
	  | "gnu_dev_minor"
	  | "gnu_dev_makedev" -> 
	      if !keep_gnu_dev_all then
		O.External_declaration_1 fun_decl
	      else
		O.External_declaration_2 [O.Obj_decl (O.Extern, fun_type, id), ref false]
	  | _ -> 
	      O.External_declaration_1 fun_decl
	      
  and compile_c_declaration_list: bool -> I.c_declaration list -> 
  O.c_declaration_list = 
    fun is_global c_declaration_list ->
      let v = 
	Safe_list.map (compile_c_declaration is_global) c_declaration_list
      in
      Safe_list.flatten v

  and compile_c_declaration_list_opt: O.function_type -> 
  I.c_declaration list option -> O.c_type = 
    fun function_type expr ->
      match expr with
	| Some c_declaration_list -> 
	    begin
	      let l = 
		compile_c_declaration_list false c_declaration_list
	      in
	      O.Function_type_ex 
		(function_type, 
		Safe_list.map 
		  (fun (c_decl, v) ->
		    match c_decl with
		      | O.Obj_decl (O.Auto, typ, id) -> (typ, id)
		      | O.Obj_decl (O.Register, typ, id) -> (typ, id)
		      | _ -> assert false
		  ) l
		)
	    end
	| None -> 
	    O.Function_type function_type
	      
  and get_c_declarator_id: I.c_declarator -> string = 
    fun expr ->
      match expr with
	| I.Declarator (c_pointer_opt, c_direct_declarator) -> 
	    get_c_direct_declarator_id c_direct_declarator
	      
	| I.Declarator_GNU (declarator, attributes) -> assert false
	    
  and get_c_direct_declarator_id: I.c_direct_declarator -> string = 
    fun expr ->
      match expr with
	| I.Direct_declarator_1 c_identifier ->
	    c_identifier
	      
	| I.Direct_declarator_2 c_declarator ->
	    get_c_declarator_id c_declarator
	      
	| I.Direct_declarator_3 (c_direct_declarator, _, _)
	| I.Direct_declarator_4_STATIC (c_direct_declarator, _, _)
	| I.Direct_declarator_5_STATIC (c_direct_declarator, _, _)
	| I.Direct_declarator_6_STAR (c_direct_declarator, _)
	| I.Direct_declarator_7 (c_direct_declarator, _)
	| I.Direct_declarator_8 (c_direct_declarator, _)
	  ->
	    get_c_direct_declarator_id c_direct_declarator
  in compile_c_translation_unit c_translation_unit
      
