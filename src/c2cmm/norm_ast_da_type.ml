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
open Ast_da_type
open Collection

exception Found

let current_coord = ref ("", 0, 0)

type usage_t =
  | USED
  | NOTYET of c_declaration_ex


let disable_normalization = ref false

let user_opts = [
  ("--ast-da-type-disable-norm",
   Arg.Set disable_normalization,
   "disable normalization of type030 syntax tree which eliminate unused identifiers");
]

let normalize: c_translation_unit -> c_translation_unit = 
  fun c_translation_unit ->
    let changed = ref false
    and usage_stack = Stack.create ()
    in
    let rec push_scope () = 
      Stack.push (StringHashtbl.create 211) usage_stack
    and pop_scope () = 
      ignore(Stack.pop usage_stack)
	
    and add_c_decl key c_decl =
      let top_usage_hash = Stack.top usage_stack
      and is_used = ref false
      in
      let usages = StringHashtbl.find_all top_usage_hash key
      in
      match usages with
	| [USED] -> use_c_declaration c_decl; is_used := true
	| a::l ->
	    begin
	      if !is_used then
		Safe_list.iter
		  (function 
		    | USED -> assert false
		    | NOTYET c_decl0 ->
			let (expr0, is_used0) = c_decl0
			in
			assert (not !is_used0);
			use_c_declaration c_decl0;
			StringHashtbl.remove top_usage_hash key
		  ) usages
	      else
		StringHashtbl.add top_usage_hash key (NOTYET c_decl)
	    end
	| [] ->
	    StringHashtbl.add top_usage_hash key (NOTYET c_decl)

    and use_id key = 
      try 
	begin
	  Stack.iter
	    (fun usage_hash -> 
	      try
		begin
		  let c_decls = StringHashtbl.find_all usage_hash key
		  in
		  match c_decls with
		    | [USED] -> raise Found
		    | [] -> ()
		    | _ ->
			begin
			  Safe_list.iter
			    (fun c_decl_opt ->
			      let _ = match c_decl_opt with
				| NOTYET c_decl -> 
				    use_c_declaration c_decl
				| USED -> ()
			      in
			      StringHashtbl.remove usage_hash key
			    ) c_decls;
			  StringHashtbl.add usage_hash key USED;
			  raise Found
			end
		end
	      with
		  Not_found -> ()
	    ) usage_stack;
	  raise Not_found
	end
      with
	| Found -> ()
	| Not_found ->
	    let top_usage_hash = Stack.top usage_stack
	    in
	    StringHashtbl.add top_usage_hash key USED
	      
    and use_c_expr020: c_expr020 -> unit = 
      fun expr ->
	match expr with
	  | Comma (expr0, expr1) -> 
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1
	      end
		
	  | Constant c_constant -> 
	      begin
		(* always use wchar *)
		use_c_type (Typename "wchar_t")
		  (*
		match c_constant with
		    | C99.Constant_wcharacter _ -> 
		    use_c_type (Typename "wchar_t")
		  | _ -> ()
		  *)
	      end

	  | String c_string_literal -> 
	      begin
		match c_string_literal with
		  | C_syntax_symbol.String_literal _ -> ()
		  | C_syntax_symbol.WString_literal _ -> 
		      use_c_type (Typename "wchar_t")
	      end

	  | Call (expr0, expr_list) -> 
	      begin
		use_c_expr020 expr0;
		Safe_list.iter use_c_expr020 expr_list;
	      end

	  | Macro_va_start (expr0, expr1) -> 
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end

	  | Macro_va_arg (expr0, c_type) ->
	      use_c_expr020 expr0;
	      use_c_type c_type;
	      
	  | Macro_va_end expr0 -> 
	      use_c_expr020 expr0

	  | Builtin_types_compatible (t0, t1) ->
	      use_c_type t0;
	      use_c_type t1

	  | Builtin_constant_p (e0) ->
	      use_c_expr020 e0

	  | Builtin_expect (e0, e1) ->
	      use_c_expr020 e0;
	      use_c_constant_expression e1
		
	  | Gnu_block v -> 
	      ignore(normalize_c_compound_stmt010 v)
		
	  | Gnu_labeladdr string -> use_id string
		
	  | Variable string -> use_id string

	  | Memberof (expr0, _) -> 
	      use_c_expr020 expr0
		
	  | Memberof_ptr (expr0, _) -> 
	      use_c_expr020 expr0
		
	  | Indexof (expr0, expr1) -> 
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end
		
	  | Binary_arithm (bop, expr0, expr1) ->
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end
		
	  | Binary_predicate (bop, expr0, expr1) ->
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end

	  | Binary_logic (bop, expr0, expr1) ->
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end
		
	  | Unary_arithm (uop, expr0) ->
	      begin
		use_c_expr020 expr0;
	      end
		
	  | Logic_not expr0 -> 
	      use_c_expr020 expr0
		
	  | Sizeof_expr expr0 -> 
	      use_c_expr020 expr0
		
	  | Sizeof_type (c_type, has_def) -> 
	      if not has_def then
		use_c_type c_type

	  | Alignof_expr expr0 -> 
	      use_c_expr020 expr0
		
	  | Alignof_type (c_type, has_def) -> 
	      if not has_def then
		use_c_type c_type
		  
	  | Cast (c_type, expr0) ->
	      use_c_type c_type;
	      use_c_expr020 expr0;

	      
	  | Assign (expr0, expr1) -> 
	      begin
		use_c_expr020 expr0;
		use_c_expr020 expr1;
	      end
		
	  | Post_decr expr0 -> 
	      use_c_expr020 expr0
		
	  | Post_incr expr0 ->
	      use_c_expr020 expr0
		
	  | Pre_decr expr0 -> 
	      use_c_expr020 expr0
		
	  | Pre_incr expr0 -> 
	      use_c_expr020 expr0
		
	  | Assign_arithm (binary_arithmatic, expr0, expr1) ->
	      begin
		use_c_expr020 expr0; 
		use_c_expr020 expr1;
	      end

	  | Cast_init (c_type, c_initializer_list) -> 
	      use_c_type c_type;
	      use_c_initializer_list c_initializer_list
		
	  | Memof expr0 -> 
	      use_c_expr020 expr0
		
	  | Addrof expr0 -> 
	      use_c_expr020 expr0
		
	  | Question (expr0, expr1, expr2) ->
	      begin
		use_c_expr020 expr0;
		Mapping.apply_opt use_c_expr020 expr1;
		use_c_expr020 expr2;
	      end

    and use_c_expr020_opt c_expr020_opt =
      match c_expr020_opt with
	| Some c_expr020 -> use_c_expr020 c_expr020
	| None -> ()
	    
    and use_c_assignment_expression: c_assignment_expression -> unit =
      fun expr -> use_c_expr020 expr
	
    and use_c_expression: c_expression -> unit = 
      fun expr ->
	use_c_expr020 expr
	  
    and use_c_constant_expression: c_constant_expression -> unit =
      fun expr -> 
	use_c_expr020 expr

    and use_fields: field list -> unit = 
      fun fields ->
	let use_field_type: c_type -> unit =
	  fun expr ->
	    match expr with
	      | Typeid _ -> assert false
	      | GCC_attribute_type (t,_) -> use_c_type t
	      | Primitive_type _ -> ()
	      | Pointer c_type -> use_c_type c_type
	      | Array (c_type, c_expr020) -> 
		  use_c_type c_type;
		  use_c_expr020 c_expr020
	      | Xarray c_type -> use_c_type c_type
	      | Struct_type (string, fields, layout) -> 
		  use_fields fields
	      | Struct_type_name string -> 
		  use_id ("struct-" ^ string)
	      | Union_type (string, fields) -> 
		  use_fields fields
	      | Union_type_name string -> 
		  use_id ("union-" ^ string)
	      | Enum_type (string, enum_items) -> 
		  use_enum_items enum_items
	      | Enum_type_name string -> 
		  use_id ("enum-" ^ string)
	      | Function_type function_type -> 
		  use_function_type function_type;
	      | Function_type_ex (function_type, params) ->
		  begin
		    use_function_type function_type;
		    Safe_list.iter
		      (fun (c_type, _) -> use_c_type c_type)
		      params
		  end
	      | Typename string -> use_id string
	      | Typeof expr -> use_c_expression expr
	      | Qualified_type (_, c_type) ->
		  use_c_type c_type
	      | Incomplete_qualified_type (c_type_opt, _) ->
		  begin
		    match c_type_opt with
		      | Some c_type -> use_c_type c_type
		      | None -> ()
		  end
	      | Incomplete_gnu_attribute_type (c_type_opt, _) ->
		  begin
		    match c_type_opt with
		      | Some c_type -> use_c_type c_type
		      | None -> ()
		  end
	in
	Safe_list.iter
	  (function Regular (c_type, _) -> use_field_type c_type
	    | Bits (c_type, _, _) -> use_field_type c_type
	  ) fields

    and use_enum_items: enum_item list -> unit = 
      fun enum_items -> 
	Safe_list.iter
	  (fun (id, expr020_opt) ->
	    use_c_expr020_opt expr020_opt
	  ) enum_items

    and use_c_type: c_type -> unit =
      fun expr ->
	match expr with
	  | Typeid _ -> assert false
	  | GCC_attribute_type (t,_) -> use_c_type t
	  | Primitive_type _ -> ()
	  | Pointer c_type -> use_c_type c_type
	  | Array (c_type, c_expr020) -> 
	      use_c_type c_type;
	      use_c_expr020 c_expr020
	  | Xarray c_type -> use_c_type c_type
	  | Struct_type (string, fields, layout) ->  
	      use_id ("struct-" ^ string);
	  | Struct_type_name string -> 
	      use_id ("struct-" ^ string)
	  | Union_type (string, fields) -> 
	      use_id ("union-" ^ string);
	  | Union_type_name string -> 
	      use_id ("union-" ^ string)
	  | Enum_type (string, _) -> 
	      use_id ("enum-" ^ string)
	  | Enum_type_name string -> 
	      use_id ("enum-" ^ string)
	  | Function_type function_type -> 
	      use_function_type function_type;
	  | Function_type_ex (function_type, params) ->
	      begin
		use_function_type function_type;
		Safe_list.iter
		  (fun (c_type, _) -> use_c_type c_type)
		  params
	      end
	  | Typename string -> use_id string
	  | Typeof expr -> use_c_expression expr
	  | Qualified_type (_, c_type) ->
	      use_c_type c_type
	  | Incomplete_qualified_type (c_type_opt, _) ->
	      begin
		match c_type_opt with
		  | Some c_type -> use_c_type c_type
		  | None -> ()
	      end
	  | Incomplete_gnu_attribute_type (c_type_opt, _) ->
	      begin
		match c_type_opt with
		  | Some c_type -> use_c_type c_type
		  | None -> ()
	      end

    and use_function_type: function_type -> unit =
      fun function_type ->
	use_c_type function_type.return_type;
	match function_type.param_type with
	  | Param_type_va l 
	  | Param_type_fix l -> Safe_list.iter use_c_type l 
	  | Param_list _ -> ()
	      

    and use_c_declaration: c_declaration_ex -> unit = 
      fun (expr, is_used) ->
	if not !is_used then
	  begin
	    is_used := true;
	    match expr with
	      | Obj_decl (_, c_type, _) -> use_c_type c_type
	      | Obj_decl_init (_, c_type, _, c_initializer) -> 
		  use_c_type c_type;
		  use_c_initializer c_initializer
	      | Typedef (c_type, _) ->
		  use_c_type c_type
	      | Type_decl c_type ->
		  begin
		    match c_type with
		      | Typeid _ -> assert false
		      | GCC_attribute_type (t,_) -> 
			  begin
			    (* recursivelly mark inner type as used declaration *)
			    let _ = is_used := false
			    in
			    use_c_declaration (Type_decl t, is_used)
			  end
		      | Primitive_type _ -> ()
		      | Pointer c_type -> use_c_type c_type
		      | Array (c_type, c_expr020) -> 
			  use_c_type c_type;
			  use_c_expr020 c_expr020
		      | Xarray c_type -> use_c_type c_type
		      | Struct_type (string, fields, layout) -> 
			  use_fields fields
		      | Struct_type_name string -> 
			  ()
		      | Union_type (string, fields) -> 
			  use_fields fields
		      | Union_type_name string -> ()
		      | Enum_type (string, enum_items) -> 
			  use_enum_items enum_items
		      | Enum_type_name string -> ()
		      | Function_type function_type -> 
			  use_function_type function_type;
		      | Function_type_ex (function_type, params) ->
			  begin
			    use_function_type function_type;
			    Safe_list.iter
			      (fun (c_type, _) -> use_c_type c_type)
			      params
			  end
		      | Typename string -> ()
		      | Typeof expr -> ()
		      | Qualified_type (_, c_type) ->
			  use_c_type c_type
		      | Incomplete_qualified_type (c_type_opt, _) ->
			  begin
			    match c_type_opt with
			      | Some c_type -> use_c_type c_type
			      | None -> ()
			  end
		      | Incomplete_gnu_attribute_type (c_type_opt, _) ->
			  begin
			    match c_type_opt with
			      | Some c_type -> use_c_type c_type
			      | None -> ()
			  end
		  end
	      | Type_only c_type -> ()
	  end

    and register_c_declaration: c_declaration_ex -> unit =
      fun expr ->
	let (expr0, is_used) = expr
	in
	match expr0 with
	  | Obj_decl (linkage, c_type, string) -> 
	      begin
		add_c_decl string expr;
		match linkage with
		  | Extern
		  | Static -> ()
		  | _ -> 
		      use_c_type c_type;
		      use_id string;
	      end
		
	  | Obj_decl_init (linkage, c_type, string, c_initializer) ->
	      begin
		use_c_initializer c_initializer;
		add_c_decl string expr;
		match linkage with
		  | Extern
		  | Static -> ()
		  | _ -> use_id string
	      end
		
	  | Typedef (c_type, string) ->
	      add_c_decl string expr

	  | Type_decl c_type -> 
	      begin
		let rec f c_type = 
		  match c_type with
		    | Struct_type (string, _, _) ->
			add_c_decl ("struct-" ^ string) expr
		    | Enum_type (string, enum_item_list) ->
			add_c_decl ("enum-" ^ string) expr;
			Safe_list.iter
			  (fun (id, c_expr020) -> 
			    add_c_decl id expr
			  ) enum_item_list
			  
		    | Union_type (string, _) ->
			add_c_decl ("union-" ^ string) expr
		    | Function_type function_type 
		    | Function_type_ex (function_type, _) ->
		      begin
			match function_type.func_name with
			  | Some id -> add_c_decl id expr
			  | None -> ()
		      end
		  | GCC_attribute_type (t, atts) ->
		      f t
		  | _ -> ()
		in f c_type
	      end
		
	  | Type_only c_type ->
	      begin
		let rec f = function
		  | Struct_type_name string ->
		      add_c_decl ("struct-" ^ string) expr
		  | Enum_type_name string ->
		      add_c_decl ("enum-" ^ string) expr
		  | Union_type_name string ->
		      add_c_decl ("union-" ^ string) expr
		  | GCC_attribute_type (typ, attributes) ->
		      f typ
		  | _ -> assert false
		in f c_type
	      end
		
    and use_c_initializer: c_initializer -> unit = 
      fun expr ->
	match expr with
	  | Initializer_1 c_assignment_expression ->
	      (use_c_assignment_expression c_assignment_expression)
		
	  | Initializer_2 c_initializer_list  ->
	      (use_c_initializer_list c_initializer_list)

    and use_c_initializer_list: c_initializer_list -> unit = 
      fun expr ->
	Safe_list.iter
	  (fun (c_designator_list, c_initializer) -> 
	    Safe_list.iter use_c_designator c_designator_list;
	    use_c_initializer c_initializer
	  ) expr

    and use_c_designator: c_designator -> unit =
      fun expr ->
	match expr with
	  | Designator_1 c_constant_expression ->
	      use_c_constant_expression c_constant_expression
	  | Designator_2 _ -> ()

	  | Designator_gnu_range (e0, e1) ->
	      let _ = use_c_constant_expression e0
	      in use_c_constant_expression e1

    and normalize_c_stmt010: c_stmt010 -> c_stmt010 = 
      fun expr ->
	match expr with
	  | STMT_AT (coord, stmt) -> STMT_AT(coord, normalize_c_stmt010 stmt)
	  
	  | NOP -> NOP

	  | SEQUENCE (str_opt, c_stmt010_list) ->
	      SEQUENCE (str_opt, Safe_list.map normalize_c_stmt010 c_stmt010_list)
		
	  | COMPUTATION (c_expression) ->
	      use_c_expression c_expression;
	      expr
		
	  | COMPOUND (str_opt, c_compound_stmt010) ->
	      COMPOUND 
		(str_opt, 
		normalize_c_compound_stmt010 c_compound_stmt010)
		
	  | IF (c_expression, then_c_stmt010, else_c_stmt010) ->
	      use_c_expression c_expression;
	      IF (c_expression,
	      normalize_c_stmt010 then_c_stmt010,
	      normalize_c_stmt010 else_c_stmt010)

	  | WHILE (c_expression, c_stmt010) -> 
	      use_c_expression c_expression;
	      WHILE (c_expression, normalize_c_stmt010 c_stmt010)

	  | LOOP (c_stmt010) -> 
	      LOOP (normalize_c_stmt010 c_stmt010)
		
	  | BREAK  -> expr
	  | CONTINUE  -> expr
	  | RETURN_VALUE (c_expression) -> 
	      use_c_expression c_expression;
	      expr
		
	  | RETURN -> expr
	  | SWITCH (c_expression, c_stmt010) ->
	      use_c_expression c_expression;
	      SWITCH (c_expression, normalize_c_stmt010 c_stmt010)
		
	  | CASE (c_constant_expression, c_stmt010) ->
	      use_c_constant_expression c_constant_expression;
	      CASE (c_constant_expression, 
	      normalize_c_stmt010 c_stmt010)

	  | CASE_RANGE (e0, e1, c_stmt010) ->
	      use_c_constant_expression e0;
	      use_c_constant_expression e1;
	      CASE_RANGE (e0, e1, normalize_c_stmt010 c_stmt010)
		
	  | DEFAULT (c_stmt010) ->
	      DEFAULT (normalize_c_stmt010 c_stmt010)

	  | LABEL (string, c_stmt010) ->
	      LABEL (string, normalize_c_stmt010 c_stmt010)

	  | GOTO _ -> expr

	  | GCC_GOTO expr ->
	      use_c_expression expr;
	      GCC_GOTO expr
		
	  | ASM (str_list, asm_details_opt) ->
	      let asm_details_opt = match asm_details_opt with
		| Some asm_details ->
		    let f (so, s, e) = 
		      let _ = use_c_expression e
		      in
		      (so, s, e)
		    in
		    Some 
		      ({
			asm_outputs = Safe_list.map f asm_details.asm_outputs;
			asm_inputs = Safe_list.map f asm_details.asm_inputs;
			asm_clobbers = asm_details.asm_clobbers;
		      })
		| None -> None
	      in
	      ASM (str_list, asm_details_opt)
	      
    and normalize_c_compound_stmt010: c_compound_stmt010 -> c_compound_stmt010 =
      fun expr ->
	let _ = push_scope ()
	in
	let BLOCK (labels, old_decls, stmts) = expr
	in
	let _ = Safe_list.iter register_c_declaration old_decls
	in
	let stmts = Safe_list.map normalize_c_stmt010 stmts
	in
	let new_decls = filter_c_declaration_list old_decls
	in 
	let _ = pop_scope ()
	in
	if Safe_list.length old_decls > Safe_list.length new_decls then
	  changed := true;
	BLOCK (labels, new_decls, stmts)


    and normalize_c_translation_unit: c_translation_unit -> c_translation_unit = 
      fun c_translation_unit ->
	match c_translation_unit with
	  | Translation_unit l ->
	      Translation_unit 
		(Safe_list.map
		  (fun external_declaration -> 
		    normalize_c_external_declaration external_declaration
		  ) l
		)
		
    and normalize_c_external_declaration: c_external_declaration -> c_external_declaration =
      fun expr ->
	match expr with
	  | External_declaration_at (coord, v) ->
	      External_declaration_at 
		(coord, normalize_c_external_declaration v)

	  | External_declaration_1 (c_function_definition) ->
	      External_declaration_1 (normalize_c_function_definition
	      c_function_definition)
		
	  | External_declaration_2 (c_declaration) ->
	      Safe_list.iter register_c_declaration c_declaration;
	      expr
		
	  | External_declaration_string (typ, id, c_string_literal) -> 
	      begin
	      	let _ = match c_string_literal with
		  | C_syntax_symbol.String_literal _ -> ()
		  | C_syntax_symbol.WString_literal _ -> 
		      use_c_type (Typename "wchar_t")
		in
		expr
	      end
	      
    and normalize_c_function_definition: c_function_definition -> 
    c_function_definition =
      fun expr ->
	let Function_definition 
	    (linkage,
	    function_type, 
	    fname,
	    c_compound_stmt010) = expr
	in
	use_c_type function_type;
	Function_definition 
	  (linkage,
	  function_type,
	  fname,
	  normalize_c_compound_stmt010 c_compound_stmt010)

    and filter_c_declaration_list: c_declaration_ex list -> c_declaration_ex list =
      fun exprs ->
	let lst = ref []
	in
	let rec filter linkage (expr, is_used) = 
	  match linkage with
	    | Static  
	    | Extern ->
		if !is_used then
		  lst := (expr, ref false)::!lst
	    | _ ->
		lst := (expr, ref false)::!lst
	in
	Safe_list.iter
	  (fun (expr, is_used) -> 
	    match expr with
	      | Obj_decl (linkage, _, string) 
	      | Obj_decl_init (linkage, _, string, _) ->
		  filter linkage (expr, is_used)
	      | Typedef (c_type, string) ->
		  filter (Static) (expr, is_used)
	      | Type_decl c_type -> 
		  if !is_used then
		    lst := (expr, ref false)::!lst
	      | Type_only _ -> 
		  if !is_used then
		    lst := (expr, ref false)::!lst
	  ) exprs;
	(Safe_list.rev !lst)

    and filter_c_translation_unit: c_translation_unit -> c_translation_unit = 
      fun c_translation_unit ->
	match c_translation_unit with
	  | Translation_unit l ->
	      let lst = ref []
	      in
	      let rec f expr =
		match expr with
		  | External_declaration_at (_, stmt) -> f stmt
		  | External_declaration_1 _ -> lst := expr::!lst
		  | External_declaration_2 (old_c_decls) -> 
		      let c_decls = filter_c_declaration_list old_c_decls
		      in
		      if Safe_list.length old_c_decls > Safe_list.length c_decls then
			changed := true;
		      if c_decls <> [] then
			lst := (External_declaration_2 (c_decls))::!lst
		  | External_declaration_string _ -> lst := expr::!lst
	      in
	      Safe_list.iter f l;
	      Translation_unit (Safe_list.rev !lst)
    in
    if !disable_normalization then
      c_translation_unit
    else
      let _ = changed := true
      and result = ref c_translation_unit
      in
      while !changed do
	begin
	  let _ = changed := false
	  and _ = push_scope ()
	  in
	  let v = normalize_c_translation_unit !result
	  in
	  let _ = result := filter_c_translation_unit v
	  in
	  ignore(pop_scope ())
	end;
      done;
      !result
