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

module I = Ast_aa_gram
module O = Call_graph
open Mapping

let user_opts = []

let compile_opt: ('a list -> 'b list) -> 'a list option -> 'b list =
  fun f a_opt ->
    match a_opt with
    | Some a -> f a
    | None -> []


let map_flatten: ('a -> 'b list) -> 'a list -> 'b list =
  fun f lst ->
    let lst_lst = List.map f lst
    in
    List.flatten lst_lst

let map_flatten_opt: ('a -> 'b list) -> 'a list option -> 'b list = 
  fun f a_list_opt ->
    compile_opt (map_flatten f) a_list_opt
      

let compile: I.c_translation_unit -> O.property list = 
  fun c_translation_unit ->
    let rec compile_c_identifier = function
      | I.Identifier s -> [s]
	    
    and compile_c_identifier_as_typ = function
      | I.Identifier_as_typ s -> []
	    
    and compile_c_enumeration_constant = function
      | I.Enumeration_constant c_identifier -> []
	    
    and compile_c_primary_expression = function
      | I.Primary_expression_1 c_identifier -> 
	  ([]:(O.property list))
	    
      | I.Primary_expression_2 c_constant -> []
	    
      | I.Primary_expression_3 c_string_literal -> []
	    
      | I.Primary_expression_4 c_expression ->
	  (compile_c_expression c_expression)
	    
      | I.Primary_expression_macro_va_arg (c_expression, c_type_name) -> 
	  (compile_c_expression c_expression)
	    
      | I.Primary_expression_macro_va_start (expr0, expr1) -> 
	  (compile_c_expression expr0) @ (compile_c_expression expr1)

      | I.Primary_expression_macro_va_end (c_expression) -> 
	  (compile_c_expression c_expression)
	    
    and compile_c_postfix_expression = function
      | I.Postfix_expression_1 c_primary_expression ->
	  (compile_c_primary_expression c_primary_expression)
	    
      | I.Postfix_expression_2 (c_postfix_expression, c_expression) ->
	  (compile_c_postfix_expression c_postfix_expression) @
	  (compile_c_expression c_expression)
	    
      | I.Postfix_expression_3 (c_postfix_expression, 
				c_argument_expression_list_opt) 
	->
	  (compile_c_postfix_expression c_postfix_expression) @
	  ( match c_argument_expression_list_opt with
	  | Some lst -> compile_c_argument_expression_list lst
	  | None -> [])
	    
      | I.Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	  (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	  (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	  (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	  (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Postfix_expression_8 (c_type_name, c_initializer_list) ->
	  (compile_c_initializer_list c_initializer_list)
	    
    and compile_c_argument_expression_list = function
      | I.Argument_expression_list l ->
	  map_flatten compile_c_assignment_expression l
	    
    and compile_c_unary_expression = function
      | I.Unary_expression_1 c_postfix_expression ->
	  (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Unary_expression_2_PLUS_PLUS c_unary_expression ->
	  (compile_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_3_MINUS_MINUS c_unary_expression ->
	  (compile_c_unary_expression c_unary_expression)

      | I.Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	  (compile_c_unary_operator c_unary_operator) @
	  (compile_c_cast_expression c_cast_expression)
	    
      | I.Unary_expression_5_SIZEOF c_unary_expression -> []
	    
      | I.Unary_expression_6_SIZEOF c_type_name -> []

      | I.Unary_expression_7_ALIGNOF c_unary_expression -> []
	    
      | I.Unary_expression_8_ALIGNOF c_type_name -> []

    and compile_c_unary_operator = function
      | I.Unary_operator_AND -> [O.AddrOf]
	    
      | I.Unary_operator_STAR ->  [O.Deref]
	    
      | I.Unary_operator_PLUS ->  []
	    
      | I.Unary_operator_MINUS ->  []
	    
      | I.Unary_operator_TILDE -> []
	    
      | I.Unary_operator_EXCLAM -> []
	    
    and compile_c_cast_expression = function
      | I.Cast_expression_1 c_unary_expression ->
	  (compile_c_unary_expression c_unary_expression)
	    
      | I.Cast_expression_2 (c_type_name, c_cast_expression) ->
	  (compile_c_cast_expression c_cast_expression)
	    
    and compile_c_multiplicative_expression = function
      | I.Multiplicative_expression_1 c_cast_expression ->
	  (compile_c_cast_expression c_cast_expression)

      | I.Multiplicative_expression_2_STAR 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (compile_c_multiplicative_expression c_multiplicative_expression) @
	  (compile_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_3_SLASH 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (compile_c_multiplicative_expression c_multiplicative_expression) @
	  (compile_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_4_PERCENT 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (compile_c_multiplicative_expression c_multiplicative_expression) @
	  (compile_c_cast_expression c_cast_expression)
	    
    and compile_c_additive_expression = function
      | I.Additive_expression_1 c_multiplicative_expression ->
	  (compile_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_2_PLUS (c_additive_expression, 
				      c_multiplicative_expression) 
	->
	  (compile_c_additive_expression c_additive_expression) @
	  (compile_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_3_MINUS (c_additive_expression, 
				       c_multiplicative_expression) 
	->
	  (compile_c_additive_expression c_additive_expression) @
	  (compile_c_multiplicative_expression c_multiplicative_expression)

    and compile_c_shift_expression = function
      | I.Shift_expression_1 c_additive_expression ->
	  (compile_c_additive_expression c_additive_expression)

      | I.Shift_expression_2_INF_INF (c_shift_expression, 
				      c_additive_expression) 
	->
	  (compile_c_shift_expression c_shift_expression) @
	  (compile_c_additive_expression c_additive_expression)
	    
      | I.Shift_expression_3_SUP_SUP (c_shift_expression, 
				      c_additive_expression) 
	->
	  (compile_c_shift_expression c_shift_expression) @
	  (compile_c_additive_expression c_additive_expression)

    and compile_c_relational_expression = function
      | I.Relational_expression_1 c_shift_expression ->
	  (compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_2_INF (c_relational_expression, 
				       c_shift_expression) 
	->
	  (compile_c_relational_expression c_relational_expression) @
	  (compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_3_SUP (c_relational_expression, 
				       c_shift_expression) 
	->
	  (compile_c_relational_expression c_relational_expression) @
	  (compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_4_INF_EQ (c_relational_expression, 
					  c_shift_expression) 
	->
	  (compile_c_relational_expression c_relational_expression) @
	  (compile_c_shift_expression c_shift_expression)
	    
      | I.Relational_expression_5_SUP_EQ (c_relational_expression, 
					  c_shift_expression) 
	->
	  (compile_c_relational_expression c_relational_expression) @
	  (compile_c_shift_expression c_shift_expression)

    and compile_c_equality_expression = function
      | I.Equality_expression_1 c_relational_expression ->
	  (compile_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_2_EQ_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  (compile_c_equality_expression c_equality_expression) @
	  (compile_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_3_EXCLAM_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  (compile_c_equality_expression c_equality_expression) @
	  (compile_c_relational_expression c_relational_expression)

    and compile_c_and_expression = function
      | I.And_expression_1 c_equality_expression ->
	  (compile_c_equality_expression c_equality_expression)
	    
      | I.And_expression_2_AND (c_and_expression, c_equality_expression) ->
	  (compile_c_and_expression c_and_expression) @
	  (compile_c_equality_expression c_equality_expression)
	    
    and compile_c_exclusive_or_expression = function
      | I.Exclusive_or_expression_1 c_and_expression ->
	  (compile_c_and_expression c_and_expression)

      | I.Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, 
					  c_and_expression) 
	->
	  (compile_c_exclusive_or_expression c_exclusive_or_expression) @
	  (compile_c_and_expression c_and_expression)
	    
    and compile_c_inclusive_or_expression = function
      | I.Inclusive_or_expression_1 c_exclusive_or_expression ->
	  (compile_c_exclusive_or_expression c_exclusive_or_expression)

      | I.Inclusive_or_expression_2_PIPE 
	  (c_inclusive_or_expression, c_exclusive_or_expression) 
	->
	  (compile_c_inclusive_or_expression c_inclusive_or_expression) @
	  (compile_c_exclusive_or_expression c_exclusive_or_expression)
	    
    and compile_c_logical_and_expression = function
      | I.Logical_and_expression_1 c_inclusive_or_expression ->
	  (compile_c_inclusive_or_expression c_inclusive_or_expression)
	    
      | I.Logical_and_expression_2_AND_AND 
	  (c_logical_and_expression, c_inclusive_or_expression) 
	->
	  (compile_c_logical_and_expression c_logical_and_expression) @
	  (compile_c_inclusive_or_expression c_inclusive_or_expression)
	    
    and compile_c_logical_or_expression = function
      | I.Logical_or_expression_1 c_logical_and_expression ->
	  (compile_c_logical_and_expression c_logical_and_expression)
	    
      | I.Logical_or_expression_2_PIPE_PIPE 
	  (c_logical_or_expression, c_logical_and_expression) 
	->
	  (compile_c_logical_or_expression c_logical_or_expression) @
	  (compile_c_logical_and_expression c_logical_and_expression)
	    
    and compile_c_conditional_expression = function
      | I.Conditional_expression_1 c_logical_or_expression ->
	  (compile_c_logical_or_expression c_logical_or_expression)

      | I.Conditional_expression_2 
	  (c_logical_or_expression, c_expression, c_conditional_expression) 
	->
	  (compile_c_logical_or_expression c_logical_or_expression) @
	  (compile_c_expression c_expression) @
	  (compile_c_conditional_expression c_conditional_expression)
	    
    and compile_c_assignment_expression = function
      | I.Assignment_expression_1 c_conditional_expression ->
	  (compile_c_conditional_expression c_conditional_expression)
	    
      | I.Assignment_expression_2 
	  (c_unary_expression, c_assignment_operator, 
	   c_assignment_expression) 
	->
	  (compile_c_unary_expression c_unary_expression) @
	  (compile_c_assignment_expression c_assignment_expression)

    and compile_c_assignment_operator = function
      | I.Assignment_operator_EQ -> []
      | I.Assignment_operator_STAR_EQ -> []
      | I.Assignment_operator_SLASH_EQ -> []
      | I.Assignment_operator_PERCENT_EQ ->  []
      | I.Assignment_operator_PLUS_EQ -> []
      | I.Assignment_operator_MINUS_EQ -> []
      | I.Assignment_operator_INF_INF_EQ -> []
      | I.Assignment_operator_SUP_SUP_EQ -> []
      | I.Assignment_operator_AND_EQ -> []
      | I.Assignment_operator_CIRC_EQ -> []
      | I.Assignment_operator_PIPE_EQ -> []
	    
    and compile_c_expression = function
      | I.Expression_1 c_assignment_expression -> 
	  (compile_c_assignment_expression c_assignment_expression)

      | I.Expression_2 (c_expression, c_assignment_expression) ->
	  (compile_c_expression c_expression) @
	  (compile_c_assignment_expression c_assignment_expression)

    and compile_c_constant_expression = function
      | I.Constant_expression c_conditional_expression ->
	  (compile_c_conditional_expression c_conditional_expression)

    and compile_c_declaration = function
      | I.Declaration (c_declaration_specifiers,  
		       c_init_declarator_list_opt) 
	->
	  (compile_c_declaration_specifiers c_declaration_specifiers) @
	  (compile_opt (List.map compile_c_init_declarator)  c_init_declarator_list_opt)
	    
    and compile_c_declaration_specifiers = function
      | I.Declaration_specifiers_1 
	  (c_storage_class_specifier, c_declaration_specifiers_opt) 
	-> []

      | I.Declaration_specifiers_2 (c_type_specifier, 
				    c_declaration_specifiers_opt) 
	-> []
	    
      | I.Declaration_specifiers_3 (c_type_qualifier, 
				    c_declaration_specifiers_opt) 
	-> []
	    
      | I.Declaration_specifiers_4 (c_function_specifier, 
				    c_declaration_specifiers_opt) 
	-> []
	    
    and compile_c_init_declarator = function
      | I.Init_declarator_1 c_declarator -> []
	    
      | I.Init_declarator_2 (c_declarator, c_initializer) ->
	  (compile_c_initializer c_initializer)

    and compile_c_storage_class_specifier = function
      | I.Storage_class_specifier_TYPEDEF -> []
      | I.Storage_class_specifier_EXTERN -> []
      | I.Storage_class_specifier_STATIC -> []
      | I.Storage_class_specifier_AUTO -> []
      | I.Storage_class_specifier_REGISTER -> []

    and compile_c_type_specifier a = []
    and compile_c_struct_or_union_specifier a = []
    and compile_c_struct_or_union a = []
    and compile_c_struct_declaration a = []
    and compile_c_specifier_qualifier_list a = []
    and compile_c_struct_declarator a = []
    and compile_c_enum_specifier a = []
    and compile_c_enumerator a = []
    and compile_c_type_qualifier a = []
    and compile_c_function_specifier a = []

    and compile_c_declarator = function
      | I.Declarator (c_pointer_opt, c_direct_declarator) ->
	  let (l0, l1) = (compile_c_direct_declarator c_direct_declarator)
	  in
	  ((l0, l1 @ (match c_pointer_opt with
	  | Some c_pointer -> compile_c_pointer c_pointer
	  | None -> [])):string list * O.property list)

    and compile_c_direct_declarator = function
      | I.Direct_declarator_1 c_identifier ->
	  (((compile_c_identifier c_identifier), []):string list * O.property list)
  
      | I.Direct_declarator_2 c_declarator ->
	  (compile_c_declarator c_declarator)

      | I.Direct_declarator_3 (c_direct_declarator, 
			       c_type_qualifier_list_opt, 
			       c_assignment_expression_opt) 
	->
	  let (l0, l1) = (compile_c_direct_declarator c_direct_declarator)
	  in
	  (l0, l1@(match c_assignment_expression_opt with 
	    Some a -> compile_c_assignment_expression a | None -> []))
	    
      | I.Direct_declarator_4_STATIC (c_direct_declarator, 
				      c_type_qualifier_list_opt, 
				      c_assignment_expression) 
	->
	  let (l0, l1) = (compile_c_direct_declarator c_direct_declarator)
	  in
	  (l0, l1@(compile_c_assignment_expression c_assignment_expression))

      | I.Direct_declarator_5_STATIC (c_direct_declarator, 
				      c_type_qualifier_list, 
				      c_assignment_expression) 
	->
	  let (l0, l1) = (compile_c_direct_declarator c_direct_declarator)
	  in
	  (l0, l1 @ (compile_c_assignment_expression c_assignment_expression))
	    
      | I.Direct_declarator_6_STAR (c_direct_declarator, 
				    c_type_qualifier_list_opt) 
	->
	  (compile_c_direct_declarator c_direct_declarator)
	    
      | I.Direct_declarator_7 (c_direct_declarator, c_parameter_type_list) ->
	  let (l0, l1) = (compile_c_direct_declarator c_direct_declarator)
	  in
	  (l0, l1 @ (compile_c_parameter_type_list c_parameter_type_list))
	    
      | I.Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	  (compile_c_direct_declarator c_direct_declarator) 
	    
    and compile_c_pointer = function
      | I.Pointer_1 c_type_qualifier_list_opt ->
	  [O.Pointer_level 1]
	    
      | I.Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
	  let v = compile_c_pointer c_pointer
	  in
	  match v with
	  | [O.Pointer_level n] -> [O.Pointer_level (n + 1)]
	  | _ -> assert false
		
    and compile_c_parameter_type_list = function
      | I.Parameter_type_list_FIX c_parameter_list ->
	  (compile_c_parameter_list c_parameter_list)

      | I.Parameter_type_list_VAR c_parameter_list ->
	  (compile_c_parameter_list c_parameter_list)
	    
    and compile_c_parameter_list = function
      | I.Parameter_list l -> 
	  (List.map compile_c_parameter_declaration l)

    and compile_c_parameter_declaration = function
      | I.Parameter_declaration_1 (c_declaration_specifiers, c_declarator) ->
	  (compile_c_declaration_specifiers c_declaration_specifiers) @
	  (compile_c_declarator c_declarator)

      | I.Parameter_declaration_2 (c_declaration_specifiers, 
				   c_abstract_declarator_opt) 
	->
	  (compile_c_declaration_specifiers c_declaration_specifiers) @
	  (compile_opt compile_c_abstract_declarator c_abstract_declarator_opt)

    and compile_c_type_name = function
      | I.Type_name (c_specifier_qualifier_list, c_abstract_declarator_opt) ->
	  (compile_c_specifier_qualifier_list c_specifier_qualifier_list) @
	  (compile_opt compile_c_abstract_declarator c_abstract_declarator_opt)

    and compile_c_abstract_declarator = function
      | I.Abstract_declarator_1 c_pointer -> 
	  (compile_c_pointer c_pointer)
	    
      | I.Abstract_declarator_2 (c_pointer_opt, 
				 c_direct_abstract_declarator) 
	->
	  (compile_opt compile_c_pointer c_pointer_opt) @
	  (compile_c_direct_abstract_declarator c_direct_abstract_declarator)
	    
    and compile_c_direct_abstract_declarator = function
      | I.Direct_abstract_declarator_error -> assert false
      | I.Direct_abstract_declarator_1 c_abstract_declarator ->
	  (compile_c_abstract_declarator c_abstract_declarator)

      | I.Direct_abstract_declarator_2 
	  (c_direct_abstract_declarator_opt, c_assignment_expression_opt) 
	->
	  (compile_opt compile_c_direct_abstract_declarator c_direct_abstract_declarator_opt) @
	  (compile_opt compile_c_assignment_expression c_assignment_expression_opt)
	    
      | I.Direct_abstract_declarator_3_STAR c_direct_abstract_declarator_opt 
	->
	  (compile_opt compile_c_direct_abstract_declarator c_direct_abstract_declarator_opt)

      | I.Direct_abstract_declarator_4 (c_direct_abstract_declarator_opt, 
					c_parameter_type_list_opt) 
	->
	  (compile_opt compile_c_direct_abstract_declarator c_direct_abstract_declarator_opt) @
	  (compile_opt compile_c_parameter_type_list c_parameter_type_list_opt)
	    
    and compile_c_typedef_name = function
      | I.Typedef_name c_identifier_as_typ ->
	  (compile_c_identifier_as_typ c_identifier_as_typ)
	    
    and compile_c_initializer = function
      | I.Initializer_1 c_assignment_expression ->
	  (compile_c_assignment_expression c_assignment_expression)

      | I.Initializer_2 c_initializer_list  ->
	  (compile_c_initializer_list c_initializer_list)
	    
    and compile_c_initializer_list = function
      | I.Initializer_list l ->
	  (List.map
	     (fun (c_designation_opt, c_initializer) -> 
	       (compile_opt compile_c_designation c_designation_opt) @
	       (compile_c_initializer c_initializer)
	     ) 
	     l)
	    
    and compile_c_designation = function
      | I.Designation c_designator_list ->
	  (List.map compile_c_designator c_designator_list)

    and compile_c_designator = function
      | I.Designator_1 c_constant_expression ->
	  (compile_c_constant_expression c_constant_expression)
	    
      | I.Designator_2 c_identifier ->
	  (compile_c_identifier c_identifier)

    and compile_c_statement = function
      | I.Statement_1 (c_labeled_statement, coord) ->
	  compile_c_labeled_statement c_labeled_statement coord
      | I.Statement_2 (c_compound_statement) ->
	  (compile_c_compound_statement c_compound_statement)
      | I.Statement_3 (c_expression_statement, coord)  ->
	  compile_c_expression_statement c_expression_statement coord 
      | I.Statement_4 (c_selection_statement, coord) -> 
	  compile_c_selection_statement c_selection_statement coord
      | I.Statement_5 (c_iteration_statement, coord) ->
	  compile_c_iteration_statement c_iteration_statement coord
      | I.Statement_6 (c_jump_statement, coord) -> 
	  compile_c_jump_statement c_jump_statement coord

    and compile_c_labeled_statement expr coord =
      match expr with
      | I.Labeled_statement_1 ((I.Identifier str), c_statement) ->
	  compile_c_statement c_statement

      | I.Labeled_statement_2_case 
	  (c_constant_expression, c_statement) ->
	    compile_c_statement c_statement
	      
      | I.Labeled_statement_3_default c_statement ->
	  (compile_c_statement c_statement)
	    
    and compile_c_compound_statement: I.c_compound_statement -> 
      O.c_compound_stmt010 = 
      fun expr ->
	let stmt010 = ref (O.BLOCK ([], []))
	in
	Stack.push (Stack.create ()) compound_stack;
	match expr with
	| I.Compound_statement c_block_item_list_opt ->
	    begin
	      let _ = match c_block_item_list_opt with
	      | Some c_block_item_list -> 
                  let _ = List.iter (compile_c_block_item (Stack.top compound_stack)) c_block_item_list  
		  in
		  let stack = Stack.pop compound_stack
		  in
		  while (not (Stack.is_empty stack)) do
		    let compound = Stack.pop stack
		    in
		    match !stmt010 with
		    | O.BLOCK ([], []) -> 
			stmt010 := 
			  O.BLOCK 
			    (compound.decls, 
			     compound.stmt010s)
		    | _ ->
			stmt010 := 
			  O.BLOCK 
			    (compound.decls, 
			     compound.stmt010s @ 
			     [O.COMPOUND (None, !stmt010)])
		  done
	      | None -> ()
	      in
	      !stmt010
	    end

    and compile_c_block_item: compound Stack.t -> I.c_block_item -> unit = 
      fun stack expr ->
	match expr with
	| I.Block_item_1 c_declaration ->
	    begin
	      let block_item = compile_c_declaration c_declaration
	      in
	      if (Stack.is_empty stack) then
		let compound = {
		  decls = [block_item];
		  stmt010s = [];
		}
		in
		Stack.push compound stack
	      else
		begin
		  let compound = Stack.top stack 
		  in
		  if compound.stmt010s = [] then
		    compound.decls <- compound.decls @ [block_item]
		  else
		    let compound = {
		      decls = [block_item];
		      stmt010s = [];
		    }
		    in
		    Stack.push compound stack
		end
	    end
	| I.Block_item_2 c_statement ->
	    begin
	      let block_item = compile_c_statement c_statement
	      in 
	      if (Stack.is_empty stack) then
		let compound = {
		  decls = [];
		  stmt010s = [block_item];
		}
		in
		Stack.push compound stack
	      else
		let compound = Stack.top stack 
		in
		compound.stmt010s <- compound.stmt010s @ [block_item]
	    end
	      
    and compile_c_expression_statement: I.c_expression_statement -> I.coord -> 
      O.c_stmt010 =
      fun expr coord ->
	match expr with
	| I.Expression_statement c_expression_opt ->
	    begin
	      match c_expression_opt with 
	      | Some c_expression ->
		  O.COMPUTATION 
		    (compile_c_expression c_expression, coord)
	      | None -> O.NOP coord
	    end
	      
    and compile_c_selection_statement: I.c_selection_statement -> I.coord -> 
      O.c_stmt010 = 
      fun expr coord ->
	match expr with
	| I.Selection_statement_1_if (c_expression, c_statement) ->
	    O.IF 
	      (compile_c_expression c_expression, 
	       compile_c_statement c_statement, 
	       O.NOP (coord), coord)
	      
	| I.Selection_statement_2_if_else 
	    (c_expression, 
	     then_c_statement, else_c_statement) 
	  ->
	    O.IF 
	      (compile_c_expression c_expression, 
	       compile_c_statement then_c_statement, 
	       compile_c_statement else_c_statement, coord)
	      
	| I.Selection_statement_3_switch (c_expression, c_statement) ->
	    O.SWITCH 
	      (compile_c_expression c_expression, 
	       compile_c_statement c_statement, coord)
	      
    and compile_c_iteration_statement: I.c_iteration_statement -> I.coord -> 
      O.c_stmt010 = 
      fun expr coord ->
	let function_data = Stack.top function_scope
	in
	match expr with
	| I.Iteration_statement_1_while (c_expression, c_statement) ->
	    (compile_c_expression c_expression) @ 
	    (compile_c_statement c_statement)
	      
	| I.Iteration_statement_2_do (c_statement, c_expression) ->
	    (compile_c_statement c_statement) @ (compile_c_expression c_expression)
	      
	| I.Iteration_statement_3_for 
	    (c_expression_opt0, c_expression_opt1, c_expression_opt2, c_statement) 
	  ->
	    (compile_opt c_expression_opt0) @
	    (compile_opt c_expression_opt1) @
	    (compile_opt c_expression_opt2) @
	    (compile_c_statement c_statement)
	      
	| I.Iteration_statement_4_for (c_declaration, c_expression_opt1, 
				       c_expression_opt2, c_statement) 
	  ->
	    (compile_c_declaration c_declaration) @
	    (compile_opt compile_c_expression c_expression_opt1) @
    	    (compile_opt compile_c_expression c_expression_opt2) @
	    (compile_c_statement c_statement)


    and compile_c_jump_statement: I.c_jump_statement -> I.coord -> O.c_stmt010 =
      fun expr coord ->
	match expr with
	| I.Jump_statement_1_goto (I.Identifier str) -> []
	| I.Jump_statement_2_continue -> []
	| I.Jump_statement_3_break -> []
	| I.Jump_statement_4_return c_expression_opt ->
	    begin
	      match c_expression_opt with
	      | Some expr -> (compile_c_expression expr, coord)
	      | None -> []
	    end

    and compile_c_translation_unit = function
      | I.Translation_unit l ->
	  (List.map
	     (fun external_declaration -> 
	       compile_c_external_declaration external_declaration
	     ) l
	  )
	    
    and compile_c_external_declaration = function
      | I.External_declaration_1 (c_function_definition, coord) ->
	  (compile_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 (c_declaration, coord) ->
	  (compile_c_declaration c_declaration)

    and compile_c_function_definition = function
	(c_declaration_specifiers,
	 c_declarator,
	 c_declaration_list_opt,
	 c_compound_statement) ->
	   (compile_c_declaration_specifiers c_declaration_specifiers,
	    compile_c_declarator c_declarator,
	    map_list_opt compile_c_declaration c_declaration_list_opt,
	    compile_c_compound_statement c_compound_statement)
    in
    compile_c_translation_unit c_translation_unit
      
