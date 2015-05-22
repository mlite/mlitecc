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

open Ast_aa_gram

let normalize: string * c_translation_unit -> c_translation_unit = 
  fun (filename, c_translation_unit) ->
    let var_db = Var_db.create ""
    in
    let rec add_c_identifier: bool -> c_identifier -> c_identifier = 
      fun is_static expr ->
	match expr with
	| Identifier s -> 
	    let s = 
	      if is_static then
		Var_db.add_static_sname var_db 0 s
	      else
		Var_db.add_sname var_db 0 ~is_var:true s
	    in
	    Identifier s

    and normalize_c_identifier: c_identifier -> c_identifier = 
      fun expr ->
	match expr with
	| Identifier s -> 
	    let (_,s,_) = Var_db.find_identifier var_db ~name:s
	    in
	    Identifier s
	      
    and add_c_identifier_as_typ: c_identifier_as_typ -> 
      c_identifier_as_typ = 
      fun expr ->
	match expr with
	| Identifier_as_typ s -> 
	    let s = Var_db.add_sname var_db 0 ~is_var:false s
	    in
	    Identifier_as_typ s
	      
    and normalize_c_enumeration_constant: c_enumeration_constant -> 
      c_enumeration_constant =
      fun expr ->
	match expr with
	| Enumeration_constant c_identifier -> 
	    Enumeration_constant (add_c_identifier false c_identifier)
	      
    and normalize_c_primary_expression: c_primary_expression -> 
      c_primary_expression = 
      fun expr ->
	match expr with
	| Primary_expression_1 c_identifier ->
	    Primary_expression_1
	      (normalize_c_identifier c_identifier)
	      
	| Primary_expression_2 c_constant ->
	    Primary_expression_2 c_constant
	      
	| Primary_expression_3 c_string_literal ->
	    Primary_expression_3 c_string_literal
	      
	| Primary_expression_4 c_expression ->
	    Primary_expression_4 
	      (normalize_c_expression c_expression)
	      
	| Primary_expression_macro_va_arg (c_expression, c_type_name) -> 
	    Primary_expression_macro_va_arg 
	      (normalize_c_expression c_expression,
	       normalize_c_type_name c_type_name)
	      
	| Primary_expression_macro_va_start (expr0, expr1) -> 
	    Primary_expression_macro_va_start
	      (normalize_c_expression expr0,
	       normalize_c_expression expr1)

	| Primary_expression_macro_va_end (c_expression) -> 
	    Primary_expression_macro_va_end 
	      (normalize_c_expression c_expression)
	      
    and normalize_c_postfix_expression: c_postfix_expression -> 
      c_postfix_expression = 
      fun expr ->
	match expr with
	| Postfix_expression_1 c_primary_expression ->
	    Postfix_expression_1 
	      (normalize_c_primary_expression c_primary_expression)
	      
	| Postfix_expression_2 (c_postfix_expression, c_expression) ->
	    begin
	      Postfix_expression_2
		(normalize_c_postfix_expression c_postfix_expression,
		 normalize_c_expression c_expression)
	    end
	| Postfix_expression_3 (c_postfix_expression, 
				  c_argument_expression_list_opt) 
	  ->
	    begin
	      Postfix_expression_3 
		(normalize_c_postfix_expression c_postfix_expression,
		 normalize_c_argument_expression_list_opt 
		   c_argument_expression_list_opt)
	    end
	| Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	    begin
	      Postfix_expression_4_DOT
		(normalize_c_postfix_expression c_postfix_expression,
		 normalize_c_identifier c_identifier)
	    end
	| Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	    begin
	      Postfix_expression_5_ARROW
		(normalize_c_postfix_expression c_postfix_expression,
		 normalize_c_identifier c_identifier)
	    end
	| Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	    begin
	      Postfix_expression_6_PLUS_PLUS
		(normalize_c_postfix_expression c_postfix_expression)
	    end
	| Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	    begin
	      Postfix_expression_7_MINUS_MINUS
		(normalize_c_postfix_expression c_postfix_expression)
	    end
	| Postfix_expression_8 (c_type_name, c_initializer_list) ->
	    begin
	      Postfix_expression_8
		(normalize_c_type_name c_type_name,
		 normalize_c_initializer_list c_initializer_list)
	    end
	      
    and normalize_c_argument_expression_list: c_argument_expression_list -> 
      c_argument_expression_list =
      fun expr ->
	match expr with
	| Argument_expression_list l ->
	    Argument_expression_list
	      ((List.map
		  normalize_c_assignment_expression
	       ) l)
	      
    and normalize_c_unary_expression: c_unary_expression -> 
      c_unary_expression = 
      fun expr ->
	match expr with
	| Unary_expression_1 c_postfix_expression ->
	    Unary_expression_1 
	      (normalize_c_postfix_expression c_postfix_expression)
	      
	| Unary_expression_2_PLUS_PLUS c_unary_expression ->
	    Unary_expression_2_PLUS_PLUS
	      (normalize_c_unary_expression c_unary_expression)
	      
	| Unary_expression_3_MINUS_MINUS c_unary_expression ->
	    Unary_expression_3_MINUS_MINUS
	      (normalize_c_unary_expression c_unary_expression)

	| Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	    Unary_expression_4
	      (normalize_c_unary_operator c_unary_operator,
	       normalize_c_cast_expression c_cast_expression)
	      
	| Unary_expression_5_SIZEOF c_unary_expression ->
	    Unary_expression_5_SIZEOF
	      (normalize_c_unary_expression c_unary_expression)
	      
	| Unary_expression_6_SIZEOF c_type_name ->
	    Unary_expression_6_SIZEOF
	      (normalize_c_type_name c_type_name)

	| Unary_expression_7_ALIGNOF c_unary_expression ->
	    Unary_expression_7_ALIGNOF
	      (normalize_c_unary_expression c_unary_expression)
	      
	| Unary_expression_8_ALIGNOF c_type_name ->
	    Unary_expression_8_ALIGNOF
	      (normalize_c_type_name c_type_name)

    and normalize_c_unary_operator: c_unary_operator -> c_unary_operator = 
      fun expr ->
	match expr with
	| Unary_operator_AND -> 
	    Unary_operator_AND
	      
	| Unary_operator_STAR -> 
	    Unary_operator_STAR
	      
	| Unary_operator_PLUS -> 
	    Unary_operator_PLUS
	      
	| Unary_operator_MINUS -> 
	    Unary_operator_MINUS
	      
	| Unary_operator_TILDE -> 
	    Unary_operator_TILDE
	      
	| Unary_operator_EXCLAM ->
	    Unary_operator_EXCLAM
	      
    and normalize_c_cast_expression: c_cast_expression -> c_cast_expression = 
      fun expr ->
	match expr with
	| Cast_expression_1 c_unary_expression ->
	    Cast_expression_1
	      (normalize_c_unary_expression c_unary_expression)
	      
	| Cast_expression_2 (c_type_name, c_cast_expression) ->
	    Cast_expression_2
	      (normalize_c_type_name c_type_name,
	       normalize_c_cast_expression c_cast_expression)
	      
    and normalize_c_multiplicative_expression: c_multiplicative_expression -> 
      c_multiplicative_expression = 
      fun expr ->
	match expr with
	| Multiplicative_expression_1 c_cast_expression ->
	    Multiplicative_expression_1
	      (normalize_c_cast_expression c_cast_expression)

	| Multiplicative_expression_2_STAR 
	    (c_multiplicative_expression, c_cast_expression) 
	  ->
	    Multiplicative_expression_2_STAR
	      (normalize_c_multiplicative_expression c_multiplicative_expression,
	       normalize_c_cast_expression c_cast_expression)
	      
	| Multiplicative_expression_3_SLASH 
	    (c_multiplicative_expression, c_cast_expression) 
	  ->
	    Multiplicative_expression_3_SLASH
	      (normalize_c_multiplicative_expression c_multiplicative_expression,
	       normalize_c_cast_expression c_cast_expression)
	      
	| Multiplicative_expression_4_PERCENT 
	    (c_multiplicative_expression, c_cast_expression) 
	  ->
	    Multiplicative_expression_4_PERCENT
	      (normalize_c_multiplicative_expression c_multiplicative_expression,
	       normalize_c_cast_expression c_cast_expression)
	      
    and normalize_c_additive_expression: c_additive_expression -> 
      c_additive_expression = 
      fun expr ->
	match expr with
	| Additive_expression_1 c_multiplicative_expression ->
	    Additive_expression_1
	      (normalize_c_multiplicative_expression c_multiplicative_expression)
	      
	| Additive_expression_2_PLUS (c_additive_expression, 
					c_multiplicative_expression) 
	  ->
	    Additive_expression_2_PLUS
	      (normalize_c_additive_expression c_additive_expression,
	       normalize_c_multiplicative_expression c_multiplicative_expression)
	      
	| Additive_expression_3_MINUS (c_additive_expression, 
					 c_multiplicative_expression) 
	  ->
	    Additive_expression_3_MINUS
	      (normalize_c_additive_expression c_additive_expression,
	       normalize_c_multiplicative_expression c_multiplicative_expression)

    and normalize_c_shift_expression: c_shift_expression ->
      c_shift_expression = 
      fun expr ->
	match expr with
	| Shift_expression_1 c_additive_expression ->
	    Shift_expression_1
	      (normalize_c_additive_expression c_additive_expression)

	| Shift_expression_2_INF_INF (c_shift_expression, 
					c_additive_expression) 
	  ->
	    Shift_expression_2_INF_INF
	      (normalize_c_shift_expression c_shift_expression,
	       normalize_c_additive_expression c_additive_expression)
	      
	| Shift_expression_3_SUP_SUP (c_shift_expression, 
					c_additive_expression) 
	  ->
	    Shift_expression_3_SUP_SUP
	      (normalize_c_shift_expression c_shift_expression,
	       normalize_c_additive_expression c_additive_expression)

    and normalize_c_relational_expression: c_relational_expression -> 
      c_relational_expression =
      fun expr ->
	match expr with
	| Relational_expression_1 c_shift_expression ->
	    Relational_expression_1 
	      (normalize_c_shift_expression c_shift_expression)

	| Relational_expression_2_INF (c_relational_expression, 
					 c_shift_expression) 
	  ->
	    Relational_expression_2_INF
	      (normalize_c_relational_expression c_relational_expression,
	       normalize_c_shift_expression c_shift_expression)

	| Relational_expression_3_SUP (c_relational_expression, 
					 c_shift_expression) 
	  ->
	    Relational_expression_3_SUP
	      (normalize_c_relational_expression c_relational_expression,
	       normalize_c_shift_expression c_shift_expression)

	| Relational_expression_4_INF_EQ (c_relational_expression, 
					    c_shift_expression) 
	  ->
	    Relational_expression_4_INF_EQ
	      (normalize_c_relational_expression c_relational_expression,
	       normalize_c_shift_expression c_shift_expression)
	      
	| Relational_expression_5_SUP_EQ (c_relational_expression, 
					    c_shift_expression) 
	  ->
	    Relational_expression_5_SUP_EQ
	      (normalize_c_relational_expression c_relational_expression,
	       normalize_c_shift_expression c_shift_expression)

    and normalize_c_equality_expression: c_equality_expression -> 
      c_equality_expression = 
      fun expr ->
	match expr with
	| Equality_expression_1 c_relational_expression ->
	    Equality_expression_1
	      (normalize_c_relational_expression c_relational_expression)
	      
	| Equality_expression_2_EQ_EQ 
	    (c_equality_expression, c_relational_expression) 
	  ->
	    Equality_expression_2_EQ_EQ
	      (normalize_c_equality_expression c_equality_expression,
	       normalize_c_relational_expression c_relational_expression)
	      
	| Equality_expression_3_EXCLAM_EQ 
	    (c_equality_expression, c_relational_expression) 
	  ->
	    Equality_expression_3_EXCLAM_EQ
	      (normalize_c_equality_expression c_equality_expression,
	       normalize_c_relational_expression c_relational_expression)

    and normalize_c_and_expression: c_and_expression -> c_and_expression = 
      fun expr ->
	match expr with
	| And_expression_1 c_equality_expression ->
	    And_expression_1
	      (normalize_c_equality_expression c_equality_expression)
	      
	| And_expression_2_AND (c_and_expression, c_equality_expression) ->
	    And_expression_2_AND
	      (normalize_c_and_expression c_and_expression,
	       normalize_c_equality_expression c_equality_expression)
	      
    and normalize_c_exclusive_or_expression: c_exclusive_or_expression -> 
      c_exclusive_or_expression = 
      fun expr ->
	match expr with
	| Exclusive_or_expression_1 c_and_expression ->
	    Exclusive_or_expression_1
	      (normalize_c_and_expression c_and_expression)

	| Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, 
					    c_and_expression) 
	  ->
	    Exclusive_or_expression_2_CIRC
	      (normalize_c_exclusive_or_expression c_exclusive_or_expression,
	       normalize_c_and_expression c_and_expression)
	      
    and normalize_c_inclusive_or_expression: c_inclusive_or_expression -> 
      c_inclusive_or_expression = 
      fun expr ->
	match expr with
	| Inclusive_or_expression_1 c_exclusive_or_expression ->
	    Inclusive_or_expression_1
	      (normalize_c_exclusive_or_expression c_exclusive_or_expression)

	| Inclusive_or_expression_2_PIPE 
	    (c_inclusive_or_expression, c_exclusive_or_expression) 
	  ->
	    Inclusive_or_expression_2_PIPE
	      (normalize_c_inclusive_or_expression c_inclusive_or_expression,
	       normalize_c_exclusive_or_expression c_exclusive_or_expression)
	      
    and normalize_c_logical_and_expression: c_logical_and_expression -> 
      c_logical_and_expression = 
      fun expr ->
	match expr with
	| Logical_and_expression_1 c_inclusive_or_expression ->
	    Logical_and_expression_1
	      (normalize_c_inclusive_or_expression c_inclusive_or_expression)
	      
	| Logical_and_expression_2_AND_AND 
	    (c_logical_and_expression, c_inclusive_or_expression) 
	  ->
	    Logical_and_expression_2_AND_AND 
	      (normalize_c_logical_and_expression c_logical_and_expression,
	       normalize_c_inclusive_or_expression c_inclusive_or_expression)
	      
    and normalize_c_logical_or_expression: c_logical_or_expression -> 
      c_logical_or_expression = 
      fun expr ->
	match expr with
	| Logical_or_expression_1 c_logical_and_expression ->
	    Logical_or_expression_1
	      (normalize_c_logical_and_expression c_logical_and_expression)
	      
	| Logical_or_expression_2_PIPE_PIPE 
	    (c_logical_or_expression, c_logical_and_expression) 
	  ->
	    Logical_or_expression_2_PIPE_PIPE
	      (normalize_c_logical_or_expression c_logical_or_expression,
	       normalize_c_logical_and_expression c_logical_and_expression)
	      
    and normalize_c_conditional_expression: c_conditional_expression -> 
      c_conditional_expression = 
      fun expr ->
	match expr with
	| Conditional_expression_1 c_logical_or_expression ->
	    Conditional_expression_1
	      (normalize_c_logical_or_expression c_logical_or_expression)

	| Conditional_expression_2 
	    (c_logical_or_expression, c_expression, c_conditional_expression) 
	  ->
	    Conditional_expression_2 
	      (normalize_c_logical_or_expression c_logical_or_expression,
	       normalize_c_expression c_expression,
	       normalize_c_conditional_expression c_conditional_expression)

    and normalize_c_assignment_expression: c_assignment_expression -> 
      c_assignment_expression = 
      fun expr ->
	match expr with
	| Assignment_expression_1 c_conditional_expression ->
	    Assignment_expression_1
	      (normalize_c_conditional_expression c_conditional_expression)
	      
	| Assignment_expression_2 
	    (c_unary_expression, c_assignment_operator, 
	     c_assignment_expression) 
	  ->
	    Assignment_expression_2 
	      (normalize_c_unary_expression c_unary_expression,
	       normalize_c_assignment_operator c_assignment_operator,
	       normalize_c_assignment_expression c_assignment_expression)


    and normalize_c_assignment_operator: c_assignment_operator -> 
      c_assignment_operator =
      fun expr ->
	match expr with
	| Assignment_operator_EQ -> Assignment_operator_EQ
	| Assignment_operator_STAR_EQ -> Assignment_operator_STAR_EQ
	| Assignment_operator_SLASH_EQ -> Assignment_operator_SLASH_EQ
	| Assignment_operator_PERCENT_EQ ->  Assignment_operator_PERCENT_EQ
	| Assignment_operator_PLUS_EQ -> Assignment_operator_PLUS_EQ
	| Assignment_operator_MINUS_EQ -> Assignment_operator_MINUS_EQ
	| Assignment_operator_INF_INF_EQ -> Assignment_operator_INF_INF_EQ
	| Assignment_operator_SUP_SUP_EQ -> Assignment_operator_SUP_SUP_EQ
	| Assignment_operator_AND_EQ -> Assignment_operator_AND_EQ
	| Assignment_operator_CIRC_EQ -> Assignment_operator_CIRC_EQ
	| Assignment_operator_PIPE_EQ -> Assignment_operator_PIPE_EQ
	      
    and normalize_c_expression: c_expression -> c_expression = 
      fun expr ->
	match expr with
	| Expression_1 c_assignment_expression -> 
	    Expression_1
	      (normalize_c_assignment_expression c_assignment_expression)

	| Expression_2 (c_expression, c_assignment_expression) ->
	    Expression_2 
	      (normalize_c_expression c_expression,
	       normalize_c_assignment_expression c_assignment_expression)

    and normalize_c_constant_expression: c_constant_expression -> 
      c_constant_expression = 
      fun expr ->
	match expr with
	| Constant_expression c_conditional_expression ->
	    Constant_expression
	      (normalize_c_conditional_expression c_conditional_expression)

    and normalize_c_declaration: c_declaration -> c_declaration = 
      fun expr ->
	match expr with
	| Declaration (c_declaration_specifiers,  
			 c_init_declarator_list_opt) 
	  ->
	    let (c_declaration_specifiers, is_static) =
	      normalize_c_declaration_specifiers c_declaration_specifiers
	    in
	    Declaration
	      (c_declaration_specifiers,
	       normalize_c_init_declarator_list_opt is_static c_init_declarator_list_opt)
	      
    and normalize_c_declaration_specifiers: c_declaration_specifiers -> 
      c_declaration_specifiers * bool = 
      fun expr ->
	match expr with
	| Declaration_specifiers_1 
	    (c_storage_class_specifier, c_declaration_specifiers_opt) 
	  ->
	    let (c_storage_class_specifier, is_static0) = 
	      normalize_c_storage_class_specifier 
		c_storage_class_specifier
	    and (c_declaration_specifiers_opt, is_static1) = 
	      normalize_c_declaration_specifiers_opt 
		c_declaration_specifiers_opt
	    in
	    (Declaration_specifiers_1
	       (c_storage_class_specifier,
		c_declaration_specifiers_opt), 
	     is_static0 or is_static1)
	      
	| Declaration_specifiers_2 (c_type_specifier, 
				      c_declaration_specifiers_opt) 
	  ->
	    let (c_declaration_specifiers_opt, is_static) = 
	      normalize_c_declaration_specifiers_opt 
		c_declaration_specifiers_opt
	    in
	    (Declaration_specifiers_2
	       (normalize_c_type_specifier c_type_specifier,
		c_declaration_specifiers_opt), is_static)
	      
	| Declaration_specifiers_3 (c_type_qualifier, 
				      c_declaration_specifiers_opt) 
	  ->
	    let (c_declaration_specifiers_opt, is_static) = 
	      normalize_c_declaration_specifiers_opt 
		c_declaration_specifiers_opt
	    in
	    (Declaration_specifiers_3
	       (normalize_c_type_qualifier c_type_qualifier,
		c_declaration_specifiers_opt), is_static)
	       
	| Declaration_specifiers_4 (c_function_specifier, 
				      c_declaration_specifiers_opt) 
	  ->
	    let (c_declaration_specifiers_opt, is_static) = 
	      normalize_c_declaration_specifiers_opt 
		c_declaration_specifiers_opt
	    in
	    (Declaration_specifiers_4
	       (normalize_c_function_specifier c_function_specifier,
		c_declaration_specifiers_opt), is_static)

    and normalize_c_init_declarator_list: bool -> c_init_declarator_list -> 
      c_init_declarator_list = 
      fun is_static expr ->
	match expr with
	| Init_declarator_list c_init_declarator_list ->
	    Init_declarator_list 
	      (List.map (normalize_c_init_declarator is_static) c_init_declarator_list)
	      

    and normalize_c_init_declarator: bool -> c_init_declarator -> c_init_declarator = 
      fun is_static expr ->
	match expr with
	| Init_declarator_1 c_declarator ->
	    Init_declarator_1
	      (normalize_c_declarator is_static c_declarator)
	      
	| Init_declarator_2 (c_declarator, c_initializer) ->
	    Init_declarator_2
	      (normalize_c_declarator is_static c_declarator,
	       normalize_c_initializer c_initializer)

    and normalize_c_storage_class_specifier: c_storage_class_specifier ->
      c_storage_class_specifier * bool =
      fun expr ->
	match expr with
	| Storage_class_specifier_TYPEDEF ->
	    (Storage_class_specifier_TYPEDEF, false)
	| Storage_class_specifier_EXTERN ->
	    (Storage_class_specifier_EXTERN, false)
	| Storage_class_specifier_STATIC ->
	    (Storage_class_specifier_STATIC, true)
	| Storage_class_specifier_AUTO ->
	    (Storage_class_specifier_AUTO, false)
	| Storage_class_specifier_REGISTER ->
	    (Storage_class_specifier_REGISTER, false)

    and normalize_c_type_specifier: c_type_specifier -> c_type_specifier = 
      fun expr ->
	match expr with
	| Type_specifier_VOID -> 
	    Type_specifier_VOID 
	| Type_specifier_CHAR -> 
	    Type_specifier_CHAR
	| Type_specifier_SHORT -> 
	    Type_specifier_SHORT
	| Type_specifier_INT -> 
	    Type_specifier_INT
	| Type_specifier_LONG -> 
	    Type_specifier_LONG
	| Type_specifier_FLOAT -> 
	    Type_specifier_FLOAT
	| Type_specifier_DOUBLE -> 
	    Type_specifier_DOUBLE
	| Type_specifier_SIGNED -> 
	    Type_specifier_SIGNED
	| Type_specifier_UNSIGNED -> 
	    Type_specifier_UNSIGNED
	| Type_specifier_BOOL -> 
	    Type_specifier_BOOL
	| Type_specifier_COMPLEX -> 
	    Type_specifier_COMPLEX
	| Type_specifier_STRUCT_OR_UNION c_struct_or_union_specifier ->
	    Type_specifier_STRUCT_OR_UNION
	      (normalize_c_struct_or_union_specifier c_struct_or_union_specifier)
	| Type_specifier_ENUM c_enum_specifier ->
	    Type_specifier_ENUM
	      (normalize_c_enum_specifier c_enum_specifier)
	      
	| Type_specifier_TYPENAME c_typedef_name ->
	    Type_specifier_TYPENAME
	      (normalize_c_typedef_name c_typedef_name)

    and normalize_c_struct_or_union_specifier: c_struct_or_union_specifier -> 
      c_struct_or_union_specifier =
      fun expr ->
	match expr with
	| Struct_or_union_specifier_1 
	    (c_struct_or_union, 
	     c_identifier_opt, 
	     c_struct_declaration_list) 
	  ->
	    Struct_or_union_specifier_1 
	      (normalize_c_struct_or_union c_struct_or_union,
	       normalize_c_identifier_opt c_identifier_opt,
	       normalize_c_struct_declaration_list 
		 c_struct_declaration_list)
	      
	| Struct_or_union_specifier_2 (c_struct_or_union, c_identifier) ->
	    Struct_or_union_specifier_2
	      (normalize_c_struct_or_union c_struct_or_union,
	       normalize_c_identifier c_identifier)

    and normalize_c_struct_or_union: c_struct_or_union -> c_struct_or_union =
      fun expr ->
	match expr with
	| Struct_or_union_STRUCT ->
	    Struct_or_union_STRUCT
	| Struct_or_union_UNION->
	    Struct_or_union_UNION
	      
    and normalize_c_struct_declaration_list: c_struct_declaration_list ->
      c_struct_declaration_list = 
      fun expr ->
	match expr with
	| Struct_declaration_list l ->
	    Struct_declaration_list
	      (List.map normalize_c_struct_declaration l)

    and normalize_c_struct_declaration: c_struct_declaration -> 
      c_struct_declaration = 
      fun expr ->
	match expr with
	| Struct_declaration 
	    (c_specifier_qualifier_list, c_struct_declarator_list) 
	  ->
	    Struct_declaration 
	      (normalize_c_specifier_qualifier_list c_specifier_qualifier_list,
	       normalize_c_struct_declarator_list c_struct_declarator_list)
	      
    and normalize_c_specifier_qualifier_list: c_specifier_qualifier_list -> 
      c_specifier_qualifier_list = 
      fun expr ->
	match expr with
	| Specifier_qualifier_list_1 (c_type_specifier, 
					c_specifier_qualifier_list_opt) 
	  ->
	    Specifier_qualifier_list_1
	      (normalize_c_type_specifier c_type_specifier,
	       normalize_c_specifier_qualifier_list_opt 
		 c_specifier_qualifier_list_opt)
	      
	| Specifier_qualifier_list_2 (c_type_qualifier, 
					c_specifier_qualifier_list_opt) 
	  ->
	    Specifier_qualifier_list_2
	      (normalize_c_type_qualifier c_type_qualifier,
	       normalize_c_specifier_qualifier_list_opt 
		 c_specifier_qualifier_list_opt)
	      
    and normalize_c_struct_declarator_list: c_struct_declarator_list -> 
      c_struct_declarator_list = 
      fun expr ->
	match expr with
	| Struct_declarator_list l -> 
	    Struct_declarator_list
	      (List.map normalize_c_struct_declarator l)
	      
    and normalize_c_struct_declarator: c_struct_declarator -> 
      c_struct_declarator = 
      fun expr ->
	match expr with
	| Struct_declarator_1 c_declarator ->
	    Struct_declarator_1
	      (normalize_c_declarator false c_declarator)

	| Struct_declarator_2 (c_declarator_opt, c_constant_expression) ->
	    Struct_declarator_2
	      (normalize_c_declarator_opt false c_declarator_opt,
	       normalize_c_constant_expression c_constant_expression)

    and normalize_c_enum_specifier: c_enum_specifier -> c_enum_specifier = 
      fun expr ->
	match expr with
	| Enum_specifier_1 (c_identifier_opt, c_enumerator_list) ->
	    Enum_specifier_1
	      (normalize_c_identifier_opt c_identifier_opt,
	       normalize_c_enumerator_list c_enumerator_list)
	      
	| Enum_specifier_2 c_identifier ->
	    Enum_specifier_2
	      (normalize_c_identifier c_identifier)

    and normalize_c_enumerator_list: c_enumerator_list -> c_enumerator_list = 
      fun expr ->
	match expr with
	| Enumerator_list l ->
	    Enumerator_list
	      (List.map normalize_c_enumerator l)

    and normalize_c_enumerator: c_enumerator -> c_enumerator = 
      fun expr ->
	match expr with
	| Enumerator_1 c_enumeration_constant -> 
	    Enumerator_1
	      (normalize_c_enumeration_constant c_enumeration_constant)
	      
	| Enumerator_2 (c_enumeration_constant, c_constant_expression) ->
	    Enumerator_2
	      (normalize_c_enumeration_constant c_enumeration_constant,
	       normalize_c_constant_expression c_constant_expression)

    and normalize_c_type_qualifier: c_type_qualifier -> c_type_qualifier = 
      fun expr ->
	match expr with
	| Type_qualifier_CONST -> Type_qualifier_CONST
	| Type_qualifier_RESTRICT -> Type_qualifier_RESTRICT
	| Type_qualifier_VOLATILE -> Type_qualifier_VOLATILE

    and normalize_c_function_specifier expr = 
      match expr with
      | Function_specifier_INLINE -> Function_specifier_INLINE
	    
    and normalize_c_declarator: bool -> c_declarator -> c_declarator = 
      fun is_static expr ->
	match expr with
	| Declarator (c_pointer_opt, c_direct_declarator) ->
	    Declarator
	      (normalize_c_pointer_opt c_pointer_opt,
	       normalize_c_direct_declarator is_static c_direct_declarator)

    and normalize_c_direct_declarator: bool -> c_direct_declarator -> 
      c_direct_declarator = 
      fun is_static expr ->
	match expr with
	| Direct_declarator_1 c_identifier ->
	    Direct_declarator_1
	      (add_c_identifier is_static c_identifier)
	      
	| Direct_declarator_2 c_declarator ->
	    Direct_declarator_2
	      (normalize_c_declarator is_static c_declarator)

	| Direct_declarator_3 (c_direct_declarator, 
				 c_type_qualifier_list_opt, 
				 c_assignment_expression_opt) 
	  ->
	    Direct_declarator_3
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_type_qualifier_list_opt c_type_qualifier_list_opt,
	       normalize_c_assignment_expression_opt c_assignment_expression_opt)
	      
	| Direct_declarator_4_STATIC (c_direct_declarator, 
					c_type_qualifier_list_opt, 
					c_assignment_expression) 
	  ->
	    Direct_declarator_4_STATIC
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_type_qualifier_list_opt c_type_qualifier_list_opt,
	       normalize_c_assignment_expression c_assignment_expression)

	| Direct_declarator_5_STATIC (c_direct_declarator, 
					c_type_qualifier_list, 
					c_assignment_expression) 
	  ->
	    Direct_declarator_5_STATIC
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_type_qualifier_list c_type_qualifier_list,
	       normalize_c_assignment_expression c_assignment_expression)

	| Direct_declarator_6_STAR (c_direct_declarator, 
				      c_type_qualifier_list_opt) 
	  ->
	    Direct_declarator_6_STAR
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_type_qualifier_list_opt c_type_qualifier_list_opt)
	      
	| Direct_declarator_7 (c_direct_declarator, c_parameter_type_list) ->
	    Direct_declarator_7
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_parameter_type_list c_parameter_type_list)
	      
	| Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	    Direct_declarator_8
	      (normalize_c_direct_declarator is_static c_direct_declarator,
	       normalize_c_identifier_list_opt c_identifier_list_opt)
	      
    and normalize_c_pointer: c_pointer -> c_pointer = 
      fun expr ->
	match expr with
	| Pointer_1 c_type_qualifier_list_opt ->
	    Pointer_1
	      (normalize_c_type_qualifier_list_opt c_type_qualifier_list_opt)
	      
	| Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
	    Pointer_2
	      (normalize_c_type_qualifier_list_opt c_type_qualifier_list_opt,
	       normalize_c_pointer c_pointer)

    and normalize_c_type_qualifier_list: c_type_qualifier_list -> 
      c_type_qualifier_list = 
      fun expr ->
	match expr with
	| Type_qualifier_list l ->
	    Type_qualifier_list
	      (List.map normalize_c_type_qualifier l)

    and normalize_c_parameter_type_list: c_parameter_type_list -> 
      c_parameter_type_list = 
      fun expr ->
	match expr with
	| Parameter_type_list_FIX c_parameter_list ->
	    Parameter_type_list_FIX
	      (normalize_c_parameter_list c_parameter_list)

	| Parameter_type_list_VAR c_parameter_list ->
	    Parameter_type_list_VAR
	      (normalize_c_parameter_list c_parameter_list)
	      
    and normalize_c_parameter_list: c_parameter_list -> c_parameter_list = 
      fun expr ->
	match expr with
	| Parameter_list l -> 
	    Parameter_list 
	      (List.map normalize_c_parameter_declaration l)

    and normalize_c_parameter_declaration: c_parameter_declaration -> 
      c_parameter_declaration = 
      fun expr ->
	match expr with
	| Parameter_declaration_1 (c_declaration_specifiers, c_declarator) ->
	    let (c_declaration_specifiers, _)
		= normalize_c_declaration_specifiers c_declaration_specifiers
	    in
	    Parameter_declaration_1
	    (c_declaration_specifiers,
	     normalize_c_declarator false c_declarator)

	| Parameter_declaration_2 (c_declaration_specifiers, 
				   c_abstract_declarator_opt) 
	  ->
	    let (c_declaration_specifiers, _)
		= normalize_c_declaration_specifiers c_declaration_specifiers
	    in
	    Parameter_declaration_2
	      (c_declaration_specifiers,
	       normalize_c_abstract_declarator_opt c_abstract_declarator_opt)

    and normalize_c_identifier_list: c_identifier_list -> c_identifier_list = 
      fun expr ->
	match expr with
	| Identifier_list l ->
	    Identifier_list
	      (List.map normalize_c_identifier l)
	      
	      
    and normalize_c_type_name: c_type_name -> c_type_name = 
      fun expr ->
	match expr with
	| Type_name (c_specifier_qualifier_list, c_abstract_declarator_opt) ->
	    Type_name
	      (normalize_c_specifier_qualifier_list c_specifier_qualifier_list,
	       normalize_c_abstract_declarator_opt c_abstract_declarator_opt)

    and normalize_c_abstract_declarator: c_abstract_declarator -> 
      c_abstract_declarator = 
      fun expr ->
	match expr with
	| Abstract_declarator_1 c_pointer -> 
	    Abstract_declarator_1
	      (normalize_c_pointer c_pointer)
	      
	| Abstract_declarator_2 (c_pointer_opt, 
				   c_direct_abstract_declarator) 
	  ->
	    Abstract_declarator_2
	      (normalize_c_pointer_opt c_pointer_opt,
	       normalize_c_direct_abstract_declarator 
		 c_direct_abstract_declarator)
	      
    and normalize_c_direct_abstract_declarator: c_direct_abstract_declarator -> 
      c_direct_abstract_declarator = 
      fun expr ->
	match expr with
	| Direct_abstract_declarator_error -> assert false
	| Direct_abstract_declarator_1 c_abstract_declarator ->
	    Direct_abstract_declarator_1
	      (normalize_c_abstract_declarator c_abstract_declarator)

	| Direct_abstract_declarator_2 
	    (c_direct_abstract_declarator_opt, c_assignment_expression_opt) 
	  ->
	    Direct_abstract_declarator_2
	      (normalize_c_direct_abstract_declarator_opt 
		 c_direct_abstract_declarator_opt,
	       normalize_c_assignment_expression_opt 
		 c_assignment_expression_opt)
	      
	| Direct_abstract_declarator_3_STAR c_direct_abstract_declarator_opt 
	  ->
	    Direct_abstract_declarator_3_STAR
	      (normalize_c_direct_abstract_declarator_opt 
		 c_direct_abstract_declarator_opt)

	| Direct_abstract_declarator_4 (c_direct_abstract_declarator_opt, 
					  c_parameter_type_list_opt) 
	  ->
	    Direct_abstract_declarator_4
	      (normalize_c_direct_abstract_declarator_opt 
		 c_direct_abstract_declarator_opt,
	       normalize_c_parameter_type_list_opt c_parameter_type_list_opt)
	      
    and normalize_c_typedef_name: c_typedef_name ->
      c_typedef_name =
      fun expr ->
	match expr with
	| Typedef_name c_identifier_as_typ ->
	    Typedef_name 
	      (add_c_identifier_as_typ
		 c_identifier_as_typ)
	      
    and normalize_c_initializer: c_initializer -> c_initializer = 
      fun expr ->
	match expr with
	| Initializer_1 c_assignment_expression ->
	    Initializer_1
	      (normalize_c_assignment_expression c_assignment_expression)

	| Initializer_2 c_initializer_list  ->
	    Initializer_2
	      (normalize_c_initializer_list c_initializer_list)
	      
    and normalize_c_initializer_list: c_initializer_list -> 
      c_initializer_list = 
      fun expr ->
	match expr with
	| Initializer_list l ->
	    Initializer_list
	      (List.map
		 (fun (c_designation_opt, c_initializer) -> 
		   (normalize_c_designation_opt c_designation_opt,
		    normalize_c_initializer c_initializer)
		 ) 
		 l)

    and normalize_c_designation: c_designation -> c_designation = 
      fun expr ->
	match expr with
	| Designation c_designator_list ->
	    Designation
	      (normalize_c_designator_list c_designator_list)

    and normalize_c_designator_list: c_designator_list -> c_designator_list = 
      fun expr ->
	match expr with
	| Designator_list l ->
	    Designator_list
	      (List.map normalize_c_designator l)

    and normalize_c_designator: c_designator -> c_designator = 
      fun expr ->
	match expr with
	| Designator_1 c_constant_expression ->
	    Designator_1 
	      (normalize_c_constant_expression c_constant_expression)

	| Designator_2 c_identifier ->
	    Designator_2 
	      (normalize_c_identifier c_identifier)

    and normalize_c_argument_expression_list_opt expr =
      match expr with
      | Some c_argument_expression_list -> 
	  Some 
	    (normalize_c_argument_expression_list 
	       c_argument_expression_list)
      | None -> None
	    
    and normalize_c_init_declarator_list_opt is_static expr = 
      match expr with
      | Some c_init_declarator_list -> 
	  Some 
	    (normalize_c_init_declarator_list is_static
	       c_init_declarator_list)
      | None -> None
	    
    and normalize_c_declaration_specifiers_opt expr = 
      match expr with
      | Some c_declaration_specifiers ->
	  let (c_declaration_specifiers, is_static) = 
	    normalize_c_declaration_specifiers c_declaration_specifiers
	  in
	  (Some c_declaration_specifiers, is_static)
      | None -> 
	  (None, false)

    and normalize_c_declarator_opt is_static expr = 
      match expr with
      | Some c_declarator ->
	  Some (normalize_c_declarator is_static c_declarator)
      | None -> None
	    
    and normalize_c_identifier_opt expr =
      match expr with
      | Some c_identifier -> 
	  Some 
	    (normalize_c_identifier c_identifier)
      | None -> None
	    
    and normalize_c_specifier_qualifier_list_opt expr = 
      match expr with
      | Some c_specifier_qualifier_list ->
	  Some 
	    (normalize_c_specifier_qualifier_list 
	       c_specifier_qualifier_list)

      | None -> None
	    
    and normalize_c_assignment_expression_opt expr =
      match expr with 
      | Some c_assignment_expression ->
	  Some 
	    (normalize_c_assignment_expression 
	       c_assignment_expression)
      | None -> None
	    
    and normalize_c_abstract_declarator_opt expr = 
      match expr with
      | Some c_abstract_declarator ->
	  Some
	    (normalize_c_abstract_declarator 
	       c_abstract_declarator)
      | None -> None
	    
    and normalize_c_type_qualifier_list_opt expr = 
      match expr with
      | Some c_type_qualifier_list ->
	  Some
	    (normalize_c_type_qualifier_list 
	       c_type_qualifier_list)
      | None -> None
	    
    and normalize_c_identifier_list_opt expr = 
      match expr with
      | Some c_identifier_list -> 
	  Some 
	    (normalize_c_identifier_list 
	       c_identifier_list) 
      | None -> None
	    
    and normalize_c_pointer_opt expr = 
      match expr with
      | Some c_pointer -> 
	  Some (normalize_c_pointer c_pointer)
      | None -> None
	    
    and normalize_c_direct_abstract_declarator_opt expr = 
      match expr with
      | Some c_direct_abstract_declarator ->
	  Some 
	    (normalize_c_direct_abstract_declarator 
	       c_direct_abstract_declarator)
      | None -> None
	    
    and normalize_c_parameter_type_list_opt expr = 
      match expr with
      | Some c_parameter_type_list ->
	  Some 
	    (normalize_c_parameter_type_list 
	       c_parameter_type_list )
      | None -> None

    and normalize_c_designation_opt expr =
      match expr with
      | Some c_designation -> 
	  Some (normalize_c_designation c_designation)
      | None -> None
	    
    and normalize_c_statement: c_statement -> c_statement =
      fun expr ->
	match expr with
	| Statement_1 (c_labeled_statement, coord) ->
	    Statement_1 (normalize_c_labeled_statement c_labeled_statement, coord)
	| Statement_2 (c_compound_statement) ->
	    Statement_2 
	      (normalize_c_compound_statement 
		 c_compound_statement)
	| Statement_3 (c_expression_statement, coord)  ->
	    Statement_3
	      (normalize_c_expression_statement c_expression_statement, coord)
	      
	| Statement_4 (c_selection_statement, coord) -> 
	    Statement_4
	      (normalize_c_selection_statement c_selection_statement, coord)
	| Statement_5 (c_iteration_statement, coord) ->
	    Statement_5
	      (normalize_c_iteration_statement c_iteration_statement, coord)
	| Statement_6 (c_jump_statement, coord) -> 
	    Statement_6
	      (normalize_c_jump_statement c_jump_statement, coord)

    and normalize_c_labeled_statement: c_labeled_statement -> c_labeled_statement =
      fun expr ->
	match expr with
	| Labeled_statement_1 (id, c_statement) ->
	    Labeled_statement_1 (id, normalize_c_statement c_statement)

	| Labeled_statement_2_case (c_constant_expression, c_statement) ->
	    Labeled_statement_2_case 
	      (normalize_c_constant_expression
		 c_constant_expression, 
	       normalize_c_statement c_statement)
		
	| Labeled_statement_3_default c_statement ->
	    Labeled_statement_3_default (normalize_c_statement c_statement)
	      
    and normalize_c_compound_statement: c_compound_statement -> c_compound_statement = 
      fun expr ->
	match expr with
	| Compound_statement c_block_item_list_opt ->
	    begin
	      let _ = Var_db.begin_block var_db
	      in
	      let stmt = match c_block_item_list_opt with
	      | Some c_block_item_list -> 
		  Some (normalize_c_block_item_list c_block_item_list)
	      | None -> 
		  None
	      in
	      let _ = Var_db.end_block var_db
	      in
	      Compound_statement stmt
	    end

    and normalize_c_block_item_list: c_block_item_list -> c_block_item_list = 
      fun expr ->
	match expr with
	| Block_item_list l -> 
	    Block_item_list (List.map normalize_c_block_item l)

    and normalize_c_block_item: c_block_item -> c_block_item = 
      fun expr ->
	match expr with
	| Block_item_1 c_declaration ->
	    Block_item_1 (normalize_c_declaration c_declaration)

	| Block_item_2 c_statement ->
	    Block_item_2 (normalize_c_statement c_statement)

    and normalize_c_expression_statement: c_expression_statement -> 
      c_expression_statement =
      fun expr ->
	match expr with
	| Expression_statement expr0_opt -> 
	    Expression_statement
	      (normalize_c_expression_opt expr0_opt)
	      
    and normalize_c_selection_statement: c_selection_statement -> 
      c_selection_statement =
      fun expr ->
	match expr with
	| Selection_statement_1_if (c_expression, c_statement) ->
	    Selection_statement_1_if
	      (normalize_c_expression c_expression, 
	       normalize_c_statement c_statement)
	      
	| Selection_statement_2_if_else 
	    (c_expression, 
	     then_c_statement, else_c_statement) 
	  ->
	    Selection_statement_2_if_else
	      (normalize_c_expression c_expression, 
	       normalize_c_statement then_c_statement, 
	       normalize_c_statement else_c_statement)
	      
	| Selection_statement_3_switch (c_expression, c_statement) ->
	    Selection_statement_3_switch
	      (normalize_c_expression c_expression, 
	       normalize_c_statement c_statement)
	      
    and normalize_c_iteration_statement: c_iteration_statement -> 
      c_iteration_statement =
      fun expr ->
	match expr with
	| Iteration_statement_1_while (c_expression, c_statement) ->
	    Iteration_statement_1_while
	      (normalize_c_expression c_expression, 
	       normalize_c_statement c_statement)
	      
	| Iteration_statement_2_do (c_statement, c_expression) ->
	    Iteration_statement_2_do 
	      (normalize_c_statement c_statement,
	       normalize_c_expression c_expression)
	      
	| Iteration_statement_3_for (expr0_opt, expr1_opt, 
				       expr2_opt, c_statement) 
	  ->
	    Iteration_statement_3_for
	      (normalize_c_expression_opt expr0_opt,
	       normalize_c_expression_opt expr1_opt,
	       normalize_c_expression_opt expr2_opt,
	       normalize_c_statement c_statement)
		
	| Iteration_statement_4_for (c_declaration, expr1_opt,
				     expr2_opt, c_statement) 
	  ->
	    Iteration_statement_4_for
	      (normalize_c_declaration c_declaration,
	       normalize_c_expression_opt expr1_opt,
	       normalize_c_expression_opt expr2_opt,
	       normalize_c_statement c_statement)

    and normalize_c_jump_statement: c_jump_statement -> c_jump_statement = 
      fun expr ->
        match expr with
	| Jump_statement_1_goto _ -> expr
	| Jump_statement_2_continue -> expr
	| Jump_statement_3_break -> expr
	| Jump_statement_4_return expr0_opt ->
	    Jump_statement_4_return 
	      (normalize_c_expression_opt expr0_opt)
	      
    and normalize_c_translation_unit: c_translation_unit -> 
      c_translation_unit = 
      fun c_translation_unit ->
	match c_translation_unit with
	| Translation_unit l ->
	    Translation_unit 
	      (List.map
		 (fun external_declaration -> 
		   normalize_c_external_declaration external_declaration
		 ) l
	      )
	      
    and normalize_c_external_declaration: c_external_declaration -> 
      c_external_declaration =
      fun expr ->
	match expr with
	| External_declaration_1 (c_function_definition, coord) ->
	    External_declaration_1 
	      (normalize_c_function_definition c_function_definition, coord)
	      
	| External_declaration_2 (c_declaration, coord) ->
	    External_declaration_2 
	      (normalize_c_declaration c_declaration, coord)

    and normalize_c_declaration_list: c_declaration_list -> 
      c_declaration_list = 
      fun expr ->
	match expr with
	| Declaration_list l ->
	    Declaration_list
	      (List.map normalize_c_declaration l)
	      
    and normalize_c_declaration_list_opt c_declaration_list_opt = 
      match c_declaration_list_opt with
      | Some c_declaration_list ->
	  Some 
	    (normalize_c_declaration_list c_declaration_list)
      | None -> None

    and normalize_c_expression_opt: c_expression option -> c_expression option = 
      fun expr ->
	match expr with
	| Some expr0 -> 
	    Some (normalize_c_expression expr0)
	| None -> None
	    
    and normalize_c_function_definition: c_function_definition -> 
      c_function_definition =
      fun expr ->
	let Function_definition 
	    (c_declaration_specifiers,
	     c_declarator,
	     c_declaration_list_opt,
	     c_compound_statement) = expr
	in
	let _ = Var_db.begin_function var_db ~fun_name:"abc" ~formal_params:[] 
	in
	let (c_declaration_specifiers, is_static) = 
	  normalize_c_declaration_specifiers c_declaration_specifiers
	in
	let v = Function_definition 
	    (c_declaration_specifiers,
	     normalize_c_declarator is_static c_declarator,
	     normalize_c_declaration_list_opt c_declaration_list_opt,
	     normalize_c_compound_statement c_compound_statement)
	in
	let _ = Var_db.end_function var_db
	in
	v
    in
    let allow_undefined_symbols = !Mlite_config.allow_undefined_symbols
    in
    let _ = Mlite_config.allow_undefined_symbols := true
    in
    let _ = Var_db.begin_file var_db filename
    in
    let v  = normalize_c_translation_unit c_translation_unit
    in
    let _ = Var_db.end_file var_db
    in
    let _ = Mlite_config.allow_undefined_symbols := allow_undefined_symbols
    in
    v
      
