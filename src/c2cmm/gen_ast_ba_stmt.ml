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
module O = Ast_ba_stmt
open Mapping
open Safe_list

type compound = {
  mutable decls:  O.c_declaration list;
  mutable stmt010s:  O.c_stmt010 list;
}

and function_data = {
    continue_target_stack: (string * bool * bool ref) Stack.t;
    label_serno: int ref;
    prefix: string;
  }

let user_opts = []

let compile: string -> I.c_translation_unit -> O.c_translation_unit = 
  fun basename c_translation_unit ->
    let compound_stack = Stack.create ()
    and function_scope = Stack.create ()
    in
    let begin_function () = 
      let function_data = 
	{ 
	  label_serno = ref 0;
	  continue_target_stack = Stack.create ();
	  prefix = Random_prefix.get ("_" ^ (C_str.c_identifierize basename) ^ "_ba_");
	}
      in
      Stack.push function_data function_scope
	
    and end_function () = 
      ignore (Stack.pop function_scope)
	
    and new_label () = 
      let function_data = Stack.top function_scope
      in
      let v = !(function_data.label_serno)
      in
      incr function_data.label_serno;
      Printf.sprintf "%s%d" function_data.prefix v
    in
    let rec compile_c_identifier = function
      | I.Identifier s -> O.Identifier s

    and compile_c_identifier_as_typ = function
      | I.Identifier_as_typ s -> O.Identifier_as_typ s
	  
    and compile_c_enumeration_constant = function
      | I.Enumeration_constant c_identifier -> 
	  O.Enumeration_constant (compile_c_identifier c_identifier)

    and compile_gnu_attribute (str, expr_list) = 
      (str, List.map compile_c_expression expr_list)
	    
    and compile_c_primary_expression = function
      | I.Primary_expression_1 c_identifier ->
	  O.Primary_expression_1
	    (compile_c_identifier c_identifier)
	    
      | I.Primary_expression_2 c_constant ->
	  O.Primary_expression_2 c_constant
	    
      | I.Primary_expression_3 c_string_literal ->
	  O.Primary_expression_3 c_string_literal
	    
      | I.Primary_expression_4 c_expression ->
	  O.Primary_expression_4 
	    (compile_c_expression c_expression)
	    
      | I.Primary_expression_macro_va_arg (c_expression, c_type_name) -> 
	  O.Primary_expression_macro_va_arg 
	    (compile_c_expression c_expression,
	    compile_c_type_name c_type_name)
	    
      | I.Primary_expression_macro_va_start (expr0, expr1) -> 
	  O.Primary_expression_macro_va_start
	    (compile_c_expression expr0,
	    compile_c_expression expr1)

      | I.Primary_expression_macro_va_end (c_expression) -> 
	  O.Primary_expression_macro_va_end 
	    (compile_c_expression c_expression)

      | I.Primary_expression_builtin_types_compatible (t0, t1) ->
	  O.Primary_expression_builtin_types_compatible
	    (compile_c_type_name t0, compile_c_type_name t1)

      | I.Primary_expression_builtin_constant_p (c_expression) -> 
	  O.Primary_expression_builtin_constant_p
	    (compile_c_expression c_expression)

      | I.Primary_expression_builtin_expect (c_expression, c_constant_expression) -> 
	  O.Primary_expression_builtin_expect
	    (compile_c_expression c_expression,
	    compile_c_constant_expression c_constant_expression)
	    
      | I.Primary_expression_gnu_block (block) -> 
	  O.Primary_expression_gnu_block (compile_c_compound_statement block)
	    
      | I.Primary_expression_gnu_labeladdr c_identifier ->
	  O.Primary_expression_gnu_labeladdr
	    (compile_c_identifier c_identifier)
	    
    and compile_c_postfix_expression = function
      | I.Postfix_expression_1 c_primary_expression ->
	  O.Postfix_expression_1 
	    (compile_c_primary_expression c_primary_expression)
	    
      | I.Postfix_expression_2 (c_postfix_expression, c_expression) ->
	  O.Postfix_expression_2
	    (compile_c_postfix_expression c_postfix_expression,
	    compile_c_expression c_expression)
	    
      | I.Postfix_expression_3 (c_postfix_expression, 
	c_argument_expression_list_opt) 
	->
	  O.Postfix_expression_3 
	    (compile_c_postfix_expression c_postfix_expression,
	    map_opt compile_c_argument_expression_list
	      c_argument_expression_list_opt)
	    
      | I.Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	  O.Postfix_expression_4_DOT
	    (compile_c_postfix_expression c_postfix_expression,
	    compile_c_identifier c_identifier)
	    
      | I.Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	  O.Postfix_expression_5_ARROW
	    (compile_c_postfix_expression c_postfix_expression,
	    compile_c_identifier c_identifier)
	    
      | I.Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	  O.Postfix_expression_6_PLUS_PLUS
	    (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	  O.Postfix_expression_7_MINUS_MINUS
	    (compile_c_postfix_expression c_postfix_expression)

      | I.Postfix_expression_8 (c_type_name, c_initializer_list) ->
	  O.Postfix_expression_8
	    (compile_c_type_name c_type_name,
	    compile_c_initializer_list c_initializer_list)
	    
    and compile_c_argument_expression_list (I.Argument_expression_list l) =
      O.Argument_expression_list 
	((Safe_list.map
	  compile_c_assignment_expression
	) l)
	
    and compile_c_unary_expression = function
      | I.Unary_expression_1 c_postfix_expression ->
	  O.Unary_expression_1 
	    (compile_c_postfix_expression c_postfix_expression)
	    
      | I.Unary_expression_2_PLUS_PLUS c_unary_expression ->
	  O.Unary_expression_2_PLUS_PLUS
	    (compile_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_3_MINUS_MINUS c_unary_expression ->
	  O.Unary_expression_3_MINUS_MINUS
	    (compile_c_unary_expression c_unary_expression)

      | I.Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	  O.Unary_expression_4
	    (c_unary_operator,
	    compile_c_cast_expression c_cast_expression)
	    
      | I.Unary_expression_5_SIZEOF c_unary_expression ->
	  O.Unary_expression_5_SIZEOF
	    (compile_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_6_SIZEOF c_type_name ->
	  O.Unary_expression_6_SIZEOF
	    (compile_c_type_name c_type_name)

      | I.Unary_expression_7_ALIGNOF c_unary_expression ->
	  O.Unary_expression_7_ALIGNOF
	    (compile_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_8_ALIGNOF c_type_name ->
	  O.Unary_expression_8_ALIGNOF
	    (compile_c_type_name c_type_name)

    and compile_c_cast_expression = function
      | I.Cast_expression_1 c_unary_expression ->
	  O.Cast_expression_1
	    (compile_c_unary_expression c_unary_expression)
	    
      | I.Cast_expression_2 (c_type_name, c_cast_expression) ->
	  O.Cast_expression_2
	    (compile_c_type_name c_type_name,
	    compile_c_cast_expression c_cast_expression)
	    
    and compile_c_multiplicative_expression = function
      | I.Multiplicative_expression_1 c_cast_expression ->
	  O.Multiplicative_expression_1
	    (compile_c_cast_expression c_cast_expression)

      | I.Multiplicative_expression_2_STAR 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  O.Multiplicative_expression_2_STAR
	    (compile_c_multiplicative_expression c_multiplicative_expression,
	    compile_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_3_SLASH 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  O.Multiplicative_expression_3_SLASH
	    (compile_c_multiplicative_expression c_multiplicative_expression,
	    compile_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_4_PERCENT 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  O.Multiplicative_expression_4_PERCENT
	    (compile_c_multiplicative_expression c_multiplicative_expression,
	    compile_c_cast_expression c_cast_expression)
	    
    and compile_c_additive_expression = function
      | I.Additive_expression_1 c_multiplicative_expression ->
	  O.Additive_expression_1
	    (compile_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_2_PLUS (c_additive_expression, 
	c_multiplicative_expression) 
	->
	  O.Additive_expression_2_PLUS
	    (compile_c_additive_expression c_additive_expression,
	    compile_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_3_MINUS (c_additive_expression, 
	c_multiplicative_expression) 
	->
	  O.Additive_expression_3_MINUS
	    (compile_c_additive_expression c_additive_expression,
	    compile_c_multiplicative_expression c_multiplicative_expression)

    and compile_c_shift_expression = function
      | I.Shift_expression_1 c_additive_expression ->
	  O.Shift_expression_1
	    (compile_c_additive_expression c_additive_expression)

      | I.Shift_expression_2_INF_INF (c_shift_expression, 
	c_additive_expression) 
	->
	  O.Shift_expression_2_INF_INF
	    (compile_c_shift_expression c_shift_expression,
	    compile_c_additive_expression c_additive_expression)
	    
      | I.Shift_expression_3_SUP_SUP (c_shift_expression, 
	c_additive_expression) 
	->
	  O.Shift_expression_3_SUP_SUP
	    (compile_c_shift_expression c_shift_expression,
	    compile_c_additive_expression c_additive_expression)

    and compile_c_relational_expression = function
      | I.Relational_expression_1 c_shift_expression ->
	  O.Relational_expression_1 
	    (compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_2_INF (c_relational_expression, 
	c_shift_expression) 
	->
	  O.Relational_expression_2_INF
	    (compile_c_relational_expression c_relational_expression,
	    compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_3_SUP (c_relational_expression, 
	c_shift_expression) 
	->
	  O.Relational_expression_3_SUP
	    (compile_c_relational_expression c_relational_expression,
	    compile_c_shift_expression c_shift_expression)

      | I.Relational_expression_4_INF_EQ (c_relational_expression, 
	c_shift_expression) 
	->
	  O.Relational_expression_4_INF_EQ
	    (compile_c_relational_expression c_relational_expression,
	    compile_c_shift_expression c_shift_expression)
	    
      | I.Relational_expression_5_SUP_EQ (c_relational_expression, 
	c_shift_expression) 
	->
	  O.Relational_expression_5_SUP_EQ
	    (compile_c_relational_expression c_relational_expression,
	    compile_c_shift_expression c_shift_expression)

    and compile_c_equality_expression = function
      | I.Equality_expression_1 c_relational_expression ->
	  O.Equality_expression_1
	    (compile_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_2_EQ_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  O.Equality_expression_2_EQ_EQ
	    (compile_c_equality_expression c_equality_expression,
	    compile_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_3_EXCLAM_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  O.Equality_expression_3_EXCLAM_EQ
	    (compile_c_equality_expression c_equality_expression,
	    compile_c_relational_expression c_relational_expression)

    and compile_c_and_expression = function
      | I.And_expression_1 c_equality_expression ->
	  O.And_expression_1
	    (compile_c_equality_expression c_equality_expression)
	    
      | I.And_expression_2_AND (c_and_expression, c_equality_expression) ->
	  O.And_expression_2_AND
	    (compile_c_and_expression c_and_expression,
	    compile_c_equality_expression c_equality_expression)
	    
    and compile_c_exclusive_or_expression = function
      | I.Exclusive_or_expression_1 c_and_expression ->
	  O.Exclusive_or_expression_1
	    (compile_c_and_expression c_and_expression)

      | I.Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, 
	c_and_expression) 
	->
	  O.Exclusive_or_expression_2_CIRC
	    (compile_c_exclusive_or_expression c_exclusive_or_expression,
	    compile_c_and_expression c_and_expression)
	    
    and compile_c_inclusive_or_expression = function
      | I.Inclusive_or_expression_1 c_exclusive_or_expression ->
	  O.Inclusive_or_expression_1
	    (compile_c_exclusive_or_expression c_exclusive_or_expression)

      | I.Inclusive_or_expression_2_PIPE 
	  (c_inclusive_or_expression, c_exclusive_or_expression) 
	->
	  O.Inclusive_or_expression_2_PIPE
	    (compile_c_inclusive_or_expression c_inclusive_or_expression,
	    compile_c_exclusive_or_expression c_exclusive_or_expression)
	    
    and compile_c_logical_and_expression = function
      | I.Logical_and_expression_1 c_inclusive_or_expression ->
	  O.Logical_and_expression_1
	    (compile_c_inclusive_or_expression c_inclusive_or_expression)
	    
      | I.Logical_and_expression_2_AND_AND 
	  (c_logical_and_expression, c_inclusive_or_expression) 
	->
	  O.Logical_and_expression_2_AND_AND 
	    (compile_c_logical_and_expression c_logical_and_expression,
	    compile_c_inclusive_or_expression c_inclusive_or_expression)
	    
    and compile_c_logical_or_expression = function
      | I.Logical_or_expression_1 c_logical_and_expression ->
	  O.Logical_or_expression_1
	    (compile_c_logical_and_expression c_logical_and_expression)
	    
      | I.Logical_or_expression_2_PIPE_PIPE 
	  (c_logical_or_expression, c_logical_and_expression) 
	->
	  O.Logical_or_expression_2_PIPE_PIPE
	    (compile_c_logical_or_expression c_logical_or_expression,
	    compile_c_logical_and_expression c_logical_and_expression)
	    
    and compile_c_conditional_expression = function
      | I.Conditional_expression_1 c_logical_or_expression ->
	  O.Conditional_expression_1
	    (compile_c_logical_or_expression c_logical_or_expression)

      | I.Conditional_expression_2 
	  (c_logical_or_expression, c_expression, c_conditional_expression) 
	->
	  O.Conditional_expression_2 
	    (compile_c_logical_or_expression c_logical_or_expression,
	    compile_c_expression c_expression,
	    compile_c_conditional_expression c_conditional_expression)

      | I.Conditional_expression_gnu 
	  (c_logical_or_expression, c_conditional_expression) 
	->
	  O.Conditional_expression_gnu 
	    (compile_c_logical_or_expression c_logical_or_expression,
	    compile_c_conditional_expression c_conditional_expression)

    and compile_c_assignment_expression = function
      | I.Assignment_expression_1 c_conditional_expression ->
	  O.Assignment_expression_1
	    (compile_c_conditional_expression c_conditional_expression)
	    
      | I.Assignment_expression_2 
	  (c_unary_expression, c_assignment_operator, 
	  c_assignment_expression) 
	->
	  O.Assignment_expression_2 
	    (compile_c_unary_expression c_unary_expression,
	    c_assignment_operator,
	    compile_c_assignment_expression c_assignment_expression)

    and compile_c_expression = function
      | I.Expression_1 c_assignment_expression -> 
	  O.Expression_1
	    (compile_c_assignment_expression c_assignment_expression)

      | I.Expression_2 (c_expression, c_assignment_expression) ->
	  O.Expression_2 
	    (compile_c_expression c_expression,
	    compile_c_assignment_expression c_assignment_expression)

    and compile_c_constant_expression = function
      | I.Constant_expression c_conditional_expression ->
	  O.Constant_expression
	    (compile_c_conditional_expression c_conditional_expression)

    and compile_c_declaration = function
      | I.Declaration (c_declaration_specifiers,  
	c_init_declarator_list_opt) 
	->
	  O.Declaration
	    (compile_c_declaration_specifiers c_declaration_specifiers,
	    map_opt (Safe_list.map compile_c_init_declarator)  c_init_declarator_list_opt)
	    
    and compile_c_declaration_specifiers = function
      | I.Declaration_specifiers_1 
	  (c_storage_class_specifier, c_declaration_specifiers_opt) 
	->
	  O.Declaration_specifiers_1
	    (c_storage_class_specifier,
	    map_opt compile_c_declaration_specifiers
	      c_declaration_specifiers_opt)
	    
      | I.Declaration_specifiers_2 (c_type_specifier, 
	c_declaration_specifiers_opt) 
	->
	  O.Declaration_specifiers_2
	    (compile_c_type_specifier c_type_specifier,
	    map_opt compile_c_declaration_specifiers
	      c_declaration_specifiers_opt)
	    
      | I.Declaration_specifiers_3 (c_type_qualifier, 
	c_declaration_specifiers_opt) 
	->
	  O.Declaration_specifiers_3
	    (c_type_qualifier, map_opt compile_c_declaration_specifiers
	      c_declaration_specifiers_opt)
	    
      | I.Declaration_specifiers_4 (c_function_specifier, 
	c_declaration_specifiers_opt) 
	->
	  O.Declaration_specifiers_4
	    (c_function_specifier,
	    map_opt compile_c_declaration_specifiers
	      c_declaration_specifiers_opt)

      | I.Declaration_specifiers_GNU (attribute,
	c_declaration_specifiers_opt) 
	->
	  O.Declaration_specifiers_GNU
	    (compile_gnu_attribute attribute,
	    map_opt compile_c_declaration_specifiers
	      c_declaration_specifiers_opt)

    and compile_c_init_declarator = function
      | I.Init_declarator_1 c_declarator ->
	  O.Init_declarator_1
	    (compile_c_declarator c_declarator)
	    
      | I.Init_declarator_2 (c_declarator, c_initializer) ->
	  O.Init_declarator_2
	    (compile_c_declarator c_declarator,
	    compile_c_initializer c_initializer)

    and compile_c_type_specifier = function
      | I.Type_builtin builtin_type -> O.Type_builtin builtin_type
      | I.Type_specifier_STRUCT_OR_UNION c_struct_or_union_specifier ->
	  O.Type_specifier_STRUCT_OR_UNION
	    (compile_c_struct_or_union_specifier c_struct_or_union_specifier)
      | I.Type_specifier_ENUM c_enum_specifier ->
	  O.Type_specifier_ENUM
	    (compile_c_enum_specifier c_enum_specifier)

      | I.Type_specifier_TYPENAME c_typedef_name ->
	  O.Type_specifier_TYPENAME
	    (compile_c_typedef_name c_typedef_name)

      | I.Type_specifier_GCC_TYPEOF_E c_expression ->
	  O.Type_specifier_GCC_TYPEOF_E 
	    (compile_c_expression c_expression)

      | I.Type_specifier_GCC_TYPEOF_T c_type_name ->
	  O.Type_specifier_GCC_TYPEOF_T 
	    (compile_c_type_name c_type_name)

    and compile_c_struct_or_union_specifier = function
      | I.Struct_or_union_specifier_1 
	  (c_struct_or_union, 
	  c_identifier_opt, 
	  c_struct_declaration_list) 
	->
	  O.Struct_or_union_specifier_1 
	    (c_struct_or_union,
	    map_opt compile_c_identifier c_identifier_opt,
	    Safe_list.map compile_c_struct_declaration
	      c_struct_declaration_list)
	    
      | I.Struct_or_union_specifier_2 (c_struct_or_union, c_identifier) ->
	  O.Struct_or_union_specifier_2
	    (c_struct_or_union, compile_c_identifier c_identifier)

    and compile_c_struct_declaration = function
      | I.Struct_declaration 
	  (c_specifier_qualifier_list, c_struct_declarator_list) 
	->
	  O.Struct_declaration 
	    (compile_c_specifier_qualifier_list c_specifier_qualifier_list,
	    Safe_list.map compile_c_struct_declarator c_struct_declarator_list)
	    
    and compile_c_specifier_qualifier_list = function
      | I.Specifier_qualifier_list_1 (c_type_specifier, 
	c_specifier_qualifier_list_opt) 
	->
	  O.Specifier_qualifier_list_1
	    (compile_c_type_specifier c_type_specifier,
	    map_opt compile_c_specifier_qualifier_list 
	      c_specifier_qualifier_list_opt)
	    
      | I.Specifier_qualifier_list_2 (c_type_qualifier, 
	c_specifier_qualifier_list_opt) 
	->
	  O.Specifier_qualifier_list_2
	    (c_type_qualifier,
	    map_opt compile_c_specifier_qualifier_list
	      c_specifier_qualifier_list_opt)

      | I.Specifier_qualifier_list_GNU (gnu_attribute, 
	c_specifier_qualifier_list_opt) 
	->
	  O.Specifier_qualifier_list_GNU
	    (compile_gnu_attribute gnu_attribute,
	    map_opt compile_c_specifier_qualifier_list
	      c_specifier_qualifier_list_opt)
	    
    and compile_c_struct_declarator = function
      | I.Struct_declarator_1 c_declarator ->
	  O.Struct_declarator_1
	    (compile_c_declarator c_declarator)

      | I.Struct_declarator_2 (c_declarator_opt, c_constant_expression) ->
	  O.Struct_declarator_2
	    (map_opt compile_c_declarator c_declarator_opt,
	    compile_c_constant_expression c_constant_expression)

      | I.Struct_declarator_GNU (c_struct_declarator, attributes) ->
	  O.Struct_declarator_GNU
	    (compile_c_struct_declarator c_struct_declarator, 
	    List.map compile_gnu_attribute attributes)

    and compile_c_enum_specifier = function
      | I.Enum_specifier_1 (c_identifier_opt, c_enumerator_list) ->
	  O.Enum_specifier_1
	    (map_opt compile_c_identifier c_identifier_opt,
	    Safe_list.map compile_c_enumerator c_enumerator_list)
	    
      | I.Enum_specifier_2 c_identifier ->
	  O.Enum_specifier_2
	    (compile_c_identifier c_identifier)

    and compile_c_enumerator = function
      | I.Enumerator_1 c_enumeration_constant -> 
	  O.Enumerator_1
	    (compile_c_enumeration_constant c_enumeration_constant)
	    
      | I.Enumerator_2 (c_enumeration_constant, c_constant_expression) ->
	  O.Enumerator_2
	    (compile_c_enumeration_constant c_enumeration_constant,
	    compile_c_constant_expression c_constant_expression)

    and compile_c_declarator = function
      | I.Declarator (c_pointer_opt, c_direct_declarator) ->
	  O.Declarator
	    (map_opt compile_c_pointer c_pointer_opt,
	    compile_c_direct_declarator c_direct_declarator)

      | I.Declarator_GNU (declarator, attributes) ->
	  O.Declarator_GNU 
	    (compile_c_declarator declarator, 
	    List.map compile_gnu_attribute attributes)

    and compile_c_direct_declarator = function
      | I.Direct_declarator_1 c_identifier ->
	  O.Direct_declarator_1
	    (compile_c_identifier c_identifier)
	    
      | I.Direct_declarator_2 c_declarator ->
	  O.Direct_declarator_2
	    (compile_c_declarator c_declarator)

      | I.Direct_declarator_3 (c_direct_declarator, 
	c_type_qualifier_list_opt, 
	c_assignment_expression_opt) 
	->
	  O.Direct_declarator_3
	    (compile_c_direct_declarator c_direct_declarator,
	    c_type_qualifier_list_opt,
	    map_opt compile_c_assignment_expression c_assignment_expression_opt)
	    
      | I.Direct_declarator_4_STATIC (c_direct_declarator, 
	c_type_qualifier_list_opt, 
	c_assignment_expression) 
	->
	  O.Direct_declarator_4_STATIC
	    (compile_c_direct_declarator c_direct_declarator,
	    c_type_qualifier_list_opt,
	    compile_c_assignment_expression c_assignment_expression)

      | I.Direct_declarator_5_STATIC (c_direct_declarator, 
	c_type_qualifier_list, 
	c_assignment_expression) 
	->
	  O.Direct_declarator_5_STATIC
	    (compile_c_direct_declarator c_direct_declarator,
	    c_type_qualifier_list,
	    compile_c_assignment_expression c_assignment_expression)

      | I.Direct_declarator_6_STAR (c_direct_declarator, 
	c_type_qualifier_list_opt) 
	->
	  O.Direct_declarator_6_STAR
	    (compile_c_direct_declarator c_direct_declarator,
	    c_type_qualifier_list_opt)
	    
      | I.Direct_declarator_7 (c_direct_declarator, c_parameter_type_list) ->
	  O.Direct_declarator_7
	    (compile_c_direct_declarator c_direct_declarator,
	    compile_c_parameter_type_list c_parameter_type_list)
	    
      | I.Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	  O.Direct_declarator_8
	    (compile_c_direct_declarator c_direct_declarator,
	    map_list_opt compile_c_identifier c_identifier_list_opt)
	    
    and compile_c_pointer = function
      | I.Pointer_1 c_type_qualifier_list_opt ->
	  O.Pointer_1 (c_type_qualifier_list_opt)
	    
      | I.Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
	  O.Pointer_2 (c_type_qualifier_list_opt,
	  compile_c_pointer c_pointer)

    and compile_c_parameter_type_list = function
      | I.Parameter_type_list_FIX c_parameter_list ->
	  O.Parameter_type_list_FIX
	    (compile_c_parameter_list c_parameter_list)

      | I.Parameter_type_list_VAR c_parameter_list ->
	  O.Parameter_type_list_VAR
	    (compile_c_parameter_list c_parameter_list)
	    
    and compile_c_parameter_list = function
      | I.Parameter_list l -> 
	  O.Parameter_list 
	    (Safe_list.map compile_c_parameter_declaration l)

    and compile_c_parameter_declaration = function
      | I.Parameter_declaration_1 (c_declaration_specifiers, c_declarator) ->
	  O.Parameter_declaration_1
	    (compile_c_declaration_specifiers c_declaration_specifiers,
	    compile_c_declarator c_declarator)

      | I.Parameter_declaration_2 (c_declaration_specifiers, 
	c_abstract_declarator_opt) 
	->
	  O.Parameter_declaration_2
	    (compile_c_declaration_specifiers c_declaration_specifiers,
	    map_opt compile_c_abstract_declarator c_abstract_declarator_opt)

    and compile_c_type_name = function
      | I.Type_name (c_specifier_qualifier_list, c_abstract_declarator_opt) ->
	  O.Type_name
	    (compile_c_specifier_qualifier_list c_specifier_qualifier_list,
	    map_opt compile_c_abstract_declarator c_abstract_declarator_opt)

    and compile_c_abstract_declarator = function
      | I.Abstract_declarator_1 c_pointer -> 
	  O.Abstract_declarator_1
	    (compile_c_pointer c_pointer)
	    
      | I.Abstract_declarator_2 (c_pointer_opt, 
	c_direct_abstract_declarator) 
	->
	  O.Abstract_declarator_2
	    (map_opt compile_c_pointer c_pointer_opt,
	    compile_c_direct_abstract_declarator 
	      c_direct_abstract_declarator)
	    
    and compile_c_direct_abstract_declarator = function
      | I.Direct_abstract_declarator_error -> assert false
      | I.Direct_abstract_declarator_1 c_abstract_declarator ->
	  O.Direct_abstract_declarator_1
	    (compile_c_abstract_declarator c_abstract_declarator)

      | I.Direct_abstract_declarator_2 
	  (c_direct_abstract_declarator_opt, c_assignment_expression_opt) 
	->
	  O.Direct_abstract_declarator_2
	    (map_opt compile_c_direct_abstract_declarator 
	      c_direct_abstract_declarator_opt,
	    map_opt compile_c_assignment_expression
	      c_assignment_expression_opt)
	    
      | I.Direct_abstract_declarator_3_STAR c_direct_abstract_declarator_opt 
	->
	  O.Direct_abstract_declarator_3_STAR
	    (map_opt compile_c_direct_abstract_declarator
	      c_direct_abstract_declarator_opt)

      | I.Direct_abstract_declarator_4 (c_direct_abstract_declarator_opt, 
	c_parameter_type_list_opt) 
	->
	  O.Direct_abstract_declarator_4
	    (map_opt compile_c_direct_abstract_declarator
	      c_direct_abstract_declarator_opt,
	    map_opt compile_c_parameter_type_list c_parameter_type_list_opt)
	    
    and compile_c_typedef_name = function
      | I.Typedef_name c_identifier_as_typ ->
	  O.Typedef_name 
	    (compile_c_identifier_as_typ
	      c_identifier_as_typ)
	    
    and compile_c_initializer = function
      | I.Initializer_1 c_assignment_expression ->
	  O.Initializer_1
	    (compile_c_assignment_expression c_assignment_expression)

      | I.Initializer_2 c_initializer_list  ->
	  O.Initializer_2
	    (compile_c_initializer_list c_initializer_list)
	    
    and compile_c_initializer_list = function
      | I.Initializer_list l ->
	  O.Initializer_list
	    (Safe_list.map
	      (fun (c_designation_opt, c_initializer) -> 
		(map_opt compile_c_designation c_designation_opt,
		compile_c_initializer c_initializer)
	      ) 
	      l)
	    
    and compile_c_designation = function
      | I.Designation c_designator_list ->
	  O.Designation
	    (Safe_list.map compile_c_designator c_designator_list)

    and compile_c_designator = function
      | I.Designator_1 c_constant_expression ->
	  O.Designator_1 (compile_c_constant_expression c_constant_expression)
	    
      | I.Designator_2 c_identifier ->
	  O.Designator_2 (compile_c_identifier c_identifier)
	    
      | I.Designator_gnu_range (e0, e1) ->
	  O.Designator_gnu_range 
	    (compile_c_constant_expression e0,
	    compile_c_constant_expression e1)

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

    and compile_c_statement = function
      | I.Statement_at (coord, stmt) ->
	  O.STMT_AT (coord, compile_c_statement stmt)
	    
      | I.Statement_1 (c_labeled_statement) ->
	  compile_c_labeled_statement c_labeled_statement
	    
      | I.Statement_2 (c_compound_statement) ->
	  O.COMPOUND 
	    (None, compile_c_compound_statement 
	      c_compound_statement)
	    
      | I.Statement_3 (c_expression_statement)  ->
	  compile_c_expression_statement c_expression_statement
	    
      | I.Statement_4 (c_selection_statement) -> 
	  compile_c_selection_statement c_selection_statement
	    
      | I.Statement_5 (c_iteration_statement) ->
	  compile_c_iteration_statement c_iteration_statement
	    
      | I.Statement_6 (c_jump_statement) -> 
	  compile_c_jump_statement c_jump_statement
	    
      | I.Statement_asm (str_list, asm_details_opt) ->
	  compile_asm (str_list, asm_details_opt)
	    
    and compile_c_labeled_statement expr =
      match expr with
	| I.Labeled_statement_1 ((I.Identifier str), c_statement) ->
	    O.LABEL (str, compile_c_statement c_statement)
	| I.Labeled_statement_2_case 
	    (c_constant_expression, c_statement) ->
	    O.CASE 
	      (compile_c_constant_expression
		c_constant_expression, 
	      compile_c_statement c_statement)
	| I.Labeled_statement_3_default c_statement ->
	    O.DEFAULT (compile_c_statement c_statement)
	      
	| I.Labeled_statement_gnu_case_range
	    (e0, e1, c_statement) ->
	    O.CASE_RANGE 
	      (compile_c_constant_expression e0,
	      compile_c_constant_expression e1,
	      compile_c_statement c_statement)
	      
    and compile_c_compound_statement: I.c_compound_statement -> 
    O.c_compound_stmt010 = 
      fun expr ->
	let stmt010 = ref (O.BLOCK ([], [], []))
	in
	Stack.push (Stack.create ()) compound_stack;
	match expr with
	  | I.Compound_statement (labels, c_block_item_list_opt) ->
	      begin
		let _ = match c_block_item_list_opt with
		  | Some c_block_item_list -> 
                      let _ = Safe_list.iter
			(compile_c_block_item (Stack.top compound_stack)) c_block_item_list  
		      in
		      let stack = Stack.pop compound_stack
		      in
		      while (not (Stack.is_empty stack)) do
			let compound = Stack.pop stack
			in
			match !stmt010 with
			  | O.BLOCK ([], [], []) -> 
			      stmt010 := 
				O.BLOCK 
				  (labels, compound.decls, 
				  compound.stmt010s)
			  | _ ->
			      stmt010 := 
				O.BLOCK 
				  (labels, 
				  compound.decls, 
				  compound.stmt010s @$ 
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
		      compound.decls <- compound.decls @$ [block_item]
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
		  compound.stmt010s <- compound.stmt010s @$ [block_item]
	      end
		
    and compile_c_expression_statement = function
      | I.Expression_statement c_expression_opt ->
	  begin
	    match c_expression_opt with 
	      | Some c_expression ->
		  O.COMPUTATION 
		    (compile_c_expression c_expression)
	      | None -> O.NOP
	  end
	    
    and compile_c_selection_statement: I.c_selection_statement -> O.c_stmt010 = 
      fun expr ->
	match expr with
	  | I.Selection_statement_1_if (c_expression, c_statement) ->
	      O.IF 
		(compile_c_expression c_expression, 
		compile_c_statement c_statement, 
		O.NOP)
		
	  | I.Selection_statement_2_if_else 
	      (c_expression, 
	      then_c_statement, else_c_statement) 
	    ->
	      O.IF 
		(compile_c_expression c_expression, 
		compile_c_statement then_c_statement, 
		compile_c_statement else_c_statement)
		
	  | I.Selection_statement_3_switch (c_expression, c_statement) ->
	      O.SWITCH 
		(compile_c_expression c_expression, 
		compile_c_statement c_statement)
		
    and compile_c_iteration_statement: I.c_iteration_statement -> O.c_stmt010 = 
      fun expr ->
	let function_data = Stack.top function_scope
	in
	match expr with
	  | I.Iteration_statement_1_while (c_expression, c_statement) ->
	      let _ = Stack.push ("", false, ref false) (** begin **)
		function_data.continue_target_stack
	      in
	      let v = O.WHILE 
		(compile_c_expression c_expression, 
		compile_c_statement c_statement)
	      in
	      let _ = 
		Stack.pop function_data.continue_target_stack (** end **)
	      in
	      v
		
	  | I.Iteration_statement_2_do (c_statement, c_expression) ->
	      let txt_opt = camlp4_macro_str_opt_pp_print 
		(fun fm -> 
		  Format.pp_open_box fm 0;
		  Format.pp_print_string fm "do {...} while (";
		  Ast_aa_gram_printer.pp_print_c_expression 
		    fm ~need_paren:false c_expression;
		  Format.pp_print_string fm ");";
		  Format.pp_close_box fm ()
		)
	      in
	      let _ = Stack.push (new_label (), true, ref false)  (** begin **)
		function_data.continue_target_stack
	      in
	      let loop_body = compile_c_statement c_statement
	      in
	      let (lb, _, is_used) = 
		Stack.pop function_data.continue_target_stack (** end **)
	      in
	      let cond_stmt = 
		O.IF (compile_c_expression c_expression, 
		O.COMPOUND (Some ("do_true"), O.BLOCK ([], [], [])), O.BREAK)
	      in
	      let label_stmt = 
		if !is_used then
		  O.LABEL (lb, cond_stmt)
		else
		  cond_stmt
	      in
	      O.LOOP (O.COMPOUND (txt_opt, O.BLOCK ([], [],[loop_body;label_stmt])))
		
	  | I.Iteration_statement_3_for 
	      (c_expression_opt0, c_expression_opt1, c_expression_opt2, c_statement) 
	    ->
	      begin
		let txt_opt = camlp4_macro_str_opt_pp_print 
		  (fun fm -> 
		    Format.pp_open_box fm 0;
		    Format.pp_print_string fm "for (";
		    let _ = match c_expression_opt0 with
		      | Some expr -> 
			  Ast_aa_gram_printer.pp_print_c_expression 
			    fm ~need_paren:false expr;
		      | None -> ()
		    in
		    Format.pp_print_string fm ";";
		    let _ = match c_expression_opt1 with
		      | Some expr -> 
			  Ast_aa_gram_printer.pp_print_c_expression 
			    fm ~need_paren:false expr;
		      | None -> ()
		    in
		    Format.pp_print_string fm ";";
		    let _ = match c_expression_opt2 with
		      | Some expr -> 
			  Ast_aa_gram_printer.pp_print_c_expression 
			    fm ~need_paren:false expr;
		      | None -> ()
		    in
		    Format.pp_print_string fm ") {...}";
		    Format.pp_close_box fm ()
		  )
		in
		let pre_loop = match c_expression_opt0 with
		  | Some expr -> 
		      [O.COMPUTATION 
			(compile_c_expression expr)]
		  | None -> []
		in
		let _ = match c_expression_opt2 with
		  | Some _ ->
		      Stack.push (new_label (), true, ref false) 
			function_data.continue_target_stack
		  | None ->
		      Stack.push ("", false, ref false) 
			function_data.continue_target_stack
		in
		let loop_body = compile_c_statement c_statement
		in
		let (lb, _, is_used) = Stack.pop function_data.continue_target_stack
		in
		let loop_stmt = match c_expression_opt1 with
		  | Some expr1 -> 
		      begin
			let expr1 = compile_c_expression expr1
			in
			match c_expression_opt2 with
			  | Some expr2 -> 
			      let label_stmt = 
				if !is_used then
				  O.LABEL 
				    (lb, O.COMPUTATION 
				      (compile_c_expression expr2))
				else
				  O.COMPUTATION 
				    (compile_c_expression expr2)
			      in
			      O.WHILE 
				(expr1,
				O.COMPOUND
				  (txt_opt,
				  O.BLOCK 
				    ([], [],[loop_body;label_stmt])))
			  | None -> 
			      O.WHILE 
				(expr1, 
				O.COMPOUND 
				  (Some "for", 
				  O.BLOCK 
				    ([], [],[loop_body])))
		      end
		  | None ->
		      begin
			match c_expression_opt2 with
			  | Some expr2 -> 
			      let label_stmt = 
				if !is_used then
				  O.LABEL 
				    (lb, O.COMPUTATION 
				      (compile_c_expression expr2))
				else
				  O.COMPUTATION 
				    (compile_c_expression expr2)
			      in
			      O.LOOP
				(O.COMPOUND 
				  (None, O.BLOCK 
				    ([], [], [loop_body;label_stmt])))
			  | None -> 
			      O.LOOP
				(O.COMPOUND 
				  (None, O.BLOCK 
				    ([], [], [loop_body])))
		      end
		in
		O.COMPOUND
		  (txt_opt,
		  O.BLOCK ([],[], pre_loop @$ [loop_stmt]))
	      end
		
	  | I.Iteration_statement_4_for (c_declaration, c_expression_opt1, 
	    c_expression_opt2, c_statement) 
	    ->
	      begin
		let txt_opt = camlp4_macro_str_opt_pp_print 
		  (fun fm -> 
		    Format.pp_open_box fm 0;
		    Format.pp_print_string fm "for (";
		    Ast_aa_gram_printer.pp_print_c_declaration fm c_declaration;
		    let _ = match c_expression_opt1 with
		      | Some expr -> 
			  Ast_aa_gram_printer.pp_print_c_expression 
			    fm ~need_paren:false expr;
		      | None -> ()
		    in
		    Format.pp_print_string fm ";";
		    let _ = match c_expression_opt2 with
		      | Some expr -> 
			  Ast_aa_gram_printer.pp_print_c_expression 
			    fm ~need_paren:false expr;
		      | None -> ()
		    in
		    Format.pp_print_string fm ") {...}";
		    Format.pp_close_box fm ()
		  )
		in
		let pre_loop = [compile_c_declaration c_declaration]
		in
		let _ = match c_expression_opt2 with  (** begin **)
		  | Some _ ->
		      Stack.push (new_label (), true, ref false) 
			function_data.continue_target_stack
		  | None ->
		      Stack.push ("", false, ref false) 
			function_data.continue_target_stack
		in
		let loop_body = compile_c_statement c_statement
		in
		let (lb, _, is_used) = 
		  Stack.pop function_data.continue_target_stack (** end **)
		in
		let loop_stmt = match c_expression_opt1 with
		  | Some expr1 -> 
		      begin
			match c_expression_opt2 with
			  | Some expr2 -> 
			      let label_stmt = 
				if !is_used then
				  O.LABEL 
				    (lb, O.COMPUTATION 
				      (compile_c_expression expr2))
				else
				  O.COMPUTATION (compile_c_expression expr2)
			      in
			      O.WHILE 
				(compile_c_expression expr1,
				O.COMPOUND 
				  (txt_opt,
				  O.BLOCK 
				    ([],[], [loop_body;label_stmt])))
			  | None -> 
			      O.LOOP
				(O.COMPOUND
				  (None, O.BLOCK 
				    ([],[], [loop_body])))
		      end
		  | None ->
		      begin
			match c_expression_opt2 with
			  | Some expr2 -> 
			      let label_stmt = 
				if !is_used then
				  O.LABEL 
				    (lb, O.COMPUTATION 
				      (compile_c_expression expr2))
				else
				  O.COMPUTATION 
				    (compile_c_expression expr2)
			      in
			      O.LOOP
				(O.COMPOUND
				  (txt_opt, O.BLOCK 
				    ([],[], [loop_body;label_stmt])))
			  | None -> 
			      O.LOOP
				(O.COMPOUND 
				  (None, O.BLOCK 
				    ([],[], [loop_body])))
		      end
		in
		O.COMPOUND (txt_opt, O.BLOCK ([],pre_loop,[loop_stmt]))
	      end

    and compile_c_jump_statement: I.c_jump_statement -> O.c_stmt010 =
      fun expr ->
	match expr with
	  | I.Jump_statement_1_goto (I.Identifier str) -> 
	      O.GOTO (str)
	  | I.Jump_statement_2_continue ->
	      let function_data = Stack.top function_scope
	      in
	      let (lb, has_target, is_used) = 
		Stack.top function_data.continue_target_stack
	      in
	      if has_target then
		let _ = is_used := true
		in
		O.GOTO (lb)
	      else
		O.CONTINUE
		  
	  | I.Jump_statement_3_break -> O.BREAK 
	  | I.Jump_statement_4_return_expression c_expression ->
	      O.RETURN_VALUE (compile_c_expression c_expression)
		
	  | I.Jump_statement_5_return -> 
	      O.RETURN
		
	  | I.Jump_statement_gnu_goto expr -> 
	      O.GCC_GOTO (compile_c_expression expr)
		

    and compile_c_translation_unit = function
      | I.Translation_unit l ->
	  O.Translation_unit 
	    (Safe_list.map
	      (fun external_declaration -> 
		compile_c_external_declaration external_declaration
	      ) l
	    )
	    
    and compile_c_external_declaration = function
      | I.External_declaration_at (coord, expr) ->
	  O.External_declaration_at (coord, compile_c_external_declaration expr)
	    
      | I.External_declaration_1 (c_function_definition) ->
	  O.External_declaration_1 
	    (compile_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 (c_declaration) ->
	  O.External_declaration_2 
	    (compile_c_declaration c_declaration)

    and compile_c_function_definition = function
      | I.Function_definition 
	  (c_declaration_specifiers,
	  c_declarator,
	  c_declaration_list_opt,
	  c_compound_statement) ->
	  let _ = begin_function ()
	  in
	  let v = O.Function_definition 
	    (compile_c_declaration_specifiers c_declaration_specifiers,
	    compile_c_declarator c_declarator,
	    map_list_opt compile_c_declaration c_declaration_list_opt,
	    compile_c_compound_statement c_compound_statement)
	  in
	  let _ = end_function ()
	  in
	  v
    in
    compile_c_translation_unit c_translation_unit
      
