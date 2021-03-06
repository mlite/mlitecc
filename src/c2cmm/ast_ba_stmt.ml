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

open C_syntax_symbol
  
let version = "ISO/IEC 9899:TC"
  (** 1. Normalized C statements, all for statements are 
      converted to equivalent compound statements.
  **)
let description = 
  ["Normalized C statements, all for-loop statements are";
  "converted to equivalent compound statements."]

let suffix = ".ba.stmt"
  
type coord = Coordinate.t

and gnu_attribute = string * c_expression list

and c_identifier = Identifier of string
  
and c_identifier_as_typ = Identifier_as_typ of string
    
(** A.1.4 Universal character names **)
(** A.1.5 Constants **)

and c_constant = Ast_aa_gram.c_constant

and c_enumeration_constant =
  | Enumeration_constant of c_identifier

and c_string_literal = C_syntax_symbol.c_string_literal
      
(** A.2.1 Expressions **)
and c_primary_expression = 
  | Primary_expression_1 of c_identifier
  | Primary_expression_2 of c_constant
  | Primary_expression_3 of c_string_literal
  | Primary_expression_4 of c_expression
  | Primary_expression_macro_va_start of c_expression * c_expression
  | Primary_expression_macro_va_arg of c_expression * c_type_name
  | Primary_expression_macro_va_end of c_expression
  | Primary_expression_gnu_block of c_compound_stmt010
  | Primary_expression_gnu_labeladdr of c_identifier
  | Primary_expression_builtin_types_compatible of c_type_name * c_type_name
  | Primary_expression_builtin_constant_p of c_expression
  | Primary_expression_builtin_expect of c_expression * c_constant_expression
	
and c_postfix_expression = 
  | Postfix_expression_1 of 
      c_primary_expression
  | Postfix_expression_2 of 
      c_postfix_expression * (*[*) c_expression (*]*)
  | Postfix_expression_3 of 
      c_postfix_expression * (*"("*) c_argument_expression_list option (*")"*)
  | Postfix_expression_4_DOT of 
      c_postfix_expression * (*"."*) c_identifier
  | Postfix_expression_5_ARROW of 
      c_postfix_expression * (*"->"*) c_identifier
  | Postfix_expression_6_PLUS_PLUS of 
      c_postfix_expression 
  | Postfix_expression_7_MINUS_MINUS of 
      c_postfix_expression
  | Postfix_expression_8 of 
      c_type_name * c_initializer_list
  
and c_argument_expression_list =
  | Argument_expression_list of 
      c_assignment_expression list
	
and c_unary_expression = 
  | Unary_expression_1 of 
      c_postfix_expression
  | Unary_expression_2_PLUS_PLUS of 
      c_unary_expression
  | Unary_expression_3_MINUS_MINUS of 
      c_unary_expression
  | Unary_expression_4 of 
      c_unary_operator * c_cast_expression
  | Unary_expression_5_SIZEOF of 
      c_unary_expression
  | Unary_expression_6_SIZEOF of 
      c_type_name
  | Unary_expression_7_ALIGNOF of (* gnu C extension *)
      c_unary_expression
  | Unary_expression_8_ALIGNOF of (* gnu C extension *)
      c_type_name 

and c_unary_operator = C_syntax_symbol.c_unary_operator

and c_cast_expression = 
  | Cast_expression_1 of 
      c_unary_expression
  | Cast_expression_2 of 
      c_type_name * c_cast_expression
	
and c_multiplicative_expression = 
  | Multiplicative_expression_1 of 
      c_cast_expression
  | Multiplicative_expression_2_STAR of 
      c_multiplicative_expression * c_cast_expression
  | Multiplicative_expression_3_SLASH of 
      c_multiplicative_expression * c_cast_expression
  | Multiplicative_expression_4_PERCENT of 
      c_multiplicative_expression * c_cast_expression
	
and c_additive_expression = 
  | Additive_expression_1 of 
      c_multiplicative_expression
  | Additive_expression_2_PLUS of 
      c_additive_expression * c_multiplicative_expression
  | Additive_expression_3_MINUS of 
      c_additive_expression * c_multiplicative_expression

and c_shift_expression = 
  | Shift_expression_1 of 
      c_additive_expression
  | Shift_expression_2_INF_INF of 
      c_shift_expression * c_additive_expression
  | Shift_expression_3_SUP_SUP of 
      c_shift_expression * c_additive_expression
	
and c_relational_expression = 
  | Relational_expression_1 of 
      c_shift_expression
  | Relational_expression_2_INF of 
      c_relational_expression * c_shift_expression
  | Relational_expression_3_SUP of 
      c_relational_expression * c_shift_expression
  | Relational_expression_4_INF_EQ of 
      c_relational_expression * c_shift_expression
  | Relational_expression_5_SUP_EQ of 
      c_relational_expression * c_shift_expression

and c_equality_expression = 
  | Equality_expression_1 of c_relational_expression
  | Equality_expression_2_EQ_EQ of 
      c_equality_expression * c_relational_expression
  | Equality_expression_3_EXCLAM_EQ of 
      c_equality_expression * c_relational_expression

and c_and_expression = 
  | And_expression_1 of 
      c_equality_expression
  | And_expression_2_AND of 
      c_and_expression * c_equality_expression

and c_exclusive_or_expression = 
  | Exclusive_or_expression_1 of 
      c_and_expression
  | Exclusive_or_expression_2_CIRC of 
      c_exclusive_or_expression * c_and_expression
	
and c_inclusive_or_expression =
  | Inclusive_or_expression_1 of 
      c_exclusive_or_expression
  | Inclusive_or_expression_2_PIPE of 
      c_inclusive_or_expression * c_exclusive_or_expression
	
and c_logical_and_expression = 
  | Logical_and_expression_1 of 
      c_inclusive_or_expression
  | Logical_and_expression_2_AND_AND of 
      c_logical_and_expression * c_inclusive_or_expression
	
and c_logical_or_expression = 
  | Logical_or_expression_1 of 
      c_logical_and_expression
  | Logical_or_expression_2_PIPE_PIPE of 
      c_logical_or_expression * c_logical_and_expression

and c_conditional_expression = 
  | Conditional_expression_1 of 
      c_logical_or_expression
  | Conditional_expression_2 of 
      c_logical_or_expression * c_expression * c_conditional_expression
  | Conditional_expression_gnu of 
      c_logical_or_expression * c_conditional_expression
	
and c_assignment_expression = 
  | Assignment_expression_1 of 
      c_conditional_expression
  | Assignment_expression_2 of 
      c_unary_expression * c_assignment_operator * c_assignment_expression

and c_assignment_operator = C_syntax_symbol.c_assignment_operator

and c_expression =
  | Expression_1 of 
      c_assignment_expression
  | Expression_2 of 
      c_expression * c_assignment_expression

and c_constant_expression = 
  | Constant_expression of c_conditional_expression

(** A.2.2 Declarations **)
and c_declaration = 
  | Declaration of c_declaration_specifiers * c_init_declarator list option

and c_declaration_specifiers = 
  | Declaration_specifiers_1 of 
      c_storage_class_specifier * c_declaration_specifiers option
  | Declaration_specifiers_2 of 
      c_type_specifier * c_declaration_specifiers option
  | Declaration_specifiers_3 of 
      c_type_qualifier * c_declaration_specifiers option
  | Declaration_specifiers_4 of 
      c_function_specifier * c_declaration_specifiers option
  | Declaration_specifiers_GNU of 
      gnu_attribute * c_declaration_specifiers option

and c_init_declarator =
  | Init_declarator_1 of 
      c_declarator
  | Init_declarator_2 of 
      c_declarator * c_initializer

and c_storage_class_specifier = C_syntax_symbol.c_storage_class_specifier

and c_type_specifier =
  | Type_builtin of c_builtin_type_specifier
  | Type_specifier_STRUCT_OR_UNION of c_struct_or_union_specifier
  | Type_specifier_ENUM of c_enum_specifier
  | Type_specifier_TYPENAME of c_typedef_name
  | Type_specifier_GCC_TYPEOF_E of c_expression
  | Type_specifier_GCC_TYPEOF_T of c_type_name
      (*
  | Type_specifier_STRUCT_OR_UNION_GNU of 
      c_struct_or_union_specifier * gnu_attribute list
  | Type_specifier_ENUM_GNU of 
      c_enum_specifier * gnu_attribute list
      *)

and c_struct_or_union_specifier =
  | Struct_or_union_specifier_1 of 
      c_struct_or_union * c_identifier option * c_struct_declaration list
  | Struct_or_union_specifier_2 of 
      c_struct_or_union * c_identifier
	(*
  | Struct_or_union_specifier_GNU of 
      c_struct_or_union * c_identifier option * c_struct_declaration list *
	gnu_attribute list
	*)

and c_struct_or_union =  C_syntax_symbol.c_struct_or_union

and c_struct_declaration = 
  | Struct_declaration of 
      c_specifier_qualifier_list * c_struct_declarator list

and c_specifier_qualifier_list = 
  | Specifier_qualifier_list_1 of 
      c_type_specifier * c_specifier_qualifier_list option
  | Specifier_qualifier_list_2 of 
      c_type_qualifier * c_specifier_qualifier_list option
  | Specifier_qualifier_list_GNU of
      gnu_attribute * c_specifier_qualifier_list option
	
and c_struct_declarator = 
  | Struct_declarator_1 of 
      c_declarator
  | Struct_declarator_2 of 
      c_declarator option * c_constant_expression
  | Struct_declarator_GNU of c_struct_declarator * gnu_attribute list

and c_enum_specifier = 
  | Enum_specifier_1 of 
      c_identifier option * c_enumerator list
  | Enum_specifier_2 of c_identifier
      (*
  | Enum_specifier_GNU of 
      c_identifier option * c_enumerator list * gnu_attribute list
      *)

and c_enumerator = 
  | Enumerator_1 of 
      c_enumeration_constant
  | Enumerator_2 of 
      c_enumeration_constant * c_constant_expression

and c_type_qualifier = C_syntax_symbol.c_type_qualifier

and c_function_specifier = C_syntax_symbol.c_function_specifier

and c_declarator = 
  | Declarator of c_pointer option * c_direct_declarator
  | Declarator_GNU of c_declarator * gnu_attribute list

and c_direct_declarator =
  | Direct_declarator_1 of 
      c_identifier
  | Direct_declarator_2 of 
      c_declarator 
  | Direct_declarator_3 of 
      c_direct_declarator * c_type_qualifier list option * 
	c_assignment_expression option
  | Direct_declarator_4_STATIC of 
      c_direct_declarator * c_type_qualifier list option * 
	c_assignment_expression
  | Direct_declarator_5_STATIC of 
      c_direct_declarator * c_type_qualifier list * 
	c_assignment_expression
  | Direct_declarator_6_STAR of 
      c_direct_declarator * c_type_qualifier list option 
  | Direct_declarator_7 of 
      c_direct_declarator * c_parameter_type_list
  | Direct_declarator_8 of 
      c_direct_declarator * c_identifier list option

and c_pointer = 
  | Pointer_1 of 
      c_type_qualifier list option
  | Pointer_2 of 
      c_type_qualifier list option * c_pointer

and c_parameter_type_list = 
  | Parameter_type_list_FIX of c_parameter_list
  | Parameter_type_list_VAR of c_parameter_list
	
and c_parameter_list = 
  | Parameter_list of c_parameter_declaration list
	
and c_parameter_declaration =
  | Parameter_declaration_1 of 
      c_declaration_specifiers * c_declarator
  | Parameter_declaration_2 of 
      c_declaration_specifiers * c_abstract_declarator option

and c_type_name = 
  | Type_name of 
      c_specifier_qualifier_list * c_abstract_declarator option

and c_abstract_declarator = 
  | Abstract_declarator_1 of 
      c_pointer
  | Abstract_declarator_2 of 
      c_pointer option * c_direct_abstract_declarator

and c_direct_abstract_declarator =
  | Direct_abstract_declarator_error
  | Direct_abstract_declarator_1 of 
      c_abstract_declarator
  | Direct_abstract_declarator_2 of 
      c_direct_abstract_declarator option * c_assignment_expression option
  | Direct_abstract_declarator_3_STAR of 
      c_direct_abstract_declarator option 
  | Direct_abstract_declarator_4 of 
      c_direct_abstract_declarator option * c_parameter_type_list option

and c_typedef_name = 
  | Typedef_name of c_identifier_as_typ

and c_initializer =
  | Initializer_1 of c_assignment_expression
  | Initializer_2 of c_initializer_list
	
and c_initializer_list = 
  | Initializer_list of (c_designation option * c_initializer) list

and c_designation = 
  | Designation of c_designator list 

and c_designator = 
  | Designator_1 of c_constant_expression
  | Designator_2 of c_identifier
  | Designator_gnu_range of c_constant_expression * c_constant_expression

(** A.2.3 Statements **)
and asm_details =
    { 
      (* optional name, constraints and expressions for outputs *)
      asm_outputs: (string option * string * c_expression) list;
      (* optional name, constraints and expressions for inputs *)
      asm_inputs: (string option * string * c_expression) list; 
      asm_clobbers: string list (* clobbered registers *)
    }

and c_stmt010 = 
  | STMT_AT of coord * c_stmt010
  | NOP
  | COMPUTATION of c_expression 
  | COMPOUND of string option * c_compound_stmt010
  | SEQUENCE of string option * c_stmt010 list
  | IF of c_expression * c_stmt010 * c_stmt010
  | WHILE of c_expression * c_stmt010 
  | LOOP of c_stmt010
  | BREAK 
  | CONTINUE 
  | RETURN_VALUE of c_expression
  | RETURN
  | SWITCH of c_expression * c_stmt010
  | CASE of c_constant_expression * c_stmt010
  | CASE_RANGE of c_constant_expression * c_constant_expression * c_stmt010
  | DEFAULT of c_stmt010
  | LABEL of string * c_stmt010
  | GOTO of string
  | GCC_GOTO of c_expression
  | ASM of string list * asm_details option
      
and c_compound_stmt010 = BLOCK of string list * c_declaration list * c_stmt010 list
  
(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of 
      c_external_declaration list
	
and c_external_declaration =
  | External_declaration_at of
      coord * c_external_declaration 
  | External_declaration_1 of c_function_definition 
  | External_declaration_2 of c_declaration 
  
and c_function_definition =
  | Function_definition of 
      c_declaration_specifiers * c_declarator * 
	c_declaration list option * c_compound_stmt010

(** A.3 Preprocessing c_directives **)
and c_file = c_translation_unit
