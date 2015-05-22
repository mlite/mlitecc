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

let version = "ISO/IEC 9899:TC"
(** 1. Normalized C statements, all for statements are 
       converted to equivalent compound statements.
    2. C expressions are flatten.
**)
let description = ["Normalized C statements, all for statements are";
		   "converted to equivalent compound statements.";
		   "C expressions are flatten.";
		   "C designation are flatten.";
		 ]

let suffix = ".ca.expr"

    
(** A.1.3 Identifiers **)
type coord = Coordinate.t

and gnu_attribute = string * c_expression list

and c_identifier = string 

and c_identifier_as_typ = string
    
(** A.1.4 Universal character names **)
(** A.1.5 Constants **)
and c_constant = (* Ast_ba_stmt.c_constant *)
  | Constant_value of int * Const_folding.cval_ext
  | Constant_enumeration of c_enumeration_constant

and c_enumeration_constant = string

(** A.1.5 String literals **)
and c_string_literal = C_syntax_symbol.c_string_literal

(** A.2.1 Expressions **)
and binary_arithmatic = C_semantics_symbol.binary_arithmatic
      
and binary_predicate = C_semantics_symbol.binary_predicate

and binary_logic_connect = C_semantics_symbol.binary_logic_connect
      
and unary_arithmatic = C_semantics_symbol.unary_arithmatic

and c_expr020 = 
  | Macro_va_start of c_expr020 * c_expr020
  | Macro_va_arg of c_expr020 * c_type_name
  | Macro_va_end of c_expr020 
  | Builtin_types_compatible of c_type_name * c_type_name
  | Builtin_constant_p of c_expr020
  | Builtin_expect of c_expr020 * c_constant_expression
  | Gnu_block of c_compound_stmt010
  | Comma of c_expr020 * c_expr020
  | Constant of c_constant
  | String of c_string_literal
  | Call of c_expr020 * c_expr020 list
  | Variable of string
  | Gnu_labeladdr of string
  | Memberof of c_expr020 * string
  | Memberof_ptr of c_expr020 * string
  | Indexof of c_expr020 * c_expr020
  | Binary_arithm of binary_arithmatic * c_expr020 * c_expr020
  | Binary_predicate of binary_predicate * c_expr020 * c_expr020
  | Binary_logic of binary_logic_connect * c_expr020 * c_expr020
  | Unary_arithm of unary_arithmatic * c_expr020
  | Logic_not of c_expr020
  | Sizeof_expr of c_expr020
  | Sizeof_type of c_type_name
  | Alignof_expr of c_expr020
  | Alignof_type of c_type_name
  | Cast of c_type_name * c_expr020
  | Assign of c_expr020 * c_expr020
  | Post_decr of c_expr020
  | Post_incr of c_expr020
  | Pre_decr of c_expr020
  | Pre_incr of c_expr020
  | Assign_arithm of binary_arithmatic * c_expr020 * c_expr020
  | Cast_init of c_type_name * c_initializer_list
  | Memof of c_expr020
  | Addrof of c_expr020
  | Question of c_expr020 * c_expr020 option * c_expr020 
                (** should only be used to initialize variables **)

and c_argument_expression_list = c_expr020 list

and c_expression = c_expr020

and c_assignment_expression = c_expr020

and c_constant_expression = c_expr020

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
  | Type_builtin of C_syntax_symbol.c_builtin_type_specifier
  | Type_specifier_STRUCT_OR_UNION of 
      c_struct_or_union_specifier
  | Type_specifier_ENUM of 
      c_enum_specifier
  | Type_specifier_TYPENAME of 
      c_typedef_name
  | Type_specifier_GCC_TYPEOF_E of c_expr020
  | Type_specifier_GCC_TYPEOF_T of c_type_name

and c_struct_or_union_specifier =
  | Struct_or_union_specifier_1 of 
      c_struct_or_union * c_identifier option * c_struct_declaration list
  | Struct_or_union_specifier_2 of 
      c_struct_or_union * c_identifier
	
and c_struct_or_union = C_syntax_symbol.c_struct_or_union

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

(** (6.7.8) **)
and c_initializer =
  | Initializer_1 of c_assignment_expression
  | Initializer_2 of c_initializer_list
	
and c_initializer_list = (c_designator list * c_initializer) list

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
  | SEQUENCE of string option * c_stmt010 list
  | COMPOUND of string option * c_compound_stmt010
  | IF of c_expression * c_stmt010 * c_stmt010
  | WHILE of c_expression * c_stmt010
  | LOOP of c_stmt010
  | BREAK
  | CONTINUE
  | RETURN_VALUE of c_expression
  | RETURN
  | SWITCH of c_expression * c_stmt010
  | CASE of c_expression * c_stmt010
  | CASE_RANGE of c_expression * c_expression * c_stmt010
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
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration
	
and c_function_definition =
  | Function_definition of 
      c_declaration_specifiers * c_declarator * 
	c_declaration list option * c_compound_stmt010

(** A.3 Preprocessing c_directives **)
and c_file = c_translation_unit


