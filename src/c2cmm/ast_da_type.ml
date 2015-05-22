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
    3. Declaration & declarator are combined such that
       each declaration only declare one identifier. 
       Type declaration and object declaration are separeted.
**)
let description = ["Normalized C statements, all for statements are";
		   "converted to equivalent compound statements.";
		   "C expressions are flatten.";
		   "Declaration & declarator are combined such that";
		   "each declaration only declare one identifier.";
		   "Type declaration and object declarations are seperated, e.g.";
		   "struct type0 { ... } v0, v1 [] = ...;";
		   "is translated to";
		   "struct type0 {...};";
		   "struct type0 v0;";
		   "struct type0 v1 [] = ...;";
		 ]

let suffix = ".da.type"

    
(** A.1.3 Identifiers **)
type coord = Coordinate.t

and gnu_attribute = string * c_expression list

and c_identifier = string 

(** A.1.4 Universal character names **)
(** A.1.5 Constants **)
and c_constant = Ast_ca_expr.c_constant

(** A.1.5 String literals **)
and c_string_literal = C_syntax_symbol.c_string_literal

(** A.2.1 Expressions **)
and binary_arithmatic = C_semantics_symbol.binary_arithmatic

and binary_predicate = C_semantics_symbol.binary_predicate

and binary_logic_connect = C_semantics_symbol.binary_logic_connect
      
and unary_arithmatic = C_semantics_symbol.unary_arithmatic

and c_expr020 =
  | Comma of c_expr020 * c_expr020
  | Constant of c_constant
  | String of c_string_literal
  | Call of c_expr020 * c_expr020 list
  | Macro_va_start of c_expr020 * c_expr020
  | Macro_va_arg of c_expr020 * c_type
  | Macro_va_end of c_expr020
  | Builtin_types_compatible of c_type * c_type
  | Builtin_constant_p of c_expr020
  | Builtin_expect of c_expr020 * c_constant_expression
  | Gnu_block of c_compound_stmt010
  | Gnu_labeladdr of string
  | Variable of string
  | Memberof of c_expr020 * string
  | Memberof_ptr of c_expr020 * string
  | Indexof of c_expr020 * c_expr020
  | Binary_arithm of binary_arithmatic * c_expr020 * c_expr020
  | Binary_predicate of binary_predicate * c_expr020 * c_expr020
  | Binary_logic of binary_logic_connect * c_expr020 * c_expr020
  | Unary_arithm of unary_arithmatic * c_expr020
  | Logic_not of c_expr020
  | Sizeof_expr of c_expr020
  | Sizeof_type of c_type * bool
  | Alignof_expr of c_expr020
  | Alignof_type of c_type * bool
  | Cast of c_type * c_expr020
  | Assign of c_expr020 * c_expr020
  | Post_decr of c_expr020
  | Post_incr of c_expr020
  | Pre_decr of c_expr020
  | Pre_incr of c_expr020
  | Assign_arithm of binary_arithmatic * c_expr020 * c_expr020
  | Cast_init of c_type * c_initializer_list
  | Memof of c_expr020
  | Addrof of c_expr020
  | Question of c_expr020 * c_expr020 option * c_expr020

and c_argument_expression_list = c_expr020 list

and c_expression = c_expr020

and c_assignment_expression = c_expr020

and c_constant_expression = c_expr020

(** A.2.2 Declarations **)

and primitive_type = 
  | Void
  | Void_ptr
  | Char
  | Short_Int
  | Int
  | Default_int
  | Long_Int
  | Long_Long_Int
  | Float
  | Double
  | Bool
  | Complex
  | Signed_Char
  | Signed_Short_Int
  | Signed_Int
  | Signed_Long_Int
  | Signed_Long_Long_Int
  | Unsigned_Char
  | Unsigned_Short_Int
  | Unsigned_Int
  | Unsigned_Long_Int
  | Unsigned_Long_Long_Int
  | Long_Double
  | Float_Complex
  | Double_Complex
  | Long_Double_Complex
  | Signed
  | Unsigned
  | WChar
      
and type_qualifier = 
  | Const
  | Restrict
  | Volatile
      
and field = 
  | Regular of c_type * string
  | Bits of c_type * c_expr020 * string option
      
and enum_item = string * c_expr020 option
    
and param_type = 
  | Param_type_fix of c_type list
  | Param_type_va of c_type list
  | Param_list of string list
      
and abstract_fun_type = c_type * param_type

and function_type = 
    {
      func_name: string option;
      param_type: param_type;
      formal_param: string option list;
      return_type: c_type;
      func_attributes: gnu_attribute list;
    }
       
and c_type =
  | GCC_attribute_type of c_type * gnu_attribute list
  | Primitive_type of primitive_type
  | Pointer of c_type
  | Array of c_type * c_expr020
  | Xarray of c_type
  | Struct_type of string * field list * struct_layout
  | Struct_type_name of string
  | Union_type of string * field list 
  | Union_type_name of string
  | Enum_type of string * enum_item list
  | Enum_type_name of string 
  | Function_type of function_type
  | Function_type_ex of function_type * (c_type * string) list 
  | Typename of string
  | Typeof of c_expr020
  | Typeid of int
  | Qualified_type of type_qualifier * c_type
  | Incomplete_qualified_type of c_type option * type_qualifier
  | Incomplete_gnu_attribute_type of c_type option * gnu_attribute list

and struct_layout =
  | C_struct
  | GCC_struct
  | MS_struct
      
and linkage = 
  | Default_extern
  | Extern
  | Default
  | Extern_Inline
  | Auto
  | Static
  | Static_Inline
  | Register
  | Inline
  | Type_alias
  | Thread
  | Extern_Thread
  | Static_Thread

and c_declaration = 
  | Obj_decl of linkage * c_type * string
  | Obj_decl_init of linkage * c_type * string * c_initializer
  | Typedef of c_type * string (* typedef int myint; *)
  | Type_decl of c_type
  | Type_only of c_type

and c_declaration_ex = c_declaration * bool ref

(** (6.7.8) **)
and c_initializer =
  | Initializer_1 of c_assignment_expression
  | Initializer_2 of (c_designator list * c_initializer) list 
      
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

and c_compound_stmt010 = 
    BLOCK of string list * c_declaration_list * c_stmt010 list
    
(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of c_external_declaration list

and c_external_declaration =
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition 
  | External_declaration_2 of c_declaration_ex list
  | External_declaration_string of c_type * c_identifier * c_string_literal

and c_function_definition =
  | Function_definition of 
      linkage * c_type * string * c_compound_stmt010

and c_declaration_list = 
    c_declaration_ex list 

and c_file = c_translation_unit 

