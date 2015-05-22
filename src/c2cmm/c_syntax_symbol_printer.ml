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

open Format
open C_syntax_symbol

let print_c_integer_suffix fm v = 
  let str = match v with
    | DEC_NONE
    | OCT_NONE
    | HEX_NONE -> ""
    | DEC_U
    | HEX_U
    | OCT_U -> "U"
    | DEC_L
    | HEX_L 
    | OCT_L -> "L"
	
    | DEC_UL
    | OCT_UL
    | HEX_UL -> "UL"
	

    | DEC_LL
    | HEX_LL
    | OCT_LL -> "LL"
	
    | DEC_ULL
    | HEX_ULL
    | OCT_ULL -> "ULL"
  in
  pp_print_string fm str
    
let print_c_unary_operator fm v =
  let str = match v with
  | Unary_operator_AND -> "&"
  | Unary_operator_STAR -> "*"
  | Unary_operator_PLUS -> "+"
  | Unary_operator_MINUS -> "-"
  | Unary_operator_TILDE -> "~"
  | Unary_operator_EXCLAM -> "!"
  in
  pp_print_string fm str
      
let print_c_assignment_operator fm v = 
  let str = match v with
  | Assignment_operator_EQ -> "="
  | Assignment_operator_STAR_EQ -> "*="
  | Assignment_operator_SLASH_EQ -> "/="
  | Assignment_operator_PERCENT_EQ -> "%="
  | Assignment_operator_PLUS_EQ -> "+="
  | Assignment_operator_MINUS_EQ -> "-="
  | Assignment_operator_INF_INF_EQ -> "<<="
  | Assignment_operator_SUP_SUP_EQ -> ">>="
  | Assignment_operator_AND_EQ -> "&="
  | Assignment_operator_CIRC_EQ -> "^="
  | Assignment_operator_PIPE_EQ -> "|="
  in
  pp_print_string fm str

let print_c_storage_class_specifier fm v =
  let str = match v with
  | Storage_class_specifier_TYPEDEF -> "typedef"
  | Storage_class_specifier_EXTERN -> "extern"
  | Storage_class_specifier_STATIC -> "static"
  | Storage_class_specifier_AUTO -> "auto"
  | Storage_class_specifier_REGISTER -> "register"
  | Storage_class_specifier_THREAD -> "__thread"
  in
  pp_print_string fm str


let print_c_struct_or_union fm v = 
  let str = match v with
  | Struct_or_union_STRUCT -> "struct"
  | Struct_or_union_UNION -> "union"
  in
  pp_print_string fm str
      
let print_c_type_qualifier fm v =
  let str = match v with
  | Type_qualifier_CONST -> "const"
  | Type_qualifier_RESTRICT -> "restrict"
  | Type_qualifier_VOLATILE -> "volatile"
  in
  pp_print_string fm str

      
let print_c_function_specifier fm v = 
  let str = match v with
  | Function_specifier_INLINE -> "__inline"
  in
  pp_print_string fm str

let print_c_builtin_type_specifier fm v =
  match v with
  | Type_specifier_VOID -> pp_print_string fm "void"
  | Type_specifier_CHAR -> pp_print_string fm "char"
  | Type_specifier_SHORT -> pp_print_string fm "short"
  | Type_specifier_INT -> pp_print_string fm "int"
  | Type_specifier_LONG -> pp_print_string fm "long"
  | Type_specifier_FLOAT -> pp_print_string fm "float"
  | Type_specifier_DOUBLE -> pp_print_string fm "double"
  | Type_specifier_SIGNED -> pp_print_string fm "signed"
  | Type_specifier_UNSIGNED -> pp_print_string fm "unsigned"
  | Type_specifier_BOOL -> pp_print_string fm "_Bool"
  | Type_specifier_COMPLEX -> pp_print_string fm "_Complex"
	
let print_c_string_literal fm v =
  match v with
    | String_literal s ->
      pp_print_string fm ("\"" ^ (C_str.escape_string s) ^ "\"")
    | WString_literal int64_list ->
      pp_print_string fm ("L\"" ^ (C_str.escape_wstring int64_list) ^ "\"")


and print_c_integer_constant fm = function
  | Constant (str, suffix) ->
      begin
	pp_print_string fm str;
	print_c_integer_suffix fm suffix
      end

let pp_print_big_int fm v =
  let string = Big_int.string_of_big_int v
  in
  pp_print_string fm string

    
let print_big_int v = 
  let str = camlp4_macro_str_pp_print
	(fun fm ->
	  pp_open_box fm 0;
	  pp_print_big_int fm v;
	  pp_close_box fm ())
  in
  print_string str
    
