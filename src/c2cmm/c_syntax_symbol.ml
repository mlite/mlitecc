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

type c_integer_suffix = 
  | DEC_NONE
  | DEC_U
  | DEC_L
  | DEC_UL
  | DEC_LL
  | DEC_ULL
  | HEX_NONE
  | HEX_U
  | HEX_L
  | HEX_UL
  | HEX_LL
  | HEX_ULL
  | OCT_NONE
  | OCT_U
  | OCT_L
  | OCT_UL
  | OCT_LL
  | OCT_ULL
      
and c_unary_operator =
  | Unary_operator_AND (** "&" **)
  | Unary_operator_STAR (** "*" **)
  | Unary_operator_PLUS (** "+" **)
  | Unary_operator_MINUS (** "-" **)
  | Unary_operator_TILDE (** "~" **)
  | Unary_operator_EXCLAM (** "!" **)
      
and c_assignment_operator = 
  | Assignment_operator_EQ 
  | Assignment_operator_STAR_EQ
  | Assignment_operator_SLASH_EQ
  | Assignment_operator_PERCENT_EQ
  | Assignment_operator_PLUS_EQ 
  | Assignment_operator_MINUS_EQ
  | Assignment_operator_INF_INF_EQ 
  | Assignment_operator_SUP_SUP_EQ 
  | Assignment_operator_AND_EQ 
  | Assignment_operator_CIRC_EQ
  | Assignment_operator_PIPE_EQ

and c_storage_class_specifier = 
  | Storage_class_specifier_TYPEDEF 
  | Storage_class_specifier_EXTERN 
  | Storage_class_specifier_STATIC 
  | Storage_class_specifier_AUTO 
  | Storage_class_specifier_REGISTER 
  | Storage_class_specifier_THREAD (* GCC __thread specifier may be used alone, 
				      with the extern or static specifiers, but 
				      with no other storage class specifier. 
				      When used with extern or static, __thread 
				      must appear immediately after the other 
				      storage class specifier.
				      
				      The __thread specifier may be applied to 
				      any global, file-scoped static,
				      function-scoped static, or static data
				      member of a class. It may not be applied 
				      to block-scoped automatic or non-static 
				      data member. 
				   *)

and c_struct_or_union = 
  | Struct_or_union_STRUCT
  | Struct_or_union_UNION
      
and c_type_qualifier = 
  | Type_qualifier_CONST 
  | Type_qualifier_RESTRICT
  | Type_qualifier_VOLATILE
      
and c_function_specifier = 
  | Function_specifier_INLINE

and c_builtin_type_specifier =
  | Type_specifier_VOID
  | Type_specifier_CHAR
  | Type_specifier_SHORT
  | Type_specifier_INT
  | Type_specifier_LONG
  | Type_specifier_FLOAT
  | Type_specifier_DOUBLE
  | Type_specifier_SIGNED
  | Type_specifier_UNSIGNED
  | Type_specifier_BOOL
  | Type_specifier_COMPLEX

(** A.1.5 String literals **)
and c_string_literal = 
  | String_literal of string
  | WString_literal of int64 list (* extension *)

(** 6.4.4.1 integer-constant **)
and c_integer_constant =
  | Constant of string * c_integer_suffix	
