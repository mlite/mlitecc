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

type binary_arithmatic = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Band
  | Bor
  | Bxor
  | Shl
  | Shr
      
and binary_predicate = 
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

and binary_logic_connect = 
  | And
  | Or
      
and unary_arithmatic =
  | Neg  (** "-" operator. *)
  | Bnot (** "~" operator. *)

and 'a primitive_type = 
  | Void
  | Void_ptr
  | Typed_ptr of 'a
  | Char
  | Short_Int
  | Int
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
      
and type_qualifier = 
  | Const
  | Restrict
  | Volatile

and 'a c_basic_type = (* c data type which has *)
  | Qualified_type of (type_qualifier * 'a c_basic_type)
  | Primitive_type of 'a primitive_type
      
and linkage = 
  | Default_extern
  | Extern
  | Default
  | Extern_Inline
  | Auto
  | Static
  | Register
  | Inline
  | Type_alias
