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

open Csize
module QN = Qual_name
module QNP = Qual_name_printer

type primitive =
  | Unknown
  | Void 
  | Void_Ptr 
  | Char
  | Signed_Char
  | Unsigned_Char
  | Short
  | Unsigned_Short
  | Int
  | Unsigned_Int
  | Long
  | Unsigned_Long
  | Long_Long
  | Unsigned_Long_Long
  | Float
  | Double
  | Long_Double
  | Bool
  | Complex
  | Float_Complex
  | Double_Complex
  | Long_Double_Complex
  | Va_List

and type_qualifier = 
  | Const
  | Restrict
  | Volatile

and enum_item = QN.t * int
    
and field = 
  | Named of c_type * QN.t * csize
  | Unamed of c_type * csize

and param =
  | Labeled of c_type * QN.t
  | Unlabeled of c_type

and function_type = 
  | Fixed of param list * param
  | Varadic of param list * param
    
and c_type_decl = 
  | Struct_decl of QN.t * csize * csize * field list
  | Union_decl of QN.t * csize * csize * field list
  | Enum_decl of QN.t * enum_item list
  | Typedef of c_type * QN.t
      
and c_type =
  | Bits of c_type * csize * csize * string
  | Primitive of primitive
  | Pointer of c_type 
  | Array of c_type * csize
  | Xarray of c_type 
  | Darray of c_type 
  | Struct of QN.t
  | Union of QN.t
  | Enum of QN.t
  | Function of function_type 
  | Typename of QN.t
  | Qual of c_type * type_qualifier
  | Type_decl of c_type_decl
  | Primary of c_type
