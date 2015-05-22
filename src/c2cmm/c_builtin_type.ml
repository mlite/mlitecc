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

module Type_ast = Ast_da_type


let char_pointer = (** const char * **)
  Type_ast.Pointer (Type_ast.Primitive_type Type_ast.Char)
    
let const_char_pointer = (** const char * **)
  Type_ast.Qualified_type 
    (Type_ast.Const, Type_ast.Pointer (Type_ast.Primitive_type Type_ast.Char))
    
let void_pointer = (** "void *" **)
  Type_ast.Pointer (Type_ast.Primitive_type Type_ast.Void)
    
let const_void_pointer = (** "const void *" **)
  Type_ast.Qualified_type
    (Type_ast.Const, Type_ast.Pointer (Type_ast.Primitive_type Type_ast.Void))
    
let size_t = Type_ast.Typeid Mach.csize_t_id


let c_void = Type_ast.Typename "void"
let c_long = Type_ast.Typename "long int"
let c_long_long = Type_ast.Typename "long long int"
let c_int = Type_ast.Typename "int"
let c_double = Type_ast.Typename "double"
let c_float = Type_ast.Typename "float"
let c_long_double = Type_ast.Typename "long double"
let c_FILE = void_pointer
