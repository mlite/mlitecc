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
open C_builtin_type
  
(* ISO C90 functions *)
let std_function_table =
  [ { Type_ast.func_name = Some "printf";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer];
      Type_ast.formal_param = [Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
  ]
