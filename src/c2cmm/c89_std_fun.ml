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
  
(* ISO C89 functions *)
let std_function_table =
  [ (** _exit, _Exit - terminate the current process **)
    { Type_ast.func_name = Some "_exit";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "int"];
      Type_ast.formal_param = [Some "status"];
      Type_ast.return_type = Type_ast.Typename "void";
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "exit";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "int"];
      Type_ast.formal_param = [Some "status"];
      Type_ast.return_type = Type_ast.Typename "void";
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "alloca";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[size_t]; 
      Type_ast.formal_param = [Some "size"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "bcmp";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_void_pointer;const_void_pointer;size_t]; 
      Type_ast.formal_param = [Some "s1"; Some "s2";Some "size"];
      Type_ast.return_type = Type_ast.Typename "int";
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "bzero";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_void_pointer;size_t]; 
      Type_ast.formal_param = [Some "s";Some "size"];
      Type_ast.return_type = Type_ast.Typename "void";
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "dcgettext";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;const_char_pointer;Type_ast.Typename "int"]; 
      Type_ast.formal_param = [Some "domainname";Some "msgid";Some "category"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "dgettext";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;const_char_pointer;]; 
      Type_ast.formal_param = [Some "domainname";Some "msgid"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "drem";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_double;c_double]; 
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "dremf";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_float;c_float]; 
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "dreml";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long_double;c_long_double]; 
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "expm1";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_double]; 
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "expm1f";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_float]; 
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "expm1l";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long_double]; 
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };

    { Type_ast.func_name = Some "printf_unlocked";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer];
      Type_ast.formal_param = [Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "fprintf_unlocked";
      Type_ast.param_type = Type_ast.Param_type_va 
	[c_FILE;const_char_pointer];
      Type_ast.formal_param = [Some "stream";Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    { Type_ast.func_name = Some "fputs_unlocked";
      Type_ast.param_type = Type_ast.Param_type_fix
	[const_char_pointer;c_FILE];
      Type_ast.formal_param = [Some "s";Some "stream"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    (** index, rindex - locate character in string **)
    { Type_ast.func_name = Some "index";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;c_int]; 
      Type_ast.formal_param = [Some "s";Some "c"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "rindex";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;c_int]; 
      Type_ast.formal_param = [Some "s";Some "c"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };

    (** strdup, strndup, strdupa, strndupa - duplicate a string **)
    { Type_ast.func_name = Some "strdup";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer];
      Type_ast.formal_param = [Some "s";];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strndup";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;size_t];
      Type_ast.formal_param = [Some "s";Some "n"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strdupa";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer];
      Type_ast.formal_param = [Some "s"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strndupa";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_char_pointer;size_t];
      Type_ast.formal_param = [Some "s";Some "n"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };

    (** stpcpy - copy a string returning a pointer to its end **)
    { Type_ast.func_name = Some "stpcpy";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "dest"; Some "src";];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };

    (** stpncpy - copy a fixed-size string, returning a pointer to its end **)
    { Type_ast.func_name = Some "stpncpy";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[char_pointer;const_char_pointer;size_t];
      Type_ast.formal_param = [Some "dest"; Some "src";Some "n"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
  ]
