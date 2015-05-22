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
  
let function_list =
  [ 
    { 
      Type_ast.func_name = Some "va_start";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"];
      Type_ast.formal_param = [Some "arg"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "va_arg";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"; 
	 Type_ast.Primitive_type Type_ast.Int];
      Type_ast.formal_param = [Some "arg"; Some "size"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Long_Int);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "va_end";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"];
      Type_ast.formal_param = [Some "arg"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "__builtin_va_start";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"];
      Type_ast.formal_param = [Some "arg"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "__builtin_va_arg";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"; 
	 Type_ast.Primitive_type Type_ast.Int];
      Type_ast.formal_param = [Some "arg"; Some "size"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Long_Int);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "__builtin_va_end";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Typename "__builtin_va_list"];
      Type_ast.formal_param = [Some "arg"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "unlink";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "filename"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "remove";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "pathname"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    
    { 
      Type_ast.func_name = Some "__builtin_prefetch";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_void_pointer];
      Type_ast.formal_param = [Some "addr";];
      Type_ast.return_type = c_void;
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "__builtin_expect";
      Type_ast.param_type = Type_ast.Param_type_va 
	[c_long;c_long];
      Type_ast.formal_param = [Some "exp"; Some "c"];
      Type_ast.return_type = c_long;
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "__builtin_trap";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_void;
      Type_ast.func_attributes = [];
    };

    (** setjmp - save stack context for non-local goto **)
    { Type_ast.func_name = Some "__builtin_setjmp";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer];
      Type_ast.formal_param = [Some "buf"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    (** longjmp - non-local jump to a saved stack context **)
    { Type_ast.func_name = Some "__builtin_longjmp";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer;c_int];
      Type_ast.formal_param = [Some "env"; Some"val"];
      Type_ast.return_type = c_void;
      Type_ast.func_attributes = [];
    };

    (** fread, fwrite - binary stream input/output **)
    { Type_ast.func_name = Some "__builtin_fread";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer;size_t;size_t;c_FILE];
      Type_ast.formal_param = 
      [Some "ptr";Some "size"; Some "nmemb"; Some "stream"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "__builtin_fwrite";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[const_void_pointer;size_t;size_t;c_FILE];
      Type_ast.formal_param = 
      [Some "ptr";Some "size"; Some "nmemb"; Some "stream"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };
    
    (** double __builtin_huge_val (void) **)
    { Type_ast.func_name = Some "__builtin_huge_val";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    
    (** float __builtin_huge_valf (void) **)
    { Type_ast.func_name = Some "__builtin_huge_valf";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };

    (** long double __builtin_huge_vall (void) **)
    { Type_ast.func_name = Some "__builtin_huge_vall";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };

    (** double __builtin_inf (void) **)
    { Type_ast.func_name = Some "__builtin_inf";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };

    (** float __builtin_inff (void) **)
    { Type_ast.func_name = Some "__builtin_inff";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };

    (** long double __builtin_infl (void) **)
    { Type_ast.func_name = Some "__builtin_infl";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };

    (** double __builtin_nan (const char *str) **)
    { Type_ast.func_name = Some "__builtin_nan";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };

    (** float __builtin_nanf (const char *str) **)
    { Type_ast.func_name = Some "__builtin_nanf";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };

    (** double __builtin_nans (const char *str) **)
    { Type_ast.func_name = Some "__builtin_nans";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };

    (** float __builtin_nansf (const char *str) **)
    { Type_ast.func_name = Some "__builtin_nansf";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };

    (** long double __builtin_nansl (const char *str) **)
    { Type_ast.func_name = Some "__builtin_nansl";
      Type_ast.param_type = Type_ast.Param_type_fix [const_char_pointer];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };

    (** void __builtin_set_thread_pointer (void **str) **)
    { 
      Type_ast.func_name = Some "__builtin_set_thread_pointer";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[Type_ast.Pointer 
	  (Type_ast.Pointer 
	    (Type_ast.Primitive_type (Type_ast.Void)))];
      Type_ast.formal_param = [Some "str"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };
  ]
