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
  [ 
    (** abort - cause abnormal program termination **)
    { Type_ast.func_name = Some "abort";
      Type_ast.param_type = Type_ast.Param_type_fix [];
      Type_ast.formal_param = [];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    
    (** abs, labs, llabs, imaxabs - compute the absolute value of an integer **)
    { Type_ast.func_name = Some "abs";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_int];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "labs";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "llabs";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long_long];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long_long;
      Type_ast.func_attributes = [];
    };

    (** exp, expf, expl - base-e exponential function **)
    { Type_ast.func_name = Some "exp";
      Type_ast.param_type = Type_ast.Param_type_fix [c_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "expf";
      Type_ast.param_type = Type_ast.Param_type_fix [c_float];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "expl";
      Type_ast.param_type = Type_ast.Param_type_fix [c_long_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };

    (** calloc, malloc, free, realloc - Allocate and free dynamic memory **)
    { Type_ast.func_name = Some "calloc";
      Type_ast.param_type = Type_ast.Param_type_fix [size_t;size_t];
      Type_ast.formal_param = [Some "nmemb"; Some "size"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "malloc";
      Type_ast.param_type = Type_ast.Param_type_fix [size_t];
      Type_ast.formal_param = [Some "size"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "free";
      Type_ast.param_type = Type_ast.Param_type_fix [void_pointer];
      Type_ast.formal_param = [Some "pathname"];
      Type_ast.return_type = Type_ast.Primitive_type (Type_ast.Void);
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "realloc";
      Type_ast.param_type = Type_ast.Param_type_fix [void_pointer;size_t];
      Type_ast.formal_param = [Some "ptr";Some "size"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    (** isalnum,  isalpha,  isascii, isblank, iscntrl, isdigit, isgraph, 
       islower, isprint, ispunct, isspace, isupper, isxdigit 
       - character classification routines **)
    { Type_ast.func_name = Some "isalnum";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isalpha";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isascii";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isblank";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "iscntrl";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isdigit";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isgraph";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "islower";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isprint";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "ispunct";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isspace";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isupper";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "isxdigit";
      Type_ast.param_type = Type_ast.Param_type_fix [c_int];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    (** log, logf, logl - natural logarithmic function **)
    { Type_ast.func_name = Some "log";
      Type_ast.param_type = Type_ast.Param_type_fix [c_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "logf";
      Type_ast.param_type = Type_ast.Param_type_fix [c_float];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "logl";
      Type_ast.param_type = Type_ast.Param_type_fix [c_long_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };
    
    (** printf, fprintf, sprintf, snprintf, vprintf, vfprintf, vsprintf, 
       vsnprintf - formatted output conversion **)
    { Type_ast.func_name = Some "printf";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer];
      Type_ast.formal_param = [Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "fprintf";
      Type_ast.param_type = Type_ast.Param_type_va 
	[c_FILE;const_char_pointer];
      Type_ast.formal_param = [Some "stream";Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "sprintf";
      Type_ast.param_type = Type_ast.Param_type_va 
	[char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "str";Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "snprintf";
      Type_ast.param_type = Type_ast.Param_type_va 
	[char_pointer;size_t;const_char_pointer];
      Type_ast.formal_param = [Some "str";Some "size";Some "fmt"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    (** fputc, fputs, putc, putchar, puts - output of characters and strings **)
    { Type_ast.func_name = Some "fputc";
      Type_ast.param_type = Type_ast.Param_type_fix
	[c_int;c_FILE];
      Type_ast.formal_param = [Some "c";Some "stream"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "fputs";
      Type_ast.param_type = Type_ast.Param_type_fix
	[const_char_pointer;c_FILE];
      Type_ast.formal_param = [Some "s";Some "stream"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "putc";
      Type_ast.param_type = Type_ast.Param_type_fix
	[c_int;c_FILE];
      Type_ast.formal_param = [Some "c";Some "stream"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "putchar";
      Type_ast.param_type = Type_ast.Param_type_fix
	[c_int;];
      Type_ast.formal_param = [Some "c"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "puts";
      Type_ast.param_type = Type_ast.Param_type_fix
	[const_char_pointer];
      Type_ast.formal_param = [Some "s"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };

    
    (** memcmp - compare memory areas **)
    { Type_ast.func_name = Some "memcmp";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer; const_void_pointer;size_t];
      Type_ast.formal_param = [Some "dest"; Some "src"; Some "n"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    
    (** memcpy - copy memory area **)
    { Type_ast.func_name = Some "memcpy";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer; const_void_pointer;size_t];
      Type_ast.formal_param = [Some "dest"; Some "src"; Some "n"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    
    (** memset - fill memory with a constant byte **)
    { Type_ast.func_name = Some "memset";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[void_pointer; Type_ast.Primitive_type (Type_ast.Int);size_t];
      Type_ast.formal_param = [Some "dest"; Some "src"; Some "n"];
      Type_ast.return_type = void_pointer;
      Type_ast.func_attributes = [];
    };
    
    (** sqrt, sqrtf, sqrtl - square root function **)
    { Type_ast.func_name = Some "sqrt";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "sqrtf";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_float];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "sqrtl";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long_double];
      Type_ast.formal_param = [Some "x"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };


    (** atan2, atan2f, atan2l - arc tangent function of two variables **)
    { 
      Type_ast.func_name = Some "atan2";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_double;c_double];
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_double;
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "atan2f";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_float;c_float];
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_float;
      Type_ast.func_attributes = [];
    };
    { 
      Type_ast.func_name = Some "atan2l";
      Type_ast.param_type = Type_ast.Param_type_fix 
	[c_long_double;c_long_double];
      Type_ast.formal_param = [Some "x";Some "y"];
      Type_ast.return_type = c_long_double;
      Type_ast.func_attributes = [];
    };
    
    
    (** strcat, strncat - concatenate two strings **)
    { Type_ast.func_name = Some "strcat";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "s1"; Some "s2"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strncat";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer;size_t];
      Type_ast.formal_param = [Some "s1"; Some "s2"; Some "n"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    
    (** strchr, strrchr - locate character in string **)
    { Type_ast.func_name = Some "strchr";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;c_int];
      Type_ast.formal_param = [Some "s"; Some "c"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strrchr";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;c_int];
      Type_ast.formal_param = [Some "s"; Some "c"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    
    (** strcmp, strncmp - compare two strings **)
    { Type_ast.func_name = Some "strcmp";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "s1"; Some "s2"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strncmp";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer;size_t];
      Type_ast.formal_param = [Some "s1"; Some "s2"; Some "n"];
      Type_ast.return_type = c_int;
      Type_ast.func_attributes = [];
    };

    (** strcpy, strncpy - copy a string **)
    { Type_ast.func_name = Some "strcpy";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "dest"; Some "src"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strncpy";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer;size_t];
      Type_ast.formal_param = [Some "dest"; Some "src"; Some "n"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };

    (** strlen - calculate the length of a string **)
    { Type_ast.func_name = Some "strlen";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer];
      Type_ast.formal_param = [Some "s"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };

    (** strpbrk - search a string for any of a set of characters **)
    { Type_ast.func_name = Some "strpbrk";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "s1"; Some "accept"];
      Type_ast.return_type = char_pointer;
      Type_ast.func_attributes = [];
    };

    (** strspn, strcspn - search a string for a set of characters **)
    { Type_ast.func_name = Some "strspn";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "s1"; Some "accept"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };
    { Type_ast.func_name = Some "strcspn";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "s1"; Some "accept"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };

    (** strstr - locate a substring **)
    { Type_ast.func_name = Some "strstr";
      Type_ast.param_type = Type_ast.Param_type_va 
	[const_char_pointer;const_char_pointer];
      Type_ast.formal_param = [Some "haystack"; Some "needle"];
      Type_ast.return_type = size_t;
      Type_ast.func_attributes = [];
    };
  ]
