/* 
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
*/

/*
 * Copyrigth (c) 2006,
 * Ning Wang <wangn@ics.uci.edu>
 *
 */

#include<stddef.h>
#include<stdio.h>
#include<stdarg.h>
#include<stdbool.h>
#include<complex.h>
#include<setjmp.h>
#include<string.h>
#include<stdlib.h>
#include<assert.h>
#include<limits.h>

#ifdef _GNUCC
#define LONGLONG long long
#define CONST_STRING_LITERALS "true"
#define VERSION __VERSION__
#define VERSION_MAJOR __GNUC__
#define VERSION_MINOR __GNUC_MINOR__
#endif

#ifdef _MSVC
#define LONGLONG __int64
#define CONST_STRING_LITERALS "false"
#define VERSION "0"
#define VERSION_MAJOR 0
#define VERSION_MINOR 0
#endif

struct 
{
  char * c_typ_name;
  const char * ml_typ_name;
  unsigned int ml_typ_id;
  unsigned int ml_typ_size;
  char * ml_equiv_typ_name;
} c_type_pool [] =
  {
    {"?",    "cnon_id", -1, 0},
    {"void", "cvoid_id", -1, 0},
    
    {"char",          "cchar_id", -1, 0},
    {"signed char",   "cschar_id", -1, 0},
    {"unsigned char", "cuchar_id", -1, 0},
    
    {"short int",          "cshort_id", -1, 0},
    {"unsigned short int", "cushort_id", -1, 0},
    
    {"_Bool", "cbool_id", -1, 0},

    {"int",          "cint_id", -1, 0},
    {"unsigned int", "cuint_id", -1, 0},
    
    {"long int",           "clong_id", -1, 0},
    {"unsigned long int",  "culong_id", -1, 0},

    {"long long int",          "cllong_id", -1, 0},
    {"unsigned long long int", "cullong_id", -1, 0},

    {"float",       "cfloat_id", -1, 0},
    {"double",      "cdouble_id", -1, 0},
    {"long double", "cldouble_id", -1, 0},
    
    {"_Complex",       "ccomplex_id", -1, 0},
    {"float _Complex", "cfcomplex_id", -1, 0},
    {"double _Complex", "cdcomplex_id", -1, 0},
    {"long double _Complex", "cldcomplex_id", -1, 0},
    
    {"void *", "cptr_id", -1, 0},
    
    {"__builtin_va_list", "cva_list_id", -1, 0},

    #ifdef _GNUCC
    {"__mode__(__QI__)", "QImode", -1, 0},
    {"__mode__(__HI__)", "HImode", -1, 0},
    {"__mode__(__SI__)", "SImode", -1, 0},
    {"__mode__(__DI__)", "DImode", -1, 0},
    #endif
    
    {0, 0, -1, 0},  /* set up the bound */
};


int char_is_unsigned = 0;

void init_c_type_pool_entry (const char * ml_typ_name, 
			     const unsigned int ml_typ_id,
			     const unsigned int ml_typ_size)
{
    int i;
    i = 0;

    for (i = 0; c_type_pool[i].ml_typ_name != (char *)0; ++i)
    {
	if (strcmp (c_type_pool[i].ml_typ_name, ml_typ_name) == 0)
	{
	    c_type_pool[i].ml_typ_id = ml_typ_id;
	    c_type_pool[i].ml_typ_size = ml_typ_size;
	    break;
	}
    }
}


struct {
    char * ml_alias_name;
    char * ml_typ_name;
} alias_pool [] = 
{
  {"int32_id", 0},
  {"uint32_id", 0},
  
  {"int64_id", 0},
  {"uint64_id", 0},
  
  {"cptr_uint_id", 0},
  {"cwchar_t_id", 0},
  {"csize_t_id", 0},
  {0,0}
};


void set_alias_pool (const char * ml_alias_name, 
		     char * ml_typ_name)
{
    int i;
    for (i = 0; alias_pool[i].ml_alias_name != 0; ++i)
      {
	if (strcmp (alias_pool[i].ml_alias_name, ml_alias_name) == 0)
	{
	    alias_pool[i].ml_typ_name = ml_typ_name;
	    break;
	}
    }
}

void set_alias_types (FILE * fp)
{

  char * int32_name = "";
  char * int64_name = "";
  char * size_t_name = "";
  char * pointer_int_name = "";
  
  /* test long long */
  if (sizeof (long long int) == 4) 
    {
      int32_name = "int";
      set_alias_pool ("int32_id", "cllong_id");
      set_alias_pool ("uint32_id", "cullong_id");
    } 
  else if (sizeof (long long int) == 8)
    {
      int64_name = "int";
      set_alias_pool ("int64_id", "cllong_id");
      set_alias_pool ("uint64_id", "cullong_id");
    }
      
  if (sizeof (long long) == sizeof (void *))
    {
      pointer_int_name = "long long int";
      set_alias_pool ("cptr_uint_id", "cullong_id");
    }
      
  if (sizeof (long long) == sizeof (size_t))
    {
      size_t_name = "unsigned long long int";
      set_alias_pool ("csize_t_id", "cullong_id");
    }
      
  if (sizeof (long) == sizeof (wchar_t) && sizeof(int) != sizeof(wchar_t))
    {
      set_alias_pool ("cwchar_t_id", "cllong_id");
    }
  
  /* test long */
  if (sizeof (long) == 4) 
    {
      int32_name = "int";
      set_alias_pool ("int32_id", "clong_id");
      set_alias_pool ("uint32_id", "culong_id");
    } 
  else if (sizeof (long) == 8)
    {
      int64_name = "int";
      set_alias_pool ("int64_id", "clong_id");
      set_alias_pool ("uint64_id", "culong_id");
    }
	
  if (sizeof (long) == sizeof (void *))
    {
      pointer_int_name = "long int";
      set_alias_pool ("cptr_uint_id", "culong_id");
    }
	
  if (sizeof (long) == sizeof (size_t))
    {
      size_t_name = "unsigned long int";
      set_alias_pool ("csize_t_id", "culong_id");
    }

  if (sizeof (long) == sizeof (wchar_t))
    {
      set_alias_pool ("cwchar_t_id", "clong_id");
    }

  /* test int */
  if (sizeof (int) == 4) 
    {
      int32_name = "int";
      set_alias_pool ("int32_id", "cint_id");
      set_alias_pool ("uint32_id", "cuint_id");
    } 
  else if (sizeof (int) == 8)
    {
      int64_name = "int";
      set_alias_pool ("int64_id", "cint_id");
      set_alias_pool ("uint64_id", "cuint_id");
    }

  if (sizeof (int) == sizeof (void *))
    {
      pointer_int_name = "int";
      set_alias_pool ("cptr_uint_id", "cuint_id");
    }
	
  if (sizeof (int) == sizeof (size_t))
    {
      size_t_name = "unsigned int";
      set_alias_pool ("csize_t_id", "cuint_id");
    }
	
  if (sizeof (int) == sizeof (wchar_t))
    {
      set_alias_pool ("cwchar_t_id", "cint_id");
    }

  /* test short */
  if (sizeof (short int) == sizeof (size_t))
    {
      set_alias_pool ("csize_t_id", "cushort_id");
    }
	
  if (sizeof (short int) == sizeof (wchar_t) && sizeof(int) != sizeof(wchar_t))
    {
      set_alias_pool ("cwchar_t_id", "cshort_id");
    }

  int i;
  for (i = 0; alias_pool[i].ml_alias_name != 0; ++i)
    fprintf (fp, "let %s = %s \n", alias_pool[i].ml_alias_name, alias_pool[i].ml_typ_name);
}

int main ()
{
    FILE * fp = fopen ("config_mach.ml", "w+");
    int type_id = 0;
    int pointer_alignof = 0;
    
    fprintf(fp, "(* Generated by code in %s *)\n", __FILE__);

    fprintf(fp, "type tk = SI\n");
    fprintf(fp, "  | BI\n");
    fprintf(fp, "  | UI\n");
    fprintf(fp, "  | F\n");
    fprintf(fp, "  | NIF\n");
    fprintf(fp, "  | PTR\n");
    fprintf(fp, "  | CI\n");
    fprintf(fp, "  | CF\n");
    
    // Whether string literals contain constant characters
    /* fputs(fp, "let const_string_literals = " CONST_STRING_LITERALS ""); */

    // The type of non, void
    {
      fprintf (fp, "\n(** type_id, type_class, size, align, complete, computed, type_name **)\n");
      fprintf (fp, "let builtin_type_table =\n");
      fprintf (fp, "[\n");
      fprintf (fp, " (%d, NIF, 0L, 0L, \"?\", 0L, 0L);\n", type_id ++); /* non type */
      init_c_type_pool_entry ("cnon_id", type_id - 1, 0);
      
      fprintf (fp, " (%d, NIF, %dL, 0L, \"void\", 0L, 0L);\n", 
	       type_id ++, (int)sizeof(void));
      init_c_type_pool_entry ("cvoid_id", type_id - 1, 0);
    }
    
    
    // The type of char
    {
      struct st_char {
	char c;
	char ll;
      };
      int alignof_char = (int)(&((struct st_char*)0)->ll);
	
      char_is_unsigned = ((char)0xff) > 0;

      fprintf (fp, " (%d, %s, %dL, %dL, \"char\", %dL, %dL);\n", 
	       type_id ++, (char_is_unsigned ? "UI":"SI"), 
	       (int)sizeof(char), alignof_char, CHAR_MAX, CHAR_MIN);
      init_c_type_pool_entry ("cchar_id", type_id - 1, (int)sizeof(char));
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"signed char\", %dL, %dL);\n", 
	       type_id ++, (int)sizeof(signed char), alignof_char, CHAR_MAX, CHAR_MIN);
      init_c_type_pool_entry ("cschar_id", type_id - 1, (int)sizeof(signed char));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"unsigned char\", %uL, 0L);\n", 
	       type_id ++, (int)sizeof(unsigned char), alignof_char, UCHAR_MAX);
      init_c_type_pool_entry ("cuchar_id", type_id - 1, (int)sizeof(unsigned char));
    }


    


    // The type of short int
    {
      struct st_short_int {
	char c;
	short int ll;
      };
	
      int alignof_short_int = (int)(&((struct st_short_int*)0)->ll);
	
      fprintf (fp, " (%d, SI, %dL, %dL, \"short int\", %dL, %dL);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, SHRT_MAX, SHRT_MIN);
      init_c_type_pool_entry ("cshort_id", type_id - 1, (int)sizeof(signed short int));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"unsigned short int\", %uL, 0L);\n", 
	       type_id ++, (int)sizeof(unsigned short int), alignof_short_int, USHRT_MAX);
      init_c_type_pool_entry ("cushort_id", type_id - 1, (int)sizeof(unsigned short int));
    }

    // The type of _Bool
    {
	struct st_Bool {
	    char c;
	    _Bool ll;
	};
	int alignof_Bool = 
	    (int)(&((struct st_Bool*)0)->ll);
	fprintf (fp, " (%d, BI, %dL, %dL, \"_Bool\", 1L, 0L);\n", 
		 type_id ++, (int)sizeof(_Bool), alignof_Bool);
	init_c_type_pool_entry ("cbool_id", type_id - 1, (int)sizeof(_Bool));
    }
   
    
    // The type of int
    {
      struct st_int {
	char c;
	int ll;
      };
      int alignof_int = (int)(&((struct st_int*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"int\", %dL, %dL);\n", 
	       type_id ++, (int)sizeof(signed int), alignof_int, INT_MAX, INT_MIN);
      init_c_type_pool_entry ("cint_id", type_id - 1, (int)sizeof(signed int));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"unsigned int\", %uL, 0L);\n", 
	       type_id ++, (int)sizeof(unsigned int), alignof_int, UINT_MAX);
      init_c_type_pool_entry ("cuint_id", type_id - 1, (int)sizeof(unsigned int));
    }
    
    
    // The type of long
    {
      struct st_long {
	char c;
	long ll;
      };
      int alignof_long = 
	(int)(&((struct st_long*)0)->ll);

      fprintf (fp, " (%d, SI, %dL, %dL, \"long int\", %ldL, %ldL);\n", 
	       type_id ++, (int)sizeof(signed long int), alignof_long, LONG_MAX, LONG_MIN);
      init_c_type_pool_entry ("clong_id", type_id - 1, (int)sizeof(signed long int));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"unsigned long int\", %luL, 0L);\n", 
	       type_id ++, (int)sizeof(unsigned long int), alignof_long, ULONG_MAX);
      init_c_type_pool_entry ("culong_id", type_id - 1, 
			      (int)sizeof(unsigned long int));
    }

    
    // The type of long long int
    {
      struct st_longlong {
	char c;
	long long ll;
      };
      int alignof_longlong = 
	(int)(&((struct st_longlong*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"long long int\", %lldL, %lldL);\n", 
	       type_id ++, (int)sizeof(signed long long int), alignof_longlong,
	       ((unsigned long long int)-1)/2, -((unsigned long long int)-1)/2);
      init_c_type_pool_entry ("cllong_id", type_id - 1, 
			      (int)sizeof(signed long long int));
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"signed long long int\", %lldL, %lldL);\n", 
	       type_id - 1, (int)sizeof(signed long long int), alignof_longlong,
	       ((unsigned long long int)-1)/2, -((unsigned long long int)-1)/2);
      init_c_type_pool_entry ("cllong_id", type_id - 1, 
			      (int)sizeof(signed long long int));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"unsigned long long int\", %lluL, 0L);\n", 
	       type_id ++, (int)sizeof(unsigned long long int), alignof_longlong,
	       ((unsigned long long int)-1)/2);
      init_c_type_pool_entry ("cullong_id", type_id - 1, (int)sizeof(unsigned long long int));
    }
    
    
    // The type of float
    {
	struct st_float {
	    char c;
	    float ll;
	};
	int alignof_float = 
	    (int)(&((struct st_float*)0)->ll);
	fprintf (fp, " (%d, F, %dL, %dL, \"float\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(float), alignof_float);
	init_c_type_pool_entry ("cfloat_id", type_id - 1, (int)sizeof(float));
    }
    

    // The type of double
    {
	struct st_double {
	    char c;
	    double ll;
	};
	int alignof_double = 
	    (int)(&((struct st_double*)0)->ll);
	fprintf (fp, " (%d, F, %dL, %dL, \"double\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(double), alignof_double);
	init_c_type_pool_entry ("cdouble_id", type_id - 1, (int)sizeof(double));
    }
    
    // The type of long double
    {
	struct st_long_double {
	    char c;
	    long double ll;
	};
	int alignof_long_double = 
	    (int)(&((struct st_long_double*)0)->ll);
	fprintf (fp, " (%d, F, %dL, %dL, \"long double\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(long double), alignof_long_double);
	init_c_type_pool_entry ("cldouble_id", type_id - 1, 
				(int)sizeof(long double));
    }    
    
    // The type of complex
    {
	struct st_complex {
	    char c;
	    _Complex ll;
	};
	int alignof_complex = 
	    (int)(&((struct st_complex*)0)->ll);
	fprintf (fp, " (%d, CI, %dL, %dL, \"_Complex\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(_Complex), alignof_complex);
	init_c_type_pool_entry ("ccomplex_id", type_id - 1, (int)sizeof(_Complex));
    }
    

    // The type of float complex
    {
	struct st_float_complex {
	    char c;
	    float _Complex ll;
	};
	int alignof_float_complex = 
	    (int)(&((struct st_float_complex*)0)->ll);
	fprintf (fp, " (%d, CF, %dL, %dL, \"float _Complex\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(float _Complex), alignof_float_complex);
	init_c_type_pool_entry ("cfcomplex_id", type_id - 1, 
				(int)sizeof(float _Complex));
    }
    
    // The type of double complex
    {
	struct st_double_complex {
	    char c;
	    double _Complex ll;
	};
	int alignof_double_complex = 
	    (int)(&((struct st_double_complex*)0)->ll);
	fprintf (fp, " (%d, CF, %dL, %dL, \"double _Complex\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(double _Complex), 
		 alignof_double_complex);
	init_c_type_pool_entry ("cdcomplex_id", type_id - 1, 
				(int)sizeof(double _Complex));
    }


    // The type of long double complex
    {
	struct st_long_double_complex {
	    char c;
	    long double _Complex ll;
	};
	int alignof_long_double_complex = 
	    (int)(&((struct st_long_double_complex*)0)->ll);
	fprintf (fp, " (%d, CF, %dL, %dL, \"long double _Complex\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(long double _Complex), 
		 alignof_long_double_complex);

	init_c_type_pool_entry ("cldcomplex_id", type_id - 1, 
				(int)sizeof(long double _Complex));
    }
    

    // The type of void_ptr
    {
	struct st_void_ptr {
	    char c;
	    void * ll;
	};
	int alignof_void_ptr = 
	  (int)(&((struct st_void_ptr*)0)->ll);
	pointer_alignof = alignof_void_ptr;
	fprintf (fp, " (%d, PTR, %dL, %dL, \"void *\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(void *), alignof_void_ptr);
	init_c_type_pool_entry ("cptr_id", type_id - 1, (int)sizeof(void *));
    }
    
    // The type of va_list
    {
      struct st_va_list {
	    char c;
	    va_list ll;
	};
	int alignof_va_list = 
	    (int)(&((struct st_va_list*)0)->ll);
	fprintf (fp, " (%d, NIF, %dL, %dL, \"__builtin_va_list\", 0L, 0L);\n", 
		 type_id ++, (int)sizeof(va_list), alignof_va_list);
	init_c_type_pool_entry ("cva_list_id", type_id - 1, (int)sizeof(va_list));
    }


#ifdef _GNUCC
    // The type of QImode
    {
      struct st_short_int {
	char c;
	int ll __attribute__ ((__mode__ ( __QI__ ))) ;
      } st;
      
      int alignof_short_int = (int)(&((struct st_short_int*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"QImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("QImode", type_id - 1, (int)sizeof(st.ll));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"uQImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("uQImode", type_id - 1, (int)sizeof(st.ll));
    }

    // The type of HImode
    {
      struct st_short_int {
	char c;
	int ll __attribute__ ((__mode__ ( __HI__ ))) ;
      } st;
      
      int alignof_short_int = (int)(&((struct st_short_int*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"HImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("HImode", type_id - 1, (int)sizeof(st.ll));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"uHImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("uHImode", type_id - 1, (int)sizeof(st.ll));
    }

    // The type of SImode
    {
      struct st_short_int {
	char c;
	int ll __attribute__ ((__mode__ ( __SI__ ))) ;
      } st;
      
      int alignof_short_int = (int)(&((struct st_short_int*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"SImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("SImode", type_id - 1, (int)sizeof(st.ll));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"uSImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("uSImode", type_id - 1, (int)sizeof(st.ll));
    }

    // The type of DImode
    {
      struct st_short_int {
	char c;
	int ll __attribute__ ((__mode__ ( __DI__ ))) ;
      } st;
      
      int alignof_short_int = (int)(&((struct st_short_int*)0)->ll);
      
      fprintf (fp, " (%d, SI, %dL, %dL, \"DImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("DImode", type_id - 1, (int)sizeof(st.ll));
      
      fprintf (fp, " (%d, UI, %dL, %dL, \"uDImode\", %dL, 0L);\n",
	       type_id ++, (int)sizeof(signed short int), alignof_short_int, -1);
      init_c_type_pool_entry ("uDImode", type_id - 1, (int)sizeof(st.ll));
    }
#endif //_GNUCC
    
    fprintf (fp, "]\n");


    /////////////// 
    {
	// endianity
	{
	    int e = 0x11223344;
	    fprintf(fp, "let little_endian = %s\n",
		    (0x44 == *(char*)&e) ? "true" :
		    ((0x11 == *(char*)&e) ? "false" : (exit(1), "false")));
	}
	
	// Whether char is unsigned
	fprintf (fp, "let char_is_unsigned = %s\n", char_is_unsigned ? "true" : "false");
	fprintf (fp, "let char_bit = %d\n", CHAR_BIT);
	fprintf (fp, "let sizeof_pointer = %dL\n", (int)sizeof (void *));
	fprintf (fp, "let alignof_pointer = %dL\n", pointer_alignof);
	
	struct st_enum {
	    char c;
	    enum {A, B} ll;
	};
	int alignof_enum = (int)(&((struct st_enum*)0)->ll);
	fprintf (fp, "let sizeof_enum = %dL\n", (int)sizeof (enum {C, D}));
	fprintf (fp, "let alignof_enum = %dL\n", alignof_enum);
	fprintf (fp, "let numof_builtin_types = %d\n", type_id);
    }    

    fprintf (fp, "\n");
    // floating types
    {
	fprintf (fp, "let floating_type_table = [\n");
	fprintf (fp, "                           (\"float\", %dL);\n", (int)sizeof(float));
	fprintf (fp, "                           (\"double\", %dL);\n", (int)sizeof(double));
	fprintf (fp, "                           (\"long double\", %dL);\n", (int)sizeof(long double));
	fprintf (fp, "                          ]");
    }

    fprintf (fp, "\n");

    // c_type_pool
    {
	fprintf (fp, "(** c type constant **)\n");
	int i = 0;
	for (i = 0; c_type_pool[i].ml_typ_name != 0; ++i)
	    fprintf (fp, "let %s = %d\n", 
		     c_type_pool[i].ml_typ_name,
		     c_type_pool[i].ml_typ_id);
	
	set_alias_types (fp);
	
	char * floating_typ_list [] = { "cfloat_id", "cdouble_id", "cldouble_id", 0};
	fprintf (fp, "let floating_typ_list = [");
	
	for (i = 0; floating_typ_list[i] != 0; ++i)
	  {
	    int j;
	    for (j = 0; c_type_pool[j].ml_typ_name != 0; ++j)
	      {
		if (strcmp (floating_typ_list[i], c_type_pool[j].ml_typ_name) == 0)
		  {
		    fprintf (fp, "(%d, %dL);", 
			     c_type_pool[j].ml_typ_id,
			     c_type_pool[j].ml_typ_size
			     );
		    break;
		  }
	      }
	}
	fprintf (fp, "]\n");
    }

    {
      union {
	unsigned char uc;
	unsigned short us;
	unsigned int ui;
	unsigned long int ul;
	unsigned long long int ull;
      } data;

      int size = sizeof(unsigned long long int);
      printf ("size = %d\n", size);
      switch (size)
	{
	case 4: data.ull = 0xFFFFFFFFLL;
	case 8: data.ull = 0xFFFFFFFFFFFFFFFFLL;
	  break;
	default: assert(0);
	}
      
      fprintf (fp, "open Big_int\n");
      fprintf (fp, "open Const_folding\n");
      fprintf (fp, "open C_syntax_symbol\n");
      
      fprintf (fp, "(* decimal *)\n");
      fprintf (fp, "(* none suffix *)\n");
      fprintf (fp, "let dec_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cint_id, cint_cval_of_string);\n", 8 * sizeof(int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), clong_id, clong_cval_of_string);\n", 8 * sizeof(long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 8 * sizeof(long long int)-1);
      fprintf (fp, "]\n");

      fprintf (fp, "(* u or U *)\n");
      fprintf (fp, "let dec_u_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cuint_id, cuint_cval_of_string);\n", 8 * sizeof(int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* l or L *)\n");
      fprintf (fp, "let dec_l_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), clong_id, clong_cval_of_string);\n", 
	       8 * sizeof(long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 
	       8 * sizeof(long long int)-1);
      fprintf (fp, "]\n");

      fprintf (fp, "(* Both u or U and l or L *)\n");
      fprintf (fp, "let dec_ul_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 
	       8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 
	       8 * sizeof(long long int));
      fprintf (fp, "]\n");
      
      fprintf (fp, "(* ll or LL *)\n");
      fprintf (fp, "let dec_ll_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 
	       8 * sizeof(long long int)-1);
      fprintf (fp, "]\n");

      fprintf (fp, "(* Both u or U and ll or LL *)\n");
      fprintf (fp, "let dec_ull_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 
	       8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* octal or hexadecimal *)\n");
      fprintf (fp, "(* none suffix *)\n");
      fprintf (fp, "let oct_hex_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cint_id, cint_cval_of_string);\n", 8 * sizeof(int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cuint_id, cuint_cval_of_string);\n", 8 * sizeof(int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), clong_id, clong_cval_of_string);\n", 8 * sizeof(long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 8 * sizeof(long long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* u or U *)\n");
      fprintf (fp, "let oct_hex_u_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cuint_id, cuint_cval_of_string);\n", 8 * sizeof(int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* l or L *)\n");
      fprintf (fp, "let oct_hex_l_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), clong_id, clong_cval_of_string);\n", 8 * sizeof(long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 8 * sizeof(long long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* Both u or U and l or L *)\n");
      fprintf (fp, "let oct_hex_ul_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), culong_id, culong_cval_of_string);\n", 8 * sizeof(long int));
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 8 * sizeof(long long int));
      fprintf (fp, "]\n");
      
      fprintf (fp, "(* ll or LL *)\n");
      fprintf (fp, "let oct_hex_ll_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cllong_id, cllong_cval_of_string);\n", 
	       8 * sizeof(long long int)-1);
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 
	       8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "(* Both u or U and ll or LL *)\n");
      fprintf (fp, "let oct_hex_ull_integer_range_type = [\n");
      fprintf (fp, "(pred_big_int(power_int_positive_int 2 %u), cullong_id, cullong_cval_of_string);\n", 
	       8 * sizeof(long long int));
      fprintf (fp, "]\n");

      fprintf (fp, "let integer_range_type_table = [\n");
      fprintf (fp, " (DEC_NONE, dec_integer_range_type);\n");
      fprintf (fp, " (DEC_U, dec_u_integer_range_type);\n");
      fprintf (fp, " (DEC_L, dec_l_integer_range_type);\n");
      fprintf (fp, " (DEC_UL, dec_ul_integer_range_type);\n");
      fprintf (fp, " (DEC_LL, dec_ll_integer_range_type);\n");
      fprintf (fp, " (DEC_ULL, dec_ull_integer_range_type);\n");
      
      fprintf (fp, " (HEX_NONE, oct_hex_integer_range_type);\n");
      fprintf (fp, " (HEX_L, oct_hex_l_integer_range_type);\n");
      fprintf (fp, " (HEX_U, oct_hex_u_integer_range_type);\n");
      fprintf (fp, " (HEX_UL, oct_hex_ul_integer_range_type);\n");
      fprintf (fp, " (HEX_LL, oct_hex_ll_integer_range_type);\n");
      fprintf (fp, " (HEX_ULL, oct_hex_ull_integer_range_type);\n");
      
      fprintf (fp, " (OCT_NONE, oct_hex_integer_range_type);\n");
      fprintf (fp, " (OCT_L, oct_hex_l_integer_range_type);\n");
      fprintf (fp, " (OCT_U, oct_hex_u_integer_range_type);\n");
      fprintf (fp, " (OCT_UL, oct_hex_ul_integer_range_type);\n");
      fprintf (fp, " (OCT_LL, oct_hex_ll_integer_range_type);\n");
      fprintf (fp, " (OCT_ULL, oct_hex_ull_integer_range_type);\n");
      fprintf (fp, "]\n");
    }
    return 0;
}
