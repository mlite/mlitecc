m4_dnl
m4_dnl MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
m4_dnl   
m4_dnl The name `Mlite C Compiler' belongs to us, but the code is available free for
m4_dnl any use in any field of endeavor.  You may redistribute Mlite C Compiler in
m4_dnl whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
m4_dnl source and include this LICENSE file.  You may modify Mlite C Compiler and
m4_dnl create derived works, with which you may do as you like, but the result may
m4_dnl not be called Mlite C Compiler without written consent.
   
m4_dnl The software is placed in the public domain.  It is not protected by copyright,
m4_dnl and it is not protected by a ``copyleft'' agreement like the one used by the
m4_dnl Free Software Foundation.

m4_define(`foreach',`m4_ifelse(m4_eval($#>2),1,
`m4_pushdef(`$1',`$3')$2`'m4_popdef(`$1')
`'m4_ifelse(m4_eval($#>3),1,`$0(`$1',`$2',m4_shift(m4_shift(m4_shift($@))))')')')


m4_dnl TYPEDEF_2(int, cint)
m4_define(TYPEDEF_2, `typedef $1 $2;')

m4_dnl S2V_FUN_ML_1(cint)
m4_dnl external cint_of_string: string -> cint = "cint_of_OCAMLstring"
m4_define(S2V_FUN_ML_1, `external $1_of_string: string -> $1 = "$1_of_OCAMLstring"')


m4_dnl V2S_FUN_ML_1(cint)
m4_dnl external string_of_cint: cint -> string = "OCAMLstring_of_cint"
m4_define(V2S_FUN_ML_1, `external string_of_$1: $1 -> string = "OCAMLstring_of_$1"')
m4_define(V2BSA_FUN_ML_1, `external bsa_of_$1: $1 -> string array = "OCAMLbsa_of_$1"')

m4_dnl BINARY_FUN_ML_2(add, cint) 
m4_dnl external add_cint: cint -> cint -> cint = "C_add_cint"
m4_define(BINARY_FUN_ML_2, `external $1_$2: $2 -> $2 -> $2 = "C_$1_$2"')

m4_dnl UNARY_FUN_ML_2(neg, cint)
m4_dnl external neg_cint: cint -> cint = "C_neg_cint"
m4_define(UNARY_FUN_ML_2, `external $1_$2: $2 -> $2 = "C_$1_$2"')
m4_define(UNARY_FUN_ML_3, `external $1_$2: $2 -> $3 = "C_$1_$2"')

m4_dnl RELATIONAL_FUN_ML_2(lt, cint) 
m4_dnl external lt_cint: cfloat -> cfloat -> cint = "C_lt_cfloat"
m4_define(RELATIONAL_FUN_ML_2, `external $1_$2: $2 -> $2 -> cint = "C_$1_$2"')

m4_dnl
m4_define(T2T_FUN_ML_2, `external $1_to_$2: $1 -> $2 = "C_$1_to_$2"')


m4_define(INF_PLUS_ML_1,  `external $1_builtin_plus_inf: unit -> $1 = "$1_builtin_plus_inf"')
m4_define(INF_MINUS_ML_1, `external $1_builtin_minus_inf: unit -> $1 = "$1_builtin_minus_inf"')
m4_define(NAN_ML_1, `external $1_builtin_nan: unit -> $1 = "$1_builtin_nan"')

m4_define(INTEGER_FUN_ML_1,
`
V2S_FUN_ML_1($1)
V2BSA_FUN_ML_1($1)
S2V_FUN_ML_1($1)
BINARY_FUN_ML_2(add,$1)
BINARY_FUN_ML_2(sub,$1)
BINARY_FUN_ML_2(mul,$1)
BINARY_FUN_ML_2(div,$1)
BINARY_FUN_ML_2(mod,$1)
BINARY_FUN_ML_2(shl,$1)
BINARY_FUN_ML_2(shr,$1)
BINARY_FUN_ML_2(band,$1)
BINARY_FUN_ML_2(bor,$1)
BINARY_FUN_ML_2(bxor,$1)
UNARY_FUN_ML_2(bnot,$1)
UNARY_FUN_ML_2(neg,$1)
UNARY_FUN_ML_3(not,$1,cint)
RELATIONAL_FUN_ML_2(lt,$1)
RELATIONAL_FUN_ML_2(le,$1)
RELATIONAL_FUN_ML_2(eq,$1)
RELATIONAL_FUN_ML_2(ne,$1)
RELATIONAL_FUN_ML_2(gt,$1)
RELATIONAL_FUN_ML_2(ge,$1)
')

m4_define(CINT_FUN_ML_1,
`
INTEGER_FUN_ML_1($1)
RELATIONAL_FUN_ML_2(and,$1)
RELATIONAL_FUN_ML_2(or,$1)
')

m4_define(FLOAT_FUN_ML_1,
`
INF_PLUS_ML_1($1)
INF_MINUS_ML_1($1)
NAN_ML_1($1)
V2S_FUN_ML_1($1)
V2BSA_FUN_ML_1($1)
S2V_FUN_ML_1($1)
BINARY_FUN_ML_2(add,$1)
BINARY_FUN_ML_2(sub,$1)
BINARY_FUN_ML_2(mul,$1)
BINARY_FUN_ML_2(div,$1)
UNARY_FUN_ML_2(neg,$1)
UNARY_FUN_ML_3(not,$1,cint)
RELATIONAL_FUN_ML_2(lt,$1)
RELATIONAL_FUN_ML_2(le,$1)
RELATIONAL_FUN_ML_2(eq,$1)
RELATIONAL_FUN_ML_2(ne,$1)
RELATIONAL_FUN_ML_2(gt,$1)
RELATIONAL_FUN_ML_2(ge,$1)
')



m4_define(T2T_FUN_ML,
`
foreach(`X',
`
T2T_FUN_ML_2(X,cbool)
T2T_FUN_ML_2(X,cchar)
T2T_FUN_ML_2(X,cuchar)
T2T_FUN_ML_2(X,cschar)
T2T_FUN_ML_2(X,cshort)
T2T_FUN_ML_2(X,cushort)
T2T_FUN_ML_2(X,cint)
T2T_FUN_ML_2(X,cuint)
T2T_FUN_ML_2(X,clong)
T2T_FUN_ML_2(X,culong)
T2T_FUN_ML_2(X,cllong)
T2T_FUN_ML_2(X,cullong)
T2T_FUN_ML_2(X,cfloat)
T2T_FUN_ML_2(X,cdouble)
T2T_FUN_ML_2(X,cldouble)
T2T_FUN_ML_2(X,cfloatx)
T2T_FUN_ML_2(X,cdoublex)
T2T_FUN_ML_2(X,cldoublex)
',
`cbool',`cchar',`cschar',`cuchar',`cshort',`cushort',`cint',`cuint',`clong',`culong',
`cllong',`cullong',`cfloat',`cdouble',`cldouble',`cfloatx', `cdoublex', `cldoublex')
')


m4_dnl opcodename->typename->opcode->type
m4_dnl BINARY_FUN_C_4(add, cfloat, +)
m4_define(BINARY_FUN_C_3,
`
value C_$1_$2 (value v1,value v2) 
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  STO($2, res) = STO($2, v1) $3 STO ($2, v2);
  SET_TAG($2, res);
  return res ;
}
')


m4_dnl opcodename->typename->opcode->type
m4_dnl UNARY_FUN_C_3(neg, cfloat, -)
m4_define(UNARY_FUN_C_3,
`
value C_$1_$2 (value v1) 
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  STO($2, res) = $3 STO($2, v1);
  SET_TAG($2, res);
  return res ;
}
')

m4_define(UNARY_FUN_C_4,
`
value C_$1_$2 (value v1) 
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  STO($4, res) = $3 STO($2, v1);
  SET_TAG($4, res);
  return res ;
}
')

m4_dnl opcodename->typename->opcode->type
m4_dnl RELATIONAL_FUN_C_3(lt, cfloat, <)
m4_define(RELATIONAL_FUN_C_3,
`
value C_$1_$2 (value v1,value v2) 
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  STO(cint, res) = STO($2, v1) $3 STO ($2, v2);
  SET_TAG(cint, res);
  return res ;
}
')

m4_dnl 
m4_define(T2T_FUN_C_2,
`
value C_$1_to_$2 (value v1) 
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  STO($2, res) = ($2) STO($1, v1);
  SET_TAG($2, res);
  return res ;
}
')


m4_define(INTEGER_FUN_C_1,
`
BINARY_FUN_C_3(add,$1,+)
BINARY_FUN_C_3(sub,$1,-)
BINARY_FUN_C_3(mul,$1,*)
BINARY_FUN_C_3(div,$1,/)
BINARY_FUN_C_3(mod,$1,%)
BINARY_FUN_C_3(shl,$1,<<)
BINARY_FUN_C_3(shr,$1,>>)
BINARY_FUN_C_3(band,$1,&)
BINARY_FUN_C_3(bor,$1,|)
BINARY_FUN_C_3(bxor,$1,^)
UNARY_FUN_C_3(bnot,$1,~)
UNARY_FUN_C_3(neg,$1,-)
UNARY_FUN_C_4(not,$1,!,cint)
RELATIONAL_FUN_C_3(lt,$1,<)
RELATIONAL_FUN_C_3(le,$1,<=)
RELATIONAL_FUN_C_3(eq,$1,==)
RELATIONAL_FUN_C_3(ne,$1,!=)
RELATIONAL_FUN_C_3(gt,$1,>)
RELATIONAL_FUN_C_3(ge,$1,>=)
')

m4_define(CINT_FUN_C_1,
`
INTEGER_FUN_C_1($1)
RELATIONAL_FUN_C_3(and,$1,&&)
RELATIONAL_FUN_C_3(or,$1,||)
')

m4_define(FLOAT_FUN_C_1,
`
INF_PLUS_C_1($1)
INF_MINUS_C_1($1)
NAN_C_1($1)
BINARY_FUN_C_3(add,$1,+)
BINARY_FUN_C_3(sub,$1,-)
BINARY_FUN_C_3(mul,$1,*)
BINARY_FUN_C_3(div,$1,/)
UNARY_FUN_C_3(neg,$1,-)
UNARY_FUN_C_4(not,$1,!,cint)
RELATIONAL_FUN_C_3(lt,$1,<)
RELATIONAL_FUN_C_3(le,$1,<=)
RELATIONAL_FUN_C_3(eq,$1,==)
RELATIONAL_FUN_C_3(ne,$1,!=)
RELATIONAL_FUN_C_3(gt,$1,>)
RELATIONAL_FUN_C_3(ge,$1,>=)
')


m4_define(TYPEDEFS,
`
TYPEDEF_2(_Bool, cbool)
TYPEDEF_2(char, cchar)
TYPEDEF_2(signed char, cschar)
TYPEDEF_2(unsigned char, cuchar)
TYPEDEF_2(short, cshort)
TYPEDEF_2(unsigned short, cushort)
TYPEDEF_2(int, cint)
TYPEDEF_2(unsigned int, cuint)
TYPEDEF_2(long, clong)
TYPEDEF_2(unsigned long, culong)
TYPEDEF_2(long long, cllong)
TYPEDEF_2(unsigned long long, cullong)
TYPEDEF_2(float, cfloat)
TYPEDEF_2(double, cdouble)
TYPEDEF_2(long double, cldouble)
TYPEDEF_2(float, cfloatx)
TYPEDEF_2(double, cdoublex)
TYPEDEF_2(long double, cldoublex)
')


m4_define(T2T_FUN_C,
`
foreach(`X',
`
T2T_FUN_C_2(X,cbool)
T2T_FUN_C_2(X,cchar)
T2T_FUN_C_2(X,cuchar)
T2T_FUN_C_2(X,cschar)
T2T_FUN_C_2(X,cshort)
T2T_FUN_C_2(X,cushort)
T2T_FUN_C_2(X,cint)
T2T_FUN_C_2(X,cuint)
T2T_FUN_C_2(X,clong)
T2T_FUN_C_2(X,culong)
T2T_FUN_C_2(X,cllong)
T2T_FUN_C_2(X,cullong)
T2T_FUN_C_2(X,cfloat)
T2T_FUN_C_2(X,cdouble)
T2T_FUN_C_2(X,cldouble)
T2T_FUN_C_2(X,cfloatx)
T2T_FUN_C_2(X,cdoublex)
T2T_FUN_C_2(X,cldoublex)
',
`cbool',
`cchar',`cschar',`cuchar',
`cshort',`cushort',
`cint',`cuint',
`clong',`culong',
`cllong',`cullong',
`cfloat',`cdouble',`cldouble',
`cfloatx',`cdoublex',`cldoublex')
')



m4_dnl I2S_FUN_C_1(cint)
m4_define(S2UL_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;

  STO($1, res) = ($1) strtoul (str, &endptr, 0);
  SET_TAG($1, res);
  return res ; 
}
')

m4_dnl I2S_FUN_C_1(cint)
m4_define(S2ULL_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
       	 
  STO($1, res) = ($1) strtoull (str, &endptr, 0);
  SET_TAG($1, res);
  return res ; 
}
')


m4_dnl I2S_FUN_C_1(cint)
m4_define(S2L_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
       	 
  STO($1, res) = ($1) strtol (str, &endptr, 0);
  if (endptr == str)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  SET_TAG($1, res);
  return res ; 
}
')

m4_dnl I2S_FUN_C_1(cint)
m4_define(S2LL_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
       	 
  STO($1, res) = ($1) strtoll (str, &endptr, 0);
  if (endptr == str)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  SET_TAG($1, res);
  return res ; 
}
')


m4_dnl S2F_FUN_C_1(cfloat)
m4_define(S2F_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  double d = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  d = mpfr_get_d (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) d;
  SET_TAG($1, res);
  return res ;
}
')


m4_dnl S2D_FUN_C_1(cdouble)
m4_define(S2D_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  double d = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  d = mpfr_get_d (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) d;
  SET_TAG($1, res);
  return res;
}
')

m4_dnl S2LD_FUN_C_1(cldouble)
m4_define(S2LD_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  long double ld = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  ld = mpfr_get_ld (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) ld;
  SET_TAG($1, res);
  return res;
}
')


m4_dnl ==========================================
m4_dnl S2FX_FUN_C_1(cfloat)
m4_define(S2FX_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  double d = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  d = mpfr_get_d (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) d;
  SET_TAG($1, res);
  return res ;
}
')


m4_dnl S2DX_FUN_C_1(cfloat)
m4_define(S2DX_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  double d = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  d = mpfr_get_d (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) d;
  SET_TAG($1, res);
  return res ;
}
')

m4_dnl S2LDX_FUN_C_1(cfloat)
m4_define(S2LDX_FUN_C_1, 
`
value $1_of_OCAMLstring (value v)
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  char * str = String_val (v);
  char * endptr;
  long double ld = 0.0;
  mpfr_t mpfr;

  if (mpfr_init_set_str (mpfr, str, 0, GMP_RNDD) == -1)
  {
     fprintf (stderr, "\"%s\" is not a valid number\n", str);
     abort ();
  }
  ld = mpfr_get_ld (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  STO($1, res) = ($1) ld;
  SET_TAG($1, res);
  return res;
}
')


m4_dnl INF_PLUS_1(cfloat)
m4_define(INF_PLUS_C_1,
`
value $1_builtin_plus_inf ()
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  mpfr_t mpfr;

  mpfr_init (mpfr);
  mpfr_set_inf (mpfr, 1);
  STO ($1, res) = mpfr_get_ld (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  SET_TAG($1, res);
  return res;
}
')

m4_dnl INF_MINUS_1(cfloat)
m4_define(INF_MINUS_C_1,
`
value $1_builtin_minus_inf ()
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  mpfr_t mpfr;

  mpfr_init (mpfr);
  mpfr_set_inf (mpfr, -1);
  STO ($1, res) = ($1) mpfr_get_ld (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  SET_TAG($1, res);
  return res;
}
')

m4_dnl NAN_C_1(cfloat)
m4_define(NAN_C_1,
`
value $1_builtin_nan ()
{
  value res = caml_alloc_custom(&cvalop, sizeof(struct cval), 0, 1);
  mpfr_t mpfr;

  mpfr_init (mpfr);
  mpfr_set_nan (mpfr);
  STO ($1, res) = mpfr_get_ld (mpfr, GMP_RNDD); 
  mpfr_clear (mpfr);
  mpfr_clear_flags (); // clear all global flags

  SET_TAG($1, res);
  return res;
}
')


m4_dnl V2S_FUN_C_2(cuint,"%fU")
m4_define(V2S_FUN_C_2,
`
value OCAMLstring_of_$1 (value v)
{
  sprintf (buffer, $2, STO($1, v));
  return copy_string (buffer);
}
')


m4_dnl VF2S_FUN_C_2(cfloat,10)
m4_define(VF2S_FUN_C_2,
`
value OCAMLstring_of_$1 (value v)
{
  char * str;
  value ocaml_str;
  char * const_suffix;
  mpfr_t mpfr;
  mp_exp_t exp = 0;

  $1 cv = STO($1,v);

  mpfr_init (mpfr);  
  if (__builtin_types_compatible_p ($1, long double))
  {
      mpfr_init_set_ld (mpfr, cv, GMP_RNDD);
      const_suffix = "L";
  }  
  else if (__builtin_types_compatible_p ($1, double))
  {
      mpfr_init_set_d (mpfr, cv, GMP_RNDD);
      const_suffix = "";
  }
  else if (__builtin_types_compatible_p ($1, float))  
  {
      mpfr_init_set_d (mpfr, (double)cv, GMP_RNDD);
      const_suffix = "F";
  }
  else 
  {
     fprintf (stderr, "unexpected type $1\n");
     abort ();
  }
  
  str = mpfr_get_str(NULL, &exp, $2, 0, mpfr, GMP_RNDD);
  if (strcmp(str, "@NaN@") == 0)
     sprintf (buffer, "(0.0%s/0.0%s)", const_suffix, const_suffix);
  else if (strcmp(str, "@Inf@") == 0)
  {
     sprintf (buffer, "(+1.0%s/0.0%s)", const_suffix, const_suffix);
  }
  else if (strcmp(str, "-@Inf@") == 0)
  {
     sprintf (buffer, "(-1.0%s/0.0%s)", const_suffix, const_suffix);
  }
  else if ($2 == 10)
  {
      if (str[0] == MINUS_CHAR)
        sprintf (buffer, "-0.%se%d%s", &(str[1]), exp, const_suffix);
      else    
        sprintf (buffer, "+0.%se%d%s", str, exp, const_suffix);
  }
  else if ($2 == 16)
  {
    if (str[0] == MINUS_CHAR)
      sprintf (buffer, "-0x0.%sp%d%s", &(str[1]), exp, const_suffix);
    else
      sprintf (buffer, "+0x0.%sp%d%s", str, exp, const_suffix);
  }
  else 
  {
    fprintf (stderr, "unexpected base $2\n");
    abort ();
  }
  mpfr_free_str (str);
  mpfr_clear_flags (); // clear all global flags
  return copy_string (buffer);
}
')

m4_define(VX2S_FUN_C_2,
`
value OCAMLstring_of_$1x (value v)
{
  sprintf (buffer, $2, STO($1, v));
  return copy_string (buffer);
}
')


m4_define(V2BSA_FUN_C_1,
`
value OCAMLbsa_of_$1 (value v)
{
  char * tmp[256];

  int i = 0;
  cdata.$1 = STO($1, v);

  for (i = 0; i < sizeof($1); ++i)
  {
    memset (byte_string_array[i], 0, sizeof(byte_string_array[i]));
    sprintf (byte_string_array[i], "0x%x", cdata.bytes[i]);
    tmp[i] = &byte_string_array[i];
  }
  tmp[i] = 0;
  return copy_string_array (tmp);
}
')

m4_define(S2V_FUN_C,
`
S2L_FUN_C_1(cbool)
S2L_FUN_C_1(cchar)
S2L_FUN_C_1(cschar)
S2UL_FUN_C_1(cuchar)
S2L_FUN_C_1(cshort)
S2UL_FUN_C_1(cushort)
S2L_FUN_C_1(cint)
S2UL_FUN_C_1(cuint)
S2L_FUN_C_1(clong)
S2UL_FUN_C_1(culong)
S2LL_FUN_C_1(cllong)
S2ULL_FUN_C_1(cullong)
S2F_FUN_C_1(cfloat)
S2D_FUN_C_1(cdouble)
S2LD_FUN_C_1(cldouble)
S2FX_FUN_C_1(cfloatx)
S2DX_FUN_C_1(cdoublex)
S2LDX_FUN_C_1(cldoublex)
')

m4_define(V2S_FUN_C,
`
V2S_FUN_C_2(cbool,   "%d")
V2S_FUN_C_2(cchar,   "%d")
V2S_FUN_C_2(cschar,  "%d")
V2S_FUN_C_2(cuchar,  "%u")
V2S_FUN_C_2(cshort,  "%d")
V2S_FUN_C_2(cushort, "%u")
V2S_FUN_C_2(cint,    "%d")
V2S_FUN_C_2(cuint,   "%uU")
V2S_FUN_C_2(clong,   "%ldL")
V2S_FUN_C_2(culong,  "%luUL")
V2S_FUN_C_2(cllong,  "%lldLL")
V2S_FUN_C_2(cullong, "%lluULL")

VF2S_FUN_C_2(cfloat, 10)
VF2S_FUN_C_2(cdouble, 10)
VF2S_FUN_C_2(cldouble, 10)
VF2S_FUN_C_2(cfloatx, 16)
VF2S_FUN_C_2(cdoublex, 16)
VF2S_FUN_C_2(cldoublex, 16)

V2BSA_FUN_C_1(cbool)
V2BSA_FUN_C_1(cchar)
V2BSA_FUN_C_1(cschar)
V2BSA_FUN_C_1(cuchar)
V2BSA_FUN_C_1(cshort)
V2BSA_FUN_C_1(cushort)
V2BSA_FUN_C_1(cint)
V2BSA_FUN_C_1(cuint)
V2BSA_FUN_C_1(clong)
V2BSA_FUN_C_1(culong)
V2BSA_FUN_C_1(cllong)
V2BSA_FUN_C_1(cullong)
V2BSA_FUN_C_1(cfloat)
V2BSA_FUN_C_1(cdouble)
V2BSA_FUN_C_1(cldouble)
V2BSA_FUN_C_1(cfloatx)
V2BSA_FUN_C_1(cdoublex)
V2BSA_FUN_C_1(cldoublex)
')
