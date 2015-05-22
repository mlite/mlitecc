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

open Mach
open Const_folding

m4_define(AAA_1,
`
  begin
    if i1 = cchar_id then
      cval_$1_to_CCHAR
    else if i1 = cschar_id then
      cval_$1_to_CSCHAR
    else if i1 = cuchar_id then
      cval_$1_to_CUCHAR
    else if i1 = cshort_id then
      cval_$1_to_CSHORT
    else if i1 = cushort_id then
      cval_$1_to_CUSHORT
    else if i1 = cint_id then
      cval_$1_to_CINT
    else if i1 = cuint_id then
      cval_$1_to_CUINT
    else if i1 = clong_id then
      cval_$1_to_CLONG
    else if i1 = culong_id then
      cval_$1_to_CULONG
    else if i1 = cllong_id then
      cval_$1_to_CLLONG
    else if i1 = cullong_id then
      cval_$1_to_CULLONG
    else if i1 = cfloat_id then
      cval_$1_to_CFLOAT
    else if i1 = cdouble_id then
      cval_$1_to_CDOUBLE
    else if i1 = cldouble_id then
      cval_$1_to_CLDOUBLE
    else if i1 = cbool_id then
      cval_$1_to_CBOOL
    else if i1 = ccomplex_id then
      camlp4_macro_exception "unhandled teid %d" i1
    else if i1 = cfcomplex_id then
      camlp4_macro_exception "unhandled teid %d" i1
    else if i1 = cdcomplex_id then
      camlp4_macro_exception "unhandled teid %d" i1
    else if i1 = cldcomplex_id then
      camlp4_macro_exception "unhandled teid %d" i1
    else camlp4_macro_exception "unhandled teid %d" i1
  end
')

let get_of_string i0 =
  if i0 = cchar_id then
    cchar_cval_of_string
  else if i0 = cschar_id then
    cschar_cval_of_string
  else if i0 = cuchar_id then
    cuchar_cval_of_string
  else if i0 = cshort_id then
    cshort_cval_of_string
  else if i0 = cushort_id then
    cushort_cval_of_string
  else if i0 = cint_id then
    cint_cval_of_string
  else if i0 = cuint_id then
    cuint_cval_of_string
  else if i0 = clong_id then
    clong_cval_of_string
  else if i0 = culong_id then
    culong_cval_of_string
  else if i0 = cllong_id then
    cllong_cval_of_string
  else if i0 = cullong_id then
    cullong_cval_of_string
  else if i0 = cfloat_id then
    cfloat_cval_of_string
  else if i0 = cdouble_id then
    cdouble_cval_of_string
  else if i0 = cldouble_id then
    cldouble_cval_of_string
  else 
    camlp4_macro_exception "unhandled teid %d" i0



let get i0 i1 =
  if i0 = cchar_id then
    AAA_1(CCHAR)
  else if i0 = cschar_id then
    AAA_1(CSCHAR)
  else if i0 = cuchar_id then
    AAA_1(CUCHAR)
  else if i0 = cshort_id then
    AAA_1(CSHORT)
  else if i0 = cushort_id then
    AAA_1(CUSHORT)
  else if i0 = cint_id then
    AAA_1(CINT)
  else if i0 = cuint_id then
    AAA_1(CUINT)
  else if i0 = clong_id then
    AAA_1(CLONG)
  else if i0 = culong_id then
    AAA_1(CULONG)
  else if i0 = cllong_id then
    AAA_1(CLLONG)
  else if i0 = cullong_id then
    AAA_1(CULLONG)
  else if i0 = cfloat_id then
    AAA_1(CFLOAT)
  else if i0 = cdouble_id then
    AAA_1(CDOUBLE)
  else if i0 = cldouble_id then
    AAA_1(CLDOUBLE)
  else if i0 = cbool_id then
    AAA_1(CBOOL)
  else	  
    camlp4_macro_exception "unhandled teid %d" i0
