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

open C_arithm

type cval_tag =
  | Cchar
  | Cschar
  | Cuchar
  | Cshort
  | Cushort
  | Cint
  | Cuint
  | Clong
  | Culong
  | Cllong
  | Cullong
  | Cfloat
  | Cdouble
  | Cldouble
  | Cbool

let cval_of_string str tag =
  match tag with
    | Cchar -> (cchar_cval_of_string str)
    | Cschar -> (cschar_cval_of_string str)
    | Cuchar -> (cuchar_cval_of_string str)
    | Cshort -> (cshort_cval_of_string str)
    | Cushort -> (cushort_cval_of_string str)
    | Cint -> (cint_cval_of_string str)
    | Cuint -> (cuint_cval_of_string str)
    | Clong -> (clong_cval_of_string str)
    | Culong -> (culong_cval_of_string str)
    | Cllong -> (cllong_cval_of_string str)
    | Cullong -> (cullong_cval_of_string str)
    | Cfloat -> (cfloat_cval_of_string str)
    | Cdouble -> (cdouble_cval_of_string str)
    | Cldouble -> (cldouble_cval_of_string str)
    | Cbool -> (cbool_cval_of_string str)
      
let string_of_cval = function
  | CCHAR v -> string_of_cchar v
  | CSCHAR v -> string_of_cschar v
  | CUCHAR v -> string_of_cuchar v
  | CSHORT v -> string_of_cshort v
  | CUSHORT v -> string_of_cushort v
  | CINT v -> string_of_cint v
  | CUINT v -> string_of_cuint v
  | CLONG v -> string_of_clong v
  | CULONG v -> string_of_culong v
  | CLLONG v -> string_of_cllong v
  | CULLONG v -> string_of_cullong v
  | CFLOAT v -> string_of_cfloat v
  | CDOUBLE v -> string_of_cdouble v
  | CLDOUBLE v -> string_of_cldouble v
  | CBOOL v -> string_of_cbool v


let ( +$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval ADD v1 v2 

let ( -$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval SUB v1 v2 

let ( *$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval MUL v1 v2 
    
let ( /$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval DIV v1 v2 
    
let ( <<$) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval SHL v1 v2 
      
let ( >>$) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval SHR v1 v2 
      
let ( %$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval MOD v1 v2 
      
let ( ^$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval BXOR v1 v2 
    
let ( &$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval BAND v1 v2 
    
let ( |$ ) : cval -> cval -> cval =
  fun v1 v2 -> bin_cval BOR v1 v2 
    
let ( ~$ ) : cval -> cval =
  fun v1 -> una_cval BNOT v1
    
let neg : cval -> cval =
  fun v1 -> una_cval NEG v1

let ( =$ ) : cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval EQ v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false
	  
      
let ( <>$) : cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval NE v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false

let ( <$ ) : cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval LT v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false
	  
let ( <=$ ): cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval LE v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false
    
let ( >$ ): cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval GT v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false
    
let ( >=$ ): cval -> cval -> bool =
  fun v1 v2 -> 
    let b = rel_cval GE v1 v2 
    in
    match b with
      | CINT c -> cint_is_true c
      | _ -> assert false

let id v -> v
  
