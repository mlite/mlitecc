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

val cval_of_string : string -> cval_tag -> cval
val string_of_cval : cval -> string 
val ( +$ ) : cval -> cval -> cval
val ( -$ ) : cval -> cval -> cval
val ( *$ ) : cval -> cval -> cval
val ( /$ ) : cval -> cval -> cval
val ( <<$) : cval -> cval -> cval
val ( >>$) : cval -> cval -> cval
val ( %$ ) : cval -> cval -> cval
val ( ^$ ) : cval -> cval -> cval
val ( &$ ) : cval -> cval -> cval
val ( |$ ) : cval -> cval -> cval
val ( ~$ ) : cval -> cval
val neg : cval -> cval


val ( =$ ) : cval -> cval -> bool
val ( <>$) : cval -> cval -> bool
val ( <$ ) : cval -> cval -> bool
val ( <=$ ): cval -> cval -> bool
val ( >$ ): cval -> cval -> bool
val ( >=$ ): cval -> cval -> bool
