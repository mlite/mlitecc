type 'a ctypes = 
    { char               : 'a
    ; double             : 'a
    ; float              : 'a
    ; int                : 'a
    ; long_double        : 'a
    ; long_int           : 'a
    ; long_long_int      : 'a
    ; short              : 'a
    ; signed_char        : 'a
    ; unsigned_char      : 'a
    ; unsigned_long      : 'a
    ; unsigned_short     : 'a
    ; unsigned_int       : 'a
    ; unsigned_long_long : 'a
    ; address            : 'a
    }
      
type width   = int
    
type metrics = { w : width; va_w : width}
    
module SM = Strutil.Map
  
let sprintf = Printf.sprintf
  
let fetch_ct ct str = 
  match str with
    | "char"               -> ct.char
    | "double"             -> ct.double
    | "float"              -> ct.float
    | ""                   -> ct.int 
    | "int"                -> ct.int 
    | "long double"        -> ct.long_double
    | "long int"           -> ct.long_int
    | "long long int"      -> ct.long_long_int
    | "short"              -> ct.short
    | "signed char"        -> ct.signed_char
    | "unsigned char"      -> ct.unsigned_char
    | "unsigned long"      -> ct.unsigned_long
    | "unsigned short"     -> ct.unsigned_short
    | "unsigned int"       -> ct.unsigned_int
    | "unsigned long long" -> ct.unsigned_long_long
    | "address"            -> ct.address
    | s -> Impossible.impossible (sprintf "Unexpected C type %s\n" s)

let ct_foldi f ct z = 
  let f str = f str (fetch_ct ct str) 
  in
  f "char" (f "double" (f "float" (f "int" (f "long double"
    (f "long int" (f "long long int" (f "short" (f "signed char"
      (f "unsigned char" (f "unsigned long" (f "unsigned short"
	(f "unsigned int" (f "unsigned long long" (f "address" z))))))))))))))
    
let ct_fold f = ct_foldi (fun _ x -> f x)
  
let x86_ctypes =
  { char               = {w = 8 ; va_w = 32}
  ; double             = {w = 64; va_w = 64}
  ; float              = {w = 32; va_w = 64}
  ; int                = {w = 32; va_w = 32}
  ; long_double        = {w = 96; va_w = 96}
  ; long_int           = {w = 32; va_w = 32}
  ; long_long_int      = {w = 64; va_w = 64}
  ; short              = {w = 16; va_w = 32}
  ; signed_char        = {w = 8 ; va_w = 32}
  ; unsigned_char      = {w = 8 ; va_w = 32}
  ; unsigned_long      = {w = 32; va_w = 32}
  ; unsigned_short     = {w = 16; va_w = 32}
  ; unsigned_int       = {w = 32; va_w = 32}
  ; unsigned_long_long = {w = 64; va_w = 64}
  ; address            = {w = 32; va_w = 32}
  }
    
let enum_ct =
  { char               = ("CHAR"            , 0)
  ; double             = ("DOUBLE"          , 1)
  ; float              = ("FLOAT"           , 2)
  ; int                = ("INT"             , 3)
  ; long_double        = ("LONGDOUBLE"      , 4)
  ; long_int           = ("LONGINT"         , 5)
  ; long_long_int      = ("LONGLONGINT"     , 6)
  ; short              = ("SHORT"           , 7)
  ; signed_char        = ("SIGNEDCHAR"      , 8)
  ; unsigned_char      = ("UNSIGNEDCHAR"    , 9)
  ; unsigned_long      = ("UNSIGNEDLONG"    , 10)
  ; unsigned_short     = ("UNSIGNEDSHORT"   , 11)
  ; unsigned_int       = ("UNSIGNEDINT"     , 12)
  ; unsigned_long_long = ("UNSIGNEDLONGLONG", 13)
  ; address            = ("ADDRESS",          14)
  }
    
let ctypes_vararg_enum ct =
  let str_lst = ct_fold (fun (s,id) rst -> (sprintf "%s = %d" s id)::rst)
  enum_ct [] 
  in
  Printf.sprintf "{ %s }" (String.concat ", " str_lst)
    
let enum_int h = snd (fetch_ct enum_ct h)
