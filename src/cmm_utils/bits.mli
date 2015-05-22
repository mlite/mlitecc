# 42 "bits.nw"
type width = int

type bits      
type t = bits 

val width : t -> width         (* observer *)
val zero  : width -> t         (* constructor *)
val is_zero : t -> bool
# 55 "bits.nw"
val compare : t -> t -> int
val eq      : t -> t -> bool
# 66 "bits.nw"
exception Overflow
module S: sig  (* signed *) 
    
# 140 "bits.nw"
val of_int:     int         -> width -> t   (* raises Overflow *)
val of_native:  nativeint   -> width -> t   (* raises Overflow *)
val of_int32:   int32       -> width -> t   (* raises Overflow *)
val of_int64:   int64       -> width -> t   (* raises Overflow *)
val of_string:  string      -> width -> t   (* raises Overflow *)
# 163 "bits.nw"
val to_int:     t -> int                        (* Overflow *)
val to_native:  t -> nativeint                  (* Overflow *)
val to_int64:   t -> int64                      (* Overflow *)
# 170 "bits.nw"
val fits : width -> t -> bool    (* fits w b == (b = sxlo w b) *)
# 69 "bits.nw"
end
module U: sig (* unsigned *)
    
# 191 "bits.nw"
val of_int:     int         -> width -> t       
val of_native:  nativeint   -> width -> t       
val of_int64:   int64       -> width -> t       
val of_int32:   int32       -> width -> t       
val of_string:  string      -> width -> t   (* raises Overflow *)
# 223 "bits.nw"
val to_int:     t -> int                        (* Overflow *)
val to_native:  t -> nativeint                  (* Overflow *)
val to_int64:   t -> int64                      (* Overflow *)
# 230 "bits.nw"
val fits : width -> t -> bool    (* fits w b == (b = zxlo w b) *)
# 72 "bits.nw"
end
# 86 "bits.nw"
val to_string         : bits -> string
val to_decimal_string : bits -> string
val to_hex_or_decimal_string : declimit:int -> bits -> string
# 101 "bits.nw"
module Ops : sig
  val add       : bits -> bits -> bits
  val and'      : bits -> bits -> bits
  val com       : bits -> bits 
  val divu      : bits -> bits -> bits
  val mul       : bits -> bits -> bits
  val neg       : bits -> bits
  val or'       : bits -> bits -> bits
  val sub       : bits -> bits -> bits
  val shra      : bits -> bits -> bits
  val shrl      : bits -> bits -> bits
  val shl       : bits -> bits -> bits
  val xor       : bits -> bits -> bits

  val sx        : int  -> bits -> bits
  val zx        : int  -> bits -> bits
  val lobits    : int  -> bits -> bits
  
  val eq        : bits -> bits -> bool
  val ne        : bits -> bits -> bool
  val lt        : bits -> bits -> bool
  val gt        : bits -> bits -> bool
  val ltu       : bits -> bits -> bool
  val gtu       : bits -> bits -> bool
end
