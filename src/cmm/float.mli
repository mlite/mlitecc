type t

val name      : t -> string
val of_string : t -> string -> Rtl.width -> Bits.bits

val ieee754 : t    (* standard IEEE 754 semantics *)
val none    : t    (* for machines without floating-point support *)
