val rtl:    Rtl.rtl -> Rtl.rtl
val exp:    Rtl.exp -> Rtl.exp
val bits:   Rtl.exp -> Bits.bits    (* Error.ErrorExn, convenient function *)
val link:   Rtl.exp -> Reloc.t
val bool:   Rtl.exp -> bool         (* Error.ErrorExn, convenient function *)
module Unsafe: sig
    val rtl:    Rtl.rtl -> Rtl.rtl
end
val compile_time_ops: string list
