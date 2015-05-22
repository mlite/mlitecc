# 55 "uint64.nw"
(* external of_int:     int   -> int64                  = "uint64_i2i" *)
# 61 "uint64.nw"
external cmp:        int64 -> int64 -> int           = "uint64_compare"
external add:        int64 -> int64 -> int64         = "uint64_add"
external sub:        int64 -> int64 -> int64         = "uint64_sub"
external mul:        int64 -> int64 -> int64         = "uint64_mul"
external div:        int64 -> int64 -> int64         = "uint64_div"
external modu:       int64 -> int64 -> int64         = "uint64_mod"
# 87 "uint64.nw"
external of_string:  string -> int64                 = "uint64_of_string"
# 91 "uint64.nw"
val eq:              int64 -> int64 -> bool     (* equal         *)
val lt:              int64 -> int64 -> bool     (* less than     *)
val gt:              int64 -> int64 -> bool     (* greather than *)
val le:              int64 -> int64 -> bool     (* less equal    *)
val ge:              int64 -> int64 -> bool     (* greater equal *)
# 99 "uint64.nw"
val shl:             int -> int64 -> int64      (* shift left  *)
val shr:             int -> int64 -> int64      (* shift right *)
# 47 "uint64.nw"
module Cast : sig
 external float64 :   float -> int64 = "uint64_float64"
 external float32 :   float -> int64 = "uint64_float32"
end
