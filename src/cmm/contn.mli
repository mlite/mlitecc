type t

val init_code     : t -> Mflow.cut_args -> Rtl.rtl
val rep           : t -> Block.t     (* entire cont in memory; rep is address *)
val with_overflow : ('a, 'b, 'c) Target.t -> overflow:Block.t -> t
val cut_args    : ('a, 'b, 'c) Target.t -> contn:Rtl.exp -> Mflow.cut_args
val ovblock_exp : Rtl.exp -> int -> int -> int -> Rtl.exp
val get_contn   : Rtl.exp * Rtl.exp -> Rtl.exp
