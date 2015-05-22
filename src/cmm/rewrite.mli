type width = Rtl.width
type exp   = Rtl.exp
type loc   = Rtl.loc
type block = exp Postexpander.block
type temp  = Register.t
module Ops : sig
  val _NaN : width -> width -> exp -> exp
  val add : width -> exp -> exp -> exp
  val addc : width -> exp -> exp -> exp -> exp
  val add_overflows : width -> exp -> exp -> exp
  val _and : width -> exp -> exp -> exp
  val bit : exp -> exp
  val bool : exp -> exp
  val borrow : width -> exp -> exp -> exp -> exp
  val carry : width -> exp -> exp -> exp -> exp
  val com : width -> exp -> exp
  val conjoin : exp -> exp -> exp
  val disjoin : exp -> exp -> exp
  val div : width -> exp -> exp -> exp
  val div_overflows : width -> exp -> exp -> exp
  val divu : width -> exp -> exp -> exp
  val eq : width -> exp -> exp -> exp
  val f2f : width -> width -> exp -> exp -> exp
  val f2f_implicit_round : width -> width -> exp -> exp
  val f2i : width -> width -> exp -> exp -> exp
  val fabs : width -> exp -> exp
  val fadd : width -> exp -> exp -> exp -> exp
  val fcmp : width -> exp -> exp -> exp
  val fdiv : width -> exp -> exp -> exp -> exp
  val feq : width -> exp -> exp -> exp
  val fge : width -> exp -> exp -> exp
  val fgt : width -> exp -> exp -> exp
  val fle : width -> exp -> exp -> exp
  val float_eq : exp
  val float_gt : exp
  val float_lt : exp
  val flt : width -> exp -> exp -> exp
  val fmul : width -> exp -> exp -> exp -> exp
  val fmulx : width -> exp -> exp -> exp
  val fne : width -> exp -> exp -> exp
  val fneg : width -> exp -> exp
  val fordered : width -> exp -> exp -> exp
  val fsqrt : width -> exp -> exp -> exp
  val fsub : width -> exp -> exp -> exp -> exp
  val funordered : width -> exp -> exp -> exp
  val ge : width -> exp -> exp -> exp
  val geu : width -> exp -> exp -> exp
  val gt : width -> exp -> exp -> exp
  val gtu : width -> exp -> exp -> exp
  val i2f : width -> width -> exp -> exp -> exp
  val le : width -> exp -> exp -> exp
  val leu : width -> exp -> exp -> exp
  val lobits : width -> width -> exp -> exp
  val lt : width -> exp -> exp -> exp
  val ltu : width -> exp -> exp -> exp
  val minf : width -> exp
  val ( mod ) : width -> exp -> exp -> exp
  val modu : width -> exp -> exp -> exp
  val mul : width -> exp -> exp -> exp
  val mulux : width -> exp -> exp -> exp
  val mulx : width -> exp -> exp -> exp
  val mul_overflows : width -> exp -> exp -> exp
  val mulu_overflows : width -> exp -> exp -> exp
  val mzero : width -> exp
  val ne : width -> exp -> exp -> exp
  val neg : width -> exp -> exp
  val not : exp -> exp
  val ( or ) : width -> exp -> exp -> exp
  val pinf : width -> exp
  val popcnt : width -> exp -> exp
  val pzero : width -> exp
  val quot : width -> exp -> exp -> exp
  val quot_overflows : width -> exp -> exp -> exp
  val rem : width -> exp -> exp -> exp
  val rotl : width -> exp -> exp -> exp
  val rotr : width -> exp -> exp -> exp
  val round_down : exp
  val round_nearest : exp
  val round_up : exp
  val round_zero : exp
  val shl : width -> exp -> exp -> exp
  val shra : width -> exp -> exp -> exp
  val shrl : width -> exp -> exp -> exp
  val sub : width -> exp -> exp -> exp
  val subb : width -> exp -> exp -> exp -> exp
  val sub_overflows : width -> exp -> exp -> exp
  val sx : width -> width -> exp -> exp
  val unordered : exp
  val xor : width -> exp -> exp -> exp
  val zx : width -> width -> exp -> exp
  val signed   : width -> int -> exp
  val unsigned : width -> int -> exp
end
val div' : width -> dst:loc -> exp -> exp -> quot:exp -> rem:exp -> block
  (* uses: comparisons, add, sub *)

val div2 : width -> dst:loc -> exp -> exp -> block
  (* always 2 conditional branches *)

val div1_3 : width -> dst:loc -> exp -> exp -> block
  (* 1--3 conditional branches *)
val div_overflows : width -> exp -> exp -> exp
  (* uses: conjoin, eq *)

val (mod) : width -> exp -> exp -> exp
  (* uses : div, mul, sub *)

val modu : width -> exp -> exp -> exp
  (* uses : divu, mul, sub *)

val rem  : width -> exp -> exp -> exp
  (* uses : quot, mul, sub *)
val popcnt : width -> dst:loc -> exp -> block (* for 32 bits only *)
val sxlo : width -> width -> exp -> exp
  (* uses : shl, shra *)
val zxlo : width -> width -> exp -> exp
  (* uses : shl, shrl *)
val regpair : hi:exp -> lo:exp -> exp
val slice : width -> width -> lsb:int -> exp -> exp
type 'a pair = { hi : 'a; lo : 'a }
val splits : width -> lsb:int -> exp -> exp pair
val splitu : width -> lsb:int -> exp -> exp pair
