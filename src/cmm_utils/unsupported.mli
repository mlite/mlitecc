# 10 "unsupported.nw"
exception Unsupported
val explain : int -> int list -> unit

  (* Each function below always raises Unsupported *)

val widen_float : int -> 'a
val stack_width : have:int -> want:int -> 'a
val calling_convention : string -> 'a
val automaton_widths : int -> 'a
val automaton_widen  : have:int -> want:int -> 'a
val div_and_mod : unit -> 'a
val div_overflows : unit -> 'a
val floatlit : int -> 'a
val mulx_and_mulux : unit -> 'a
val singlebit : op:string -> 'a
val popcnt : notok:int -> ok:int -> 'a
