type t

val base:           t -> Rtl.exp
val size:           t -> int
val alignment:      t -> int
val constraints:    t -> Rtleqn.t list
val at:                 base:Rtl.exp -> size:int -> alignment:int -> t
val with_constraint:    t -> Rtleqn.t -> t
val relative  : Rtl.exp -> string -> (base:Rtl.exp  -> 'a) -> 'a
  (* construct address at unknown offset and call function *)
val srelative : Rtl.exp -> string -> (start:Rtl.exp -> 'a) -> 'a
  (* construct address at unknown offset and call function *)
exception OverlapHigh
type placement      = High | Low
val cathl:          t -> t -> t
val overlap:        placement -> t -> t -> t   (* OverlapHigh *)
val adjust:         t -> t                  (* size is multiple of alignment *)
val cathl_list:     Rtl.width -> t list -> t
val overlap_list:   Rtl.width -> placement -> t list -> t

(*
module Lua: sig
  val relative:   t -> string -> int (*size*) -> int (* align *) -> t
    (* observer *)
  val base:       t -> string         (* show base address *)
  val constraints:t -> string list    (* for debugging *)
  val size:       t -> int
  val alignment:  t -> int
    (* operations *)
  val adjust:     t -> t
  val cat:        Rtl.width -> t list -> t
  val overlap:    Rtl.width -> placement (* "high"|"low"*) -> t list -> t   
    
  val eq : t -> t -> bool
end
*)
