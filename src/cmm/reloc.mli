type symbol = Symbol.t * (Symbol.t -> Rtl.width -> Rtl.exp)
type t

(* constructors *)
val of_const : Bits.bits -> t
val of_sym   : symbol -> Rtl.width -> t
val add :  t -> t -> t
val sub :  t -> t -> t

(* observers *)
val fold : const:(Bits.bits -> 'a) -> sym:(symbol -> 'a) ->
           add:('a -> 'a -> 'a) -> sub:('a -> 'a -> 'a) -> t -> 'a

val width : t -> Rtl.width
val if_bare : t -> Bits.bits option (* if not a bare value, returns None *)
val as_simple : t -> Symbol.t option * Bits.bits
   (* checked RTE if not simple *)
