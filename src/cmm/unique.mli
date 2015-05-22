type uid
val eq : uid -> uid -> bool
val uid : unit -> uid
val distinguished_uid : uid    (* distinct from any other *)
module type MAP = sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem   : uid -> 'a t -> bool
  val add   : uid -> 'a -> 'a t -> 'a t
  val find  : uid -> 'a t -> 'a 
  val split : uid -> 'a t -> 'a * 'a t
    (* split k m = (find k m, remove k m) *)
  val splitp: (uid -> 'a -> bool) -> 'a t -> 'a * 'a t
    (* split based on predicate *)
  val union : 'a t -> 'a t -> 'a t          (* keep larger set on the right *)
  val fold  : (uid -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter  : ('a -> unit) -> 'a t -> unit
  val size  : 'a t -> int

  val map   : ('a -> 'b) -> ('a t -> 'b t)
end
module type SET = sig
  type t
  val empty : t
  val mem : uid -> t -> bool
  val add : uid -> t -> t
end
module type ARRAY = sig
  type 'a t
  val make : uid list -> 'a -> 'a t
  val get  : 'a t -> uid -> 'a
  val set  : 'a t -> uid -> 'a -> unit
  val update : 'a t -> uid -> ('a -> 'a) -> unit
end
module Map : MAP
module Set : SET
module Array : ARRAY
module Prop : Property.S with type list = uid
