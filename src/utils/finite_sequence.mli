module type OrderedType =
  sig
    type t
      (** The type of the set elements. *)
    val compare : t -> t -> int
      (** A total ordering function over the set elements.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the elements [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)

    val pp_print: Format.formatter -> t -> unit
  end

module type S =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val create: unit -> t

    val add:t -> elt -> unit
    (** [add x s] returns a set containing all elements of [s],
        plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
	
    val emit: t -> elt list
  end

module Make (Ord : OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
   given a totally ordered type. *)
