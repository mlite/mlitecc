module Single : sig
  type t       (* an allocator for one space -- a mutable type *)

  val for_space : Space.t -> t            (* a fresh allocator *)
  val reg : t -> (*width*) int -> Register.t
  val loc : t -> (*width*) int -> Rtl.loc
end
module Multiple : sig
  type t       (* an allocator for multiple spaces -- a mutable type *)

  val for_spaces : Space.t list -> t            (* a fresh allocator *)
  val reg : char -> t -> (*width*) int -> Register.t
  val loc : t -> char -> (*width*) int -> Rtl.loc
  val reg_like : t -> Register.t -> Register.t
end
