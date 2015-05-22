val add_operator : name:string -> result_is_float:bool -> Types.tyscheme -> unit
val mono : Rtl.opr -> Types.monotype      (* Not_found *)
val has_floating_result : Rtl.opr -> bool
val fold : (string -> Types.tyscheme -> 'a -> 'a) -> 'a -> 'a
val print_shapes : unit -> unit
val opnames : unit -> string list  (* names of all source-language operators *)
module Translate : sig
  val prefix : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
  val binary : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
  val unary  : string -> Types.ty list -> Types.ty * Rtl.opr (*ErrorExn*)
end
module Emit : sig
  val creators : unit -> unit
end
