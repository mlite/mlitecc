type generator = string -> string
val label:      generator       (* denotes pc value *)
val offset:     generator       (* offset in stack       *)
val exit:       generator       (* label for exit node in procedure *)
val cont:       generator       (* symbol for dynamic continuation value *)
val slot:       generator       (* slot address on stack *)
val block:      generator       (* offset in Lua-generated Block.t address *)

module ContEntry : sig
  val cut    : generator       (* label for cut-to entry point *)
  val unwind : generator       (* label for unwind entry point *)
  val return : generator       (* label for alternate-return entry point *)
end
(* add more as needed *)
