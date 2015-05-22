type 'c op = string * 'c list * 'c        (* op, arguments, result *)
val nonbool  : int:'c -> fp:'c -> rm:'c -> overrides:'c op list -> 'c op list
val full     : int:'c -> fp:'c -> rm:'c -> bool:'c -> overrides:'c op list -> 'c op list
type t = (Talloc.Multiple.t -> int -> Register.t) * (Register.t -> bool)
val of_space  : Space.t      -> t
val of_spaces : Space.t list -> t
val functions : t op list -> (Rtl.Private.opr -> t list) * (Rtl.Private.opr -> t)
