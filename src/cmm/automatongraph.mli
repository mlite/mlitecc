type ty = int * string * int (* width * kind * alignment *)
val print: mk:(unit -> Automaton.t) -> ty list -> unit
val paths: mk:(unit -> Automaton.t) -> ty list -> unit
val summary : what:string -> mk:(unit -> Automaton.t) -> ty list -> unit
