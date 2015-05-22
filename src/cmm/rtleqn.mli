exception Can'tSolve

type t              (* equation *)
type solution =
    { known:        (string * Rtl.exp) list  (* fully solved *)
    ; dependent:    (string * Rtl.exp) list  (* depend on other variables *)
    }

val equate : Rtl.exp -> Rtl.exp -> t   (* e1 == e2 *)

val solve  : width:int -> t list -> solution   (* Can'tSolve *)
val to_string : t -> string (* for debugging *)
