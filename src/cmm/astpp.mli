val decl       : bool -> Cmm_ast.decl  -> Pp.doc
val stmt       : Cmm_ast.stmt          -> Pp.doc
val program    : Cmm_ast.toplevel list -> Pp.doc

val emit       : out_channel -> width:int -> Cmm_ast.toplevel list -> unit
