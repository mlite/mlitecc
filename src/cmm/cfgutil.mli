val cfg2dot : compress:bool -> live:bool -> name:string -> Zipcfg.graph -> string
val cfg2ast : (Rtl.rtl -> Cmm_ast.stmt) -> Zipcfg.graph -> name:string -> Cmm_ast.proc
val emit    : Ast2ir.basic_proc -> Zipcfg.graph ->
                (Zipcfg.Rep.call -> unit) -> (Rtl.rtl -> unit) ->
                (string -> unit) -> unit
val block_name : Zipcfg.graph -> Zipcfg.uid -> string
val print_cfg: Zipcfg.graph -> unit
val pr_first : Zipcfg.Rep.first  -> unit
val pr_mid   : Zipcfg.Rep.middle -> unit
val pr_last  : Zipcfg.graph -> Zipcfg.Rep.last -> unit
val pr_last' : Zipcfg.Rep.last -> unit

type node = F of Zipcfg.Rep.first | M of Zipcfg.Rep.middle | L of Zipcfg.Rep.last
val numbered_layout_nodes : Zipcfg.graph -> (int * node) list
(* val delete : Zipcfg.zgraph -> Zipcfg.graph --- delete focus *)
