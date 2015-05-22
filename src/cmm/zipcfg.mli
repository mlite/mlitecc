type uid = Unique.uid
type label = uid * string

type regs  = Register.SetX.t  (* sets of regs for dataflow *)
type contedge = { kills:regs; defs:regs; node:label; assertion:Rtl.rtl }

module Rep : sig
  type 'a edgelist = 'a list (* could be array *)
  type call = 
      {         cal_i          : Rtl.rtl
      ;         cal_contedges  : contedge edgelist
      ;         cal_spans      : Spans.t option
      ; mutable cal_uses       : regs
      ;         cal_altrets    : int
      ;         cal_unwinds_to : int
      ;         cal_cuts_to    : int
      ;         cal_reads      : string list option
      ;         cal_writes     : string list option
      }
	
  type labelkind = 
    | Userlabel  (* user-written; cannot be deleted *)
    | Genlabel   (* generated; can be deleted *)
    | Limitlabel (* generated after limit check; cannot be deleted *)

  type first = 
    | Entry
    | Label of label * labelkind * Spans.t option ref

  type middle = 
    | Instruction  of Rtl.rtl
    | Stack_adjust of Rtl.rtl

  type last = 
    | Exit
    | Branch  of Rtl.rtl * label
    | Cbranch of Rtl.rtl * label * label    (* true, false *)
    | Mbranch of Rtl.rtl * label edgelist (* possible successors *)
    | Call    of call  
    | Cut     of Rtl.rtl * contedge edgelist * regs  (* out edges, registers used *)
    | Return  of exit_num * Rtl.rtl * regs
    | Jump    of Rtl.rtl * regs * string list  (* inst, registers used, targets *)
    | Forbidden of Rtl.rtl (* cause a run-time error *)
	
  and exit_num = int
  type head = First of first | Head of head * middle
  type tail = Last  of last  | Tail of middle * tail
  type zblock = head * tail
  type block = first * tail
      
  val id  : block -> uid
  val blocklabel : block -> label option (* entry block has no label *)
  val blockkind  : block -> labelkind option (* entry block has no kind  *)
  val fid : first -> uid
  val entry_uid : uid

  val zip   : zblock -> block
  val unzip : block  -> zblock

  val first      : zblock -> first
  val last       : zblock -> last
  val goto_start : zblock -> first * tail
  val goto_end   : zblock -> head  * last
  val ht_to_first : head -> tail -> first * tail
  val ht_to_last  : head -> tail -> head  * last
  val zipht       : head -> tail -> block
  val succs : last -> uid list
  val fold_succs : (uid -> 'a -> 'a) -> last -> 'a -> 'a
  val iter_succs : (uid -> unit) -> last -> unit
  val mid_instr  : middle -> Rtl.rtl
  val last_instr : last -> Rtl.rtl  (* may be nop for, e.g., [[Exit]] *)
  val is_executable : middle -> bool
  val fold_fwd_block :
    (first -> 'a -> 'a) -> (middle -> 'a -> 'a) -> (last -> 'a -> 'a) ->
    block -> 'a -> 'a
end

type graph
type zgraph

val empty   : graph
val entry   : graph -> zgraph           (* focus on edge out of entry node *)
val exit    : graph -> zgraph           (* focus on edge into default exit node *)
val focus   : uid -> graph -> zgraph    (* focus on edge out of node with uid *)
val unfocus : zgraph -> graph           (* lose focus *)
val splice_focus_entry : zgraph -> graph -> zgraph
val splice_focus_exit  : zgraph -> graph -> zgraph
val add_blocks : graph -> zgraph -> graph
val uid : unit -> uid
type exp_of_lbl = label -> Rtl.exp (* exp of code label *)
type 'a machine = 'a * 'a Mflow.machine * exp_of_lbl (* useful pairing *)
type nodes      = zgraph -> zgraph (* sequence of nodes in Hughes's representation *)
type cbranch    = ifso:label -> ifnot:label -> nodes (* ability to branch conditionally *)

val label        : 'a machine -> label -> nodes  (* spans? *)
val instruction  : Rtl.rtl -> nodes
val stack_adjust : Rtl.rtl -> nodes
val branch       : 'a machine -> label -> nodes
val jump         : 'a machine -> Rtl.exp -> uses:regs -> targets:string list -> nodes
val cbranch      : 'a machine -> Rtl.exp -> cbranch
val mbranch      : 'a machine -> Rtl.exp -> targets:label list -> nodes
val call         : 'a machine -> Rtl.exp -> altrets:contedge list -> 
  unwinds_to:contedge list -> cuts_to:contedge list ->
  aborts:bool -> uses:regs -> defs:regs -> kills:regs ->
  reads:string list option -> writes:string list option ->
  spans:Spans.t option -> succ_assn:Rtl.rtl -> nodes
  
val cut_to       : 'a machine -> Mflow.cut_args -> cuts_to:contedge list ->
  aborts:bool -> uses:regs -> nodes
  
val return       : Rtl.rtl -> exit:int -> uses:regs -> nodes
val forbidden    : 'a machine -> nodes
  (* control should not reach; causes checked RTE *)
val if_then_else : 'a machine -> cbranch -> t:nodes -> f:nodes -> nodes
val while_do     : 'a machine -> cbranch -> body:nodes -> nodes
val limitcheck : 'a machine -> cbranch -> t:nodes -> nodes
val make_target : 'a machine -> zgraph -> label * zgraph
val set_spans : zgraph -> Spans.t -> unit  (* set spans at node preceding focus *)
val single_middle : Rep.middle -> graph
val single_last   : Rep.last   -> graph
val splice_head : Rep.head -> graph -> graph * Rep.head
val splice_tail : graph -> Rep.tail -> Rep.tail * graph
val splice_head_only : Rep.head -> graph -> graph
val remove_entry : graph -> Rep.tail * graph
val to_blocks : graph -> Rep.block Unique.Map.t
val of_blocks : Rep.block Unique.Map.t -> graph  (* cheap *)
val of_block_list : Rep.block list -> graph  (* expensive *)
val openz : zgraph -> Rep.zblock * Rep.block Unique.Map.t
val tozgraph : Rep.zblock * Rep.block Unique.Map.t -> zgraph
val postorder_dfs : graph -> Rep.block list
val fold_layout : (Rep.block -> label option -> 'a -> 'a) -> 'a -> graph -> 'a
val fold_blocks : (Rep.block -> 'a -> 'a) -> 'a -> graph -> 'a
val iter_blocks : (Rep.block -> unit) -> graph -> unit
val expand : (Rep.middle -> graph) -> (Rep.last -> graph) -> graph -> graph
val iter_spans : (Spans.t -> unit) -> graph -> unit
val fold_spans : (Spans.t -> 'a -> 'a) -> graph -> 'a -> 'a
val iter_nodes :
  (Rep.first -> unit) -> (Rep.middle -> unit) -> (Rep.last -> unit) -> graph -> unit
  
val iter_rtls : (Rtl.rtl -> unit) -> graph -> unit
val map_rtls : (Rtl.rtl -> Rtl.rtl) -> graph -> graph
val map_nodes :
  (Rep.first -> Rep.first) -> (Rep.middle -> Rep.middle) -> (Rep.last -> Rep.last) ->
  graph -> graph

val new_rtlm : Rtl.rtl -> Rep.middle -> Rep.middle
val new_rtll : Rtl.rtl -> Rep.last   -> Rep.last
val map_rtlm : (Rtl.rtl -> Rtl.rtl) -> Rep.middle -> Rep.middle
val map_rtll :
  map_rtl:(Rtl.rtl -> Rtl.rtl) -> map_assn:(Rtl.rtl -> Rtl.rtl) -> Rep.last -> Rep.last

val union_over_outedges :
  Rep.last -> noflow:(uid -> regs) -> flow:(contedge -> regs) -> regs

val iter_outedges :
  Rep.last -> noflow:(uid -> unit) -> flow:(contedge -> unit) -> unit
  
val add_inedge_uses : Rep.last  -> regs -> regs
val add_live_spansl : Rep.last  -> regs -> regs
val add_live_spansf : Rep.first -> regs -> regs
val block_before : 'a machine -> (Rtl.exp -> Rtl.rtl) Dag.block -> zgraph ->
  (zgraph * bool)

val block2cfg    : 'a machine -> (Rtl.exp -> Rtl.rtl) Dag.block -> (zgraph * bool)
  
val cbranch2cfg  : 'a machine -> (Rtl.exp -> Rtl.rtl) Dag.cbranch ->
  ifso:label -> ifnot:label -> zgraph -> (zgraph * bool)
