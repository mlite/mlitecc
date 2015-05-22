type temp      = Register.t
type rtl       = Rtl.rtl
type brtl      = Rtl.exp -> Rtl.rtl (* given expression, create branch rtl *)
type exp       = Rtl.exp
type address   = Rtl.exp
type width     = Rtl.width
type assertion = Rtl.assertion
type operator  = Rtl.Private.opr
type wtemp = fill * temp
and  fill  = HighS | HighZ | HighAny  
and  warg  = WBits of Bits.bits
           | WTemp of Register.x  (* high bits, if any, could contain anything *)
type 'a block   = 'a Dag.block
type 'a branch  = 'a Dag.branch
type 'a cbranch = 'a Dag.cbranch
type 'a cbinst  = 'a Dag.cbinst
type operator_class = Register | Stack of push * int
and  push = LeftFirst | RightFirst
val warg_val : warg -> exp
module Alloc : sig
  val temp : char -> width -> temp
  val slot : width:width -> aligned:int -> Automaton.loc
  val isValid : unit -> bool
end
module Expand : sig
  val block    : exp block   -> brtl block
  val branch   : exp branch  -> brtl branch
  val cbranch  : exp cbranch -> brtl cbranch
  val cbranch' : exp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
end
val with_hw : hard:Register.x -> soft:warg -> temp:temp -> brtl block -> brtl block
(*
val (<:>) : 'a block -> 'a block -> 'a block
*)
(*
val shared : 'a cbranch -> 'a cbranch
*)
(*
val cond : exp -> exp cbranch  (* branch taken iff exp true *)
*)
(*
type 'a nodeset
val empty : 'a nodeset
val lookup : uid -> 'a nodeset -> 'a   (* raises Not_found *)
val insert : uid -> 'a -> 'a nodeset -> 'a nodeset
*)
val remember_allocators : Talloc.Multiple.t -> Automaton.t -> unit
val forget_allocators   : unit -> unit
val remember_expanders : 
  (exp block -> brtl block) -> (exp branch -> brtl branch) ->
  (exp cbranch -> brtl cbranch) -> unit
val forget_expanders : unit -> unit
module type S = sig
  val byte_order : Rtl.aggregation (* used to check access to memory *)
  val exchange_alignment : int     (* alignment for an exchange slot *)
  val load  : dst:temp  -> addr:address -> assertion -> brtl block
  val store : addr:address -> src:temp  -> assertion -> brtl block
  val sxload  : dst:temp  -> addr:address -> width -> assertion -> brtl block
  val zxload  : dst:temp  -> addr:address -> width -> assertion -> brtl block
  val lostore : addr:address  -> src:temp -> width -> assertion -> brtl block
  val move : dst:temp -> src:temp -> brtl block
  val extract   : dst:temp -> lsb:int -> src:temp -> brtl block
  val aggregate : dst:temp -> src:temp list       -> brtl block (* little-endian *)
  val hwset : dst:Register.x -> src:warg       -> brtl block
  val hwget : dst:wtemp      -> src:Register.x -> brtl block
  val li  : dst:temp -> Rtl.Private.const -> brtl block
  val lix : dst:temp -> Rtl.exp           -> brtl block
  val block_copy :
    dst:address -> assertion -> src:address -> assertion -> width -> brtl block
  val unop  : dst:temp -> operator -> temp         -> brtl block
  val binop : dst:temp -> operator -> temp -> temp -> brtl block
  val unrm  : dst:temp -> operator -> temp         -> warg -> brtl block
  val binrm : dst:temp -> operator -> temp -> temp -> warg -> brtl block
  val dblop : dsthi:temp -> dstlo:temp -> operator -> temp -> temp -> brtl block
  val wrdop  : dst:temp  -> operator -> temp -> temp -> warg -> brtl block
  val wrdrop : dst:wtemp -> operator -> temp -> temp -> warg -> brtl block
  val icontext : Context.t (* for ints *)
  val fcontext : Context.t (* for floats *)
  val acontext : Context.t (* for addresses *)
  val constant_context : width    -> Context.t
  val arg_contexts     : operator -> Context.t list
  val result_context   : operator -> Context.t
  val itempwidth : int  (* maximum width for one integer temporary *)
  val pc_lhs : Rtl.loc                    (* program counter as assigned by branch *)
  val pc_rhs : Rtl.loc                    (* program counter as captured by call *)
  val br : tgt:temp -> brtl branch               (* branch register *)
  val b  : tgt:Rtl.Private.const -> brtl branch  (* branch *)
  val bc_guard    : temp -> operator -> temp -> brtl block * Rtl.exp
  val bc_of_guard : brtl block * Rtl.exp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch)
                      -> brtl cbranch
  (* Formerly:
  val bc : temp -> operator -> temp -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch)
                -> brtl cbranch
  *)
  val bnegate : Rtl.rtl -> Rtl.rtl
  val callr : tgt:temp              -> brtl branch
  val call  : tgt:Rtl.Private.const -> brtl branch
  val cut_to : Mflow.cut_args -> brtl branch
  val return    : Rtl.rtl
  val forbidden : Rtl.rtl   (* causes a run-time error *)
  val don't_touch_me : Rtl.Private.effect list -> bool
  val opclass : operator -> operator_class
  val stack_depth : int
  val stack_width : int
  val converts_stack_to_temp : operator -> bool
  val push      : addr:address -> assertion -> brtl block
  val store_pop : addr:address -> assertion -> brtl block
  val push_cvt      : operator -> width -> addr:address -> assertion -> brtl block
  val store_pop_cvt : operator -> width -> addr:address -> assertion -> brtl block
  val push_cvt_rm      : operator -> warg -> width -> addr:address -> assertion
                                  -> brtl block
  val store_pop_cvt_rm : operator -> warg -> width -> addr:address -> assertion
                                  -> brtl block
  module SlotTemp : sig
    val is               : temp -> bool
    val push             : temp -> brtl block
    val store_pop        : temp -> brtl block
    val push_cvt         : operator -> width -> temp -> brtl block
    val store_pop_cvt    : operator -> width -> temp -> brtl block
    val push_cvt_rm      : operator -> warg -> width -> temp -> brtl block
    val store_pop_cvt_rm : operator -> warg -> width -> temp -> brtl block
  end
  val pushk     :                      Rtl.Private.const -> brtl block
  val pushk_cvt : operator -> width -> Rtl.Private.const -> brtl block
  val stack_op    : operator         -> brtl block
  val stack_op_rm : operator -> warg -> brtl block
  val bc_stack : operator -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
  val stack_top_proxy    : Rtl.loc
  val is_stack_top_proxy : Rtl.Private.loc -> bool
end
module Nostack (Address : sig type t val reg : temp -> t end) : sig
 val opclass : operator -> operator_class
 val stack_depth : int
 val stack_width : int
 val converts_stack_to_temp : operator -> bool
 val push      : addr:address -> assertion -> brtl block
 val store_pop : addr:address -> assertion -> brtl block
 val push_cvt      : operator -> width -> addr:address -> assertion -> brtl block
 val store_pop_cvt : operator -> width -> addr:address -> assertion -> brtl block
 val push_cvt_rm      : operator -> warg -> width -> addr:address -> assertion
                                 -> brtl block
 val store_pop_cvt_rm : operator -> warg -> width -> addr:address -> assertion
                                 -> brtl block
 module SlotTemp : sig
   val is               : temp -> bool
   val push             : temp -> brtl block
   val store_pop        : temp -> brtl block
   val push_cvt         : operator -> width -> temp -> brtl block
   val store_pop_cvt    : operator -> width -> temp -> brtl block
   val push_cvt_rm      : operator -> warg -> width -> temp -> brtl block
   val store_pop_cvt_rm : operator -> warg -> width -> temp -> brtl block
 end
 val pushk     :                      Rtl.Private.const -> brtl block
 val pushk_cvt : operator -> width -> Rtl.Private.const -> brtl block
 val stack_op    : operator         -> brtl block
 val stack_op_rm : operator -> warg -> brtl block
 val bc_stack : operator -> ifso:(brtl cbranch) -> ifnot:(brtl cbranch) -> brtl cbranch
 val stack_top_proxy    : Rtl.loc
 val is_stack_top_proxy : Rtl.Private.loc -> bool
end
