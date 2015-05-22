module DG  = Dag
module G   = Zipcfg
module R   = Rtl
module RU  = Rtlutil
type uid = int
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
    
module type S = 
sig
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
module Alloc = struct
  let badslot : width -> int -> Automaton.loc =
    fun _ -> Impossible.impossible "slot allocator misconfigured"
  let badtemp : char -> width -> temp =
    fun _ _ -> Impossible.impossible "temporary allocator misconfigured"
  let valid = Reinit.ref false
  let theslot = Reinit.ref badslot
  let thetemp = Reinit.ref badtemp
  let slot ~width ~aligned = !theslot width aligned
  let temp c w = !thetemp c w
  let isValid () = !valid
end
let remember_allocators t s =
  if !Alloc.valid then
    Impossible.impossible "too many allocators";
  Alloc.valid := true;
  Alloc.thetemp := (fun c w -> Talloc.Multiple.reg c t w);
  Alloc.theslot := (fun w a -> Automaton.allocate s w "" a)
let forget_allocators () = 
  if not !Alloc.valid then
    Impossible.impossible "too few allocators";
  Alloc.valid := false;
  Alloc.theslot := Alloc.badslot;
  Alloc.thetemp := Alloc.badtemp
module Expand = struct
  let bad : exp block -> brtl block =
    fun _ -> Impossible.impossible "block expander misconfigured"
  let badb : exp branch -> brtl branch =
    fun _ -> Impossible.impossible "branch expander misconfigured"
  let badcb : exp cbranch -> brtl cbranch =
    fun _ -> Impossible.impossible "conditional branch expander misconfigured"
  let valid   = Reinit.ref false
  let theblock   = Reinit.ref bad
  let thebranch  = Reinit.ref badb
  let thecbranch = Reinit.ref badcb
  let block   b = !theblock   b
  let branch  b = !thebranch  b
  let cbranch b = !thecbranch b
  let cbranch' e ~ifso ~ifnot =
    let rec upd_cbr = function
      | DG.Exit true  -> ifso
      | DG.Exit false -> ifnot
      | DG.Test (b, c) -> DG.Test (upd_block b, upd_cbi c)
      | DG.Shared (u, cbr) -> DG.Shared (u, upd_cbr cbr)
    and upd_block b = match b with
      | DG.Rtl _ | DG.Nop -> b
      | DG.Seq (b1, b2) -> DG.Seq (upd_block b1, upd_block b2)
      | DG.If (e, b1, b2) -> DG.If (e, upd_block b1, upd_block b2)
      | DG.While (cb, b) -> DG.While (upd_cbr cb, upd_block b)
    and upd_cbi (i, cb1, cb2) = (i, upd_cbr cb1, upd_cbr cb2) in
    upd_cbr (cbranch (DG.Test (DG.Nop, (e, DG.Exit true, DG.Exit false))))
end
let remember_expanders b br cb =
  if !Expand.valid then
    Impossible.impossible "too many expanders";
  Expand.valid := true;
  Expand.theblock   := b;
  Expand.thebranch  := br;
  Expand.thecbranch := cb
let forget_expanders () = 
  if not !Expand.valid then
    Impossible.impossible "too few expanders";
  Expand.valid := false;
  Expand.theblock   := Expand.bad;
  Expand.thebranch  := Expand.badb;
  Expand.thecbranch := Expand.badcb
let (<:>) = DG.(<:>)
module RO = struct
  let lobits w w' x = Rtl.app (Rtl.opr "lobits" [w;w';]) [x; ]
  let zx w w' x = Rtl.app (Rtl.opr "zx" [w;w';]) [x; ]
end

let warg_val = function
  | WTemp (Register.Reg t) -> R.fetch (R.reg t) (Register.width t)
  | WTemp (Register.Slice (w, lsb, t)) -> R.fetch (R.slice w ~lsb (R.reg t)) w
  | WBits b -> R.bits b (Bits.width b)

let with_hw ~hard ~soft ~temp block = 
  match soft with
  | WTemp r when Register.eqx r hard -> block
  | _ ->
      let n    = Register.widthx hard in
      let w    = Register.width temp in
      let t    = R.reg temp in
      let tval = R.fetch t w in
      let hardloc = match hard with
      | Register.Reg r -> R.reg r
      | Register.Slice (w, lsb, r) -> R.slice w ~lsb (R.reg r) in
      let hard = R.fetch hardloc n in
      let save    = DG.Rtl (R.store t (RO.zx n w hard) w) in
      let set     = DG.Rtl (R.store hardloc (warg_val soft) n) in
      let restore = DG.Rtl (R.store hardloc (RO.lobits w n tval) n) in
      Expand.block (save <:> set) <:> block <:> Expand.block restore
module Nostack (Address : sig type t val reg : temp -> t end) = struct
  let imposs = Impossible.impossible
  let opclass _ = Register
  let stack_depth = 0
  let stack_width = 0
  let converts_stack_to_temp _ = false
  let push ~addr _                   = imposs "stack op on register machine"
  let store_pop ~addr _              = imposs "stack op on register machine"
  let push_cvt _ _ ~addr _           = imposs "stack op on register machine"
  let push_cvt_rm _ _ _ ~addr _      = imposs "stack op on register machine"
  let store_pop_cvt _ _ ~addr _      = imposs "stack op on register machine"
  let store_pop_cvt_rm _ _ _ ~addr _ = imposs "stack op on register machine"
  let pushk _                        = imposs "stack op on register machine"
  let pushk_cvt _ _ _                = imposs "stack op on register machine"
  let stack_op _                     = imposs "stack op on register machine"
  let stack_op_rm _                  = imposs "stack op on register machine"
  let bc_stack _ ~ifso ~ifnot        = imposs "stack op on register machine"
  let stack_top_proxy = Rtl.reg (('\000', Rtl.Identity, Cell.of_size 0), 0, Rtl.C 0)
  let is_stack_top_proxy _ = false

  module SlotTemp = struct
    let is _ = false
    let push _                   = imposs "stack op on register machine"
    let store_pop _              = imposs "stack op on register machine"
    let push_cvt _ _ _           = imposs "stack op on register machine"
    let push_cvt_rm _ _ _ _      = imposs "stack op on register machine"
    let store_pop_cvt _ _ _      = imposs "stack op on register machine"
    let store_pop_cvt_rm _ _ _ _ = imposs "stack op on register machine"
  end
end
