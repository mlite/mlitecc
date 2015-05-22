include Common
module DG  = Dag
module M   = Mflow
module R   = Rtl
module RSX = Register.SetX
module U   = Unique
module UM  = Unique.Map
module US  = Unique.Set

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimpf  fmt = Printf.kprintf Impossible.unimp      fmt
let ( **> ) f x = f x

type uid        = U.uid
type label      = uid * string
type exp_of_lbl = label -> Rtl.exp (* exp of code label *)
type 'a machine = 'a * 'a Mflow.machine * exp_of_lbl (* useful pairing *)
let uid         = U.uid
type regs = Register.SetX.t (* sets of regs for dataflow *)
type contedge = { kills:regs; defs:regs; node:label; assertion: Rtl.rtl }

module Rep = 
struct
  let entry_uid = U.distinguished_uid
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
      
  let fid = function Entry -> entry_uid | Label ((u, _), _, _) -> u
    
  let id (f, t) = fid f
    
  let blocklabel (f, t) = match f with
    | Entry -> None
    | Label (l, _, _) -> Some l
	
  let blockkind (f, t) = match f with 
    | Entry -> None
    | Label (_, k, _) -> Some k
	
  let rec ht_to_first head tail = match head with
    | First f -> f, tail
    | Head (h, m) -> ht_to_first h (Tail (m, tail)) 

  let goto_start (h, t) = ht_to_first h t

  let rec ht_to_last head tail = match tail with
    | Last l -> head, l
    | Tail (m, t) -> ht_to_last (Head (head, m)) t 

  let goto_end (h, t) = ht_to_last h t

  let zip = goto_start
    
  let zipht = ht_to_first
    
  let unzip (n, ns) = (First n, ns)
    
  let rec lastt = function (Last l) -> l | Tail (_, t) -> lastt t 
    
  let last (h, t) = lastt t
    
  let first =
    let rec first = function (First f) -> f | Head (h, _) -> first h in
    fun (h, t) -> first h
      
  let succs = function
    | Exit                  -> []
    | Branch  (_, l)        -> [fst l]
    | Cbranch (_, t, f)     -> [fst f; fst t]  (* order meets layout constraint *)
    | Mbranch (_, edges)    -> List.map fst edges
    | Call    c             -> List.map (fun e -> fst e.node) c.cal_contedges
    | Cut     (_, edges, _) -> List.map (fun e -> fst e.node) edges
    | Return  _   -> []
    | Jump    _   -> []
    | Forbidden _ -> []

  let fold_succs f t z = match t with
    | Exit                  -> z
    | Branch  (_, l)        -> f (fst l) z
    | Cbranch (_, te, fe)   -> f (fst te) (f (fst fe) z) (* order meets layout constraint *)
    | Mbranch (_, edges)    -> List.fold_left (fun z e -> f (fst e) z) z edges
    | Call    c             -> List.fold_left (fun z e -> f (fst e.node) z) z c.cal_contedges
    | Cut     (_, edges, _) -> List.fold_left (fun z e -> f (fst e.node) z) z edges
    | Return  _   -> z
    | Jump    _   -> z
    | Forbidden _ -> z

  let iter_succs f t = fold_succs (fun t () -> f t) t ()
    
  let fold_fwd_block first middle last (f, t) z =
    let z = first f z in
    let rec tail t z = match t with
      | Tail (m, t) -> tail t (middle m z)
      | Last l      -> last l z 
    in
    tail t z
      
  let mid_instr m = match m with
    | Instruction  r -> r
    | Stack_adjust r -> r

  let is_executable _ = true

  let nop = Rtl.par [] 
  let last_instr l = match l with
    | Exit    -> nop
    | Branch  (r, _) -> r
    | Cbranch (r, _, _) -> r
    | Mbranch (r, _) -> r
    | Call    c -> c.cal_i  
    | Cut     (r, _, _) -> r
    | Return  (_, r, _) -> r
    | Jump    (r, _, _) -> r
    | Forbidden r -> r
end
  
open Rep
module type BLOCKS = 
sig
  type t
  val empty  : t
  val insert : block -> t -> t
  val find   : t -> uid -> block 
  val focus  : t -> uid -> block * t
  val focusp : t -> (block -> bool) -> block * t
  val union  : t -> t -> t
    (* keep larger set on the right *)

  val fold   : (block -> 'a -> 'a) -> t -> 'a -> 'a
  val iter   : (block -> unit) -> t -> unit
end

module Blocks : BLOCKS with type t = block UM.t = 
struct
  type t = block UM.t
  let empty = UM.empty
  let insert block = UM.add (id block) block 
  let find blocks u = UM.find u blocks
  let focusp blocks p = UM.splitp (fun _ b -> p b) blocks
  let focus blocks u = UM.split u blocks
  let union = UM.union
  let fold f blocks z = UM.fold (fun _ b z -> f b z) blocks z
  let iter = UM.iter
end
  
type graph  = Blocks.t
type zgraph = zblock * Blocks.t
type nodes   = zgraph -> zgraph (* sequence of nodes in Hughes's representation *)
type cbranch = ifso:label -> ifnot:label -> nodes (* ability to branch conditionally *)

let of_blocks g = g
let to_blocks g = g
let openz z = z
let tozgraph z = z
let empty = Blocks.insert (Entry, Last Exit) Blocks.empty

let focus uid blocks =
  let (b, bs) = Blocks.focus blocks uid 
  in
  unzip b, bs
    
let entry blocks = focus entry_uid blocks
  
let exit g =
  let is_exit b = 
    match last (unzip b) with Exit -> true | _ -> false 
  in
  let (b, bs) = Blocks.focusp g is_exit in
  let (h, l) = goto_end (unzip b) in
  ((h, Last l), bs)

let unfocus (bz, bs) = Blocks.insert (zip bz) bs
  
let consm middle ((h, t), blocks) = ((h, Tail (middle, t)), blocks)

let instruction  rtl g = consm (Instruction  rtl) g
let stack_adjust rtl g = consm (Stack_adjust rtl) g

let unreachable = function
  | Last (Branch _ | Forbidden _ | Exit) -> ()
  | t ->
      let pr s = Cmm_debug.eprintf "zipcfg" s in
      pr "warning: unreachable code?\n";
      let rec warn = function
        | Tail (m, t) -> pr "  %s\n" (Rtlutil.ToString.rtl (mid_instr m)); warn t
        | Last l -> pr "  %s\n"  (Rtlutil.ToString.rtl (last_instr l)) in
      warn t

let consl last ((head, tail), blocks) = unreachable tail; ((head, Last last), blocks)
  
let return  rtl ~exit ~uses  = consl (Return (exit, rtl, uses))

let forbidden (_, machine,_) = consl (Forbidden machine.M.forbidden)

let check_single_exit g =
  let check block found = match last (unzip block) with
    | Exit when not found -> true
    | _ -> found in
  if not (Blocks.fold check g false) then
    impossf "graph does not have an exit"
      
let set_spans (bz, blocks) spans = match bz with
  | First (Label (l, u, r)), _ -> r := Some spans
  | _ -> impossf "setting spans on non-label"
      
let iter_spans f g =
  let span s = match s with Some s -> f s | None -> () 
  in
  let first = function Label (_, _, s) -> span (!s) | _ -> () 
  in
  let block (f, t) =
    first f;
    match lastt t with
      | Call c -> span c.cal_spans
      | _ -> () 
  in
  Blocks.iter block g
    
let fold_spans f g z =
  let span s z = match s with Some s -> f s z | None -> z in
  let first f z = match f with Label (_, _, s) -> span (!s) z | _ -> z in
  let block (f, t) z =
    let z = first f z in
    match lastt t with
      | Call c -> span c.cal_spans z
      | _ -> z 
  in
  Blocks.fold block g z
    
let add_blocks blocks (focus, newblocks) =
  match focus with
  | First Entry, Last (Branch _ | Forbidden _ | Exit) ->
      let rec add block blocks = match goto_end (unzip block) with
      | First (Label _), Exit -> blocks
      | _, Exit -> impossf "exit contains nontrivial code"
      | _ -> Blocks.insert block blocks in
      Blocks.fold add newblocks blocks
  | First Entry, _ -> impossf "entry contains nontrivial code"
  | _ -> impossf "focus not on entry"

let postorder_dfs g =
  let entry, blocks = entry g in
  let rec vnode block cont acc visited =
    let u = id block in
    if US.mem u visited then
      cont acc visited
    else
      vchildren block (get_children block) cont acc (US.add u visited)
  and get_children block =
    let uids = succs (last (unzip block)) in
    (*List.map (Blocks.find blocks) uids*)
    List.fold_left (fun rst bid -> try Blocks.find blocks bid :: rst
    with Not_found -> rst) [] uids
  and vchildren block children cont acc visited = 
    let rec next children acc visited = match children with
      | [] -> cont (block :: acc) visited
      | n::rst -> vnode n (next rst) acc visited in
    next children acc visited in
  vnode (zip entry) (fun acc _visited -> acc) [] US.empty


let fold_layout f z g =
  let nextlabel (f, t) = match f with
    | Entry -> impossf "entry as successor"
    | Label (l, _, _) -> Some l 
  in
  let rec fold blocks z = match blocks with
    | [] -> z
    | [b] -> f b None z
    | b1 :: b2 :: bs -> fold (b2 :: bs) (f b1 (nextlabel b2) z) 
  in
  fold (postorder_dfs g) z
    
let iter_nodes first middle last g =
  let block (f, t) =
    let () = first f in
    let rec tail t = match t with
      | Tail (m, t) -> (middle m; tail t)
      | Last l -> last l in
    tail t 
  in
  UM.iter block g

let iter_rtls rfun g =
  iter_nodes
    (fun f -> ()) (fun m -> rfun (mid_instr m)) (fun l -> rfun (last_instr l)) g
    
let fold_blocks f z bs = UM.fold (fun _ -> f) bs z
  
let iter_blocks = UM.iter
  
let new_rtll rtl l = match l with
  | Exit -> l
  | Branch  (r, l) -> Branch (rtl, l)
  | Cbranch (r, t, f) -> Cbranch (rtl, t, f)
  | Mbranch (r, tgts) -> Mbranch (rtl, tgts)
  | Call    c -> Call { c with cal_i = rtl }
  | Cut     (r, es, uses) -> Cut (rtl, es, uses)
  | Return  (i, r, uses) -> Return (i, rtl, uses)
  | Jump    (r, uses, tgts) -> Jump (rtl, uses, tgts)
  | Forbidden r -> Forbidden (rtl)

let new_rtlm rtl m = match m with
  | Instruction r  -> Instruction rtl
  | Stack_adjust r -> Stack_adjust rtl
      
let map_rtll ~map_rtl ~map_assn l =
  let map_ces ces =
    List.map (fun ce -> { ce with assertion = map_assn ce.assertion }) ces 
  in
  match l with
    | Exit -> l
    | Branch  (r, l) -> Branch (map_rtl r, l)
    | Cbranch (r, t, f) -> Cbranch (map_rtl r, t, f)
    | Mbranch (r, tgts) -> Mbranch (map_rtl r, tgts)
    | Call    c -> Call { c with cal_i = map_rtl c.cal_i ;
        cal_contedges = map_ces c.cal_contedges }
    | Cut     (r, es, uses) -> Cut (map_rtl r, map_ces es, uses)
    | Return  (i, r, uses) -> Return (i, map_rtl r, uses)
    | Jump    (r, uses, tgts) -> Jump (map_rtl r, uses, tgts)
    | Forbidden r -> Forbidden (map_rtl r)

let map_rtlm map m = match m with
  | Instruction r  -> Instruction (map r)
  | Stack_adjust r -> Stack_adjust (map r)

let map_rtls map g =
  let block (f, t) =
    let rec tail t = match t with
      | Tail (m, t) -> Tail (map_rtlm map m, tail t)
      | Last l -> Last (map_rtll ~map_rtl:map ~map_assn:map l) in
    (f, tail t) 
  in
  UM.map block g

let map_nodes first middle last g =
  let block (f, t) =
    let rec tail t = match t with
    | Tail (m, t) -> Tail (middle m, tail t)
    | Last l -> Last (last l) in
    (first f, tail t) in
  UM.map block g
(* should this fn be defined on exi and ill?
   when there are cont edges, do we still need to cover the regular edges?*)

let (++) = RSX.union
  
let union_over_outedges node ~noflow ~flow =
  let noflow (u, l) = noflow u in
  let union_contedges ce = List.fold_left (fun r s -> r ++ flow s) RSX.empty ce 
  in
  match node with
    | Call c -> union_contedges c.cal_contedges
    | Cut (_, es, _) -> union_contedges es
    | Cbranch (c, t, f) -> noflow t ++ noflow f
    | Mbranch (_, ls) -> List.fold_left (fun r s -> r ++ noflow s) RSX.empty ls
    | Branch (_, l) -> noflow l
    | Return (_, _, regs) -> regs 
    | Jump _
    | Exit 
    | Forbidden _ -> RSX.empty
  
let iter_outedges node ~noflow ~flow =
  let noflow (u, l) = noflow u in
  match node with
  | Call c -> List.iter flow c.cal_contedges
  | Cut (_, es, _) -> List.iter flow es
  | Cbranch (c, t, f) -> (noflow t; noflow f)
  | Mbranch (_, ls) -> List.iter noflow ls
  | Branch (_, l) -> noflow l
  | Jump _
  | Return _
  | Exit 
  | Forbidden _ -> ()
  
let add_inedge_uses node regs =
  let reg_add  = RSX.fold RSX.add in
  match node with
    | Call c -> reg_add c.cal_uses regs
    | Cut (_, _, uses) -> reg_add uses regs
    | Jump (_, uses, _) -> reg_add uses regs
    | Return (_, _, uses) -> reg_add uses regs
    | Exit | Branch _ | Cbranch _ | Mbranch _ | Forbidden _ -> regs

let span_add spans rst = match spans with
  | Some ss -> Spans.fold_live_locs RSX.add ss rst
  | None    -> rst 

let add_live_spansl node regs = match node with
  | Call c -> span_add c.cal_spans regs
  | _ -> regs

let add_live_spansf node regs = match node with
  | Label (_, _, sp) -> span_add (!sp) regs
  | Entry -> regs
      
let of_block_list blocks =
  List.fold_left (fun m b -> Blocks.insert b m) Blocks.empty blocks 

let prepare_for_splicing graph single multi =
  let gentry, gblocks = Blocks.focus graph entry_uid 
  in
  if UM.is_empty gblocks then
    ((match last (unzip gentry) with Exit -> () | _ -> impossf "bad single block");
    single (snd gentry)
    )
  else
    let gexit, gblocks = exit gblocks in
    let gh, gl = goto_end gexit in
    (match gl with Exit -> () | _ -> impossf "exit is not exit?!");
    multi ~entry:(snd gentry) ~exit:gh ~others:gblocks

let _ = (prepare_for_splicing :
  graph -> 
  (Rep.tail -> 'answer) -> 
  (entry:Rep.tail -> exit:Rep.head -> others:Rep.block Unique.Map.t -> 'answer) ->
  'answer)

let splice_head head g =
  check_single_exit g;
  let splice_one_block tail' = match ht_to_last head tail' with
    | head, Exit -> Blocks.empty, head
    | _ -> impossf "spliced graph without exit" in
  let splice_many_blocks ~entry ~exit ~others =
    Blocks.insert (zipht head entry) others, exit in
  prepare_for_splicing g splice_one_block splice_many_blocks

let splice_tail g tail =
  check_single_exit g;
  let splice_one_block tail' =  (* return tail' .. tail *)
    match ht_to_last (First Entry) tail' with
      | head', Exit ->
          (match ht_to_first head' tail with
            | Entry, t -> (t, Blocks.empty)
            | _ -> impossf "entry in; garbage out")
      | _ -> impossf "spliced single block without Exit" 
  in
  let splice_many_blocks ~entry ~exit ~others =
    (entry, Blocks.insert (zipht exit tail) others) 
  in
  prepare_for_splicing g splice_one_block splice_many_blocks
    
let splice_focus_entry ((head, tail), blocks) g =
  let tail, blocks' = splice_tail g tail in
  ((head, tail), Blocks.union blocks' blocks)

let splice_focus_exit ((head, tail), blocks) g =
  let blocks', head = splice_head head g in
  ((head, tail), Blocks.union blocks' blocks)
    
let splice_head_only head graph =
  let gentry, gblocks = Blocks.focus graph entry_uid in
  match gentry with
  | Entry, tail -> Blocks.insert (zipht head tail) gblocks
  | _ -> impossf "splice graph does not start with entry"
      
let remove_entry graph =
  let gentry, gblocks = Blocks.focus graph entry_uid in
  match gentry with
  | Entry, tail -> tail, gblocks
  | _ -> impossf "removing nonexistent entry"
      
let expand expand_middle expand_last graph =
  let expand_block block expanded =
    let rec expand_tail h t expanded = match t with
      | Tail (m, t) ->
          let g, h = splice_head h (expand_middle m) in
          expand_tail h t (Blocks.union g expanded)
      | Last l ->
          Blocks.union (splice_head_only h (expand_last l)) expanded 
    in
    let (f, t) = block in
    expand_tail (First f) t expanded 
  in
  Blocks.fold expand_block graph Blocks.empty 
    
let single_middle m =
  let block = (Entry, Tail (m, Last Exit)) in
  Blocks.insert block Blocks.empty

let single_last l =
  let block = (Entry, Last l) in
  Blocks.insert block Blocks.empty
    
let modified = ref false
  
let rec consl' m b last ((head, tail), blocks) =
  unreachable tail;
  fst (block_before m b ((head, Last last), blocks))
    
and branch (p, machine, l2e as m) target =
  let (b, r) = machine.M.goto.M.embed p (l2e target) 
  in consl' m b (Branch (r, target))

and jump (p, machine, l2e as m) e ~uses ~targets =
  let (b, r) = machine.M.jump.M.embed p e in
  consl' m b (Jump (r, uses, targets))

and cbranch (p, machine,l2e as m) guard ~ifso ~ifnot succ =
  let cbr = machine.M.branch.M.embed p guard in
  fst (cbranch2cfg m cbr ~ifso ~ifnot succ)

and mbranch (p, machine, l2e as m) e ~targets ((head, tail), blocks) =
  let (b, r) = machine.M.goto.M.embed p e in
  unreachable tail;
  fst (block_before m b ((head, Last (Mbranch (r, targets))), blocks))

and cut_to (p, machine, l2e as m) cut_args ~cuts_to ~aborts ~uses =
  let (b, r) = machine.M.cutto.M.embed p cut_args in
  consl' m b (Cut (r, cuts_to, uses))

and label' user (p, machine, l2e as m) lbl ((head, tail), blocks) =
  let (b, r) = machine.M.goto.M.embed p (l2e lbl) in
  fst (block_before m b ((head, Last (Branch (r, lbl))),
  Blocks.insert (Label (lbl, user, ref None), tail) blocks))

and label        x = label' Userlabel x

and privatelabel x = label' Genlabel x

and limitlabel   x = label' Limitlabel x

and make_target machine ((b, bs) as gz) = 
  match b with
    | First (Label (u, _, _)), _        -> u, gz
    | First Entry, Last (Branch (_, u)) -> u, gz
    | First Entry, _ ->
	let lbl = (uid (), Idgen.label "branch target") in
	let gz = branch machine lbl **> privatelabel machine lbl **> gz in
	lbl, gz
    | _ -> impossf "focus not on entry"

and if_then_else machine cbranch ~t ~f g =
  let endif, g = make_target machine g in
  let not,   g = make_target machine (f g) in
  let        g = branch      machine endif g in
  let so,    g = make_target machine (t g) in
  cbranch ~ifso:so ~ifnot:not g

and while_do machine cbranch ~body g =
  let lbl = (uid (), Idgen.label "loop head") in
  let endwhile, g = make_target machine g in
  let body,     g = make_target machine (body (branch machine lbl g)) in
  let g = cbranch ~ifso:body ~ifnot:endwhile g in
  label machine lbl g

and limitcheck machine cbranch ~t g =
  let endif = (uid (), Idgen.label "post-limitcheck label") in
  let g = limitlabel machine endif g in
  let so, g = make_target machine (t g) in
  cbranch ~ifso:so ~ifnot:endif g

and call (p, machine,l2e as m) exp ~altrets ~unwinds_to ~cuts_to ~aborts
    ~uses ~defs ~kills ~reads ~writes ~spans ~succ_assn succ =
  let lbl = (uid (), Idgen.label "call successor") in
  let succ_ce = { kills = kills; defs = defs; node = lbl; assertion = succ_assn } in
  let edgelist = succ_ce :: List.flatten [altrets; unwinds_to; cuts_to] in
  let (b, r) = machine.M.call.M.embed p exp in
  let call = 
    { cal_i = r; cal_contedges = edgelist; cal_spans = spans;
    cal_uses = uses; cal_altrets = List.length altrets;
    cal_unwinds_to = List.length unwinds_to; cal_cuts_to = List.length cuts_to;
    cal_reads = reads; cal_writes = writes; } 
  in
  let succ = privatelabel m lbl succ 
  in
  match succ with
    | (First Entry, Last (Branch (_, lbl'))), blocks when lbl' =*= lbl -> 
	fst (block_before m b ((First Entry, Last (Call call)), blocks))
    | _ -> impossf "internal error in call constructor"

and block_before' m block succ =
  let rec before b = match b with
    | DG.Seq (DG.Nop, b) -> before b
    | DG.Seq (b, DG.Nop) -> before b
    | DG.Rtl i        -> modified := true; instruction i succ
    | DG.Seq (b, b')  -> block_before' m b (before b')
    | DG.If (c, t, f) ->
	if_then_else m (cbranch2cfg' m c) ~t:(block_before' m t) ~f:(block_before' m f)
          succ
    | DG.While (c, b) -> while_do m (cbranch2cfg' m c) ~body:(block_before' m b) succ
    | DG.Nop -> succ in
  before block 
    
and block2cfg' machine block = block_before' machine block (entry empty)
  
and cbranch2cfg' (_, _, l2e as machine) c ~ifso ~ifnot succ =
  let nodemap = ref DG.empty in
  let rec cbi (br, tk, fk) succ =
    let tlbl, succ = cbr tk succ in
    let flbl, succ = cbr fk succ in
    consl (Cbranch (br (l2e tlbl), tlbl, flbl)) succ
  and cbr c succ = match c with
    | DG.Exit p        -> (if p then ifso else ifnot), succ
    | DG.Test   (b, i) -> make_target machine (block_before' machine b (cbi i succ))
    | DG.Shared (u, c) -> shared u c succ
  and shared u c succ =
    try DG.lookup u (!nodemap), succ with
      | Not_found ->
          let lbl, succ = cbr c succ in
          nodemap := DG.insert u lbl (!nodemap);
          (lbl, succ) in
  modified := true;
  let lbl, g = cbr c succ in
  branch machine lbl g
    
and block_before m b s =
  modified := false;
  (block_before' m b s, !modified)
    
and block2cfg m b =
  modified := false;
  (block2cfg' m b, !modified)
    
and cbranch2cfg m c ~ifso ~ifnot s =
  modified := false;
  (cbranch2cfg' m c ifso ifnot s, !modified)
