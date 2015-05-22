module A  = Automaton
module Dn = Rtl.Dn
module R  = Rtl
module RP = Rtl.Private
module RS = Register.SetX
module RU = Rtlutil
module Up = Rtl.Up
type kind    = string
type width   = int
type aligned = int
type types = (width * kind * aligned) list

type 'insp answer =
  { 
(* locs     : Automaton2.loc list *)  (* where passed values reside *)
    overflow : Block.t              (* includes all locs for values passed in mem *)
(*    ; sploc    : Rtl.exp              (* where sp is required when values are passed *) *)
  ; insp     : 'insp                (* specify sp when overflow block is introduced;
                                       used in assertions entries from calls, cuts *)
  ; regs     : Register.Set.t       (* set of locations defined (used) by partner *)
  ; pre_sp   : Rtl.rtl              (* conditional SP adjustment pre-shuffle *)
  ; shuffle  : Rtl.rtl              (* shuffle parms where they go *)
  ; post_sp  : Rtl.rtl              (* unconditional SP adjustment post-shuffle *)
  }
    
type party = Caller | Callee
    
type overflow = 
    { parameter_deallocator:    party
    ; result_allocator:         party
    }
      
type ('a, 'b) split_blocks = { caller : 'a; callee : 'b }
    
type outgoing = types -> Rtl.exp       list -> unit    answer
  
type incoming = types -> Automaton.loc list -> (Block.t -> Rtl.rtl) answer
  
type ('inc, 'out) pair' = { in' : 'inc ; out : 'out }
    
type pair     = (incoming, outgoing)              pair'
    
type cut_pair = (incoming, (Rtl.exp -> outgoing)) pair'

type t = (* part of a calling convention *)
  (* we get 3 dual pairs *)
  { name           : string   (* canonical name of this cc *)
  ; overflow_alloc : overflow
  ; call_parms     : pair
  ; results        : pair
  ; cut_parms      : cut_pair
       (* exp is continuation val; used to address overflow block *)
                  
  ; stable_sp_loc : Rtl.exp
       (* address where sp points after prolog, sits between calls,
          and should be set to on arrival at a continuation *)

  ; jump_tgt_reg : Rtl.loc
       (* When jumping through a register, we need a hardware register -- otherwise
          we might try to spill a temp after moving the sp, which would be very bad. *)

  ; stack_growth   : Memalloc.growth
  ; sp_align       : int             (* alignment of stack pointer at call/cut *)
  ; ra_on_entry     : Block.t -> Rtl.exp
  ; where_to_save_ra : Rtl.exp -> Talloc.Multiple.t -> Rtl.loc
  ; ra_on_exit      : Rtl.loc -> Block.t -> Talloc.Multiple.t -> Rtl.loc
  ; sp_on_jump      : Block.t -> Talloc.Multiple.t -> Rtl.rtl
  ; sp_on_unwind   : Rtl.exp -> Rtl.rtl
  ; pre_nvregs     : Register.Set.t          (* registers preserved across calls *)
  ; volregs        : Register.Set.t          (* registers not preserved across calls *)
  ; saved_nvr      : Talloc.Multiple.t -> Register.t -> Rtl.loc (* where to save NVR *)
  ; return         : int -> int -> ra:Rtl.exp -> Rtl.rtl    (* alternate return *)
(*  ; alt_return_table : node list -> node *)
  (* these next two encapsulate knowledge of which reg. is sp *)
  ; replace_vfp    : Zipcfg.graph -> Zipcfg.graph * bool
  } 
    
type valpass = unit -> Automaton.t
  
let ignore r s = s

let too_small growth sp target =
  let w = RU.Width.loc sp in
  let ( >* ) x y = R.app (R.opr "gt" [w]) [x; y] in
  let ( <* ) x y = R.app (R.opr "lt" [w]) [x; y] in
  match growth with
  | Memalloc.Down -> RU.fetch sp >* target
  | Memalloc.Up   -> RU.fetch sp <* target

let ne sp target = R.app (R.opr "ne" [RU.Width.loc sp]) [RU.fetch sp; target]
  
let outgoing ~growth ~sp ~mkauto ~autosp ~postsp types actuals =
  let a = mkauto () in
  let crank effects' (w, k, aligned) actual =
    let l = A.allocate a ~width:w ~kind:k ~align:aligned in
    A.store l actual w :: effects' in
  let shuffle = R.par (List.rev (List.fold_left2 crank [] types actuals)) in
  let a = A.freeze a in
  let autosp = autosp a in
  let postsp = postsp a autosp in
  let setsp  = RU.store sp postsp in
  { overflow = a.A.overflow
  ; insp     = ()
  ; regs     = a.A.regs_used
  ; shuffle  = shuffle
  ; post_sp  = R.guard (ne sp postsp)               setsp
  ; pre_sp   = R.guard (too_small growth sp postsp) setsp
  }
    
let incoming ~growth ~sp ~mkauto ~autosp ~postsp ~insp types formals =
  let a = mkauto() in
  let crank effects' (w, k, aligned) formal =
    let l = A.allocate a ~width:w ~kind:k ~align:aligned in
    A.store formal (A.fetch l w) w :: effects' in
  let shuffle = R.par (List.rev (List.fold_left2 crank [] types formals)) in
  let a = A.freeze a in
  let autosp = autosp a in
  let postsp = postsp a autosp in
  let insp   = insp a autosp in
  let setsp  = RU.store sp postsp in
  { overflow = a.A.overflow
  ; insp     = (fun b -> RU.store sp (insp b))
  ; regs     = a.A.regs_used
  ; shuffle  = shuffle
  ; post_sp  = R.guard (ne sp postsp)               setsp
  ; pre_sp   = R.guard (too_small growth sp postsp) setsp
  }
    
type 'a tgt = ('a, (Rtl.exp -> Automaton.t), t) Target.t

let add_cc specs name ~call ~results ~cutto = 
  let base = List.remove_assoc name specs in
  (name, 
  { A.call    = call
  ; A.results = results
  ; A.cutto   = cutto
  }) :: base
    
let register_cc t name ~call ~results ~cutto =
  let newspecs = add_cc t.Target.cc_specs name ~call ~results ~cutto
  in t.Target.cc_specs <- newspecs ; ()

let get_ccspec tgt name =
  try  List.assoc name tgt.Target.cc_specs
  with Not_found -> Unsupported.calling_convention name

let get_cc tgt name =
  tgt.Target.cc_spec_to_auto name (get_ccspec tgt name)

let dump what autofun target ccname =
  let wordsize  = target.Target.wordsize  in
  let automaton = autofun (get_ccspec target ccname) in
  what ~mk:(fun () -> 
    A.at ~start:(R.bits (Bits.zero wordsize) wordsize) 
      target.Target.memspace  automaton)
        
let dump_proc    x = dump Automatongraph.print (fun s -> s.A.call) x
let dump_return  x = dump Automatongraph.print (fun s -> s.A.results) x
let dump_cutto   x = dump Automatongraph.print (fun s -> s.A.cutto) x
let paths_proc   x = dump Automatongraph.paths (fun s -> s.A.call) x
let paths_return x = dump Automatongraph.paths (fun s -> s.A.results) x
let paths_cutto  x = dump Automatongraph.paths (fun s -> s.A.cutto) x
let summary_proc x =
  dump (Automatongraph.summary ~what:"parameters") (fun s -> s.A.call) x
    
let summary_return x =
  dump (Automatongraph.summary ~what:"results")    (fun s -> s.A.results) x
    
let summary_cutto  x =
  dump (Automatongraph.summary ~what:"cont parms") (fun s -> s.A.cutto) x
    
let findpath_1_in_overflow autofun target ccname =
  let wordsize  = target.Target.wordsize  in
  let automaton = autofun (get_ccspec target ccname) in
  let rec find_overflow_1 tys =
    let an  = A.at ~start:(R.bits (Bits.zero wordsize) wordsize) 
      target.Target.memspace automaton in
    let tys = (wordsize, "unsigned") :: tys in
    let _   = List.map (fun (w,h) -> A.allocate an ~width:w ~kind:h) tys in
    let res = A.freeze an in
    if res.A.mems_used == [] then find_overflow_1 tys else tys 
  in
  find_overflow_1 []

let findpath_2_in_overflow autofun target ccname =
  (target.Target.wordsize, "unsigned") :: 
  (findpath_1_in_overflow autofun target ccname)

let path_2_in_overflow target ccname =
  List.iter (fun (h,w) -> Printf.printf "%d/int " target.Target.wordsize)
    (findpath_2_in_overflow (fun s -> s.A.call) target ccname);
  Printf.printf "\n"

let locs aloc w =
  let RP.Rtl gs = Dn.rtl (A.store aloc (Rtl.bits (Bits.zero w) w) w) in
  let getloc = function RP.Store (l, _, _) -> l | RP.Kill l -> l in
  let rec add_locs l locs = match l with
    | RP.Reg (s, n, RP.C c) when c <= 0 -> locs
    | RP.Reg (s, n, RP.C c) -> Up.loc l :: add_locs (RP.Reg (s, n+1, RP.C (c-1))) locs
    | l -> Up.loc l :: locs 
  in
  List.fold_right (fun (_,  e) locs -> add_locs (getloc e) locs) gs []
    
let run_cc_on_sig_and_return autofun target ccname tys =
  let loc2strs l (w, k, a) = List.map Rtlutil.ToString.loc (locs l w) in
  let wordsize = target.Target.wordsize  in
  let auto   = autofun (get_ccspec target ccname) in
  let an     = A.at ~start:(Rtl.late "ovflw" wordsize) target.Target.memspace auto in
  let allocs = List.map (fun (w,k,a) -> A.allocate an ~width:w ~kind:k ~align:a) tys in
  List.map2 loc2strs allocs tys

let run_cc_on_sig_and_print autofun target ccname tys =
  let locs = run_cc_on_sig_and_return autofun target ccname tys in
  let print (w, k, a) ls =
    Printf.printf "\"%s\":%d@%d => { %s }\n" k w a (String.concat "," ls) in
  List.iter2 print tys locs
    
type call_t = t

module type SPEC = 
sig
  type party = Caller | Callee
  type overflow = 
      { parm_deallocator : party
      ; result_allocator : party
      } 
	
  val c_overflow    : overflow
  val tail_overflow : overflow
  type nvr_saver    = Talloc.Multiple.t -> Register.t -> Rtl.loc
  val save_nvrs_anywhere : Space.t list -> nvr_saver
     (* save h/w register in suitable temp space from the list *)
    (* one day: add [[save_nvrs_in_conventional_locations]] *)
    
  module ReturnAddress : 
  sig
    type style =
      | KeepInPlace       (* leave the RA where it comes in --- probably on stack *)
      | PutInTemporaries  (* put the RA in temporaries *)
	  
    (* values for the three RA-related functions in the Call.t *)
    (* these functions are not needed by a client but will be used to convert
       a Spec.t into a Call.t.  Some client may want to use such functions to 
       modify a Call.t *)
    val enter_in_loc : Rtl.loc -> Block.t -> Rtl.exp (* in_loc l b = fetch l *)
      
    val save_in_temp : Rtl.exp -> Talloc.Multiple.t -> Rtl.loc
    val save_as_is   : Rtl.exp -> Talloc.Multiple.t -> Rtl.loc

    val exit_in_temp : Rtl.exp -> Block.t -> Talloc.Multiple.t -> Rtl.loc
    val exit_as_is   : Rtl.exp -> Block.t -> Talloc.Multiple.t -> Rtl.loc
  end

  type t =
    { name          : string   (* canonical name of this cc *)
    ; stack_growth  : Memalloc.growth
    ; sp            : Register.t
    ; sp_align      : int             (* alignment of stack pointer at call/cut *)
    ; allregs       : Register.Set.t           (* registers visible to the allocator *)
    ; nvregs        : Register.Set.t           (* registers preserved across calls *)
    ; saved_nvr     : nvr_saver
    ; ra            : Rtl.loc * ReturnAddress.style
                                        (* where's the RA and what to do with it *)
    } 

  val to_call : t -> call_t
end
