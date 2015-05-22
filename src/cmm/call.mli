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
  
val outgoing :
  growth:Memalloc.growth -> sp:Rtl.loc -> mkauto:valpass ->
  autosp:(Automaton.result -> Rtl.exp) ->
  postsp:(Automaton.result -> Rtl.exp -> Rtl.exp) -> outgoing
  
val incoming : 
  growth:Memalloc.growth -> sp:Rtl.loc -> mkauto:valpass ->
  autosp:(Automaton.result -> Rtl.exp) ->
  postsp:(Automaton.result -> Rtl.exp -> Rtl.exp) ->
  insp:(Automaton.result -> Rtl.exp -> Block.t -> Rtl.exp) -> incoming
  
type 'a tgt = ('a, (Rtl.exp -> Automaton.t), t) Target.t

val register_cc : 
  'a tgt -> string -> call:Automaton.stage ->
  results:Automaton.stage ->
  cutto:Automaton.stage -> unit
  
(* val get_cc : ('a, 'cc) Target.t -> string -> 'cc *)
val get_cc : ('p, 'a, 'cc) Target.t -> string -> 'cc
val dump_proc      : 'a tgt -> string -> types -> unit
val dump_return    : 'a tgt -> string -> types -> unit
val dump_cutto     : 'a tgt -> string -> types -> unit
val paths_proc     : 'a tgt -> string -> types -> unit
val paths_return   : 'a tgt -> string -> types -> unit
val paths_cutto    : 'a tgt -> string -> types -> unit
val summary_proc   : 'a tgt -> string -> types -> unit
val summary_return : 'a tgt -> string -> types -> unit
val summary_cutto  : 'a tgt -> string -> types -> unit

val path_2_in_overflow : 'a tgt -> string -> unit
  
val run_cc_on_sig_and_return :
  (Automaton.cc_spec -> Automaton.stage) -> 'a tgt -> string -> types -> string list list
  
val run_cc_on_sig_and_print :
  (Automaton.cc_spec -> Automaton.stage) -> 'a tgt -> string -> types -> unit
