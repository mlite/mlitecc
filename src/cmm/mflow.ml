type ('em, 'pr) map' = ('em, 'pr) Ep.pre_map =
    { embed   : 'em
    ; project : 'pr
    }
      
type brtl = Rtl.exp -> Rtl.rtl
  
type ('a,'b) map  = ('a -> 'b -> brtl Dag.branch,  Rtl.rtl -> 'b) map'
    
type ('a,'b) mapc = ('a -> 'b -> brtl Dag.cbranch, Rtl.rtl -> 'b) map'
    
type cut_args = { new_sp : Rtl.exp; new_pc : Rtl.exp }
    
type 'a machine =
    { bnegate:   Rtl.rtl -> Rtl.rtl
    ; goto:      ('a, Rtl.exp) map
    ; jump:      ('a, Rtl.exp) map
    ; call:      ('a, Rtl.exp) map
    ; branch:    ('a, Rtl.exp) mapc (* condition *)
    ; retgt_br:  Rtl.rtl -> brtl Dag.cbranch
    ; move:      'a -> src:Register.t -> dst:Register.t -> brtl Dag.block
    ; spill:     'a -> Register.t     -> Rtlutil.aloc   -> brtl Dag.block
    ; reload:    'a -> Rtlutil.aloc   -> Register.t     -> brtl Dag.block
    ; cutto:     ('a, cut_args) map    (* newpc * newsp map*)
    ; return:    Rtl.rtl
    ; forbidden: Rtl.rtl   (* causes a run-time error *)
    }
      
module type PC = 
sig
  val pc_lhs : Rtl.loc
  val pc_rhs : Rtl.loc
  val ra_reg : Rtl.loc
  val ra_offset : int   (* at call, ra_reg  := PC + ra_offset *)
end

module type S = 
sig
  val machine : sp:Rtl.loc -> 'a machine
    (* N.B. sp is a dynamic argument because it could differ among call conventions *)
end
  
module DG = Dag
module RU = Rtlutil
module R  = Rtl
module RP = Rtl.Private
module Up   = Rtl.Up
module Down = Rtl.Dn
  
let ( =/  ) = RU.Eq.loc
let ( =// ) = RU.Eq.exp

module MakeStandard (P : PC) = 
struct
  type 'a map = ('a, Rtl.rtl) Ep.map
  let w = RU.Width.loc P.pc_lhs
  let downrtl = R.Dn.rtl
  let uploc   = R.Up.loc
  let upexp   = R.Up.exp
  let cmpneg w ~cop ~fetch =
    let flip_op flip = RP.App ((flip, [w]), fetch) 
    in
    match cop with
      | "eq"   -> flip_op "ne"
      | "ne"   -> flip_op "eq"
      | "lt"   -> flip_op "ge"
      | "le"   -> flip_op "gt"
      | "gt"   -> flip_op "le"
      | "ge"   -> flip_op "lt"
      | "ltu"  -> flip_op "geu"
      | "leu"  -> flip_op "gtu"
      | "gtu"  -> flip_op "leu"
      | "geu"  -> flip_op "ltu"
      | "feq"  -> flip_op "fne"
      | "fne"  -> flip_op "feq"
      | "flt"  -> flip_op "fge"
      | "fle"  -> flip_op "fgt"
      | "fgt"  -> flip_op "fle"
      | "fge"  -> flip_op "flt"
      | "not"  -> 
	  (match fetch with
            | [r] -> r
            | _ -> Impossible.impossible "negation of multiple arguments")
      | _      -> RP.App (("not", [w]), fetch)
  let bnegate r = match Down.rtl r with
    | RP.Rtl [RP.App((cop, [w1]), [RP.Fetch (flags, w2)]), RP.Store (pc, tgt, w3)]
      when pc =/ Down.loc P.pc_lhs && w1 = w && w2 = w && w3 = w ->
        Up.rtl (RP.Rtl [cmpneg w ~cop ~fetch:[RP.Fetch (flags, w)],
                                 RP.Store (pc, tgt, w)])
    | _ -> Impossible.impossible "ill-formed conditional branch"
  let gotor = 
    { Ep.embed   = (fun e -> R.store P.pc_lhs e w)
    ; Ep.project = (fun r -> match downrtl r with
      | RP.Rtl [(_, RP.Store(_, e, _))] -> upexp e
      | _ -> Impossible.impossible "projected non-goto")
    } 
  let goto = 
    { Ep.embed   = (fun _ e -> (DG.Nop, gotor.Ep.embed e))
    ; Ep.project = gotor.Ep.project
    } 
  let jump = goto
  let cutto ~sp =
    { Ep.embed   = (fun _ {new_sp=new_sp; new_pc=new_pc} -> 
      let assign loc e = Rtl.store loc e w in
      (DG.Nop, Rtl.par [assign P.pc_lhs new_pc; assign sp new_sp]))
    ; Ep.project = (fun r -> match downrtl r with
                             | RP.Rtl [ (_, RP.Store(_,  npc, _))
                                      ; (_, RP.Store(_ , nsp, _))] ->
                                  { new_sp=upexp nsp; new_pc= upexp npc }
                             | _ -> Impossible.impossible "projected non-cutto")
    }
      
  let ra_val = 
    let pc = R.fetch P.pc_rhs w in
    RU.addk w pc P.ra_offset
      
  let call = { Ep.embed   = (fun _ e -> (DG.Nop, R.par [R.store P.pc_lhs e w;
                                                      R.store P.ra_reg ra_val w]))
             ; Ep.project =
                (fun r -> match downrtl r with
                   | RP.Rtl [(_, RP.Store(_, e, _)); _] -> upexp e
                   | _ -> Impossible.impossible (Printf.sprintf "projected non-call: %s"
                                                                (RU.ToString.rtl r)))
             } 
    
  let return = R.store P.pc_lhs (R.fetch P.ra_reg w) w
    
  let branch =
    { Ep.embed   = (fun _ cond ->
                      DG.cond (fun tgt -> R.guard cond (gotor.Ep.embed tgt)))
    ; Ep.project = (fun r -> match downrtl r with
                            | RP.Rtl [(b, RP.Store(_, _, _))] -> upexp b
                            | _ -> Impossible.impossible "projected non-branch")
    } 
      
  let forbidden = R.kill P.pc_lhs
    
  let machine ~sp =
    let fail _ = assert false in
    { bnegate = bnegate
    ; goto = goto
    ; jump = jump
    ; call = call
    ; cutto = cutto sp
    ; retgt_br = fail
    ; spill = fail
    ; reload = fail
    ; move = fail
    ; return = return
    ; branch = branch
    ; forbidden = forbidden 
    }
end
