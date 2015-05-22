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

module MakeStandard (Pc : PC) : S
