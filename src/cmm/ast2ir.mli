type tgt = Preast2ir.tgt = T of (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t
  
and basic_proc = Preast2ir.basic_proc
    
type proc      = Preast2ir.proc
    
val set_headroom : int -> unit
  
val translate : tgt
  -> proc Fenv.Clean.env'
  -> optimizer: (proc -> unit)
  -> defineglobals: bool
  -> proc Nelab.compunit
  -> unit   (* side-effects the assembler in the environment *)
