type tgt  = T of (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t

and basic_proc = (Automaton.t, unit, Call.t, tgt) Proc.t
    
type proc = Zipcfg.graph * basic_proc
    
let tgt {Proc.target = T t} = t
