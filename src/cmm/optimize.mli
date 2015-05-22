val simplify_exps : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
val trim_unreachable_code : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
val collapse_branch_chains: 'a -> Ast2ir.proc -> Ast2ir.proc * bool
val remove_nops : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
val validate : 'a -> Ast2ir.proc -> Ast2ir.proc * bool
