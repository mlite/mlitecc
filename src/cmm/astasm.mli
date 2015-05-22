module type PERSONALITY = sig
    val wordsize:       int
    val pointersize:    int
    val memsize:        int
    val byteorder:      Rtl.aggregation
    val float:          string
    val charset:        string
    type proc
    val cfg2ast : proc -> Cmm_ast.proc
end
module type S = sig
    type proc
    val asm: out_channel -> proc Asm.assembler
end    
module Make(P: PERSONALITY): S with type proc = P.proc
