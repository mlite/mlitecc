type tgt   = Preast2ir.tgt
type proc' = Preast2ir.proc

val asm':   byteorder:Rtl.aggregation -> memsize:int -> ptrsize:int
            -> out_channel -> proc'  Asm.assembler
