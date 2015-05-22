type label = string
type link  = Reloc.t
type aligned = int
    
type rep =
    { mutable inalloc  : Rtl.loc
    ; mutable outalloc : Rtl.loc
    ; mutable ra       : Rtl.loc
    ; mutable users    : (Bits.bits * link) list
    ; mutable csregs   : (Register.t * Rtl.loc option) list
    ; mutable conts    : (label * Rtl.loc * (string * int * aligned) list) list
    ; mutable sds      : Rtl.loc list
    ; mutable vars     : Rtl.loc option array
    }
      
type t = rep
    
let to_spans ~inalloc ~outalloc ~ra ~users ~csregs ~conts ~sds ~vars =
  { inalloc = inalloc
  ; outalloc = outalloc
  ; ra = ra
  ; users = users
  ; csregs = csregs
  ; conts = conts
  ; sds = sds
  ; vars = vars; 
  }
    
let expose spans = spans
  
let fold_live_locs f spans z =
  let fold = Rtlutil.Fold.RegX.loc f in
  let foldcsreg z = function (_, Some l) -> fold l z | (_, None) -> z in
  List.fold_left foldcsreg (fold spans.inalloc (fold spans.outalloc (fold spans.ra z)))
    spans.csregs
