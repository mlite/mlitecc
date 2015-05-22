module M = Mflow
module R = Rtl
module T = Target

type t =
    { block    : Block.t     (* memory block for pair + overflow incoming parms *)
    ; sp       : Rtl.loc     (* location inside block for sp *)
    ; pc       : Rtl.loc     (* location in block for code ptr *)
    }
      
let init_code t vals =
  Rtl.par 
    [ Rtl.store t.pc vals.M.new_pc (Rtlutil.Width.loc t.pc)
    ; Rtl.store t.sp vals.M.new_sp (Rtlutil.Width.loc t.sp)
    ]

let rep t = t.block
  
let offset base n w  = Rtlutil.addk w base n
  
let pc_sp base t =
  let (_, _, cell)    = t.T.memspace in
  let (Rtl.C n) as ct = Cell.to_count cell t.T.pointersize in
  let mem addr        = Rtl.mem Rtl.none t.T.memspace ct addr in
  let pc              = mem (offset base 0 t.T.pointersize) in
  let sp              = mem (offset base n t.T.pointersize) in
  pc, sp

let ovblock_exp e memsize ptrsize alignment = 
  let ptrcells = ptrsize / memsize in
  offset e (Auxfuns.round_up_to alignment (2 * ptrcells)) ptrsize
    
let with_overflow t ~overflow =
  let size = t.T.pointersize / t.T.memsize in
  let my_pc_sp =
    Block.relative (Block.base overflow) "continuation block"
      Block.at ~size:(2 * size) ~alignment:t.T.alignment in
  let my_rep = Block.cathl overflow my_pc_sp in
  let pc, sp = pc_sp (Block.base my_rep) t in
  { block = my_rep; sp = sp; pc = pc }
    
let cut_args t ~contn =
  let w = t.T.pointersize in
  let pc, sp = pc_sp contn t in
  {Mflow.new_pc=R.fetch pc w; Mflow.new_sp=R.fetch sp w}
    
let get_contn (newpc, newsp) = newpc
