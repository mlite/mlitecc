type growth = Up | Down
type unchanging = 
    { start  : Rtl.exp     (* start address of block *)
    ; growth : growth
    ; exp_at : int -> Rtl.exp  (* exp at distance k from start *)
    } 
type t =
    { u             : unchanging
    ; num_allocated : int         (* size of block      *)
    ; max_alignment : int         (* max alignment ever requested *)
    }

let at ~start g =
  let w = Rtlutil.Width.exp start in
  let addk = Rtlutil.addk w start in
  { u = 
      { start = start
      ; growth = g
      ; exp_at = match g with Up -> addk | Down -> fun k -> addk (-k)
      } 
  ; num_allocated = 0
  ; max_alignment = 1
  }
    
let relative ~anchor ~dbg g = 
  let w = Rtlutil.Width.exp anchor in
  at (Rtlutil.add w anchor (Rtl.late (Idgen.offset dbg) w)) g
    
let allocate t ~size = 
  assert (size >= 0);    
  { t with num_allocated = t.num_allocated + size }

let align    t n = (* align both size and base alignment *)
  assert (n > 0);
  { t with max_alignment = max t.max_alignment n (*SHOULD BE LEAST COMMON MULTIPLE*)
    ; num_allocated = Auxfuns.round_up_to t.num_allocated ~multiple_of:n 
  }
    
let current t = t.u.exp_at t.num_allocated
let alignment t = t.max_alignment

let freeze t = match t.u.growth with
  | Up   -> Block.at t.u.start   t.num_allocated t.max_alignment
  | Down -> let t = align t t.max_alignment in
    Block.at (current t) t.num_allocated t.max_alignment
      
let num_allocated t = t.num_allocated
  
