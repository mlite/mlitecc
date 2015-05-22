module C  = Rtleqn
module RU = Rtlutil

type t =
    { base:         Rtl.exp
    ; size:         int
    ; alignment:    int
    ; constraints:  C.t list
    }

let base t          = t.base
let size t          = t.size
let alignment t     = t.alignment
let constraints t   = t.constraints

let at ~base ~size ~alignment =
  { base          = base
  ; size          = size
  ; alignment     = alignment
  ; constraints   = []
  }

let with_constraint t c = {t with constraints = c :: t.constraints}

let relative anchor dbg f = 
  let w = RU.Width.exp anchor in
  f ~base:(RU.add w anchor (Rtl.late (Idgen.offset dbg) w))
    
let srelative anchor dbg f = relative anchor dbg (fun ~base -> f ~start:base)
  
let align x n = Auxfuns.round_up_to ~multiple_of:n x
  
let add exp i = RU.addk (RU.Width.exp exp) exp i
  
let offset base name ptrwidth =
  RU.add ptrwidth base (Rtl.late (Idgen.offset name) ptrwidth)

let empty ptrwidth = 
  relative (Vfp.mk ptrwidth) "empty block" at ~size:0 ~alignment:1
    
let cathl hi lo =
  let size' = align (size lo) (alignment hi) 
  in
  { base          = base lo
  ; size          = size' + size hi
  ; alignment     = max (alignment lo) (alignment hi)
  ; constraints   = C.equate (add (base lo) size') (base hi)  
      :: (constraints lo) @ (constraints hi) 
  }
    
type placement = High | Low
    
exception OverlapHigh

let overlap place x y = match place with
  | Low ->      
      { base          = base x
      ; size          = max (size x) (size y)
      ; alignment     = max (alignment x) (alignment y)
      ; constraints   = C.equate (base x) (base y)
          :: (constraints x) @ (constraints y)
      }    
	
  | High -> let x,y = if size x < size y then x,y else y,x in
    if (size y - size x) mod (alignment x) <> 0 then raise OverlapHigh else
      { base          = base y    (* y is the larger block *)
      ; size          = size y
      ; alignment     = max (alignment x) (alignment y)
      ; constraints   = C.equate (add (base x) (size x))
          (add (base y) (size y))
          :: (constraints x) @ (constraints y)
      } 
        
let adjust t = { t with size = align (size t) (alignment t) }
  
let cathl_list w = function
  | [] -> empty w
  | b :: bs -> List.fold_left cathl b bs
      
let overlap_list w p = function
  | [] -> empty w
  | b :: bs -> List.fold_left (overlap p) b bs
      
(*
module Lua = struct 
  let size      = size 
  let alignment = alignment 
  let adjust    = adjust
  let cat     = cathl_list
  let overlap = overlap_list
        
  let constraints block = List.map Rtleqn.to_string (constraints block)

  let relative block name size alignment =
    relative (base block) name at ~size ~alignment
  let base block        = Rtlutil.ToString.exp (base block)

  let eq b1 b2 =
    b1.size = b2.size && b1.alignment = b1.alignment &&
    Rtlutil.Eq.exp (Rtl.Dn.exp b1.base) (Rtl.Dn.exp b2.base) (* ignore constraints! *)
end
*)
