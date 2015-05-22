module RU = Rtlutil
module TS = RU.ToString
type uid = int
type 'a block = 
  | Rtl   of Rtl.rtl
  | Seq   of 'a block * 'a block
  | If    of 'a cbranch * 'a block * 'a block
  | While of 'a cbranch * 'a block
  | Nop
      
(* block ending in branch, including multiway branch *)
and  'a branch  = 'a block * Rtl.rtl
    
and  'a cbranch = 
  | Exit   of bool
  | Test   of 'a block * 'a cbinst
  | Shared of uid * 'a cbranch  (* don't duplicate this node *)
      
and  'a cbinst  = 'a * 'a cbranch * 'a cbranch
    
let (<:>) b b' = 
  match b, b' with
    | Nop, b' -> b'
    | b, Nop  -> b
    | _, _    -> Seq (b, b')
	
type 'a nodeset = (uid * 'a) list
    
let empty = []
let lookup = List.assoc
let insert i x l = (i, x) :: l
  
module Shared = 
struct
  let n = Reinit.ref 0
  let shared c = match c with
    | Shared _ -> c
    | _ -> (n := !n + 1; Shared (!n, c))
end
  
let shared = Shared.shared
let cond g = Test (Nop, (g, Exit true, Exit false))

let sprintf = Printf.sprintf

let rec pr_block pr = function
  | Rtl r -> TS.rtl r
  | Seq (b1, b2) -> pr_block pr b1 ^ pr_block pr b2
  | If (c, t, f) -> sprintf "if (%s) { %s; } else { %s; }" 
      (pr_cbranch pr c) (pr_block pr t) (pr_block pr f)
  | While (c, b) -> sprintf "while (%s) { %s; }" (pr_cbranch pr c) (pr_block pr b)
  | Nop -> "<nop>"
      
and pr_cbranch pr c = match c with
  | Exit p        -> if p then "true" else "false"
  | Shared (_, c) -> sprintf "[%s]" (pr_cbranch pr c)
  | Test (Nop, c) -> pr_cbi pr c
  | Test (b, c)   -> sprintf "{%s; %s}" (pr_block pr b) (pr_cbi pr c)
      
and pr_cbi pr c = match c with
  | a, Exit true,  Exit false -> pr a
  | a, Exit false, Exit true  -> sprintf "!(%s)" (pr a)
  | a, Exit true,  p -> sprintf "(%s || %s)" (pr a) (pr_cbranch pr p)
  | a, p, Exit false -> sprintf "(%s && %s)" (pr a) (pr_cbranch pr p)
  | a, p, q -> sprintf "(%s ? %s : %s)" (pr a) (pr_cbranch pr p) (pr_cbranch pr q)
