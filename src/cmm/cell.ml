type count = C of int   (* a number of cells *)
type width = int        (* a number of bits *)

type t = int * (count -> width) * (width -> count) * (width -> bool)
    
let to_width (_, f, _, _) = f
let to_count (_, _, f, _) = f
let size     (w, _, _, _) = w
let divides  (_, _, _, f) = f

let c1  = ( 1, (fun (C c) ->      c), (fun w -> C w), (fun w -> true))
let c8  = ( 8, (fun (C c) ->  8 * c), (fun w -> C (w / 8)),  (fun w -> w mod  8 = 0))
let c16 = (16, (fun (C c) -> 16 * c), (fun w -> C (w / 16)), (fun w -> w mod 16 = 0))
let c32 = (32, (fun (C c) -> 32 * c), (fun w -> C (w / 32)), (fun w -> w mod 32 = 0))
let c64 = (64, (fun (C c) -> 64 * c), (fun w -> C (w / 64)), (fun w -> w mod 64 = 0))

let of_size n = match n with
  | 1  -> c1
  | 8  -> c8
  | 16 -> c16
  | 32 -> c32
  | 64 -> c64
  |  _ -> (n, (fun (C c) -> n * c), (fun w -> C (w / n)), (fun w -> w mod n = 0))
