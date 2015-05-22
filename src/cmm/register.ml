include Common
type aggregation = 
  | BigEndian
  | LittleEndian
  | Identity

type space = char * aggregation * Cell.t   (* name, byte order, cell size *)
type count = Cell.count = C of int
type width = int
type reg = space * int * count (* Rtl.space, index, number of cells *)
type t = reg
type x = 
  | Reg   of t
  | Slice of width * int * t
      
module type SETX = 
sig
  include Set.S
  val of_list   : elt list -> t
  val to_string : t -> string    (* elements sep. by commas (no braces) *)
end

module Compare = struct
  type t = reg 
  let compare (((xs,_,_),xi,C xc):t) (((ys,_,_),yi,C yc):t) =
    let x = comparec xs ys in
    if x <> 0 then x
    else let i = comparei xi yi in if i <> 0 then i 
    else comparei xc yc
  let compare' x y = compare y x
    
  let compare = 
    try ignore (Sys.getenv "QCREVERSE"); compare' 
    with _ -> compare
    
(*
    let compare (((xs,_,_),xi,C xc) as x:t) (((ys,_,_),yi,C yc) as y:t) =
      let answer = compare x y in
      let prime = compare y x in
      if prime <> -answer then
        Printf.kprintf Impossible.impossible "$%c[%d:%d] ? $%c[%d:%d] == %d (neg %d)\n" xs xi xc ys yi yc answer prime;
      answer
*)
end
  
let compare = Compare.compare
let eq r r' = Compare.compare r r' = 0
  
module CompareX = 
struct
  type t = x
  let compare x y = match x, y with
    | Slice (w, lsb, r), Slice (w', lsb', r') ->
        let x = comparei w w' in
        if x <> 0 then x
        else
          let x = comparei lsb lsb' in
          if x <> 0 then x
          else Compare.compare r r'
    | Slice _, Reg _  ->  1
    | Reg _, Slice _  -> -1
    | Reg r, Reg r'   -> Compare.compare r r'
end
  
module SetX = 
struct
  module S = Set.Make(CompareX)
  include S
  let of_list l = List.fold_right add l empty
  let to_string s =
    let elt = function
      | Reg ((s,_,_), i, C 1) -> Printf.sprintf "%c%d" s i
      | Reg ((s,_,_), i, C n) -> Printf.sprintf "%c%d:%d" s i n
      | Slice (w, i, ((s,_,_), i', _)) ->
          Printf.sprintf "%c%d@[%d..%d]" s i' i (i + w - 1) in
    String.concat ", " (List.map elt (elements s))
end
  
module Set = 
struct
  module S = Set.Make(Compare)
  include S
  let of_list l = List.fold_right add l empty
  let to_string s =
    let elt ((s,_,_), i, C n) =
      if n = 1 then
        Printf.sprintf "%c%d" s i
      else
        Printf.sprintf "%c%d:%d" s i n in
    String.concat ", " (List.map elt (elements s))
end
  
module MapX = Map.Make(CompareX)
module Map  = Map.Make(Compare)
  
let promote_x = function Reg r | Slice (_, _, r) -> r
  
let rset_to_rxset set = Set.fold (fun r rst -> SetX.add (Reg r) rst) set SetX.empty
  
let promote_rxset set =
  SetX.fold (fun r rst -> match r with Reg r | Slice (_,_,r) -> Set.add r rst)
            set Set.empty
    
let reg_int_map regs =
  let cmp ((s1,_,_),i1,_) ((s2,_,_),i2,_) =
    match comparec s1 s2 with 0 -> comparei i1 i2 | n -> n in
  let order = List.sort cmp regs in
  (List.fold_left (fun (i,map) r -> (i+1, Map.add r i map)) (0, Map.empty)
    order)


let width ((_, _, ms), _, c) = Cell.to_width ms c
  
let widthx = function
  | Reg r -> width r
  | Slice (w, _, _) -> w
      
let rec eqx x x' = 
  match x, x' with
    | Reg r, Reg r' -> eq r r'
    | Slice (w, lsb, r), Slice (w', lsb', r') -> w = w && lsb = lsb && eq r r'
    | Reg _, Slice _ -> false
    | Slice _, Reg _ -> false
      
let contains ~outer ~inner = 
  match outer, inner with
    | Reg ((s, _, _), i, C c), Reg ((s', _, _), i', C c') ->
	s =<= s' && i' >= i && i' + c' <= i + c
    | Reg ((s, _, _), i, C c), Slice (w', lsb', ((s', _, _), i', C c')) ->
	s =<= s' && i' >= i && i' + c' <= i + c
    | Slice (w, lsb, ((s, _, _), i, C c)), Reg ((s', _, cell), i', C c') ->
	if c <> 1 then
	  Impossible.impossible "slice of register aggregate";
	lsb = 0 && w = Cell.to_width cell (C c') && s =<= s' && i = i' && c = c'
    | Slice (w, lsb, ((s, _, _), i, C c)), Slice (w', lsb', ((s', _, _), i', C c')) ->
	if c <> 1 || c' <> 1 then
	  Impossible.impossible "slice of register aggregate";
	s =<= s' && i = i' && lsb <= lsb' && lsb+w >= lsb'+w'
