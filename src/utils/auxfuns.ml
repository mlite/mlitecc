(* 
   MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
   
   The name `Mlite C Compiler' belongs to us, but the code is available free for
   any use in any field of endeavor.  You may redistribute Mlite C Compiler in
   whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
   source and include this LICENSE file.  You may modify Mlite C Compiler and
   create derived works, with which you may do as you like, but the result may
   not be called Mlite C Compiler without written consent.
   
   The software is placed in the public domain.  It is not protected by copyright,
   and it is not protected by a ``copyleft'' agreement like the one used by the
   Free Software Foundation.
*)

(* This is not actually a noweb file, but it *is* included in front
   of every generated .ml file.  It turns off evil polymorphic functions *)

   (* permit bare = on integers only *)
let (=|=) (x:int)    (y:int)    = (=) x y
let (=<=) (x:char)   (y:char)   = (=) x y
let (=$=) (x:string) (y:string) = (=) x y
let (=:=) (x:bool)   (y:bool)   = (=) x y
let (=*=) = (=)
let (=) = (=|=)

  (* mnemonics:  |  I  int
                 <  C  char
                 $  S  string
                 :  B  bool
                 *     any type
   *)

let (<>) (x:int) (y:int) = (<>) x y


let comparei (x:int)    (y:int)    = compare x y
let comparec (x:char)   (y:char)   = compare x y
let compares (x:string) (y:string) = compare x y
let compareb (x:bool)   (y:bool)   = compare x y
let compare = comparei

let (<.)  (x:float) (y:float) = (<) x y
let (>.)  (x:float) (y:float) = (>) x y
let (<=.) (x:float) (y:float) = (<=) x y
let (>=.) (x:float) (y:float) = (>=) x y
let (<>.) (x:float) (y:float) = Pervasives.(<>) x y

let (<)  (x:int) (y:int) = (<) x y
let (>)  (x:int) (y:int) = (>) x y
let (<=) (x:int) (y:int) = (<=) x y
let (>=) (x:int) (y:int) = (>=) x y
# 86 "auxfuns.nw"
type void = Void of void (* used as placeholder for polymorphic variable *)

module Option = struct
    let is_some = function Some _ -> true  | None -> false 
    let is_none = function Some _ -> false | None -> true
    let get x = function
        | Some y -> y
        | None   -> x
    let map f = function Some x -> Some (f x) | None -> None
end
# 98 "auxfuns.nw"
let round_up_to ~multiple_of:n k = n * ((k+(n-1)) / n)
# 101 "auxfuns.nw"
let foldri f l z =
  let rec next n = function
    | [] -> z
    | x :: xs -> f n x (next (n+1) xs) in
  next 0 l

let rec from first ~upto = 
    if first > upto then [] else first :: from (first+1) ~upto

let substr start stop str = 
    let start = if start <  0 then String.length str + start else start in
    let stop  = if stop  <= 0 then String.length str + stop  else stop  in
        String.sub str start (stop - start)
# 116 "auxfuns.nw"
module String = struct
  let foldr f s z =
    let rec down_from n z =
      if n < 0 then z else down_from (n-1) (f (String.get s n) z) in
    down_from (String.length s - 1) z
end
# 124 "auxfuns.nw"
let rec last = function
  | [] -> raise (Invalid_argument "empty list")
  | [x] -> x
  | x :: xs -> last xs
# 130 "auxfuns.nw"
let rec map_partial f = function
  | [] -> []
  | x :: xs ->
      match f x with
      | Some y -> y :: map_partial f xs
      | None -> map_partial f xs
# 138 "auxfuns.nw"
let rec compare_list cmp x y = match x, y with
| [], [] -> 0
| [], _ :: _ -> -1
| _ :: _, [] ->  1
| x :: xs, y :: ys ->
    match cmp x y with
    | 0 -> compare_list cmp xs ys
    | diff -> diff
# 148 "auxfuns.nw"
module List = struct
  let rec take n xs = match xs with
  | [] -> []
  | _ :: _ when n = 0 -> []
  | x :: xs -> x :: take (n-1) xs
end
