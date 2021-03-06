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
