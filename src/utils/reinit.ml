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
# 22 "reinit.nw"
let tasks = ref ([] : (unit -> unit) list)
let at f = tasks := f :: !tasks
let reset () = List.iter (fun f -> f ()) (!tasks)
let ref x =
  let r = ref x in
  at (fun () -> r := x);
  r
