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

open Int
open Collection

type 'a t = { 
    mutable base: 'a array;
    mutable singleton: 'a option;
  }

let make size def_v = 
  {
   base = Array.make size def_v;
   singleton = None;
 }
    
    
let length: 'a t -> int =
  fun t ->
    (Array.length t.base)
    
      

      
let get: 'a t -> gnode_id:Int.gnode_id -> 'a = 
  fun a ~gnode_id ->
    Debug_array.get a.base (t_int gnode_id)

let iteri f a = 
  Array.iteri
    (fun i v -> f (int_t i) v) a.base

let map f a = 
  let def_v = Array.get a.base 0
  in
  let new_a = 
    {
     base = Array.map f a.base;
     singleton = None;
   }
  in
  new_a

let mapi f a = 
  let def_v = Array.get a.base 0
  in
  let new_a = 
    {
     base = Array.mapi (fun i v -> f (int_t i) v) a.base;
     singleton = None;
   }
  in
  new_a


let append: 'a t -> 'a -> Int.gnode_id = 
  fun t n ->
    let gnode_id = int_t (Array.length t.base)
    and a = Array.make 1 n
    in
    let _ = t.base <- Array.append t.base a
    in
    gnode_id


let set: 'a t -> gnode_id:Int.gnode_id -> elmt:'a -> unit =
  fun a ~gnode_id ~elmt -> 
    let idx = t_int gnode_id
    in
    if idx < Array.length a.base then
      Debug_array.set a.base idx elmt
    else
      ignore (append a elmt)
