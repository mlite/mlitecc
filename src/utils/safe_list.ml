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

let find = List.find
    

let map : ('a -> 'b) -> 'a list -> 'b list =
  fun f a_list ->
    let b_list = ref []
    in
    List.iter 
      (fun a -> 
        b_list := (f a)::!b_list
      ) a_list;
    List.rev !b_list 

let split: ('a * 'b) list -> 'a list * 'b list = 
  fun lst ->
    let a_list = ref []
    and b_list = ref []
    in
    List.iter
      (fun (a, b) ->
	a_list := a::!a_list;
	b_list := b::!b_list;
      ) (List.rev lst);
    (!a_list, !b_list)


let concat: 'a list -> 'a list -> 'a list = 
  fun l1 l2 ->
    let l = ref (List.rev l1)
    in
    List.iter
      (fun v -> 
	l := v::!l
      ) l2;
    List.rev !l

let ( @$ ) : 'a list -> 'a list -> 'a list = concat

let map2 = List.map2

let rev = List.rev

let iter = List.iter

let length = List.length

let hd = List.hd

let tl = List.tl

let nth = List.nth

let for_all = List.for_all

let flatten: 'a list list -> 'a list = 
  fun lstlst ->
    let l = ref []
    in
    List.iter
      (fun sublst ->
	l := concat !l sublst
      ) lstlst;
    !l
    
