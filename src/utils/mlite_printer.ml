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

open Format

let pp_print_list: formatter -> (formatter -> 'a -> unit) -> (formatter -> unit) -> 'a list -> unit = 
  fun fm f delimit l ->
    let len = ref (List.length l)
    in
    List.iter
      (fun a ->
	decr len;
	f fm a;
	if !len > 0 then 
	  delimit fm 
      ) l


let pp_print_listi: formatter -> (formatter -> int -> 'a -> unit) -> (formatter -> unit) -> 'a list -> unit = 
  fun fm f delimit l ->
    let len = ref (List.length l)
    and idx = ref 0
    in
    List.iter
      (fun a ->
	decr len;
	f fm !idx a;
	incr idx;
	if !len > 0 then 
	  delimit fm 
      ) l
    

let pp_print_array: formatter -> (formatter -> int -> 'a -> unit) -> 
  (formatter -> unit) -> 'a array -> unit = 
  fun fm f delimit l ->
    let len = ref (Array.length l)
    in
    Array.iteri
      (fun i a ->
	decr len;
	f fm i a;
	if !len > 0 then 
	  delimit fm 
      ) l


let pp_print_weak: formatter -> (formatter -> int -> 'a -> unit) -> 
  (formatter -> unit) -> 'a Weak.t -> unit = 
  fun fm f delimit l ->
    let len = ref (Weak.length l)
    in
    for i = 0 to (Weak.length l) - 1 do
      decr len;
      let a_opt = Weak.get l i
      in
      match a_opt with
      | Some a ->
	  f fm i a;
	  if !len > 0 then 
	    delimit fm 
      | None -> ()
    done
    
