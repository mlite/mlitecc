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

let map_opt: ('a -> 'b) -> 'a option -> 'b option =
    fun f a_opt ->
    match a_opt with
    | Some a -> Some (f a)
    | None -> None

let map_list_opt: ('a -> 'b) -> 'a list option -> 'b list option =
  fun f a_list_opt ->
    match a_list_opt with
    | Some a_list -> Some (List.map f a_list)
    | None -> None
	


let apply_opt: ('a -> unit) -> 'a option -> unit =
  fun f a_opt ->
    match a_opt with
    | Some a -> (f a)
    | None -> ()
