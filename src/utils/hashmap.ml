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

module Make (HT:Hashtbl.S)  =
  struct
    type 'b t =  'b HT.t

    let create:int -> 'b HT.t = 
      fun size ->
	HT.create size

    (*
    let is_empty: 'b HT.t -> bool = 
      fun hash_tbl ->
        HT.length hash_tbl = 0
    *)
	  
    let find: 'b HT.t -> 'k -> 'b option = 
      fun hash_tbl key ->
	try
	  Some (HT.find hash_tbl key)
	with
	  Not_found -> None

    let set: 'b HT.t -> 'k -> 'b -> unit =
      fun hash_tbl key b'->
	try
	  begin
	    let b = HT.find hash_tbl key
	    in
	    Printf.fprintf stderr "binding exist\n";
	    assert false
	  end
	with
	  Not_found ->
	    HT.add hash_tbl key b'


    let add: 'b HT.t -> 'k -> ('b option -> 'b) -> unit = 
      fun hash_tbl key combine ->
	try 
	  let b = HT.find hash_tbl key
	  in
	  HT.remove hash_tbl key;
	  HT.add hash_tbl key (combine (Some b))
	with
	  Not_found ->
	    HT.add hash_tbl key (combine None)

	      
    let iter: ('k -> 'b -> unit) -> 'b HT.t -> unit = 
      fun fn hash_tbl ->
	HT.iter fn hash_tbl
  end
