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

open Log
open Collection

type silly =
	{ mutable int_value: int }

let _ = 
  let _ = init ("foo_bar", 
		[ 
		  { unit_enable = true; 
		    unit_suffix = ".log";
		    unit_items = [ { item_tag = "iris"; 
				     item_enable = true;};
				   { item_tag = "ning"; 
				     item_enable = true;};
				 ]
		  } 
		])
  in
  camlp4_extend_log "iris" (Printf.fprintf "this is logging to iris\n");
  camlp4_extend_log "ning" (Printf.fprintf "this is logging to ning\n");
  close ();
  let v = IntHashmap.create 100
  in
  IntHashmap.set v 0 { int_value = 10; };
  let s = IntHashmap.find v 0 
  in
  s.int_value <- 46;
  IntHashmap.iter
    (fun key b ->
       Printf.printf "key %d value %d\n" key b
    ) v 
