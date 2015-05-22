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

include Config_mach

exception Found
  
let get_type_matrix i = 
  let matrix = ref (0, NIF, 0L, 0L, "?", 0L, 0L)
  in
  try
    let _ = 
      List.iter 
	(fun (id, v, size, offset, str, max, min) -> 
	  if (i == id) then
	    begin
	      matrix := (id, v, size, offset, str, max, min);
	      raise Found
	    end
	  )
	builtin_type_table
    in assert false;
  with
    | Found -> !matrix

let get_unsigned_tid in_size = 
  let tid = ref 0
  in
  try
    let _ = 
      List.iter 
	(fun (id, tag, size, offset, str, max, min) -> 
	  if (in_size = size && tag = UI) then
	    begin
	      tid := id;
	      raise Found
	    end
	  )
	builtin_type_table
    in camlp4_macro_exception "unknown size \"%d\"" (Int64.to_int in_size)
  with
    | Found -> !tid
