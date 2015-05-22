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

let get a i = 
  let idx = i in
  try 
    Array.get a i
  with 
    Invalid_argument s ->
      let msg = Printf.sprintf "array.length %d, get element index %d" 
	  (Array.length a) idx 
      in
      raise (Invalid_argument (s ^ ":" ^ msg))
	
let set a i e = 
  let idx = i in
  try
    Array.set a i e
  with
    Invalid_argument s ->
      let msg = Printf.sprintf "array.length %d, set element index %d" 
	  (Array.length a) idx 
      in
      raise (Invalid_argument (s ^ ":" ^ msg))
