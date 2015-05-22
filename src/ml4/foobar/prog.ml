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

record bob = { foo : string = "Hello";
               bar : string;
               mutable n : int = 1 }

record weird = { x : weird option = (Some (create_weird ~x:None ())) }

let _ =
  let x = create_bob ~bar:"World" () in
  x.n <- x.n + 1;
  Printf.printf "%s %s %i\n" x.foo x.bar x.n
