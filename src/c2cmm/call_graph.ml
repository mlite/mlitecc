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

let version = "ISO/IEC 9899:TC"
let description = ["ISO/IEC 9899:TC"]
let suffix = ".000.call_graph"
    
type node = 
  | Unresolved of string 
  | Function_Ptr of int
  | Leaf of string 
  | Recursion of string 
  | Subtree of string * (node * int) list

and property = 
  | Deref
  | AddrOf
  | String
  | Malloc
  | Loop
  | Ifelsethen
  | Pointer_level of int
