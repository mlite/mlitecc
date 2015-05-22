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

 (** **)
type size = int
and offset = int
and offset64 = int64
and size64 = int64
and contour_id = int
and tmp = int
and gnode_id = int
and semcoord_id = int
and graphcoord_id = int
and type_id = int
and stack_addr = int
and heap_addr = int
and tok_cat_id = int 
and abs_set = int
and const_id = int
and meta_tok_id = int
and sta_proc_addr = int
and dll_proc_addr = int
and generic_addr = int
and len = int
and stack_depth = int
and heap_serno = int
and sn = int
and att = int
and typ = int
and k = int
and a = int
and c = int
and d = int
and b = int
and l = int
and o = int
and s = int
and t = int

external identity : 'a -> 'a = "%identity"

type 'a int_t = int
let int_t = identity
let t_int = identity
let t_string = identity
let string_t = identity
let any_int = identity

let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let neg = ( ~- )

let successor = succ
let predecessor = pred
let increment = incr

let print_int_t = print_int
