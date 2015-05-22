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

external identity : 'a -> 'a = "%identity"

type 'a int_t = int
let int_t = identity
let t_int = identity
let any_int = identity

let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let neg = ( ~- )

let successor = succ
let predecessor = pred
let increment = incr

let print_int_t = Format.print_int

type 'a string_t = string
let string_t = identity
let t_string = identity
let any_string = identity

let concat = ( ^ )
let concat_list = String.concat

let print_string_t = Format.print_string
