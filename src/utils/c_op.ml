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

let c_bor: int -> int -> int =
  fun v0 v1 ->
    v0 + v1

let c_bxor: int -> int -> int =
  fun v0 v1 -> assert false

let c_band: int -> int -> int =
  fun v0 v1 -> assert false

let c_mod: int -> int -> int = 
  fun v0 v1 ->  v0 mod v1

let c_shr: int -> int -> int = 
  fun v1 v2 ->
    let v = ref v1
    and i = ref v2
    in
    while !i > 0 do
      v := (!v / 2);
      decr i
    done;
    !v

let c_shl: int -> int -> int = 
  fun v1 v2 ->
    let v = ref v1
    and i = ref v2
    in
    while !i > 0 do
      v := (!v * 2);
      decr i
    done;
    !v

let c_div: int -> int -> int = 
  fun v1 v2 -> v1 / v2

let c_mul: int -> int -> int = 
  fun v1 v2 -> v1 * v2

let c_sub: int -> int -> int =
  fun v1 v2 -> v1 - v2

let c_add: int -> int -> int = 
  fun v1 v2 -> v1 + v2
