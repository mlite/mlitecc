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
open Int

let generic_addr: Int.generic_addr -> unit =
  fun i ->
    print_int_t i

let sta_proc_addr: Int.sta_proc_addr -> unit = 
  fun i ->
    print_int_t i

let dll_proc_addr: Int.dll_proc_addr -> unit = 
  fun i ->
    print_int_t i
      
let type_id: Int.type_id -> unit =
  fun i ->
    print_int_t i

let size: Int.size -> unit = 
  fun i -> 
    print_int_t i


let contour_id: Int.contour_id -> unit = 
  fun i -> 
    print_int_t i
      
let stack_addr: Int.stack_addr -> unit = 
  fun i -> 
    print_int_t i

let heap_addr: Int.heap_addr -> unit = 
  fun i -> 
    print_int_t i
      
let tmp: Int.tmp -> unit = 
  fun i -> 
    print_int_t i
      
let tok_cat_id: Int.tok_cat_id -> unit = 
  fun i ->
    print_int_t i
      
let offset: Int.offset -> unit = 
  fun i ->
    print_int_t i
      
      
let const_id: Int.const_id -> unit =
  fun i ->
    print_int_t i
      
