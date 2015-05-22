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

type t =
  | TARGET
  | JOIN of int
  | SEQ of malloc_site_opt

and malloc_site_opt = int option ref

let get_num_string nid = 
  if      nid < 10 then
    Printf.sprintf "000000%d" nid
  else if nid < 100 then
    Printf.sprintf "00000%d" nid
  else if nid < 1000 then
    Printf.sprintf "0000%d" nid
  else if nid < 10000 then
    Printf.sprintf "000%d" nid
  else if nid < 100000 then
    Printf.sprintf "00%d" nid
  else if nid < 1000000 then
    Printf.sprintf "0%d" nid
  else
    Printf.sprintf "%d" nid


let prefix = Random_prefix.get "lab_"

let pp_print_jmp_join_label: formatter -> int -> unit =
  fun fm nid ->
    let num = get_num_string nid
    in
    fprintf fm "goto %s%s;" prefix num

let pp_print_jmp_label: formatter -> int -> unit =
  fun fm nid ->
    let num = get_num_string nid
    in
    fprintf fm "goto %s%s;" prefix num

let pp_print_label: formatter -> t -> int -> unit =
  fun fm t nid ->
    let num = get_num_string nid
    in
    match t with
    | TARGET -> 
	fprintf fm "/*_%s_*/%s%s:" num prefix num
    | JOIN n -> 
	fprintf fm "/*_%s_[%d]*/%s%s:" num n prefix num
    | SEQ n -> 
	begin
	  match !n with
	  | Some v ->
	      fprintf fm "/*_%s_:malloc#%d*/" num v
	  | None ->
	      fprintf fm "/*_%s_*/" num 
	end
