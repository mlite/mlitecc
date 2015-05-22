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
open Csize
open Cent
open Collection

module QNP = Qual_name_printer

let pp_print_ce (fm:formatter) (ce:ce) : unit = 
  let (ce_tbl, ceid) = ce
  in
  let ce_info = Int.Type_idHashtbl.find ce_tbl.ceid_to_ce_info ceid
  in
  let typ = ce_info.ce_te
  and str = QNP.to_decl_str ce_info.ce_qname
  in
  pp_open_box fm 0;
  Tent_c_printer.pp_print_t fm typ
    ~print_def:false ~c_declarator_opt:(Some str);
  pp_close_box fm ()



let print_ce (ce:ce) :unit = (** can be invoked by ocaml debugger **)
  let str = camlp4_macro_str_pp_print
    (fun fm ->
      pp_open_box fm 0;
      pp_print_ce fm ce;
      pp_close_box fm ())
  in print_string str
