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
open Collection
include Cent

module QNP = Qual_name_printer

let indent = 2

let enable_m_name_debug = true
let enable_type_signature_debug = true


let pp_print_c_comma: formatter -> unit =
  fun fm ->
    pp_print_string fm ",";
    pp_print_space fm ()

      
let pp_print_c_semicolon: formatter -> unit =
  fun fm ->
    pp_print_string fm ";";
    pp_print_space fm ()


  
let pp_print_typ_id (fm:formatter) ceid : unit = 
  pp_open_box fm 0;
  fprintf fm "T_%d" ceid;
  pp_close_box fm ()

let pp_print_te_attribute fm att = 
  let str = match att with
    | GCC_weak -> "weak"
    | GCC_always_inline -> "always_inline"
    | GCC_alias str -> "alias(" ^ str ^ ")"
    | GCC_const -> "const"
    | GCC_malloc -> "malloc"
    | GCC_noinline -> "noinline"
    | GCC_noreturn -> "noreturn"
    | GCC_pure -> "pure"
    | GCC_section str -> "section(" ^ str ^ ")"
    | GCC_unused -> "unused"
    | GCC_nothrow -> "nothrow"
    | GCC_no_instrument_function -> assert false
    | GCC_constructor -> "constructor"
    | GCC_destructor -> "destructor"
    | GCC_used -> "used"
    | GCC_warn_unused_result -> "warn_unused_result"
    | GCC_regparm i -> assert false
    | GCC_stdcall -> assert false
    | GCC_fastcall -> assert false
    | GCC_cdecl -> assert false
    | GCC_format (str, i, j) -> assert false
    | GCC_format_arg i -> assert false
    | GCC_nonnull lst -> assert false
    | GCC_all_nonnull -> assert false
    | GCC_deprecated -> "deprecated"
    | GCC_visibility str -> assert false
	(* variable attributes *)
    | GCC_cleanup str -> assert false
    | GCC_common -> "common"
    | GCC_nocommon -> "nocommon"
    | GCC_shared -> "shared"
  in pp_print_string fm str
       

let pp_print_te_attribute_list fm lst = 
  if lst <> [] then
    begin
      pp_open_box fm 0;
      pp_print_space fm ();
      pp_print_string fm "__attribute__((";
      Mlite_printer.pp_print_list fm
	pp_print_te_attribute
	pp_print_c_comma
	lst;
      pp_print_string fm "))";
      pp_close_box fm ()
    end

let pp_print_attributed_te_info fm (ceid, attribute_list) = 
  pp_print_typ_id fm ceid;
  pp_print_space fm ();
  pp_print_te_attribute_list fm attribute_list
      

let pp_print_typ_id_log: formatter -> te -> unit = 
  fun fm typ ->
    if !Mlite_config.enable_type_table_print then
      fprintf fm "/* T_%d */" (snd typ)

let pp_print_serno: formatter -> int -> unit = 
  fun fm i ->
    pp_open_box fm 0;
    begin
      pp_print_string fm "[";
      if i < 10 then
	pp_print_string fm ("   " ^ (string_of_int i))
      else if i < 100 then
	pp_print_string fm ("  " ^ (string_of_int i))
      else if i < 1000 then
	pp_print_string fm (" " ^ (string_of_int i))
      else
	pp_print_string fm (string_of_int i);
      pp_print_string fm "]";
      pp_print_string fm "  ";
    end;
    pp_close_box fm ()

let pp_print_size_str fm i =
  let len = String.length i
  in
  if len < 2 then
    fprintf fm "   %s" i
  else if len  < 3 then
    fprintf fm "  %s" i
  else if len < 4 then
    fprintf fm " %s" i
  else
    fprintf fm "%s" i      

let pp_print_size fm i =
  let str = (string_of_csize i)
  in
  if i < 10L then
    fprintf fm "   %s" str
  else if i < 100L then
    fprintf fm "  %s" str
  else if i < 1000L then
    fprintf fm " %s" str
  else
    fprintf fm "%st" str

let pp_print_align fm i =
  let str = (string_of_csize i)
  in
  if i < 10L then
    fprintf fm " %s" str
  else 
    fprintf fm "%s" str

let pp_print_decl_ce_info (fm:formatter) (ce_tbl:ce_tbl) (ce_info:ce_info) : unit =
  let v = ce_info
  in
  pp_open_vbox fm 0;
  QNP.pp_print_qn_decl_str fm v.ce_qname;
  pp_close_box fm ()

let pp_print_ref_ce_info (fm:formatter) (ce_tbl:ce_tbl) (ce_info:ce_info) : unit =
  let v = ce_info
  in
  pp_open_vbox fm 0;
  QNP.pp_print_qn_ref_str fm v.ce_qname;
  pp_close_box fm ()



let pp_print_ce_tbl (fm:formatter) (ce_tbl:ce_tbl): unit = 
  let pp_print_all_typ_ids () =
    let len = ce_tbl.ce_count
    in
    let type_array = Array.make len None
    in
    Int.Type_idHashtbl.iter
      (fun k b ->
	Array.set type_array k (Some b)
      ) ce_tbl.ceid_to_ce_info;
    pp_open_vbox fm 0;
    begin
      if enable_m_name_debug then
	begin
	  pp_print_string fm 
	    "-TYPE---C---sizeof--alignof--m_name---------------------------------------";
	  pp_print_space fm ();
	  for i = 0 to len - 1 do
	    let v_opt = Array.get type_array i
	    in
	    match v_opt with
	      | Some v ->
		  begin
		    fprintf fm "%d qname:%s" i (Qual_name_printer.to_decl_str v.ce_qname);
		    pp_print_space fm ()
		  end
	      | _ -> ()
	  done;
	  pp_print_space fm ();
	  pp_print_string fm 
	    "--------------------------------------------------------------------------";
	  pp_print_space fm ();
	end
    end;
    pp_close_box fm ()
  in
  pp_open_vbox fm 0;
  begin
    pp_print_string fm "/*********************";
    pp_print_space fm ();
    pp_print_string fm "  begin of type table";
    pp_print_space fm ();
    pp_open_vbox fm 2;
    begin
      pp_print_space fm ();
      pp_print_all_typ_ids ();
      pp_print_space fm ();
    end;
    pp_close_box fm ();
    pp_print_space fm ();
    pp_print_string fm "  end of type table";
    pp_print_space fm ();
    pp_print_string fm "**********************/";
  end;
  pp_close_box fm ()

let print_ce_tbl: ce_tbl -> unit = (** can be invoked by ocaml debugger **)
  fun ce_tbl ->
    let str = camlp4_macro_str_pp_print
	(fun fm ->
	  pp_open_box fm 0;
	  let _ = 
	    try
	      pp_print_ce_tbl fm ce_tbl;
	    with
	      Not_found ->
	      pp_print_string fm "error"
	  in
	  pp_close_box fm ())
    in
    print_string str
      

let pp_print_decl_ce fm ((ce_tbl, typ_id):ce) =
  try
    let ce_info = Int.Type_idHashtbl.find ce_tbl.ceid_to_ce_info typ_id
    in pp_print_decl_ce_info fm ce_tbl ce_info
  with
      Not_found ->
	fprintf fm "typ_id %d is not in the ce_tbl" typ_id
	  
	  
let pp_print_ref_ce fm ((ce_tbl, typ_id):ce) =
  try
    let ce_info = Int.Type_idHashtbl.find ce_tbl.ceid_to_ce_info typ_id
    in pp_print_ref_ce_info fm ce_tbl ce_info
  with
      Not_found ->
	fprintf fm "typ_id %d is not in the ce_tbl" typ_id


