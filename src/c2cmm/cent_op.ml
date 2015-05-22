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

open Cent
open Qual_name
open Collection
module QNO = Qual_name_op

let create size filename = 
  let ce_tbl_idx_by_qname = QualNameHashtbl.create (size * 2)
  and ceid_to_ce_info = Int.Type_idHashtbl.create (size * 2)
  in
  {
    ceid_to_ce_info = ceid_to_ce_info;
    qname_to_ce_info = ce_tbl_idx_by_qname;
    ce_count = 0;
    num_of_ces_in_files = 0;
    max_preload_ceid = 0;
    ce_tbl_name = filename;
  }


let ce_info_of ce = 
  let (ce_tbl, ceid) = ce
  in
  let ce_info = 
    Int.Type_idHashtbl.find ce_tbl.ceid_to_ce_info ceid
  in ce_info

let ce_map_te f ce = 
  let ce_info = ce_info_of ce
  in ce_info.ce_te <- f ce_info.ce_te;
  ce

let te_of ce = 
  let ce_info = ce_info_of ce
  in ce_info.ce_te

let qname_of ce = 
  let ce_info = ce_info_of ce
  in ce_info.ce_qname
       
let is_tmp ce = 
  let ce_info = ce_info_of ce
  in QNO.is_tmp ce_info.ce_qname


let is_register ce = 
  let ce_info = ce_info_of ce
  in QNO.is_register ce_info.ce_qname

let is_enum ce = 
  let ce_info = ce_info_of ce
  in QNO.is_enum ce_info.ce_qname

let enum_const ce = 
  let ce_info = ce_info_of ce
  in QNO.enum_const ce_info.ce_qname

let is_global ce = 
  let ce_info = ce_info_of ce
  in QNO.is_global ce_info.ce_qname
       
let is_static ce = 
  let ce_info = ce_info_of ce
  in QNO.is_static ce_info.ce_qname

let is_var ce = 
  let ce_info = ce_info_of ce
  in QNO.is_var ce_info.ce_qname

let is_fun ce = 
  let ce_info = ce_info_of ce
  in QNO.is_fun ce_info.ce_qname


(*
let is_extern ce = 
  let ce_info = ce_info_of ce
  in QNO.is_extern ce_info.ce_qname
*)

let te_is_incomplete ce_info: bool =
  (*match ce_info.ce_qname.QN.qn_span with
    | QN.QN_EXTERN -> true
    | _ -> *)
  not (Tent_op.is_complete_te ce_info.ce_te)

let alloc_ceid ce_tbl : ceid =
  let ceid = ce_tbl.ce_count
  in ce_tbl.ce_count <- ce_tbl.ce_count + 1;
  ceid


let get_ceid ce_tbl qname =
  try
    let old_ce_info = QualNameHashtbl.find ce_tbl.qname_to_ce_info qname
    in old_ce_info.ce_id
  with
    | Not_found ->
	alloc_ceid ce_tbl


let add_ce_info ce_tbl ce_info = 
  try
    let old_ce_info = 
      QualNameHashtbl.find ce_tbl.qname_to_ce_info ce_info.ce_qname
    in 
    if (old_ce_info.ce_id <> ce_info.ce_id) then
      camlp4_macro_exception "ce '%s' has old_ceid '%d' and is added as new_ceid '%d'\n"
	(Qual_name_printer.to_decl_str ce_info.ce_qname) old_ce_info.ce_id ce_info.ce_id
    else if (not old_ce_info.ce_is_real_ent) then
      begin
	if (ce_info.ce_is_real_ent) then
	  begin
	    old_ce_info.ce_te <- ce_info.ce_te;
	    ce_info.ce_is_real_ent <- true;
	  end
	else
	  ((*silence*))
      end
    else if old_ce_info.ce_is_real_ent & ce_info.ce_is_real_ent &
      (not (Tent_op.typ_eq old_ce_info.ce_te ce_info.ce_te)) then
	let old_te = camlp4_macro_str_pp_print 
	  (fun fm -> Tent_c_printer.pp_print_c_type_only fm 
	    old_ce_info.ce_te)
	and new_te = camlp4_macro_str_pp_print 
	  (fun fm -> Tent_c_printer.pp_print_c_type_only fm 
	    ce_info.ce_te)
	in
	camlp4_macro_exception "ce '%s' has old_te '%s' and is added as new_te '%s'\n"
	  (Qual_name_printer.to_decl_str ce_info.ce_qname) old_te new_te    
	  (*
	    if not (QNO.is_extern ce_info.ce_qname) then
	    begin
	    QualNameHashtbl.replace ce_tbl.qname_to_ce_info 
	    ce_info.ce_qname ce_info;
	    Int.Type_idHashtbl.replace ce_tbl.ceid_to_ce_info 
	    ce_info.ce_id ce_info
	    end
	    else ()
	  *)    
    else if old_ce_info.ce_is_real_ent & not ce_info.ce_is_real_ent then
      ((* silence *))
    else if QNO.is_param ce_info.ce_qname then
      ((* silence *))
    else
      camlp4_macro_warning "ce '%s' is added more than once\n"
	(Qual_name_printer.to_decl_str ce_info.ce_qname)
  with
    | Not_found ->
	let _ = 
	  QualNameHashtbl.add 
	    ce_tbl.qname_to_ce_info ce_info.ce_qname ce_info
	in
	Int.Type_idHashtbl.add
	  ce_tbl.ceid_to_ce_info ce_info.ce_id ce_info
