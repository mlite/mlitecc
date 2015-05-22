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

open Collection
include Tent
open Csize
open Qual_name

open Const_folding
module CA = Const_folding
module QNO = Qual_name_op
module QNP = Qual_name_printer

exception Typ_exist of teid

exception FoundTeInfo of te_info

let te_eq ((te_tbl0, tid0):te) ((te_tbl1, tid1):te) : bool =
  (te_tbl0 == te_tbl1) & (tid0 = tid1)

let typ_eq = te_eq

let is_enum_kind = function
  | Enum _ -> true
  | _ -> false
	  
let is_norm_kind = function
  | Normal _ -> true
  | _ -> false

let is_alias_kind = function
  | Typedef _ -> true
  | _ -> false

let is_bit_kind = function
  | Bits _ -> true
  | _ -> false

let is_abs_fun_kind = function
  | Abs_function _ -> true
  | _ -> false
      

let load_target_metrics: unit -> target_metrics = 
  fun () ->
    { 
      little_endian = Mach.little_endian;
      pointer_size = Mach.sizeof_pointer;
      pointer_alignof = Mach.alignof_pointer;
      enum_size = Mach.sizeof_enum;
      enum_alignof = Mach.alignof_enum;
      builtin_type_num = Mach.numof_builtin_types;
    }

let load_type_table: int -> 
  te_info QualNameHashtbl.t * 
    te_info Int.Type_idHashtbl.t * 
    te_info StringHashtbl.t = 
  fun size ->
    let size = size + 200 (** builtin typs **)
    in
    let te_tbl_idx_by_sname = 
      StringHashtbl.create (int_of_float ((float_of_int size) *. 1.5));
    and te_tbl_idx_by_qname = 
      QualNameHashtbl.create (size * 2)
    and teid_to_te_info = Int.Type_idHashtbl.create (size * 2);
    in
    let _ = List.iter
      (fun (m_id, m_kind, m_size, m_align, m_name, max, min) -> 
	let te_info = 
	  { 
	    m_id = m_id;
	    m_kind = Builtin;
	    m_size = Byte_size m_size;
	    m_align = m_align;
	    m_sizeof_computed = true;
	    m_alignof_computed = true;
	    m_runtime = false;
	    m_name = 
	      {
		QN.qn_namespace = QN.QN_CLANG;
		QN.qn_span = QN.QN_AUTO;
		QN.qn_class = QN.QN_TYPE_NAME;
		QN.qn_scopes = [];
		QN.qn_init = QN.QN_NULL;
		QN.qn_sname = m_name;
	      };
	    m_ptr_teid_opt = None;
	    m_has_alias = true;
	    m_attribute_list = [];
	  }
	in
	let _ = 
	  try
	    ignore(Int.Type_idHashtbl.find teid_to_te_info te_info.m_id)
	  with
	    | Not_found ->
		begin
		  Int.Type_idHashtbl.add 
		    teid_to_te_info te_info.m_id te_info		 
		end
	in
	StringHashtbl.add 
	  te_tbl_idx_by_sname te_info.m_name.QN.qn_sname te_info;
	QualNameHashtbl.add
	  te_tbl_idx_by_qname te_info.m_name te_info;
      ) Mach.builtin_type_table
    in
    (te_tbl_idx_by_qname, teid_to_te_info, te_tbl_idx_by_sname)
      
let make_sname: string -> string =
  fun str ->
    let prefix = ref ""
    in
    String.iter
      (fun c ->
	match c with
	  | '-' -> prefix := !prefix ^ "_"
	  | '.' -> prefix := !prefix ^ "_"
	  | _ -> prefix := !prefix ^ (String.make 1 c)
      ) str;
    !prefix



let create size file_name =
  let target_metrics = load_target_metrics ()
  and (te_tbl_idx_by_qname, teid_to_te_info, te_tbl_idx_by_sname) = 
    load_type_table size
  in
  let te_tbl = 
    {
      teid_to_te_info = teid_to_te_info;
      qname_to_te_info = te_tbl_idx_by_qname;
      target_metrics = target_metrics;
      max_preload_typ = 0;
      te_count = target_metrics.builtin_type_num;
      num_of_typs_in_files = 0;
      typ_equiv_class_table = Int.Type_idHashtbl.create (size * 2);
      enum_item_const_table = QualNameHashtbl.create size;
      te_tbl_name = file_name;
    }
  in (te_tbl, te_tbl_idx_by_sname)



let create_empty_te_tbl size file_name = 
  let target_metrics = load_target_metrics ()
  in
  let te_tbl = 
    {
      teid_to_te_info = Int.Type_idHashtbl.create (size * 2);
      qname_to_te_info = QualNameHashtbl.create (size * 2);
      target_metrics = target_metrics;
      max_preload_typ = 0;
      te_count = target_metrics.builtin_type_num;      
      num_of_typs_in_files = 0;
      typ_equiv_class_table = Int.Type_idHashtbl.create (size * 2);
      enum_item_const_table = QualNameHashtbl.create size;
      te_tbl_name = file_name;
    }
  in (te_tbl, QualNameHashtbl.create (size * 2))


let set_max_preload_typ te_tbl = 
  te_tbl.max_preload_typ <- (te_tbl.te_count - 1)
    
let mk_top_te_qname str = 
  {
    QN.qn_namespace = QN.QN_CLANG;
    QN.qn_span = QN.QN_AUTO;
    QN.qn_class = QN.QN_TYPE_NAME;
    QN.qn_scopes = [];
    QN.qn_init = QN.QN_NULL;
    QN.qn_sname = str;
  }



let sizeof_pointer te_tbl = te_tbl.target_metrics.pointer_size
    
let alignof_pointer te_tbl = te_tbl.target_metrics.pointer_alignof
    
let sizeof_enum te_tbl = te_tbl.target_metrics.enum_size
    
let alignof_enum te_tbl = te_tbl.target_metrics.enum_alignof

let te_info_of te: te_info = 
  let (te_tbl, teid) = te
  in
  let te_info = 
    Int.Type_idHashtbl.find te_tbl.teid_to_te_info teid
  in te_info

let te_kind_of te: te_kind = 
  let (te_tbl, teid) = te
  in
  let te_info = 
    Int.Type_idHashtbl.find te_tbl.teid_to_te_info teid
  in te_info.m_kind

let qname_of te = 
  let te_info = te_info_of te
  in te_info.m_name


let rec unwrap_qual_typedef_normal_map te (f: te -> 'a):'a =
  match te_kind_of te with
    | Qualified v ->
	unwrap_qual_typedef_normal_map (fst te, v.m_qualified_teid) f
    | Typedef v ->
	unwrap_qual_typedef_normal_map (fst te, v.m_typedef_lhs_teid) f
    | Normal v ->
	unwrap_qual_typedef_normal_map (fst te, v.m_norm_norm_teid) f
    | Attribute (teid, _) ->
	unwrap_qual_typedef_normal_map (fst te, teid) f
    | _ -> f te

let rec is_void_typ te: bool = 
  match te_kind_of te with
    | Builtin ->  snd te = Mach.cvoid_id
    | Typedef v -> is_void_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_void_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_void_typ (fst te, teid)
    | _ -> false
	
let rec is_complete_te te: bool =
  let te_kind = te_kind_of te
  in
  match te_kind with
    | Typedef v -> is_complete_te (fst te, v.m_typedef_lhs_teid)
    | Struct_name  -> false
    | Union_name -> false
    | Enum_name -> false
    | Array v ->
	begin
	  match v.m_array_cardi with
	    | ARRAY_FIXED _ -> true
	    | ARRAY_VARIABLE  -> false
	end
    | Qualified v -> is_complete_te (fst te, v.m_qualified_teid)
    | Attribute (teid,_) -> is_complete_te (fst te, teid)
    | _ -> true	

let rec is_crt_function_typ te: bool =
  match te_kind_of te with
    | Crt_function _ -> true
    | Typedef v -> is_crt_function_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_crt_function_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_crt_function_typ (fst te, teid)
    | _ -> false
	
let rec is_abs_function_typ te: bool =
  match te_kind_of te with
    | Abs_function _ -> true
    | Typedef v -> is_abs_function_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_abs_function_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_abs_function_typ (fst te, teid)
    | _ -> false


let rec abs_function_has_hidden_ret te: bool =
  match te_kind_of te with
    | Abs_function v -> 
	begin
	  match v.aft_hidden_ret_param with
	    | Some v -> true
	    | None -> false
	end
    | Typedef v -> abs_function_has_hidden_ret (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> abs_function_has_hidden_ret (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> abs_function_has_hidden_ret (fst te, teid)
    | _ -> false
	
let rec abs_function_get_hidden_ret te: te =
  match te_kind_of te with
    | Abs_function v -> 
	begin
	  match v.aft_hidden_ret_param with
	    | Some v -> (fst te, v)
	    | None -> assert false
	end
    | Typedef v -> abs_function_get_hidden_ret (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> abs_function_get_hidden_ret (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> abs_function_get_hidden_ret (fst te, teid)
    | _ -> assert false


let is_function_te te: bool =
  (is_crt_function_typ te) or (is_abs_function_typ te)

let rec is_builtin_typ te: bool =
  match te_kind_of te with
    | Builtin -> true
    | Typedef v -> is_builtin_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_builtin_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_builtin_typ (fst te, teid)
    | _ -> false
	
	
let rec is_struct_typ te: bool = 
  match te_kind_of te with
    | Struct _ -> true    
    | Typedef v -> is_struct_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_struct_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_struct_typ (fst te, teid)
    | _ -> false


let rec is_union_typ te: bool = 
  match te_kind_of te with
    | Union _ -> true
    | Typedef v -> is_union_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_union_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_union_typ (fst te, teid)
    | _ -> false

	
let rec is_enum_typ te: bool =
  match te_kind_of te with
    | Enum _ -> true
    | Typedef v -> is_enum_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_enum_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_enum_typ (fst te, teid)
    | _ -> false

let rec is_array_typ te: bool =
  match te_kind_of te with
    | Array v -> 
	begin
	  match v.m_array_cardi with
	    | ARRAY_FIXED _ -> true
	    | _ -> false
	end
    | Typedef v -> is_array_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_array_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_array_typ (fst te, teid)
    | _ -> false

	
let rec is_xarray_typ te: bool =
  match te_kind_of te with
    | Array v -> 
	begin
	  match v.m_array_cardi with
	    | ARRAY_VARIABLE -> true
	    | _ -> false
	end
    | Typedef v -> is_xarray_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_xarray_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_xarray_typ (fst te, teid)
    | _ -> false

let rec is_abs_function_ptr_typ te: bool =
  match te_kind_of te with
    | Pointer elmt_teid -> is_abs_function_typ (fst te, elmt_teid)
    | Typedef v -> is_abs_function_ptr_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v -> is_abs_function_ptr_typ (fst te, v.m_qualified_teid)
    | Attribute (teid, _) -> is_abs_function_ptr_typ (fst te, teid)
    | _ -> false

let rec is_qual_te te =
  match te_kind_of te with
    | Typedef v -> is_qual_te (fst te, v.m_typedef_lhs_teid)
    | Qualified _ -> true
    | Attribute (teid, _) -> is_qual_te (fst te, teid)
    | _ -> false

let rec is_bit_te te: bool =
  match te_kind_of te with
    | Bits _ -> true
    | Typedef v -> is_bit_te (fst te, v.m_typedef_lhs_teid)
    | Attribute (teid, _) -> is_bit_te (fst te, teid)
    | _ -> false

let rec elmt_of te: te = 
  match te_kind_of te with
    | Pointer elmt_teid -> (fst te, elmt_teid)
    | Array v -> (fst te, v.m_elmt_teid)
    | Typedef v -> elmt_of (fst te, v.m_typedef_lhs_teid)
    | Normal v -> elmt_of (fst te, v.m_norm_norm_teid)
    | Qualified v -> elmt_of (fst te, v.m_qualified_teid)
    | Attribute (teid,_) -> elmt_of (fst te, teid)
    | _ ->
	camlp4_macro_exception 
	  "\ndeferencing of non pointer type %d\n" (snd te)





let rd_norm_type_orig_id (te_tbl:te_tbl) ~(teid:teid) :teid = 
  match te_kind_of (te_tbl, teid) with
    | Normal norm_table_entry -> 
	norm_table_entry.m_norm_orig_teid
    | _ -> assert false

let rd_norm_type_norm_id (te_tbl:te_tbl) ~(teid:teid) :teid = 
  match te_kind_of (te_tbl, teid) with
    | Normal norm_table_entry -> 
	norm_table_entry.m_norm_norm_teid
    | _ -> assert false
	
(** ----------------------------------------------- **)
let rec rd_equiv_teid (te_tbl:te_tbl) ~(teid:teid) :teid = 
  let te_kind = te_kind_of (te_tbl,teid)
  in
  match te_kind with 
    | Typedef v ->
	begin
	  rd_equiv_teid 
	    te_tbl ~teid:v.m_typedef_lhs_teid
	end
    | Normal v ->
	begin
	  rd_equiv_teid
	    te_tbl ~teid:v.m_norm_norm_teid
	end
    | _ -> teid




let unqual_of te: te = 
  let te_info = te_info_of te
  in
  match te_info.m_kind with
    | Qualified qual_te_info -> (fst te, qual_te_info.m_qualified_teid)
    | _ -> assert false

	  
let rd_enum_constant te_tbl ~(id:QN.t) : int option =
  try
    Some (QualNameHashtbl.find te_tbl.enum_item_const_table id)
  with
    | Not_found -> None
	
let rd_enum_items te_tbl ~(teid:teid) : QN.t list = 
  let te_info = te_info_of (te_tbl, teid)
  in
  match te_info.m_kind with
    | Enum enum_te_info -> 
	List.map (fun e -> e.m_enum_item_name) enum_te_info.m_enum_item_infos
    | _ -> assert false
	    


let find_field (field_table:field_table) ~(field:string) : string option * teid * csize = 
  if field = "__missing_field_name" then
    camlp4_macro_exception "__missing_field_name is not supported yet";
  let (info, found, count) = 
    List.fold_left
      (fun (info, found, count) v -> 
	match v.field_sname_opt with
	  | Some field_sname ->
	      if field_sname = field then
		((None, v.field_teid, v.field_offset), true, count + 1)
	      else
		(info, found, count)
	  | None -> (info, found, count)
      ) ((None, 0, 0L), false, 0) field_table.m_field_infos
  in
  if count > 0 & found then
    info
  else
    raise Not_found
      


let is_variadic te_tbl ~(teid:teid) : bool =
  let te_info = te_info_of (te_tbl, teid)
  in
  match te_info.m_kind with
    | Abs_function abs_fun_te_info ->
	abs_fun_te_info.aft_va_arg
    | _ -> assert false
	

let rd_in_param_types te_tbl ~(teid:teid) : teid list = 
  let te_info = te_info_of (te_tbl, teid)
  in
  match te_info.m_kind with
    | Abs_function abs_fun_te_info ->
	abs_fun_te_info.aft_param_teids
    | _ -> assert false

let rec formal_params_of (te:te) : (te * param_kind(*QN.t*)) list =
  let te_kind = te_kind_of te
  and te_tbl = fst te
  in
  match te_kind with
    | Abs_function _ -> []
    | Crt_function crt_fun_te_info -> 
	begin
	  let abs_fun_te_info = 
	    let te_kind = te_kind_of (te_tbl, crt_fun_te_info.m_crt_fun_abs_teid)
	    in match te_kind with
	      | Abs_function v -> v
	      | _ -> assert false
	  in
	  List.map2 (fun teid id -> ((te_tbl, teid), id))
	    abs_fun_te_info.aft_param_teids
	    crt_fun_te_info.m_crt_fun_in_params
	end
    | Normal norm_table_entry -> 
	formal_params_of (te_tbl, norm_table_entry.m_norm_norm_teid)
    | _ -> assert false


let alloc_teid te_tbl : teid =
  let teid = te_tbl.te_count
  in te_tbl.te_count <- te_tbl.te_count + 1;
  teid


let rd_bit_type_base_type te_tbl ~(teid:teid) : teid=
  match te_kind_of (te_tbl, teid) with
    | Bits bit_te_info ->
	bit_te_info.m_bit_base_teid
    | _ -> assert false


let get_bit_type_decl te : bit_te_info = 
  match te_kind_of te with
    | Bits bit_te_info ->
	bit_te_info
    | _ -> assert false
	

let rd_bittyp_size te_tbl ~(teid:teid) : csize =
  match te_kind_of (te_tbl, teid) with
    | Bits bit_te_info ->
	bit_te_info.m_bit_size
    | _ -> assert false


let rd_builtin_type_matrix te_tbl ~(teid:teid): csize * csize =
  let teid = rd_equiv_teid te_tbl ~teid:teid
  in
  let te_info = 
    Int.Type_idHashtbl.find te_tbl.teid_to_te_info teid
  in
  assert (not (is_bit_te (te_tbl, teid)));
  let size = 
    if te_info.m_sizeof_computed then
      match te_info.m_size with
	| Byte_size v -> v
	| Incomplete ->
	    camlp4_macro_exception 
	      "incomplete type %d\n" (teid) 
    else
      camlp4_macro_exception 
	"read sizeof non builtin type '%d'\n" (teid)
  in (size, te_info.m_align)
      

let bit_type_masks bit_te_info =
  let mask = bit_te_info.m_bit_mask
  and emask = bit_te_info.m_bit_emask
  and bnot_emask = bit_te_info.m_bit_bnot_emask
  in
  if bit_te_info.m_bit_mask_typ = 
    Mach.cuchar_id then
      (Mach.cuchar_id, 
      CA.cval_ext_of_cval (CA.CUCHAR (CA.cuchar_of_string mask)),
      CA.cval_ext_of_cval (CA.CUCHAR (CA.cuchar_of_string emask)),
      CA.cval_ext_of_cval (CA.CUCHAR (CA.cuchar_of_string bnot_emask)))
  else if bit_te_info.m_bit_mask_typ = 
    Mach.cushort_id then
      (Mach.cushort_id,
      CA.cval_ext_of_cval (CA.CUSHORT (CA.cushort_of_string mask)),
      CA.cval_ext_of_cval (CA.CUSHORT (CA.cushort_of_string emask)),
      CA.cval_ext_of_cval (CA.CUSHORT (CA.cushort_of_string bnot_emask)))
  else if bit_te_info.m_bit_mask_typ = 
    Mach.cuint_id then
      (Mach.cuint_id,
      CA.cval_ext_of_cval (CA.CUINT (CA.cuint_of_string mask)),
      CA.cval_ext_of_cval (CA.CUINT (CA.cuint_of_string emask)),
      CA.cval_ext_of_cval (CA.CUINT (CA.cuint_of_string bnot_emask)))
  else if bit_te_info.m_bit_mask_typ = 
    Mach.culong_id then
      (Mach.culong_id,
      CA.cval_ext_of_cval (CA.CULONG (CA.culong_of_string mask)),
      CA.cval_ext_of_cval (CA.CULONG (CA.culong_of_string emask)),
      CA.cval_ext_of_cval (CA.CULONG (CA.culong_of_string bnot_emask)))
  else if bit_te_info.m_bit_mask_typ = 
    Mach.cullong_id then 
      (Mach.cullong_id,
      CA.cval_ext_of_cval (CA.CULLONG (CA.cullong_of_string mask)),
      CA.cval_ext_of_cval (CA.CULLONG (CA.cullong_of_string emask)),
      CA.cval_ext_of_cval (CA.CULLONG (CA.cullong_of_string bnot_emask)))
  else
    assert false



let insert_te_info te_tbl te_info = 
  try
    let _ = Int.Type_idHashtbl.find te_tbl.teid_to_te_info
      te_info.m_id
    in assert false
  with
    | Not_found -> 
	begin
	  Int.Type_idHashtbl.add 
	    te_tbl.teid_to_te_info te_info.m_id te_info;
	  QualNameHashtbl.add 
	    te_tbl.qname_to_te_info te_info.m_name te_info
	end


let add_bit_te_info te_tbl bit_te_info: teid = 
  let (base_size, base_align) = 
    rd_builtin_type_matrix te_tbl ~teid:bit_te_info.m_bit_base_teid
  in
  let type_name = mk_top_te_qname 
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_bit_te_info fm bit_te_info))
  in
  try
    begin	
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let  te_info = 
	{
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Bits bit_te_info;
	  m_size = Byte_size base_size;
	  m_align = base_align;
	  m_sizeof_computed = true;
	  m_alignof_computed = true;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = false;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist teid -> teid

	
let create_bit_te_info ~base_te ~(bits:csize) ~(lsb:csize) : bit_te_info = 
  let base_te = unwrap_qual_typedef_normal_map base_te (fun v -> v) 
  in
  let (te_tbl, base_typ) = base_te
  in
  let base_entry = te_info_of (te_tbl, base_typ)
  and cuchar_entry = te_info_of (te_tbl, Mach.cuchar_id)
  and cushort_entry = te_info_of (te_tbl, Mach.cushort_id)
  and cuint_entry = te_info_of (te_tbl, Mach.cuint_id)
  and culong_entry = te_info_of (te_tbl, Mach.culong_id)
  and cullong_entry = te_info_of (te_tbl, Mach.cullong_id)
  in
  assert base_entry.m_sizeof_computed;
  let base_size = match base_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  and cuchar_size = match cuchar_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  and cushort_size = match cushort_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  and cuint_size = match cuint_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  and culong_size = match culong_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  and cullong_size = match cullong_entry.m_size with
    | Byte_size s -> s
    | _ -> assert false
  in
  let cuchar_zero = Const_folding.cuchar_of_string "0"
  and cushort_zero = Const_folding.cushort_of_string "0"
  and cuint_zero = Const_folding.cuint_of_string "0"
  and culong_zero = Const_folding.culong_of_string "0"
  and cullong_zero = Const_folding.cullong_of_string "0"
  and cuchar_bits = Const_folding.cuchar_of_string (string_of_csize bits)
  and cushort_bits = Const_folding.cushort_of_string (string_of_csize bits)
  and cuint_bits = Const_folding.cuint_of_string (string_of_csize bits)
  and culong_bits = Const_folding.culong_of_string (string_of_csize bits)
  and cullong_bits = Const_folding.cullong_of_string (string_of_csize bits)
  in
  let (mask_typ, bitmask, emask, bnot_emask) = 
    if base_size = cuchar_size then (* cuchar *)
      let bitmask = 
	if bits < cuchar_size *$ 8L then
	  Const_folding.bnot_cuchar
	    (Const_folding.shl_cuchar 
	      (Const_folding.bnot_cuchar cuchar_zero)
	      cuchar_bits)
	else
	  (Const_folding.bnot_cuchar cuchar_zero)
      and right = CA.cuchar_of_string (string_of_csize (lsb -$ 1L))
      in
      let emask = CA.shl_cuchar bitmask right
      in
      let bnot_emask = CA.bnot_cuchar emask
      in
      (
	Mach.cuchar_id,
	CA.string_of_cuchar bitmask,
	CA.string_of_cuchar emask,
	CA.string_of_cuchar bnot_emask
      )

    else if base_size = cushort_size then (* cushort *)
      let bitmask = 
	if bits < cushort_size *$ 8L then
	  Const_folding.bnot_cushort
	    (Const_folding.shl_cushort 
	      (Const_folding.bnot_cushort cushort_zero)
	      cushort_bits)
	else
	  (Const_folding.bnot_cushort cushort_zero)
      and right = CA.cushort_of_string (string_of_csize (lsb -$ 1L))
      in
      let emask = CA.shl_cushort bitmask right
      in
      let bnot_emask = CA.bnot_cushort emask
      in
      (
	Mach.cushort_id,
	CA.string_of_cushort bitmask,
	CA.string_of_cushort emask,
	CA.string_of_cushort bnot_emask
      )
	
    else if base_size = cuint_size then (* cuint *)
      let bitmask = 
	if bits < cuint_size *$ 8L then
	  Const_folding.bnot_cuint
	    (Const_folding.shl_cuint 
	      (Const_folding.bnot_cuint cuint_zero)
	      cuint_bits)
	else
	  (Const_folding.bnot_cuint cuint_zero)
      and right = CA.cuint_of_string (string_of_csize (lsb -$ 1L))
      in
      let emask = CA.shl_cuint bitmask right
      in
      let bnot_emask = CA.bnot_cuint emask
      in
      (
	Mach.cuint_id,
	CA.string_of_cuint bitmask,
	CA.string_of_cuint emask,
	CA.string_of_cuint bnot_emask
      )
	
    else if base_size = culong_size then (* culong *)
      let bitmask = 
	if bits < culong_size *$ 8L then
	  Const_folding.bnot_culong
	    (Const_folding.shl_culong 
	      (Const_folding.bnot_culong culong_zero) culong_bits)
	else
	  (Const_folding.bnot_culong culong_zero)
      and right = CA.culong_of_string (string_of_csize (lsb -$ 1L))
      in
      let emask = CA.shl_culong bitmask right
      in
      let bnot_emask = CA.bnot_culong emask
      in
      (
	Mach.culong_id,
	CA.string_of_culong bitmask,
	CA.string_of_culong emask,
	CA.string_of_culong bnot_emask
      )

    else if base_size = cullong_size then (* cullong *)
      let bitmask = 
	if bits < cullong_size *$ 8L then
	  Const_folding.bnot_cullong
	    (Const_folding.shl_cullong 
	      (Const_folding.bnot_cullong cullong_zero) cullong_bits)
	else
	  (Const_folding.bnot_cullong cullong_zero)
      and right = CA.cullong_of_string (string_of_csize (lsb -$ 1L))
      in
      let emask = CA.shl_cullong bitmask right
      in
      let bnot_emask = CA.bnot_cullong emask
      in
      (
	Mach.cullong_id,
	CA.string_of_cullong bitmask,
	CA.string_of_cullong emask,
	CA.string_of_cullong bnot_emask
      )
	
    else
      assert false
  in
  { 
    m_bit_base_teid = base_typ;
    m_bit_size = bits;
    m_bit_lsb = lsb;
    m_bit_right = lsb -$ 1L;
    m_bit_left = 8L *$ base_size -$ bits -$ (lsb -$ 1L);
    m_bit_mask_typ = mask_typ;
    m_bit_mask = bitmask;
    m_bit_emask = emask;
    m_bit_bnot_emask = bnot_emask;
  }
    
let add_bit_te te_tbl ~(base_teid:teid) ~(bits:csize) :teid =
  let bit_te_info = create_bit_te_info ~base_te:(te_tbl, base_teid) 
    ~bits ~lsb:(if Mach.little_endian then 1L else bits)
  in add_bit_te_info te_tbl bit_te_info
      

let add_array_te te_tbl array_te_info :teid = 
  assert (not (is_bit_te (te_tbl, array_te_info.m_elmt_teid)));
  let type_name = mk_top_te_qname
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_array_te_info fm array_te_info))
  in
  try
    begin
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info; 
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Array array_te_info;
	  m_size = Incomplete;
	  m_align = 0L;
	  m_sizeof_computed = false;
	  m_alignof_computed = false;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist teid -> teid
	  
	
let ptr_of (elmt_te:te) : te =
  let (te_tbl, elmt_teid) = elmt_te
  in
  let te_info = te_info_of elmt_te 
  in
  match te_info.m_ptr_teid_opt with
    | Some teid -> (te_tbl, teid)
    | None -> 
	begin
	  let ptr_typ = 
	    (*assert (not (is_bittyp te_tbl elmt_teid));*)
	    let type_name = mk_top_te_qname
	      (camlp4_macro_str_pp_print
		(fun fm -> Tent_printer.pp_print_pointer_te_info
		  fm elmt_teid))
	    in
	    try
	      begin
		Int.Type_idHashtbl.iter
		  (fun k b ->
		    if b.m_name = type_name then
		      raise (Typ_exist k)
		  ) te_tbl.teid_to_te_info;
		let elmt_te_info = te_info_of elmt_te
		in
		let te_info = 
		  { 
		    m_id = alloc_teid te_tbl;
		    m_name = type_name;
		    m_kind = Pointer elmt_teid;
		    m_size = Byte_size (sizeof_pointer te_tbl);
		    m_align = alignof_pointer te_tbl;
		    m_sizeof_computed = true;
		    m_alignof_computed = true;
		    m_runtime = false;
		    m_ptr_teid_opt = None;
		    m_has_alias = elmt_te_info.m_has_alias;
		    m_attribute_list = [];
		  }
		in
		insert_te_info te_tbl te_info;
		te_info.m_id
	      end
	    with
		Typ_exist teid -> teid
	  in
	  te_info.m_ptr_teid_opt <- Some (ptr_typ);
	  (te_tbl, ptr_typ)
	end

	    
let add_abs_fun_type te_tbl abs_fun_te_info : teid =
  let qname = mk_top_te_qname 
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_abs_fun_te_info fm abs_fun_te_info))
  in
  try
    begin
      Int.Type_idHashtbl.iter
	(fun k b  -> 
	  if b.m_name = qname then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = qname;
	  m_kind = Abs_function abs_fun_te_info;
	  m_size = Byte_size (sizeof_pointer te_tbl);
	  m_align = alignof_pointer te_tbl;
	  m_sizeof_computed = true;
	  m_alignof_computed = true;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist k -> k


let add_crt_fun_te (te_tbl:te_tbl) ~(abs_fun_teid:teid) ~(in_param_ids: QN.t list) : teid =
  let crt_fun_te_info = 
    {
      m_crt_fun_abs_teid = abs_fun_teid;
      m_crt_fun_in_params = List.map (fun v -> NORMAL_PARAM v) in_param_ids;
    }
  in
  let type_name = mk_top_te_qname
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_crt_fun_te_info 
	fm crt_fun_te_info))
  in
  try 
    begin
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Crt_function crt_fun_te_info;
	  m_size = Byte_size (sizeof_pointer te_tbl);
	  m_align = alignof_pointer te_tbl;
	  m_sizeof_computed = true;
	  m_alignof_computed = true;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist k -> k


let add_crt_fun_te_info (te_tbl:te_tbl) (crt_fun_te_info:crt_fun_te_info) :teid = 
  let type_name = mk_top_te_qname
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_crt_fun_te_info 
	fm crt_fun_te_info))
  in
  try 
    begin
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Crt_function crt_fun_te_info;
	  m_size = Byte_size (sizeof_pointer te_tbl);
	  m_align = alignof_pointer te_tbl;
	  m_sizeof_computed = true;
	  m_alignof_computed = true;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist k -> k
	

let add_qualified_type te_tbl (qual_te_info:qual_te_info): teid =
  let type_name = mk_top_te_qname
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_qual_te_info
	fm qual_te_info))
  in
  try
    begin
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Qualified qual_te_info;
	  m_size = Incomplete;
	  m_align = 0L;
	  m_sizeof_computed = false;
	  m_alignof_computed = false;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist teid -> teid


let add_attributed_te te_tbl (teid, attributes): teid =
  let type_name = mk_top_te_qname
    (camlp4_macro_str_pp_print
      (fun fm -> Tent_printer.pp_print_attributed_te_info
	fm (teid, attributes)))
  in
  try
    begin
      Int.Type_idHashtbl.iter
	(fun k b ->
	  if b.m_name = type_name then
	    raise (Typ_exist k)
	) te_tbl.teid_to_te_info;
      let te_info = 
	{ 
	  m_id = alloc_teid te_tbl;
	  m_name = type_name;
	  m_kind = Attribute (teid, attributes);
	  m_size = Incomplete;
	  m_align = 0L;
	  m_sizeof_computed = false;
	  m_alignof_computed = false;
	  m_runtime = false;
	  m_ptr_teid_opt = None;
	  m_has_alias = true;
	  m_attribute_list = [];
	}
      in
      insert_te_info te_tbl te_info;
      te_info.m_id
    end
  with
      Typ_exist teid -> teid

let add_user_defined_te (te_tbl:te_tbl) (qname:QN.t) te_kind : te_info =
  try
    let te_info = QualNameHashtbl.find te_tbl.qname_to_te_info qname
    in 
    assert (te_info.m_name = qname);
    let _ = match te_info.m_kind, te_kind with
      | Struct_name, Struct _ 
      | Union_name, Union _ 
      | Enum_name, Enum _ ->
	  te_info.m_kind <- te_kind
      | _ -> 
	  raise (FoundTeInfo te_info)
    in te_info
  with
    | Not_found ->
	begin
	  let te_info = 
	    { 
	      m_id = alloc_teid te_tbl;
	      m_name = qname;
	      m_kind = te_kind;
	      m_size = (match te_kind with 
		  Enum _ -> Byte_size (sizeof_enum te_tbl) 
		| _ -> Incomplete);
	      m_align = (match te_kind with Enum_name | Enum _ ->
		alignof_enum te_tbl | _ -> 0L);
	      m_sizeof_computed = (match te_kind with Enum _ -> true | _ -> false);
	      m_alignof_computed = false;
	      m_runtime = false;
	      m_ptr_teid_opt = None;
	      m_has_alias = true;
	      m_attribute_list = [];
	    }
	  in
	  insert_te_info te_tbl te_info;
	  te_info
	end
	  


let add_typedef_te te_tbl lhs_teid qname: te_info =
  let te_info = 
    { 
      m_id = alloc_teid te_tbl;
      m_name = qname;
      m_kind = Typedef 
	{
	  m_typedef_lhs_teid = lhs_teid;
	};
      m_size = Incomplete;
      m_align = 0L;
      m_sizeof_computed = false;
      m_alignof_computed = false;
      m_runtime = false;
      m_ptr_teid_opt = None;
      m_has_alias = true;
      m_attribute_list = [];
    }
  in
  insert_te_info te_tbl te_info;
  te_info


let mk_field_table te_tbl (lst:(string option * teid * bool) list) 
    :field_table = 
  let serno = ref 0
  in
  { 
    m_field_infos = 
      (List.map 
	(fun (sname_opt, teid, has_def) ->
	  let v = 
	    {
	      field_sname_opt = sname_opt;
	      field_serno = !serno;
	      field_teid = teid;
	      field_te_has_def = has_def;
	      field_offset = -1L;
	      field_is_bitfield = is_bit_te (te_tbl, teid);
	    }
	  in incr serno;
	  v) lst);
  }


let add_struct_name_te te_tbl qname =
  add_user_defined_te te_tbl qname Struct_name
    
let add_struct_te te_tbl qname (lst:(string option * teid * bool) list) =
  add_user_defined_te te_tbl qname (Struct (mk_field_table te_tbl lst))
    
let add_union_name_te te_tbl qname =
  add_user_defined_te te_tbl qname Union_name
    
let add_union_te te_tbl qname (lst:(string option * teid * bool) list) =
  add_user_defined_te te_tbl qname (Union (mk_field_table te_tbl lst))

let add_enum_name_te te_tbl qname =
  add_user_defined_te te_tbl qname Enum_name 
    

let add_enum_te te_tbl qname (lst:(string * int * int) list) 
    :(te_info * enum_item list) =
  let enum_tbl = 
    { 
      m_enum_max_value = 0;
      m_enum_item_infos = 
	List.map 
	  (fun (sname, serno, evalue) -> 
	    { 
	      m_enum_item_name = 
		Qual_name_op.alloc_enum_cnst
		  (qname.QN.qn_scopes @ [QN.QN_SCOPE_UTYPE qname.QN.qn_sname])
		  (sname, evalue);
	      m_enum_item_serno = serno;
	      m_enum_item_value = evalue;
	    }
	  ) lst;
    }
  in
  let te_info = add_user_defined_te te_tbl qname (Enum enum_tbl)
  in
  let lst = 
    List.map 
      (fun v -> 
	let _ = QualNameHashtbl.add te_tbl.enum_item_const_table 
	  v.m_enum_item_name v.m_enum_item_value
	in
	{ 
	  enum_typ = (te_tbl, te_info.m_id); 
	  enum_name = v.m_enum_item_name; 
	  enum_value = v.m_enum_item_value
	}
      ) enum_tbl.m_enum_item_infos
  in (te_info, lst)



let replace_typedef_type (te_tbl:te_tbl) ~(type_name:QN.t)
    ~(alias_teid:teid) :te_info = 
  try
    Int.Type_idHashtbl.iter
      (fun k b ->
	if b.m_name = type_name then
	  raise (Typ_exist k)
      ) te_tbl.teid_to_te_info;
    assert false
  with
      Typ_exist teid ->
	begin
	  let te_info = 
	    { 
	      m_id = teid;
	      m_name = type_name;
	      m_kind = Typedef
		{
		  m_typedef_lhs_teid = alias_teid;
		};
	      m_size = Incomplete;
	      m_align = 0L;
	      m_sizeof_computed = false;
	      m_alignof_computed = false;
	      m_runtime = false;
	      m_ptr_teid_opt = None;
	      m_has_alias = true;
	      m_attribute_list = [];
	    }
	  in	  
	  Int.Type_idHashtbl.replace
	    te_tbl.teid_to_te_info te_info.m_id te_info;
	  QualNameHashtbl.replace
	    te_tbl.qname_to_te_info te_info.m_name te_info;
	  te_info
	end    

let original_typ: te -> te =
  fun (te_tbl, tid) ->
    (te_tbl, rd_norm_type_orig_id te_tbl tid)

let add_norm_type (te_tbl:te_tbl) ~(orig_teid:teid) ~(norm_teid:teid) 
    : teid * QN.t = 
  assert (orig_teid <> norm_teid);
  let orig_te_kind = te_kind_of (te_tbl, orig_teid)
  and norm_te_kind = te_kind_of (te_tbl, norm_teid)
  in
  assert (not (is_norm_kind orig_te_kind));
  assert (not (is_norm_kind norm_te_kind));
  let type_name = camlp4_macro_str_pp_print
    (fun fm -> Tent_printer.pp_print_te fm (te_tbl, orig_teid))
  in
  let norm_type_name = mk_top_te_qname (type_name ^ ".norm")
  in
  try
    let _ = Int.Type_idHashtbl.iter
      (fun k b ->
	if b.m_name = norm_type_name then
	  begin
	    match b.m_kind with
	      | Normal v -> 
		  begin
		    assert (v.m_norm_norm_teid = norm_teid);
		    if (v.m_norm_orig_teid = orig_teid) then
		      raise (Typ_exist k);
		  end
	      | _ -> assert false
	  end
      ) te_tbl.teid_to_te_info
    in
    let te_info = 
      { 
	m_id = alloc_teid te_tbl;
	m_name = norm_type_name;
	m_kind = Normal 
	  {
	    m_norm_orig_teid = orig_teid;
	    m_norm_norm_teid = norm_teid;
	  };
	m_size = Incomplete;
	m_align = 0L;
	m_sizeof_computed = false;
	m_alignof_computed = false;
	m_runtime = false;
	m_ptr_teid_opt = None;
	m_has_alias = false;
	m_attribute_list = [];
      }
    in  
    insert_te_info te_tbl te_info;
    (te_info.m_id, te_info.m_name)
  with
    | Typ_exist k -> (k, norm_type_name)


    
let rec compute_alignof (te:te) : csize =
  let te_info = te_info_of te
  and (te_tbl, teid) = te
  in
  if te_info.m_alignof_computed then
    te_info.m_align
  else
    begin
      let alignof = match te_info.m_kind with
	| Bits _ -> assert false
	| Normal _ -> assert false
	| Struct_name 
	| Union_name
	| Enum_name -> assert false
	| Builtin -> assert false
	| Typedef v -> 
	    compute_alignof (te_tbl, v.m_typedef_lhs_teid)

	| Pointer _ -> 
	    alignof_pointer te_tbl
	      
	| Abs_function _ -> 
	    alignof_pointer te_tbl
	      
	| Crt_function _ ->
	    alignof_pointer te_tbl
	      
	| Struct field_infos  
	| Union field_infos -> 
	    (List.fold_left 
	      (fun a f -> 
		let alignof_field = 
		  compute_alignof (te_tbl, f.field_teid)
		in max a alignof_field) 1L
	      field_infos.m_field_infos);
	
	| Enum _ ->
	    alignof_enum te_tbl

	| Array v ->
	    compute_alignof (te_tbl, v.m_elmt_teid)

	| Qualified v -> 
	    compute_alignof (te_tbl, v.m_qualified_teid)
	      
	| Attribute (teid, atts) ->
	    let align = compute_alignof (te_tbl, teid)
	    in 
	    List.fold_left 
	      (fun align att ->
		match att with
		  | GCC_transparent_union -> align
		  | GCC_aligned i -> Int64.of_int i
		  | GCC_aligned_max -> assert false
		  | GCC_packed -> 1L
		  | GCC_mode _ -> 2L
		  | GCC_may_alias -> align
		  | GCC_ms_struct 
		  | GCC_gcc_struct -> assert false
		  | GCC_vector_size i -> assert false
      	      ) align atts
      in
      te_info.m_align <- alignof;
      te_info.m_alignof_computed <- true;
      te_info.m_align
    end



let overflow = ref false
let myadd x n = 
  if x > (max_size -$ n) then
    let _ = overflow := true
    in x
  else
    x +$ n

let chkoverflow x n = ignore (myadd x n)

let bits2bytes n = (n +$ 7L) /$ 8L

let roundup x n = 
  (x +$ (n -$ 1L)) &$ (~$(n -$ 1L))



type tmp_info = 
    {
      mutable offset: csize;
      mutable bits: csize;
      mutable incomplete: bool;
      mutable prev_base_te_size: csize;
      mutable max_align:csize;
	mutable max_size: csize;
    }
	
let rec compute_sizeof (te:te) : typ_size =
  let te_info = te_info_of te
  and (te_tbl, teid) = te
  in
  match te_info.m_size with
    | Incomplete ->
	let size = 
	  match te_info.m_kind with
	    | Builtin -> 
		camlp4_macro_exception 
		  "C primitive type size isn't initialized\n"
		  
	    | Typedef v -> 
		compute_sizeof (te_tbl, v.m_typedef_lhs_teid)
		  
	    | Pointer _ -> 
		Byte_size (te_tbl.target_metrics.pointer_size)
		  
	    | Abs_function _ -> 
		camlp4_macro_exception 
		  "compute_sizeof on function type\n"
		  
	    | Crt_function _ -> 
		camlp4_macro_exception 
		  "compute_sizeof on function type\n"
		  
	    | Enum_name ->
		Byte_size (te_tbl.target_metrics.enum_size)
		  
	    | Struct_name
	    | Union_name 
	    | Normal _ -> 
		camlp4_macro_exception "compute_sizeof on norm type\n"
		  
	    | Struct field_infos -> 
		let ti = 
		  {
		    offset = 0L;
		    bits =  0L;
		    incomplete = false;
		    prev_base_te_size = 0L;
		    max_align = 1L;
		    max_size = 0L;
		  }
		in 
		List.iter (fun v ->
		  let field_te = (te_tbl, v.field_teid)
		  in
		  let size = compute_sizeof field_te
		  and is_bit_te = is_bit_te field_te
		  in
		  match size with
		    | Byte_size base_type_size ->
			begin
			  let bitsize = 
			    if is_bit_te then
			      rd_bittyp_size te_tbl ~teid:v.field_teid
			    else
			      8L *$ base_type_size
			  and base_typ = 
			    if is_bit_te then
			      rd_bit_type_base_type 
				te_tbl ~teid:v.field_teid
			    else
			      v.field_teid
			  in
			  let align = compute_alignof field_te
			  in
			  if is_bit_te then
			    begin
			      if ti.prev_base_te_size = 0L then
				ti.prev_base_te_size <- base_type_size
			    end
			  else
			    ti.prev_base_te_size <- 0L;
			  
			  if bitsize = 8L *$ base_type_size or
			    (base_type_size <> ti.prev_base_te_size) or 
			    (ti.bits = 0L) or
			    (ti.bits -$ 1L +$ bitsize > 8L *$ ti.prev_base_te_size) or
			    (bitsize = 0L) (* 6.7.2.1 p 11, As a special
					      case, a bit-field
					      structure member with a
					      width of 0 indicates that
					      no futher bit-field is to
					      be packed into the unit in
					      which the previous
					      bit-field, if any, was
					      placed *)
			  then
			    begin
			      ti.offset <- myadd ti.offset (bits2bytes (ti.bits -$ 1L));
			      ti.bits <- 0L;
			      chkoverflow ti.offset (align -$ 1L);
			      ti.offset <- roundup ti.offset align;
			    end;
			  
			  ti.max_align <- max ti.max_align align;
			  v.field_offset <- ti.offset;
			  if is_bit_te then
			    begin
			      if ti.bits = 0L then ti.bits <- 1L;
			      let lsb = 
				if (Mach.little_endian) then
				  ti.bits
				else
				  8L *$ base_type_size -$ 
				    ti.bits +$ 1L -$ bitsize +$ 1L
			      in
			      let field_teid = 
				let bit_te_info = create_bit_te_info
				  ~base_te:(te_tbl, base_typ) 
				  ~bits:bitsize ~lsb
				in add_bit_te_info te_tbl bit_te_info
			      in
			      v.field_teid <- field_teid;
			      ti.bits <- ti.bits +$ bitsize;
			      ti.prev_base_te_size <- base_type_size;
			    end
			  else
			    begin
			      ti.prev_base_te_size <- 0L;
			      ti.offset <- myadd ti.offset base_type_size
			    end;
			  let v = ti.offset +$ (bits2bytes (ti.bits -$ 1L))
			  in ti.max_size <- max ti.max_size v
			end
			  
		    | Incomplete ->
			begin
			  let align = compute_alignof (te_tbl, v.field_teid)
			  in
			  ti.max_align <- max ti.max_align align;
			  v.field_offset <- ti.offset;
			  let is_last_field i = 
			    if i = (List.length field_infos.m_field_infos) - 1 then
			      true
			    else
			      false
			  in
			  if not (is_last_field v.field_serno) then
			    ti.incomplete <- true 
			  else
			    () (* last field can be incomplete array, then 
				  gcc treat this as zero *)
			end
		) field_infos.m_field_infos;
		if ti.incomplete then 
		  Incomplete
		else
		  begin
		    chkoverflow ti.max_size (ti.max_align -$ 1L);
		    ti.max_size <- roundup ti.max_size ti.max_align;
		    te_info.m_sizeof_computed <- true;
		    te_info.m_alignof_computed <- true;
		    te_info.m_align <- ti.max_align;
		    Byte_size ti.max_size
		  end

	    | Union field_infos -> 
		let ti = 
		  {
		    offset = 0L;
		    bits =  0L;
		    incomplete = false;
		    prev_base_te_size = 0L;
		    max_align = 1L;
		    max_size = 0L;
		  }
		in 
		List.iter
		  (fun v -> 
		    let field_te = (te_tbl, v.field_teid)
		    and _ = v.field_offset <- 0L
		    in
		    let size = compute_sizeof field_te
		    and is_bit_te = is_bit_te field_te
		    in
		    match size with
		      | Byte_size size ->
			  let bitsize = 
			    if is_bit_te then
			      rd_bittyp_size te_tbl ~teid:v.field_teid
			    else
			      8L *$ size
			  and base_typ = 
			    if is_bit_te then
			      rd_bit_type_base_type 
				te_tbl ~teid:v.field_teid
			    else
			      v.field_teid
			  in
			  let align = compute_alignof field_te
			  in
			  ti.max_align <- max ti.max_align align;
			  ti.max_size <- max ti.max_size size;
			  if is_bit_te then
			    begin
			      let lsb = 
				if (Mach.little_endian) then 1L
				else 8L *$ size -$ bitsize +$ 1L
			      in
			      let field_teid = 
				let bit_te_info = 
				  create_bit_te_info ~base_te:(te_tbl, base_typ) 
				    ~bits:bitsize ~lsb
				in add_bit_te_info te_tbl bit_te_info
			      in v.field_teid <- field_teid;
			    end
		      | Incomplete -> ti.incomplete <-true
		  ) field_infos.m_field_infos;
		if ti.incomplete then
		  begin
		    te_info.m_alignof_computed <- true;
		    te_info.m_align <- ti.max_align;
		    Incomplete
		  end
		else
		  begin
		    chkoverflow ti.max_size (ti.max_align -$ 1L);
		    ti.max_size <- roundup ti.max_size ti.max_align;
		    te_info.m_sizeof_computed <- true;
		    te_info.m_alignof_computed <- true;
		    te_info.m_align <- ti.max_align;
		    Byte_size ti.max_size
		  end
		    
	    | Enum _ ->
		Byte_size (sizeof_enum te_tbl) (** todo **)
		  
	    | Array info ->
		begin
		  let elmt_size = compute_sizeof (te_tbl, info.m_elmt_teid)
		  and _ = compute_alignof (te_tbl, info.m_elmt_teid)
		  in
		  match elmt_size with
		    | Byte_size v -> 
			begin
			  match info.m_array_cardi with
			    | ARRAY_FIXED cardi -> Byte_size (cardi *$ v)
			    | ARRAY_VARIABLE -> Incomplete
			end
		    | Incomplete -> assert false
		end
		  
	    | Bits _ -> 
		camlp4_macro_exception 
		  "compute sizeof bit fields\n"

	    | Qualified v -> 
		compute_sizeof (te_tbl, v.m_qualified_teid)

	    | Attribute (teid, atts) ->
		let size = compute_sizeof (te_tbl, teid)
		in
		List.fold_left 
		  (fun s att ->
		    match att with
		      | GCC_transparent_union -> s
		      | GCC_aligned i -> s
		      | GCC_aligned_max -> s
		      | GCC_packed -> s
		      | GCC_mode mode -> 
			  begin
			    let v = match mode with
			      | GCC_bi -> assert false
			      | GCC_qi -> 1L
			      | GCC_hi -> 2L
			      | GCC_psi-> 4L
			      | GCC_si -> 4L
			      | GCC_pdi-> 8L
			      | GCC_di -> 8L
			      | GCC_ti -> 16L
			      | GCC_oi -> 32L
			      | GCC_qf -> 1L
			      | GCC_hf -> 2L
			      | GCC_tqf ->3L
			      | GCC_sf -> 4L
			      | GCC_df -> 8L
			      | GCC_xf -> 16L
			      | GCC_sd -> 4L
			      | GCC_dd -> 8L 
			      | GCC_td -> 16L
			      | GCC_tf -> 16L
			      | GCC_qq -> 1L
			      | GCC_hq -> 2L
			      | GCC_sq -> 4L
			      | GCC_dq -> 8L
			      | GCC_tq -> 16L
			      | GCC_uqq -> 1L
			      | GCC_uhq -> 2L
			      | GCC_usq -> 4L
			      | GCC_udq -> 8L
			      | GCC_utq -> 16L
			      | GCC_ha -> 2L
			      | GCC_sa -> 4L
			      | GCC_da -> 8L
			      | GCC_ta -> 16L
			      | GCC_uha -> 2L
			      | GCC_usa -> 4L
			      | GCC_uda -> 8L
			      | GCC_uta -> 16L
			      | GCC_cc -> assert false
			      | GCC_word -> assert false
			      | GCC_byte -> 1L
			      | GCC_pointer -> assert false
			    in Byte_size v
			  end
		      | GCC_may_alias -> s
		      | GCC_ms_struct 
		      | GCC_gcc_struct -> s
		      | GCC_vector_size i -> assert false
		  ) size atts
	in
	(** update the size field in type_table **)
	te_info.m_size <- size;
	te_info.m_sizeof_computed <- true;
	size
    | Byte_size _ -> te_info.m_size

(** imported from type_dict.ml **)
let get_atomic_equiv_typ: te -> te = 
  fun (te_tbl, teid) ->
    (te_tbl, rd_equiv_teid te_tbl ~teid)


let rec unqualified_typ: te -> te = 
  fun (te_tbl, teid) ->
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in
    let te_kind = te_kind_of (te_tbl, teid)
    in
    match te_kind with
      | Qualified _ -> unqualified_typ (unqual_of (te_tbl, teid))
      | _ -> (te_tbl, teid)
	  

let is_variant_fun: te -> bool = 
  fun (te_tbl, teid)  ->
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in is_variadic te_tbl ~teid

let get_fun_typ: te -> te = 
  fun (te_tbl, teid)  ->
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in (te_tbl, teid)


let rec is_ptr_typ te: bool = 
  match te_kind_of te with
    | Pointer _ -> true
    | Typedef v -> is_ptr_typ (fst te, v.m_typedef_lhs_teid)
    | _ -> false

let rec static_sizeof (te:te):csize = 
  assert (not (is_bit_te te));
  let size = compute_sizeof te
  in
  match size with
    | Byte_size v -> v
    | Incomplete ->
	camlp4_macro_exception 
	  "type '%d' is incomplete, cannot compute size\n" (snd te)

let sizeof (te:te) : typ_size = 
  assert (not (is_bit_te te));
  compute_sizeof te

let alignof (te:te) : csize = 
  assert (not (is_bit_te te));
  compute_alignof te



let rec field_info (te:te) (field:string) : te * csize =
  let te_tbl = fst te
  and _ = sizeof te
  in
  match te_kind_of te with
    | Struct field_table 
    | Union field_table -> 
	let (_, field_teid, offset) = 
	  find_field field_table ~field
	in ((te_tbl, field_teid), offset)
    | Typedef v ->
	field_info (te_tbl, v.m_typedef_lhs_teid) field
    | Qualified v ->
	field_info (te_tbl, v.m_qualified_teid) field
    | _ -> 
	camlp4_macro_exception "field accessing to \"%s\"" field
       
       
let field_typ (te:te) (field:string) : te = 
  let te = fst (field_info te field)
  in
  match te_kind_of te with
    | Attribute (teid, atts) -> (fst te, teid)
    | Qualified v -> (fst te, v.m_qualified_teid)
    | _ -> te

let field_offset (te:te) field :csize = snd (field_info te field)




let bit_type_base: te -> te = 
  fun (te_tbl, teid) -> 
    let id = rd_bit_type_base_type te_tbl teid
    in
    (te_tbl, id)
    
let named_field_infos field_table =
  List.filter 
    (fun f -> match f.field_sname_opt with Some
	_ -> true | None -> false) field_table.m_field_infos
    
      
let typeof_memberof: te -> string -> te =
  fun (te_tbl, teid) field ->
    field_typ (te_tbl, teid) field


let rec get_abs_fun_te (te:te) = 
  let te_kind = te_kind_of te
  in
  match te_kind with
    | Crt_function crt_fun_te_info ->
	(fst te, crt_fun_te_info.m_crt_fun_abs_teid)
    | Qualified v ->
	get_abs_fun_te (fst te, v.m_qualified_teid)
    | Typedef v ->
	get_abs_fun_te (fst te, v.m_typedef_lhs_teid)
    | Abs_function v -> te
    | _ -> assert false


let rec return_te_of (te:te) : te = 
  let (te_tbl, teid) = te
  in
  match te_kind_of te with
    | Abs_function v -> (te_tbl, v.aft_ret_teid)
    | Crt_function _ ->
	return_te_of (get_abs_fun_te (te_tbl, teid))
    | Typedef v ->
	return_te_of (te_tbl, v.m_typedef_lhs_teid)
    | Normal v ->
	return_te_of (te_tbl, v.m_norm_orig_teid)
    | _ -> 
	camlp4_macro_exception "%d is not fuction type is %s type" (teid)
	  

let apply: te -> te list -> te =
  fun (te_tbl, fun_typ) arg_typs ->
    return_te_of (te_tbl, fun_typ)




let typeof_memberof_ptr: te -> string -> te =
  fun (te_tbl, teid) field ->
    let (te_tbl, elmt_typ) = elmt_of (te_tbl, teid)
    in field_typ (te_tbl, elmt_typ) field

	 
let array_of (elmt_te:te) (array_cardi:csize) : te = 
  let array_decl = 
    { 
      m_elmt_teid = snd elmt_te;
      m_array_cardi = ARRAY_FIXED array_cardi;
    }
  in
  let teid = add_array_te (fst elmt_te) array_decl
  in
  ignore(sizeof (fst elmt_te, teid));
  (fst elmt_te, teid)
      

let array_cardi te: csize =
  let te_info = te_info_of te
  in
  match te_info.m_kind with
    | Array array_te_info -> 
	begin
	  match array_te_info.m_array_cardi with
	    | ARRAY_FIXED csize -> csize
	    | _ -> assert false
	end
    | _ -> assert false
	

let stripoff_crt_function_typ te = 
  match te_kind_of te with
    | Crt_function crt_fun_te_info ->
	(fst te, crt_fun_te_info.m_crt_fun_abs_teid)
    | _ -> te





let norm_ptr_bit te = 
  let (te_tbl, teid) = te
  in
  let equiv_teid = match te_kind_of te with
    | Normal v -> v.m_norm_norm_teid
    | Builtin -> teid
    | Array v -> teid
    | Abs_function v -> teid
    | Crt_function v -> teid
    | Struct _
    | Union _
    | Enum _ -> teid
	
    | Union_name 
    | Struct_name
    | Enum_name -> teid
	
    | Bits v -> v.m_bit_base_teid
	  
    | Qualified v -> teid
    | Typedef v -> 
	(rd_equiv_teid te_tbl teid)
	  
    | Pointer elmt_teid ->
	begin
	  let elmt_teid = match te_kind_of (fst te, elmt_teid) with
	    | Bits v -> v.m_bit_base_teid
	    | _ -> elmt_teid
	  in snd (ptr_of (te_tbl, elmt_teid))
	end
	  
    | Attribute _ -> teid
  in (te_tbl, equiv_teid)


let rec stripoff_const_qualifier te = 
  let (te_tbl, teid) = te
  in
  let has_done_hash = Int.Type_idHashtbl.create 17
  in
  let rec stripoff teid :teid =
    let equiv_teid = match te_kind_of (te_tbl,teid) with
      | Normal v -> stripoff v.m_norm_norm_teid
      | Builtin -> teid
      | Array v -> 
	  let new_v = 
	    { 
	      m_elmt_teid = stripoff v.m_elmt_teid;
	      m_array_cardi = v.m_array_cardi
	    }
	  in add_array_te te_tbl new_v
	       
      | Abs_function v ->
	  let new_v = 
	    { 
	      aft_ret_teid = stripoff v.aft_ret_teid;
	      aft_param_teids = List.map stripoff v.aft_param_teids;
	      aft_va_arg = v.aft_va_arg;
	      aft_hidden_ret_param = Mapping.map_opt stripoff
		v.aft_hidden_ret_param;
	      aft_muton_param_pos = v.aft_muton_param_pos;
	    }
	  in add_abs_fun_type te_tbl new_v
	    
      | Crt_function v ->
	  let new_v = 
	    { 
	      m_crt_fun_abs_teid = stripoff v.m_crt_fun_abs_teid;
	      m_crt_fun_in_params = v.m_crt_fun_in_params;
	    }
	  in add_crt_fun_te_info te_tbl new_v
	    
      | Struct field_table
      | Union field_table ->
	  let _ = 
	    try
	      ignore(Int.Type_idHashtbl.find has_done_hash teid)
	    with
		Not_found ->
		  begin
		    let _ = Int.Type_idHashtbl.add has_done_hash teid true
		    in
		    (*if e.m_complete then*)
		    let field_list = field_table.m_field_infos
		    in
		    let new_field_list = List.map 
		      (fun f -> 
			{ 
			  field_sname_opt = f.field_sname_opt;
			  field_serno = f.field_serno;
			  field_teid = stripoff f.field_teid;
			  field_te_has_def = f.field_te_has_def;
			  field_offset = f.field_offset;
			  field_is_bitfield = f.field_is_bitfield;
			}
		      ) field_list
		    in
		    field_table.m_field_infos <- new_field_list
		  end
	  in teid
	       
      | Enum _ -> teid
	  
      | Union_name 
      | Struct_name
      | Enum_name -> teid
	  
      | Bits v ->
	  let new_v = 
	    { 
	      m_bit_base_teid = stripoff v.m_bit_base_teid;
	      m_bit_size = v.m_bit_size;
	      m_bit_lsb = v.m_bit_lsb;
	      m_bit_mask_typ = v.m_bit_mask_typ;
	      m_bit_mask = v.m_bit_mask;
	      m_bit_emask = v.m_bit_emask;
	      m_bit_bnot_emask = v.m_bit_bnot_emask;
	      m_bit_right = v.m_bit_right;
	      m_bit_left = v.m_bit_left;
	    }
	  in add_bit_te_info te_tbl new_v
	    
      | Qualified v ->
	  if (v.m_qualifier = QUAL_CONST) then
	    v.m_qualified_teid
	  else teid
	    
      | Typedef v -> 
	  stripoff (rd_equiv_teid te_tbl teid)
	    
      | Pointer elmt_teid ->
	  snd (ptr_of (te_tbl, (stripoff elmt_teid)))

      | Attribute (teid, atts) ->
	  let teid = stripoff teid
	  in add_attributed_te te_tbl (teid, atts)
	    
    in equiv_teid
  in (te_tbl, stripoff teid)


let get_arithmatic_te te = 
  let (te_tbl, teid) = te
  in
  let rec f teid :teid =
    let equiv_teid = match te_kind_of (te_tbl,teid) with
      | Normal _ -> assert false
      | Builtin -> teid
      | Bits _ -> teid
      | Attribute (teid, atts) ->
	  begin
	    let topt = 
	      List.fold_left
		(fun topt e ->
		  match e with
		    | GCC_aligned n -> topt
		    | GCC_packed -> topt
		    | GCC_mode mode -> 
			begin
			  match mode with
			    | _ -> assert false
			end
		    | _ -> None
		) (Some teid) atts
	    in
	    match topt with
	      | Some id -> id
	      | None ->
		  assert false
	  end
      | Typedef v ->
	  f v.m_typedef_lhs_teid
      | Pointer _ -> teid
      | Qualified v ->
	  f v.m_qualified_teid
      | Enum _ -> teid
      | _ -> assert false
    in equiv_teid
  in (te_tbl, f teid)

	

let rec get_function_typ: te -> bool * te = 
  fun (te_tbl, teid) ->
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    match te_kind_of (te_tbl, teid) with
      | Crt_function _ -> (false, (te_tbl, teid))
      | Abs_function _ -> (false, (te_tbl, teid))
      | Pointer _ ->
	  begin
	    let (te_tbl, elmt_typ) = elmt_of (te_tbl, teid)
	    in
	    let (_, typ') = get_function_typ (te_tbl, elmt_typ)
	    in (true, typ')
	  end
      | _ -> assert false
	  


let rec ptrize te : te =
  if is_ptr_typ te then
    ptr_of (ptrize (elmt_of te))
  else if is_array_typ te then
    ptr_of (ptrize (elmt_of te))
  else te
	

(** normalize typ for parameter like 'char * argv []' **)
(** 1. if typ has size zero, then we allocate type void * 
       see gcc.dg/compat/struct-by-value-1
**)
(*
let norm_param_typ (typ:te) :te =
  let (te_tbl, orig_teid) = typ
  in
  let (te_tbl, norm_teid) = 
    if is_array_typ typ then
      let typ = elmt_of typ
      in ptr_of typ
    else if is_crt_function_typ typ or is_abs_function_typ typ then
  ptr_of (te_tbl, Mach.cvoid_id)
    else
      let size = static_sizeof typ
      in
      if size = 0L then
	ptr_of (te_tbl, Mach.cptr_id)
      else
	typ
  in
  if (orig_teid <> norm_teid) then
    let (nid, _) = add_norm_type te_tbl ~orig_teid ~norm_teid
    in (te_tbl, nid)
  else
    (te_tbl, orig_teid)
*)


let norm_param_typ (typ:te) :te =
  let (te_tbl, orig_teid) = typ
  in
  let (te_tbl, norm_teid) = 
    if is_xarray_typ typ then
      let typ = elmt_of typ
      in ptr_of typ
    else if is_crt_function_typ typ or is_abs_function_typ typ then
      ptr_of typ
    else
      let size = static_sizeof typ
      in
      if size = 0L then
	(te_tbl, Mach.cptr_id)
      else
	typ
  in (te_tbl, norm_teid)
       (*
  if (orig_teid <> norm_teid) then
    let (nid, _) = add_norm_type te_tbl ~orig_teid ~norm_teid
    in (te_tbl, nid)
  else
    (te_tbl, orig_teid)
       *)


let norm_typ_size (typ:te) :te =
  let (te_tbl, orig_teid) = typ
  in
  let (te_tbl, norm_teid) = 
    if is_xarray_typ typ then
      let typ = elmt_of typ
      in ptr_of typ
    else if is_crt_function_typ typ or is_abs_function_typ typ then
      ptr_of typ
    else if is_bit_te typ then
      (*assert false*) bit_type_base typ
    else
      let size = static_sizeof typ
      in
      if size = 0L then
	(te_tbl, Mach.cptr_id)
      else
	typ
  in (te_tbl, norm_teid)


let enum_constant_identifiers: te -> QN.t list =
  fun (te_tbl, teid) ->
    rd_enum_items te_tbl teid

let is_floating_typ: te_tbl * teid -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid') = unqualified_typ (te_tbl, teid)
    in
    List.exists
      (fun (teid, size) -> teid = teid'
      ) Mach.floating_typ_list

let get_floating_typ: expect_size:csize -> teid * csize =
  fun ~expect_size ->
    let (teid, size) = List.find
	(fun (c_type_name, size) -> size >= expect_size 
	) Mach.floating_typ_list
    in
    (teid, size)


let lub: te list -> te_tbl * teid = 
  fun lts ->
    let te_tbl = fst (List.hd lts)
    in    
    let lts' = 
      List.map 
	(fun (te_tbl, v) -> 
	  match te_kind_of (te_tbl, v) with
	    | Array _ ->
		ptr_of (elmt_of (te_tbl,v))
	    | Bits _ -> 
		(te_tbl, rd_bit_type_base_type te_tbl v)
	    | _ -> (te_tbl, v)
	) lts
    in
    let (max_ty, max_size, has_floating_typ) =
      List.fold_left 
	(fun (max_ty, max_size, has_floating_typ) v ->
	  let size = sizeof v
	  in
	  match size with
	    | Byte_size size ->
		let (max_ty, max_size) = 
		  if size > max_size then (v, size)
		  else (max_ty, max_size)
		in
		(max_ty, max_size, (is_floating_typ v) or has_floating_typ)
	    | _ -> assert false
	) ((te_tbl, Mach.cnon_id), 0L, false) lts'
    in
    if has_floating_typ then
      let (ty, size) = get_floating_typ ~expect_size:max_size
      in (te_tbl, ty)
    else
      max_ty



let get_in_param_types: te -> te list = 
  fun (te_tbl, teid) ->
    let params = List.map (fun v -> (te_tbl, v)) 
      (rd_in_param_types te_tbl ~teid)
    in
    match params with
      | [a] -> 
	  if is_void_typ a then
	    []
	  else
	    params
      | _ -> 
	  params
	  

(** new import **)
let maybe_typedef_predeclaration: te -> bool =
  fun (te_tbl, teid) ->
    match te_kind_of (te_tbl, teid) with
      | Struct _ -> true
      | Union _ -> true
      | Enum _ -> true
      | _ -> false

let is_non_typ: te -> bool =
  fun (te_tbl, teid) ->
    teid = Mach.cnon_id

let rec is_const_typ te: bool = 
  match te_kind_of te with
    | Qualified v ->
	v.m_qualifier = QUAL_CONST
    | Typedef v ->
	is_const_typ (fst te, v.m_typedef_lhs_teid)
    | _ -> false
	
let rec is_char_typ te: bool =
  match te_kind_of te with
    | Builtin  -> 
	(snd te = Mach.cchar_id 
	    or snd te = (Mach.cuchar_id) 
	    or snd te = (Mach.cschar_id))
    | Typedef v -> 
	is_char_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v ->
	is_char_typ (fst te, v.m_qualified_teid)
    | _ -> false
	
let rec is_short_typ te: bool =
  match te_kind_of te with
    | Builtin  -> (snd te = Mach.cshort_id or snd te = Mach.cushort_id)
    | Typedef v -> 
	is_short_typ (fst te, v.m_typedef_lhs_teid)
    | Qualified v ->
	is_short_typ (fst te, v.m_qualified_teid)
    | _ -> false
	

let rec is_wchar_typ te:bool = 
  match te_kind_of te with
    | Builtin -> 
	(snd te = Mach.cwchar_t_id or  
	    (String.compare (QNP.to_decl_str (qname_of te)) "wchar_t" = 0))
    | Typedef v -> 
	is_wchar_typ (fst te, v.m_typedef_lhs_teid)
    | Normal v ->
	is_wchar_typ (fst te, v.m_norm_norm_teid)
    | Qualified v ->
	is_wchar_typ (fst te, v.m_qualified_teid)
    | _ -> false


let is_unsigned_integer_typ: te -> bool =
  fun (te_tbl, teid) ->
    let typ = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    let typ = unqualified_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    ((Mach.char_is_unsigned & (teid = Mach.cchar_id)) 
    or teid = Mach.cuchar_id
	or teid = Mach.cushort_id
	or teid = Mach.cuint_id
	or teid = Mach.culong_id
	or teid = Mach.cullong_id)
      
let is_signed_integer_typ: te -> bool =
  fun (te_tbl, teid) -> 
    let typ = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    let typ = unqualified_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    is_enum_typ (te_tbl, teid) 
    or (not Mach.char_is_unsigned & teid = Mach.cchar_id)
    or teid = Mach.cschar_id
	or teid = Mach.cshort_id
	or teid = Mach.cint_id
	or teid = Mach.clong_id
	or teid = Mach.cllong_id
	
let to_unsigned_typ: te -> te =
  fun (te_tbl, teid) -> 
    let typ = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    let typ = unqualified_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    assert (is_signed_integer_typ typ);
    if (is_enum_typ typ) then
      (te_tbl, Mach.cuint_id)
    else if (Mach.char_is_unsigned & teid = Mach.cchar_id) then
      (te_tbl, Mach.cuchar_id)
    else 
      let id = 
	if teid = Mach.cschar_id or 
	  teid = Mach.cshort_id then
	    Mach.cuchar_id
	else if teid = Mach.cshort_id or
	  teid = Mach.cushort_id then
	    Mach.cushort_id
	else if teid = Mach.cint_id then
	  Mach.cuint_id
	else if teid = Mach.clong_id then
	  Mach.culong_id
	else if teid = Mach.cllong_id then
	  Mach.cullong_id
	else
	  assert false
      in
      (te_tbl, id)




(* 
   Don't do any data converion. 
   char -> uchar
   short -> ushort
   int -> uint
   long -> ulong
   ...
*)
let flip_to_unsigned_te: te -> te =
  fun (te_tbl, teid) -> 
    let typ = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    let typ = unqualified_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    if (is_unsigned_integer_typ typ) then
      typ
    else if (is_enum_typ typ) then
      (te_tbl, Mach.cuint_id)
    else 
      let id = 
	if teid = Mach.cchar_id or teid = Mach.cschar_id then
	  Mach.cuchar_id
	else if teid = Mach.cshort_id then
	  Mach.cuchar_id
	else if teid = Mach.cshort_id or teid = Mach.cushort_id then
	    Mach.cushort_id
	else if teid = Mach.cint_id then
	  Mach.cuint_id
	else if teid = Mach.clong_id then
	  Mach.culong_id
	else if teid = Mach.cllong_id then
	  Mach.cullong_id
	else if teid = Mach.cbool_id then
	  Mach.cbool_id
	else
	  assert false
      in (te_tbl, id)
	    
let is_int_typ: te -> bool =
  fun (te_tbl, teid) -> 
    let typ = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    let typ = unqualified_typ (te_tbl, teid)
    in
    let (_, teid) = typ
    in
    is_enum_typ (te_tbl, teid) 
    or teid = Mach.cint_id



let is_bool_typ: te -> bool =
  fun (te_tbl, teid) ->
    teid = Mach.cbool_id

let bool_int_typ: te -> te =
  fun (te_tbl, teid) ->
    (te_tbl, Mach.cchar_id)

let is_integer_typ: te -> bool =
  fun typ ->
    (is_unsigned_integer_typ typ) or (is_signed_integer_typ typ)
    or (is_bool_typ typ)

let rank_gte: te -> te -> bool =
  fun typ1 typ2 ->
    assert ((is_integer_typ typ1) & (is_integer_typ typ2));
    let (_, t1) = typ1
    and (_, t2) = typ2
    in
    if t1 = t2 then
      true
    else if (is_bool_typ typ2) then
      true
    else if (is_bool_typ typ1) then
      false
    else if static_sizeof typ1 > static_sizeof typ2 then
      true
    else if static_sizeof typ1 = static_sizeof typ2 then
      begin
	if  (is_unsigned_integer_typ typ1) then
	  true
	else if (is_unsigned_integer_typ typ2) then
	  false
	else
	  true
      end
    else 
      false
	  
	
let sizeof_c_string_literal: te_tbl -> C_syntax_symbol.c_string_literal -> csize =
  fun te_tbl str ->
    match str with
      | C_syntax_symbol.String_literal l -> 
	  csize_of_int (((String.length l) + 1))
      | C_syntax_symbol.WString_literal l ->
	  csize_of_int ((List.length l) + 1)
	  *$ (static_sizeof (te_tbl, Mach.cwchar_t_id))
	    
let is_ge_ptr_typ: te -> bool = 
  (* return true if it's size is less than ptr_int_typ *)
  fun (te_tbl, teid) ->
    let ptr_size = static_sizeof (te_tbl, Mach.cptr_uint_id)
    and this_size = static_sizeof (te_tbl, teid)
    in
    this_size >= ptr_size


let is_float_typ: te -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    teid = Mach.cfloat_id

let is_double_typ: te -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    teid = Mach.cdouble_id

let is_long_double_typ: te -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    teid = Mach.cldouble_id

let is_llong_te: te -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in teid = Mach.cllong_id 

let is_ullong_te: te -> bool =
  fun (te_tbl, teid) -> 
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in teid = Mach.cullong_id 
	

let is_real_typ: te -> bool = 
  fun t ->
    (is_float_typ t) or (is_double_typ t) or (is_long_double_typ t)
      

let is_va_list_typ: te -> bool =
  fun (te_tbl, teid) ->
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in teid = Mach.cva_list_id

let get_scalar_typ: te -> te = 
  fun (te_tbl, teid) ->
    let c_type = unqualified_typ (te_tbl, teid)
    in
    let c_type = if is_array_typ c_type then
      let elmt_typ = elmt_of c_type
      in
      ptr_of elmt_typ
    else if is_ptr_typ c_type then
      let elmt_typ = elmt_of c_type
      in
      if is_bit_te elmt_typ then
	ptr_of (bit_type_base elmt_typ)
      else
	c_type
    else
      c_type
    in
    let (te_tbl, teid) = c_type
    in
    if is_va_list_typ (te_tbl, teid) then
      ptr_of (te_tbl, teid) 
    else if is_bit_te (te_tbl, teid) then
      (te_tbl, Mach.cint_id)
    else if is_array_typ (te_tbl, teid) then
      begin
	let elmt_typ = elmt_of (te_tbl, teid)
	in
	ptr_of elmt_typ
      end
    else if is_abs_function_typ (te_tbl, teid) then
      begin
	ptr_of (te_tbl, teid)
      end
    else if is_crt_function_typ (te_tbl, teid) then
      begin
	let (te_tbl, teid) = get_abs_fun_te (te_tbl, teid)
	in
	ptr_of (te_tbl, teid)
      end
    else if is_non_typ (te_tbl, teid) then
      (te_tbl, Mach.cint_id)
    else
      (te_tbl, teid)


let rec gnu_builtin_types_compatible: te_tbl * teid -> te_tbl * teid -> int =
  fun t0 t1 ->
    let t0 = get_atomic_equiv_typ t0
    and t1 = get_atomic_equiv_typ t1
    in
    if (snd t0) == (snd t1) then 1
    else if (is_ptr_typ t0) && (is_ptr_typ t1) then
      begin
	let t0' = elmt_of t0
	and t1' = elmt_of t1
	in gnu_builtin_types_compatible t0' t1'
      end
    else if (is_array_typ t0) && (is_array_typ t1) then
      begin
	let s0' = array_cardi t0
	and s1' = array_cardi t1
	in
	if (s0' == s1') then
	  let t0' = elmt_of t0
	  and t1' = elmt_of t1
	  in gnu_builtin_types_compatible t0' t1'
	else 0
      end
    else if (is_xarray_typ t0) && (is_xarray_typ t1) then
      begin
	let t0' = elmt_of t0
	and t1' = elmt_of t1
	in gnu_builtin_types_compatible t0' t1'
      end
    else if (is_xarray_typ t0) && (is_array_typ t1) then
      begin
	let t0' = elmt_of t0
	and t1' = elmt_of t1
	in gnu_builtin_types_compatible t0' t1'
      end
    else if (is_array_typ t0) && (is_xarray_typ t1) then
      begin
	let t0' = elmt_of t0
	and t1' = elmt_of t1
	in gnu_builtin_types_compatible t0' t1'
      end
    else 
      0
      


let promote_integer_typ: te -> te = 
  fun (te_tbl, teid) ->
    let teid = 
      if is_bit_te (te_tbl, teid) then
	rd_bit_type_base_type te_tbl ~teid:teid
      else
	teid
    in
    let (te_tbl, teid) = get_atomic_equiv_typ (te_tbl, teid)
    in
    let (te_tbl, teid) = unqualified_typ (te_tbl, teid)
    in
    let teid = 
      if teid = Mach.cchar_id
	or teid = Mach.cuchar_id
	or teid = Mach.cschar_id
	or teid = Mach.cshort_id
	or teid = Mach.cushort_id
	or teid = Mach.cint_id
	or teid = Mach.cnon_id
      then
	Mach.cint_id
      else if teid = Mach.cuint_id
	or teid = Mach.clong_id
	or teid = Mach.culong_id
	or teid = Mach.cllong_id
	or teid = Mach.cullong_id
      then
	teid
      else
	camlp4_macro_exception 
	  "integer promotion can not promote non integer type %d\n" teid
    in
    (te_tbl, teid)


let mk_crt_function_typ te =
  let te_tbl = fst te
  in
  match te_kind_of te with
    | Abs_function v ->
	let new_v = 
	  { 
	      m_crt_fun_abs_teid = snd te;
	    m_crt_fun_in_params = [];
	  }
	in (te_tbl, add_crt_fun_te_info te_tbl new_v)
    | Crt_function v -> te
    | _ -> assert false

let normalize_bit_typ_ptr c_type =
  let (te_tbl, orig_teid) = c_type
  in
  let (_, norm_teid) = 
    if is_ptr_typ c_type then
      let elmt_typ = elmt_of c_type
      in
      if is_bit_te elmt_typ then
	ptr_of (bit_type_base elmt_typ)
      else
	c_type
    else
      c_type
  in
  if (norm_teid <> orig_teid) then
    let (nid, _) = add_norm_type te_tbl ~orig_teid ~norm_teid
    in (te_tbl, nid)
  else
    (te_tbl, orig_teid)

let get_abs_function_muton_pos te: int IntHashtbl.t = 
  let te_kind = te_kind_of te
  in
  match te_kind with
    | Abs_function v -> 
	let posHash = IntHashtbl.create 13
	in
	let _ = 
	  List.iter
	    (fun v -> IntHashtbl.add posHash v v) 
	    v.aft_muton_param_pos
	in posHash
    | _ -> assert false


let norm_abs_function_xarray_params te: te * (int * teid) list =
  let (te_tbl, teid) = te
  in
  let convert_array_to_ptr teid = 
    if is_array_typ (te_tbl, teid) or is_xarray_typ (te_tbl, teid) then
      (snd (ptr_of (elmt_of (te_tbl, teid))), true)
    else 
      (teid, false)
  in
  let (teid, muton_list) = match te_kind_of te with
    | Abs_function v ->
	let (pos, aft_param_teids, muton_pos_lst) = 
	  List.fold_left
	    (fun (pos, idLst, posLst) teid ->
	      let (idLst, posLst) = 
		if teid = Mach.cvoid_id then
		  (idLst, posLst)
		else
		  let (teid, mutate_param) = convert_array_to_ptr teid
		  in
		  if mutate_param then
		    (idLst@[teid], posLst @ [(pos, teid)])
		  else
		    (idLst@[teid], posLst)
	      in
	      (pos + 1, idLst, posLst)
	    ) (0, [], []) v.aft_param_teids
	in
	if muton_pos_lst <> [] then
	  let new_v = 
	    { 
	      aft_ret_teid = v.aft_ret_teid;
	      aft_param_teids = aft_param_teids;
	      aft_va_arg = v.aft_va_arg;
	      aft_hidden_ret_param = None;
	      aft_muton_param_pos = [];
	    }
	  in (add_abs_fun_type te_tbl new_v, muton_pos_lst)
	else
	  (teid, [])
    | _ -> assert false
  in ((te_tbl, teid), muton_list)

let norm_crt_function_xarray_params te: te =
  let (te_tbl, teid) = te
  in
  match te_kind_of te with
    | Crt_function v ->
	begin
	  let (abs_te, muton_pos_lst) = 
	    (norm_abs_function_xarray_params (fst te, v.m_crt_fun_abs_teid))
	  in
	  if (snd abs_te) = v.m_crt_fun_abs_teid then
	    te
	  else
	    let posHash = IntHashtbl.create 13
	    in
	    let _ = List.iter
	      (fun (i, teid) -> IntHashtbl.add posHash i teid) muton_pos_lst
	    in
	    let i = ref 0
	    in
	    let crt_fun_in_params = 
	      List.map 
		(fun v ->
		  let _ = incr i
		  in
		  match v with
		    | NORMAL_PARAM str ->
			begin
			  try
			    let ptr_teid = IntHashtbl.find posHash (!i-1)
			    in
			    let muton = 
			      QNO.copy_alloc_qname
				str (str.QN.qn_sname) ptr_teid
			    in NORMAL_PARAM muton
			  with
			    | Not_found -> v
			end
		    | _ -> assert false
		) v.m_crt_fun_in_params
	    in
	    let new_v = 
	      { 
		m_crt_fun_abs_teid = snd abs_te;
		m_crt_fun_in_params = crt_fun_in_params;
	      }
	    in (te_tbl, add_crt_fun_te_info te_tbl new_v)
	end
    | Abs_function v -> 
	assert false
    | _ -> te


let norm_abs_function_typ te: (te * teid option * (int * int) list) = 
  let (te_tbl, teid) = te
  in
  let convert_struct_to_struct_ptr teid = 
    if is_struct_typ (te_tbl, teid) or is_union_typ (te_tbl, teid) then
      (snd (ptr_of (te_tbl, teid)), true)
    else 
      (teid, false)
  in
  let (teid, ret_opt, muton_pos_lst) = match te_kind_of te with
    | Abs_function v ->
	let (ptr_teid, mutate_ret) = 
	  convert_struct_to_struct_ptr v.aft_ret_teid
	in	
	let (pos, aft_param_teids, muton_pos_lst) = 
	  let (init_pos, init_param_teids) =
	    if mutate_ret then (0, [ptr_teid])
	    else (0, [])
	  in
	  List.fold_left
	    (fun (pos, idLst, posLst) teid ->
	      let (idLst, posLst) = 
		if teid = Mach.cvoid_id then
		  (idLst, posLst)
		else
		  let (teid, mutate_param) = convert_struct_to_struct_ptr teid
		  in
		  if mutate_param then
		    (idLst@[teid], posLst @ [(pos, teid)])
		  else
		    (idLst@[teid], posLst)
	      in
	      (pos + 1, idLst, posLst)
	    ) (init_pos, init_param_teids, []) v.aft_param_teids
	in
	if mutate_ret then
	  let new_v = 
	    { 
	      aft_ret_teid = Mach.cvoid_id;
	      aft_param_teids = aft_param_teids;
	      aft_va_arg = v.aft_va_arg;
	      aft_hidden_ret_param = Some v.aft_ret_teid;
	      aft_muton_param_pos = fst (List.split muton_pos_lst);
	    }
	  in (add_abs_fun_type te_tbl new_v, Some ptr_teid, muton_pos_lst)
	else if muton_pos_lst <> [] then
	  let new_v = 
	    { 
	      aft_ret_teid = v.aft_ret_teid;
	      aft_param_teids = aft_param_teids;
	      aft_va_arg = v.aft_va_arg;
	      aft_hidden_ret_param = None;
	      aft_muton_param_pos = fst (List.split muton_pos_lst);
	    }
	  in (add_abs_fun_type te_tbl new_v, None, muton_pos_lst)
	else
	  (teid, None, [])
    | _ -> assert false
  in ((te_tbl, teid), ret_opt, muton_pos_lst) 
  
let norm_crt_function_typ te: te * te option =
  let (te_tbl, teid) = te
  in
  match te_kind_of te with
    | Crt_function v ->
	begin
	  let (abs_te, ret_opt, muton_pos_lst) = 
	    (norm_abs_function_typ (fst te, v.m_crt_fun_abs_teid))
	  in
	  match ret_opt with
	    | None ->
		begin
		  if (snd abs_te) = v.m_crt_fun_abs_teid then
		    (te, None)
		  else
		    let posHash = IntHashtbl.create 13
		    in
		    let _ = List.iter
		      (fun (i, teid) -> IntHashtbl.add posHash i teid) muton_pos_lst
		    in
		    let i = ref 0
		    in
		    let crt_fun_in_params = 
		      List.map 
			(fun v ->
			  let _ = incr i
			  in
			  match v with
			    | NORMAL_PARAM str ->
				begin
				  try
				    let ptr_teid = IntHashtbl.find posHash (!i-1)
				    in
				    let muton = 
				      QNO.copy_alloc_qname
					str (str.QN.qn_sname ^ "_ptr") ptr_teid
				    in
				    let _ = QNO.stacknize_param_qname str
				    in STRUCT_PARAM 
					 { 
					   orig = str;
					   muton = muton;
					 }
				  with
				    | Not_found -> v
				end
			    | _ -> assert false
			) v.m_crt_fun_in_params
		    in
		    let new_v = 
		      { 
			m_crt_fun_abs_teid = snd abs_te;
			m_crt_fun_in_params = crt_fun_in_params;
		      }
		    in ((te_tbl, add_crt_fun_te_info te_tbl new_v), None)
		end
		     
	    | Some ret_teid -> 
		begin
		  let ret_teid_ptr = ret_teid
		  in
		  let hidden_ptr = Qual_name_op.alloc_param_name 
		    [] "ret_ptr" ret_teid_ptr
		  in
		  let posHash = IntHashtbl.create 13
		  in
		  let _ = List.iter
		    (fun (i, teid) -> IntHashtbl.add posHash i teid) 
		    muton_pos_lst
		  in
		  let i = ref 0
		  in
		  let crt_fun_in_params = 
		    List.map 
		      (fun v ->
			let _ = incr i
			in
			match v with
			  | NORMAL_PARAM str ->
			      begin
				try
				  let ptr_teid = IntHashtbl.find posHash (!i-1)
				  in
				  let muton = 
				    QNO.copy_alloc_qname
				      str (str.QN.qn_sname ^ "_ptr") ptr_teid
				  in
				  let _ = QNO.stacknize_param_qname str
				  in STRUCT_PARAM
				       {
					 orig = str; 
					 muton = muton;
				       }
				with
				  | Not_found -> v
			      end
			  | _ -> assert false
		    ) v.m_crt_fun_in_params
		  in
		  let new_v = 
		    { 
		      m_crt_fun_abs_teid = snd abs_te;
		      m_crt_fun_in_params = 
			(HIDDEN_RETURN hidden_ptr)::crt_fun_in_params;
		    }
		  in ((te_tbl, add_crt_fun_te_info te_tbl new_v), 
		  (Some (te_tbl, ret_teid_ptr)))
		end
	end
    | Abs_function v -> 
	assert false
    | _ -> (te, None)



let rec rec_norm_function_te te: te = 
  let has_done_hash = Int.Type_idHashtbl.create 17
  in
  let te_tbl = fst te
  in
  let rec inner_rec_norm te: te =
    match te_kind_of te with
      | Builtin -> te
      | Array v -> 
	  begin
	    let new_v = 
	      { 
		m_elmt_teid = snd (inner_rec_norm (fst te, v.m_elmt_teid));
		m_array_cardi = v.m_array_cardi
	      }
	    in (fst te, add_array_te (fst te) new_v)
	  end
      | Crt_function _ ->
	  let te = norm_crt_function_xarray_params te
	  in fst (norm_crt_function_typ te)
	    
      | Abs_function _ ->
	  let (te, _, _) = norm_abs_function_typ te
	  in te

      | Pointer elmt_teid ->
	  let elmt_te = (inner_rec_norm (fst te, elmt_teid))
	  in ptr_of elmt_te

      | Struct field_table
      | Union field_table ->
	  let _ = 
	    try
	      ignore(Int.Type_idHashtbl.find has_done_hash (snd te))
	    with
		Not_found ->
		  begin
		    let _ = Int.Type_idHashtbl.add has_done_hash (snd te) true
		    in
		    let field_list = field_table.m_field_infos
		    in
		    let new_field_list = List.map 
		      (fun f -> 
			{ 
			  field_sname_opt = f.field_sname_opt;
			  field_serno = f.field_serno;
			  field_teid = snd (inner_rec_norm (fst te, f.field_teid));
			  field_te_has_def = f.field_te_has_def;
			  field_offset = f.field_offset;
			  field_is_bitfield = f.field_is_bitfield;
			}
		      ) field_list
		    in
		    field_table.m_field_infos <- new_field_list
		  end
	  in te
	    
      | Enum _ -> te
	  
      | Enum_name 
      | Union_name 
      | Struct_name -> te
	  
      | Bits v -> te
	  
      | Qualified v ->
	  begin
	    let new_v = 
	      { 
		m_qualified_teid = snd (inner_rec_norm (fst te, v.m_qualified_teid));
		m_qualifier = v.m_qualifier;
	      }
	    in (fst te, add_qualified_type te_tbl new_v)
	  end 
	    
      | Typedef v -> 
	  inner_rec_norm (fst te, v.m_typedef_lhs_teid)
	    
      | Normal _ -> assert false
	  
      | Attribute (teid, atts) ->
	  let teid = snd (inner_rec_norm (fst te, teid))
	  in 
	  (fst te, add_attributed_te te_tbl (teid, atts))
  in inner_rec_norm te


let transfer_crt_function_te (addr_is_taken:(QN.t -> bool)) 
    (te:te) : te * (QN.t list) = 
  (* 
   * If the address of a formal paramter is taken, then we need to create a new
   * formal parameter and put the original formal paramter on local stack. 
   * e.g. f (int a) { &a } need to be translated to f (int a0) { int a; a = a0 }
  *)
  let (te_tbl, teid) = te
  in
  match te_kind_of te with
    | Crt_function v ->
	begin
	  let (changed, l, x) = 
	    List.fold_left
	      (fun (changed, l, x) e ->
		match e with
		  | THIS_PARAM _
		  | HIDDEN_RETURN _ 
		  | STRUCT_PARAM _ -> (changed, l @ [e], x)
		  | NORMAL_PARAM str -> 
		      begin
			if (addr_is_taken str) then
			  let param_teid = QNO.param_teid str
			  in
			  let muton = QNO.copy_alloc_qname str 
				  (str.QN.qn_sname ^ "_reg_")
				  param_teid
			  in
			  let _ = QNO.stacknize_param_qname str
			  in
			  (true, 
			  l @ 
			    [SCALAR_PARAM
			      { 
				orig = str;
				muton = muton;
			      }
			    ], x @[str])
			else
			  (changed, l @ [e], x)
		      end
			
		  | SCALAR_PARAM _ -> assert false
	      ) (false, [], []) v.m_crt_fun_in_params
	  in 
	  if changed then
	    let new_v = 
	      {
		m_crt_fun_abs_teid = v.m_crt_fun_abs_teid;
		m_crt_fun_in_params = l;
	      }
	    in ((te_tbl, add_crt_fun_te_info te_tbl new_v), x)
	  else
	    (te, x)
	end
    | _ -> assert false

let canonical_type (te_tbl, teid) : te = 
  let has_done_hash = Int.Type_idHashtbl.create 17
  in
  let rec compute_canonical_teid: teid -> teid =
    fun teid ->
      let typ = (te_tbl, teid)
      in
      let te_info = te_info_of typ
      in
      let equiv_teid = match te_info.m_kind with
	| Builtin -> teid
	| Array v -> 
	    begin
	      let new_v = 
		{ 
		  m_elmt_teid = compute_canonical_teid v.m_elmt_teid;
		  m_array_cardi = v.m_array_cardi
		}
	      in add_array_te te_tbl new_v
	    end
	| Abs_function v ->
	    begin
	      let new_v = 
		{ 
		  aft_ret_teid = 
		    compute_canonical_teid v.aft_ret_teid;
		  aft_param_teids = List.map
		    compute_canonical_teid v.aft_param_teids;
		  aft_va_arg = v.aft_va_arg;
		  aft_hidden_ret_param = 
		    Mapping.map_opt compute_canonical_teid
		      v.aft_hidden_ret_param;
		  aft_muton_param_pos = v.aft_muton_param_pos;
		}
	      in add_abs_fun_type te_tbl new_v
	    end
	      
	| Crt_function v ->
	    begin
	      let new_v = 
		{ 
		  m_crt_fun_abs_teid = 
		    compute_canonical_teid v.m_crt_fun_abs_teid;
		  m_crt_fun_in_params = v.m_crt_fun_in_params;
		}
	      in add_crt_fun_te_info te_tbl new_v
	    end
	| Struct field_table
	| Union field_table ->
	    let _ = 
	      try
		ignore(Int.Type_idHashtbl.find has_done_hash teid)
	      with
		  Not_found ->
		    begin
		      let _ = Int.Type_idHashtbl.add has_done_hash teid true
		      in
		      (*if te_info.m_complete then*)
		      let field_list = field_table.m_field_infos
		      in
		      let new_field_list = List.map 
			(fun f -> 
			  { 
			    field_sname_opt = f.field_sname_opt;
			    field_serno = f.field_serno;
			    field_teid = 
			      compute_canonical_teid f.field_teid;
			    field_te_has_def = f.field_te_has_def;
			    field_offset = f.field_offset;
			    field_is_bitfield = f.field_is_bitfield;
			  }
			) field_list
		      in
		      field_table.m_field_infos <- new_field_list
		    end
	    in
	    teid
	      
	| Enum _ -> teid

	| Enum_name 
	| Union_name 
	| Struct_name -> teid (*assert false*)
	    
	| Bits v ->
	    begin  
	      let new_v = 
		{ 
		  m_bit_base_teid = 
		    compute_canonical_teid v.m_bit_base_teid;
		  m_bit_size = v.m_bit_size;
		  m_bit_lsb = v.m_bit_lsb;
		  m_bit_mask_typ = v.m_bit_mask_typ;
		  m_bit_mask = v.m_bit_mask;
		  m_bit_emask = v.m_bit_emask;
		  m_bit_bnot_emask = v.m_bit_bnot_emask;
		  m_bit_right = v.m_bit_right;
		  m_bit_left = v.m_bit_left;
		}
	      in add_bit_te_info te_tbl new_v
	    end
	      
	| Qualified v ->
	    begin
	      let new_v = 
		{ 
		  m_qualified_teid = compute_canonical_teid v.m_qualified_teid;
		  m_qualifier = v.m_qualifier;
		}
	      in add_qualified_type te_tbl new_v
	    end 
	      
	| Typedef _ -> 
	    compute_canonical_teid (rd_equiv_teid te_tbl teid)

	| Normal _ -> assert false
	      
	| Pointer elmt_teid ->
	    snd (ptr_of (te_tbl, compute_canonical_teid elmt_teid))

	| Attribute (teid, atts) ->
	    let teid = compute_canonical_teid teid
	    in add_attributed_te te_tbl (teid, atts) 
      in
      let e = te_info_of (te_tbl, equiv_teid)
      in
      e.m_has_alias <- false;
      e.m_ptr_teid_opt <- None;
      (* assert (teid = equiv_teid);*)
      Int.Type_idHashtbl.replace te_tbl.typ_equiv_class_table teid equiv_teid;
      equiv_teid
  in (te_tbl, compute_canonical_teid teid)


let unqual_type (te_tbl, teid) : te = 
  let has_done_hash = Int.Type_idHashtbl.create 17
  in
  let rec compute_unqual_teid: teid -> teid =
    fun teid ->
      let typ = (te_tbl, teid)
      in
      let e = te_info_of typ
      in
      let equiv_teid = match e.m_kind with
	| Normal _ -> assert false
	| Builtin -> teid
	| Array v -> 
	    begin
	      let new_v = 
		{ 
		  m_elmt_teid = compute_unqual_teid v.m_elmt_teid;
		  m_array_cardi = v.m_array_cardi
		}
	      in add_array_te te_tbl new_v
	    end
	| Abs_function v ->
	    begin
	      let new_v = 
		{ 
		  aft_ret_teid = 
		    compute_unqual_teid
		      v.aft_ret_teid;
		  aft_param_teids = List.map
		    compute_unqual_teid v.aft_param_teids;
		  aft_va_arg = v.aft_va_arg;
		  aft_hidden_ret_param = 
		    Mapping.map_opt compute_unqual_teid v.aft_hidden_ret_param;
		  aft_muton_param_pos = v.aft_muton_param_pos;
		}
	      in add_abs_fun_type te_tbl new_v
	    end
	      
	| Crt_function v ->
	    begin
	      let new_v = 
		{ 
		  m_crt_fun_abs_teid = compute_unqual_teid v.m_crt_fun_abs_teid;
		  m_crt_fun_in_params = v.m_crt_fun_in_params;
		}
	      in add_crt_fun_te_info te_tbl new_v
	    end
	      
	| Struct field_table
	| Union field_table ->
	    let _ = 
	      try
		ignore(Int.Type_idHashtbl.find has_done_hash teid)
	      with
		  Not_found ->
		    begin
		      let _ = Int.Type_idHashtbl.add has_done_hash teid true
		      in
		      (*if e.m_complete then*)
			let field_list = field_table.m_field_infos
			in
			let new_field_list = List.map 
			  (fun f -> 
			    { 
			      field_sname_opt = f.field_sname_opt;
			      field_serno = f.field_serno;
			      field_teid = compute_unqual_teid f.field_teid;
			      field_te_has_def = f.field_te_has_def;
			      field_offset = f.field_offset;
			      field_is_bitfield = f.field_is_bitfield;
			    }
			  ) field_list
			in
			field_table.m_field_infos <- new_field_list
		    end
	    in teid
	      
	| Enum _ -> teid

	| Union_name 
	| Struct_name
	| Enum_name -> teid
	    
	| Bits v ->
	    begin  
	      let new_v = 
		{ 
		  m_bit_base_teid = compute_unqual_teid v.m_bit_base_teid;
		  m_bit_size = v.m_bit_size;
		  m_bit_lsb = v.m_bit_lsb;
		  m_bit_mask_typ = v.m_bit_mask_typ;
		  m_bit_mask = v.m_bit_mask;
		  m_bit_emask = v.m_bit_emask;
		  m_bit_bnot_emask = v.m_bit_bnot_emask;
		  m_bit_right = v.m_bit_right;
		  m_bit_left = v.m_bit_left;
		}
	      in add_bit_te_info te_tbl new_v
	    end
	      
	| Qualified v ->
	    compute_unqual_teid v.m_qualified_teid
	      
	| Typedef v -> 
	    compute_unqual_teid (rd_equiv_teid te_tbl teid)
	      
	| Pointer elmt_teid ->
	    snd (ptr_of (te_tbl, (compute_unqual_teid elmt_teid)))

	| Attribute (teid, atts) ->
	    add_attributed_te te_tbl (compute_unqual_teid teid, atts)
	      
      in equiv_teid
  in
  (te_tbl, compute_unqual_teid teid)





let unique_te typ = 
  let typ' = canonical_type typ
  in unqual_type typ'







let assert_uniqueness (te_tbl:te_tbl):unit =
  let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
  in
  let m_name_hash = QualNameHashtbl.create (len * 2)
  in
  Int.Type_idHashtbl.iter
    (fun teid b ->
      try
	let exist_teid = QualNameHashtbl.find m_name_hash b.m_name 
	in
	let fm = Format.formatter_of_out_channel stderr
	in
	Format.pp_open_vbox fm 0;
	Tent_c_printer.pp_print_c_type_only fm (te_tbl, exist_teid);
	Format.pp_print_space fm ();
	Tent_c_printer.pp_print_c_type_only fm (te_tbl, teid);
	Format.pp_close_box fm ();
	Format.pp_print_newline fm ();
	camlp4_macro_exception "\nm_name %s has teid '%d' and '%d'\n" 
	  (QNP.to_decl_str b.m_name)
	  exist_teid teid
      with
	  Not_found ->
	    QualNameHashtbl.add m_name_hash b.m_name teid
    ) te_tbl.teid_to_te_info
    

let rec convert_to_builtin_typ ((te_tbl, teid):te_tbl * teid) : te =
  match te_kind_of (te_tbl, teid) with
    | Builtin -> (te_tbl, teid)
    | Pointer _ -> (te_tbl, Mach.cptr_uint_id)
    | Enum _ -> (te_tbl, Mach.cint_id)
    | Normal _ -> 
	let teid = rd_norm_type_norm_id te_tbl teid
	in convert_to_builtin_typ (te_tbl, teid)
    | Bits _ ->
	let teid = rd_bit_type_base_type te_tbl teid
	in convert_to_builtin_typ (te_tbl, teid)
    | Qualified _ ->
	let typ = unqualified_typ (te_tbl, teid)
	in convert_to_builtin_typ typ

    | Union _ 
    | Struct _ ->
	let size = static_sizeof (te_tbl, teid)
	in (te_tbl, Mach.get_unsigned_tid size)
    | Array v ->
	begin
	  match v.m_array_cardi with
	    | ARRAY_VARIABLE -> (te_tbl, Mach.cptr_uint_id)
	    | _ -> assert false
	end
    | Attribute (teid, atts) ->
	begin
	  let ok = List.fold_left
	    (fun ok v ->
	      match v with
		| GCC_mode _ -> false
		| _ -> ok
	    ) true atts
	  in
	  if ok then
	    convert_to_builtin_typ (te_tbl, teid)
	  else
	    assert false
	end
    | Typedef te -> 
	convert_to_builtin_typ (te_tbl, te.m_typedef_lhs_teid)
	
    | Abs_function _ -> (te_tbl, Mach.cptr_uint_id)
    | Crt_function _ -> (te_tbl, Mach.cptr_uint_id)
	
    | _ -> assert false

let get_canonical_typ ((te_tbl, teid):te_tbl * teid) :te = 
  if is_builtin_typ (te_tbl, teid) then
    (te_tbl, teid)
  else
    (te_tbl, Int.Type_idHashtbl.find te_tbl.typ_equiv_class_table teid)


let normalize_cast_typ: te_tbl * teid -> te = 
  fun (te_tbl, orig_teid) ->
    let (_, norm_teid) = 
      let typ = get_canonical_typ (te_tbl, orig_teid)
      in
      let typ = unqual_type typ
      in
      if is_ptr_typ typ then
	if is_ptr_typ typ then
	  let elmt_typ = elmt_of typ
	  in
	  if is_bit_te elmt_typ then
	    ptr_of (bit_type_base elmt_typ)
	  else
	    typ
	else
	  typ
      else
	typ
    in
    if (orig_teid <> norm_teid) then
      let (nid, _) = add_norm_type te_tbl ~orig_teid ~norm_teid
      in (te_tbl, nid)
    else
      (te_tbl, orig_teid)


let normalize_decl_typ ((te_tbl, orig_teid): te_tbl * teid) :te = 
  let (_, norm_teid) = 
    let typ = get_canonical_typ (te_tbl, orig_teid)
    in
    if is_ptr_typ typ then
      if is_ptr_typ typ then
	let elmt_typ = elmt_of typ
	in
	if is_bit_te elmt_typ then
	  ptr_of (bit_type_base elmt_typ)
	else
	  typ
      else
	typ
    else
      typ
  in
  if (orig_teid <> norm_teid) then
    let (nid, _) = add_norm_type te_tbl ~orig_teid ~norm_teid
    in (te_tbl, nid)
  else
    (te_tbl, orig_teid)


let normalize_bit_typ ((te_tbl, orig_teid): te_tbl * teid) : te =
  if (is_bit_te (te_tbl, orig_teid)) then
    (te_tbl, rd_bit_type_base_type te_tbl orig_teid)
  else
    (te_tbl, orig_teid)



    
let is_equiv_typ: te_tbl * teid -> te_tbl * teid -> bool = 
  fun (te_tbl0, teid0) (te_tbl1, teid1) ->
     te_tbl0 == te_tbl1 & teid0 = teid1
      

let canonicalize (te_tbl:te_tbl) : unit = 
  let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
  in
  assert_uniqueness te_tbl;
  for i = 0 to (len - 1) do
    if !Typ_op_debug.typ_id = i then
      Debug.dummy_stop ();
    ignore(canonical_type (te_tbl, i));
  done;
  let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
  in
  for i = 0 to (len - 1) do
    let e = te_info_of (te_tbl, i)
    in
    e.m_ptr_teid_opt <- None
  done;
  assert_uniqueness te_tbl;
  let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
  in
  assert_uniqueness te_tbl;
  for i = 0 to (len - 1) do
    if !Typ_op_debug.typ_id = i then
      Debug.dummy_stop ();
    ignore(unqual_type (te_tbl, i));
  done;
  let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
  in
  for i = 0 to (len - 1) do
    let e = te_info_of (te_tbl, i)
    in e.m_ptr_teid_opt <- None
  done;
  assert_uniqueness te_tbl


exception Is_Integer
exception Is_Ptr
exception Is_Real

module Op = C_semantics_symbol
module CV = Converter

let is_ptr_or_array_typ t = is_ptr_typ t or is_array_typ t or is_xarray_typ t

let get_int_typ typ = 
  let typ = unqualified_typ typ
  in
  let typ = get_atomic_equiv_typ typ
  in
  let intid = 
    if is_ptr_typ typ or is_xarray_typ typ then
      Mach.cptr_uint_id
    else
      snd typ
  in
  (fst typ, intid)

let bin_arithm_typ (opcode:Op.binary_arithmatic) 
    (t1:te) (t2:te):
    ((te * (CA.cval -> CA.cval)) option * (te * (CA.cval -> CA.cval)) option * te) =
  let t1 = get_arithmatic_te t1
  and t2 = get_arithmatic_te t2
  in
  let t1 = 
    if is_bit_te t1 then
      bit_type_base t1
    else t1
  and t2 =
    if is_bit_te t2 then
      bit_type_base t2
    else t2
  in
  let (te_tbl, t1id) = t1
  and (_, t2id) = t2
  in
  let ui8_typ = (te_tbl, Mach.cuchar_id)
  and ui16_typ = (te_tbl, Mach.cushort_id)
  and ui32_typ = (te_tbl, Mach.uint32_id)
  and ui64_typ = (te_tbl, Mach.uint64_id)
  and int_typ = (te_tbl, Mach.cint_id)
  and uint_typ = (te_tbl, Mach.cuint_id)
  and long_typ = (te_tbl, Mach.clong_id)
  and ulong_typ = (te_tbl, Mach.culong_id)
  and llong_typ = (te_tbl, Mach.cllong_id)
  and ullong_typ = (te_tbl, Mach.cullong_id)
  in
  try
    if (is_real_typ t1) or (is_real_typ t2) then
      raise Is_Real
    else if (is_ptr_or_array_typ t1) or (is_ptr_or_array_typ t2) then
      raise Is_Ptr
    else
      raise Is_Integer
  with
    | Is_Real ->
	begin
	  let (c1, c2, r) = 
	    if (is_long_double_typ t1) then
	      begin
		if (is_long_double_typ t2) then
		  (None, None, t1)
		else
		  (None, Some ((te_tbl, Mach.cldouble_id), CV.get t2id t1id), t1)
	      end
	    else if (is_long_double_typ t2) then
	      begin
		(Some ((te_tbl, Mach.cldouble_id), CV.get t1id t2id), None, t2)
	      end
	    else if (is_double_typ t1) then
	      begin
		if (is_double_typ t2) then
		  (None, None, t1)
		else
		  (None, Some ((te_tbl, Mach.cdouble_id), CV.get t2id t1id), t1)
	      end
	    else if (is_double_typ t2) then
	      begin
		(Some ((te_tbl, Mach.cdouble_id), CV.get t1id t2id), None, t2)
	      end
	    else if (is_float_typ t1) then
	      begin
		if (is_float_typ t2) then
		  (None, None, t1)
		else
		  (None, Some ((te_tbl, Mach.cfloat_id), CV.get t2id t1id), t1)
	      end
	    else if (is_float_typ t2) then
	      begin
		(Some ((te_tbl, Mach.cfloat_id), CV.get t1id t2id), None, t2)
	      end
	    else
	      assert false
	  in
	  match opcode with
	    | Op.Add -> (c1, c2, r) (* todo integer overflow? *)
	    | Op.Sub -> (c1, c2, r)
	    | Op.Mul -> (c1, c2, r)
	    | Op.Div -> (c1, c2, r)
	    | Op.Mod -> assert false
	    | Op.Band -> assert false
	    | Op.Bor -> assert false
	    | Op.Bxor -> assert false
	    | Op.Shl -> assert false
	    | Op.Shr -> assert false
	end
    | Is_Integer ->
	begin
	  let (_, tid1) = 
	    if is_enum_typ t1 then 
	      (te_tbl, Mach.cint_id)
	    else
	      t1
	  and (_, tid2) = 
	    if is_enum_typ t2 then
	      (te_tbl, Mach.cint_id)
	    else
	      t2
	  in
	  let (c1, c2, r) =
	    match opcode with
	      | Op.Band 
	      | Op.Bor
	      | Op.Bxor -> 
		  begin
		    let maxtid = max tid1 tid2
		    in 
		    let size = static_sizeof (te_tbl, maxtid)
		    in match size with
		      | 1L -> 
			  (Some (uint_typ, CV.get tid1 Mach.cuint_id),
			  Some (uint_typ, CV.get tid2 Mach.cuint_id),
			  uint_typ)
		      | 2L -> 
			  (Some (uint_typ, CV.get tid1 Mach.cuint_id),
			  Some (uint_typ, CV.get tid2 Mach.cuint_id),
			  uint_typ)
		      | 4L -> 
			  (Some (uint_typ, CV.get tid1 Mach.cuint_id),
			  Some (uint_typ, CV.get tid2 Mach.cuint_id),
			  uint_typ)
		      | 8L ->
			  (Some (ui64_typ, CV.get tid1 Mach.uint64_id),
			  Some (ui64_typ, CV.get tid2 Mach.uint64_id),
			  ui64_typ)
		      | _ -> assert false
		  end
	      | Op.Add 
	      | Op.Sub 
	      | Op.Mul 
	      | Op.Div 
	      | Op.Mod ->
		  begin
		    (* int *)
		    if (tid1 <= Mach.cint_id) & (tid2 <= Mach.cint_id) then
		      (Some (int_typ, CV.get tid1 Mach.cint_id), Some (int_typ, CV.get
			tid2 Mach.cint_id), int_typ)
			
		    (* uint *)
		    else if (tid1 <= Mach.cuint_id) & (tid2 <= Mach.cuint_id) then
		      (Some (uint_typ, CV.get tid1 Mach.cuint_id), Some (uint_typ, CV.get
			tid2 Mach.cuint_id), uint_typ)
			
		    (* long *)
		    else if (tid1 <= Mach.clong_id) & (tid2 <= Mach.clong_id) then
		      if (tid1 = Mach.cuint_id || tid2 = Mach.cuint_id)
			& (static_sizeof long_typ = static_sizeof uint_typ)
		      then
			(Some (ulong_typ, CV.get tid1 Mach.culong_id), Some (ulong_typ, CV.get
			  tid2 Mach.culong_id), ulong_typ)
		      else
			(Some (long_typ, CV.get tid1 Mach.clong_id), Some (long_typ, CV.get
			  tid2 Mach.clong_id), long_typ)
			  
		    (* ulong *)
		    else if (tid1 <= Mach.culong_id) & (tid2 <= Mach.culong_id) then
		      (Some (ulong_typ, CV.get tid1 Mach.culong_id), Some (ulong_typ, CV.get
			tid2 Mach.culong_id), ulong_typ)
			
		    (* llong *)
		    else if (tid1 <= Mach.cllong_id) & (tid2 <= Mach.cllong_id) then
		      if (tid1 = Mach.culong_id || tid2 = Mach.culong_id)
			& (static_sizeof llong_typ = static_sizeof ulong_typ)
		      then
			(Some (ullong_typ, CV.get tid1 Mach.cullong_id), Some (ullong_typ, CV.get
			  tid2 Mach.cullong_id), ullong_typ)
		      else
			(Some (llong_typ, CV.get tid1 Mach.cllong_id), Some (llong_typ, CV.get
			  tid2 Mach.cllong_id), llong_typ)
			  
		    (* ullong *)
		    else if (tid1 <= Mach.cullong_id) & (tid2 <= Mach.cullong_id) then
		      (Some (ullong_typ, CV.get tid1 Mach.cullong_id), Some (ullong_typ, CV.get
			tid2 Mach.cullong_id), ullong_typ)
		    else
		      assert false
		  end
	      | Op.Shl 
	      | Op.Shr -> 
		  begin
		    let ltid = tid1
		      (*
		      if (tid1 <= Mach.cint_id) then 
			Mach.cint_id
		      else if (tid1 <= Mach.cuint_id) then
			Mach.cuint_id
		      else tid1
		      *)
		    and rtid = 
		      if (tid2 <= Mach.cint_id) then 
			Mach.cint_id
		      else if (tid2 <= Mach.cuint_id) then
			Mach.cuint_id
		      else tid2
		    in 
		    (
		      Some ((te_tbl, ltid), CV.get tid1 ltid), 
		      Some ((te_tbl, rtid), CV.get tid2 rtid),
		      (te_tbl, ltid)
		    )
		  end
	  in (c1, c2, r)
	       (*
	  match opcode with
	    | Op.Add -> (c1, c2, r)
	    | Op.Sub -> (c1, c2, r)
	    | Op.Mul -> (c1, c2, r)
	    | Op.Div -> (c1, c2, r)
	    | Op.Mod -> (c1, c2, r)
	    | Op.Band -> (c1, c2, r)
 		 | Op.Bor -> (c1, c2, r)
	    | Op.Bxor -> (c1, c2, r)
	    | Op.Shl 
	    | Op.Shr -> 
		let ltid = 
		  if (tid1 <= Mach.cint_id) then 
		    Mach.cint_id
		  else if (tid1 <= Mach.cuint_id) then
		    Mach.cuint_id
		  else tid1
		and rtid = 
		  if (tid2 <= Mach.cint_id) then 
		    Mach.cint_id
		  else if (tid2 <= Mach.cuint_id) then
		    Mach.cuint_id
		  else tid2
		in 
		(
		  Some ((te_tbl, ltid), CV.get tid1 ltid), 
		  Some ((te_tbl, rtid), CV.get tid2 rtid),
		  (te_tbl, ltid)
		 ) *)
	end
    | Is_Ptr ->
	begin
	  match opcode with
	    | Op.Add -> 
		if is_ptr_or_array_typ t1 & is_ptr_or_array_typ t2 then
		  assert false
		else if (is_ptr_or_array_typ t1) then
		  (None, Some ((te_tbl, Mach.cptr_uint_id), CV.get t2id
		    Mach.cptr_uint_id), t1)
		else if (is_ptr_or_array_typ t2) then
		  (Some ((te_tbl, Mach.cptr_uint_id), CV.get t1id
		    Mach.cptr_uint_id), None, t2) 
		else assert false
	    | Op.Sub -> 
		if is_ptr_or_array_typ t1 & is_ptr_or_array_typ t2 then
		  (None, None, int_typ)
		else if is_ptr_or_array_typ t1 then
		  (None, None, t1)
		else
		  assert false
	    | Op.Mul -> assert false
	    | Op.Div -> assert false
	    | Op.Mod -> assert false
	    | Op.Band -> assert false 
	    | Op.Bor -> assert false
	    | Op.Bxor -> assert false
	    | Op.Shl -> assert false
	    | Op.Shr -> assert false
	end


let bin_rel_typ (t1:te) (t2:te):
    ((te * (CA.cval -> CA.cval)) option * (te * (CA.cval -> CA.cval)) option * te) =
  let cptr_uint_typ = (fst t1, Mach.cptr_uint_id)
  in
  if is_ptr_typ t1 && is_ptr_typ t2 then
    (None, None, cptr_uint_typ)
  else if is_ptr_typ t1 then
    (None, Some (cptr_uint_typ, CV.get (snd t2) Mach.cptr_uint_id), cptr_uint_typ)
  else if is_ptr_typ t2 then
    (Some (cptr_uint_typ, CV.get (snd t1) Mach.cptr_uint_id), None, cptr_uint_typ)
  else
    bin_arithm_typ Op.Sub t1 t2

let get_cval_typ te_tbl cval =
  let tid = match cval with
    | CA.CCHAR _ -> Mach.cchar_id
    | CA.CSCHAR _ -> Mach.cschar_id
    | CA.CUCHAR _ -> Mach.cuchar_id
    | CA.CSHORT _ -> Mach.cshort_id
    | CA.CUSHORT _ -> Mach.cushort_id
    | CA.CINT _ -> Mach.cint_id
    | CA.CUINT _ -> Mach.cuint_id
    | CA.CLONG _ -> Mach.clong_id
    | CA.CULONG _ -> Mach.culong_id
    | CA.CLLONG _ -> Mach.cllong_id
    | CA.CULLONG _ -> Mach.culong_id
    | CA.CFLOAT _ -> Mach.cfloat_id
    | CA.CDOUBLE _ -> Mach.cdouble_id
    | CA.CLDOUBLE _ -> Mach.cldouble_id
    | CA.CFLOATX _ -> Mach.cfloat_id
    | CA.CDOUBLEX _ -> Mach.cdouble_id
    | CA.CLDOUBLEX _ -> Mach.cldouble_id
    | CA.CBOOL _ -> Mach.cbool_id
  in
  (te_tbl, tid)
	

open Const_folding

let make_const_small_int typ int_val =
  let typ' = unique_te typ
  in
  let (db, tid) = typ'
  in
  let cval = 
    if tid =  Mach.cchar_id then
      cchar_cval_of_string (string_of_int int_val)
    else if tid = Mach.cschar_id then
      cschar_cval_of_string (string_of_int int_val)
    else if tid = Mach.cuchar_id then
      cuchar_cval_of_string (string_of_int int_val)
    else if tid = Mach.cshort_id then
      cshort_cval_of_string (string_of_int int_val)
    else if tid = Mach.cushort_id then
      cushort_cval_of_string (string_of_int int_val)
    else if tid = Mach.cint_id then
      cint_cval_of_string (string_of_int int_val)
    else if tid = Mach.cuint_id then
      cuint_cval_of_string (string_of_int int_val)
    else if tid = Mach.clong_id then 
      clong_cval_of_string (string_of_int int_val)
    else if tid = Mach.culong_id then
      culong_cval_of_string (string_of_int int_val)
    else if tid = Mach.cllong_id then
      cllong_cval_of_string (string_of_int int_val)
    else if is_wchar_typ typ then
      if Mach.cint_id = Mach.cwchar_t_id then
	cint_cval_of_string (string_of_int int_val)
      else 
	assert false
    else
      assert false
  in
  (typ, cval_ext_of_cval cval)


let make_type_const typ str =
  let typ' = unique_te typ
  in
  let (db, tid) = typ'
  in
  let cval = 
    if is_ptr_typ typ then
      cuint_cval_of_string str
    else if is_enum_typ typ then
      cint_cval_of_string str
    else if tid = Mach.cbool_id then
      cbool_cval_of_string str
    else if tid = Mach.cchar_id then
      cchar_cval_of_string str
    else if tid = Mach.cschar_id then
      cschar_cval_of_string str
    else if tid = Mach.cuchar_id then
      cuchar_cval_of_string str
    else if tid = Mach.cshort_id then
      cshort_cval_of_string str
    else if tid = Mach.cushort_id then
      cushort_cval_of_string str
    else if tid = Mach.cint_id then
      cint_cval_of_string str
    else if tid = Mach.cuint_id then
      cuint_cval_of_string str
    else if tid = Mach.clong_id then 
      clong_cval_of_string str
    else if tid = Mach.culong_id then
      culong_cval_of_string str
    else if tid = Mach.cllong_id then
      cllong_cval_of_string str
    else if is_wchar_typ typ then
      begin
	if Mach.cint_id = Mach.cwchar_t_id then
	  cint_cval_of_string str
	else 
	  assert false
      end
    else if tid = Mach.cfloat_id then
      cfloat_cval_of_string str
    else if tid = Mach.cdouble_id then
      cdouble_cval_of_string str
    else if tid = Mach.cldouble_id then
      cldouble_cval_of_string str
    else 
      assert false
  in
  (typ, cval_ext_of_cval cval)


let is_native_assign_supported_typ typ =
  let size = static_sizeof typ
  and llsize = static_sizeof (fst typ, Mach.cllong_id)
  and lsize = static_sizeof (fst typ, Mach.clong_id)
  and isize = static_sizeof (fst typ, Mach.cint_id)
  and ssize = static_sizeof (fst typ, Mach.cshort_id)
  and csize = static_sizeof (fst typ, Mach.cchar_id)
  and fsize = static_sizeof (fst typ, Mach.cfloat_id)
  and dsize = static_sizeof (fst typ, Mach.cdouble_id)
  (*and ldsize = static_sizeof (fst typ, Mach.cldouble_id)*)
  in
  is_real_typ typ || is_integer_typ typ || is_ptr_typ typ 
  || size = fsize || size = dsize (*|| size = ldsize *)
      || size = llsize || size = lsize || size = isize 
      || size = ssize || size = csize


let create_array_type elmt_typ size =
  let te_tbl = fst elmt_typ
  in
  let array_decl = 
    { 
      m_elmt_teid = snd elmt_typ;
      m_array_cardi = ARRAY_FIXED size;
    }
  in (te_tbl, add_array_te te_tbl array_decl)


let extend_array_te_by_one typ = 
  let te_kind = te_kind_of typ
  and te_tbl = fst typ
  in
  match te_kind with
    | Array array_te_info ->
	begin
	  match array_te_info.m_array_cardi with
	    | ARRAY_FIXED size -> 
		let array_decl = 
		  { 
		    m_elmt_teid = array_te_info.m_elmt_teid;
		    m_array_cardi = ARRAY_FIXED (size +$ 1L);
		  }
		in (te_tbl, add_array_te te_tbl array_decl)
	    | _ -> assert false
	end
    | _ -> assert false



let compatible_unqual_tes te0 te1 =
  assert ((fst te0) = (fst te1));  
  let te0 = 
    if is_qual_te te0 then
      unqual_of te0
    else te0
  and te1 = 
    if is_qual_te te1 then
      unqual_of te1
    else te1
  in (snd te0) = (snd te1)
  
    
    
let is_cast_compatible te0 te1 =
  let rec unqual te = 
    match te_kind_of te with
      | Pointer elmt_teid ->
	  let elmt_te = unqual (fst te, elmt_teid)
	  in ptr_of elmt_te
      | Qualified v ->
	  unqual (fst te, v.m_qualified_teid)
      | _ -> te
  in
  let (_, did) = unqual te0
  and (_, sid) = unqual te1
  in did = sid
  
