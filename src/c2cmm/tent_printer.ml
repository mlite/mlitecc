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
include Tent

module QNP = Qual_name_printer

let indent = 2

let enable_m_name_debug = true
let enable_type_signature_debug = true

let pp_print_qual_name = Qual_name_printer.pp_print_qn_decl_str

let pp_print_c_comma: formatter -> unit =
  fun fm ->
    pp_print_string fm ",";
    pp_print_space fm ()

      
let pp_print_c_semicolon: formatter -> unit =
  fun fm ->
    pp_print_string fm ";";
    pp_print_space fm ()


  
let pp_print_typ_id (fm:formatter) teid : unit = 
  pp_open_box fm 0;
  fprintf fm "T_%d" teid;
  pp_close_box fm ()

let pp_print_te_attribute fm att = 
  let str = match att with
    | GCC_transparent_union -> "transparent_union"
    | GCC_aligned i -> "aligned(" ^ (string_of_int i) ^ ")"
    | GCC_aligned_max -> "aligned"
    | GCC_packed -> "packed"
    | GCC_mode mode -> 
	begin
	  let m = match mode with
	    | GCC_bi -> "__BI__"
	    | GCC_qi -> "__QI__"
	    | GCC_hi -> "__HI__"
	    | GCC_psi-> "__PSI__"
	    | GCC_si -> "__SI__"
	    | GCC_pdi-> "__PDI__"
	    | GCC_di -> "__SI__"
	    | GCC_ti -> "__TI__"
	    | GCC_oi -> "__OI__"
	    | GCC_qf -> "__QF__"
	    | GCC_hf -> "__HF__"
	    | GCC_tqf-> "__TQF__"
	    | GCC_sf -> "__SF__"
	    | GCC_df -> "__DF__"
	    | GCC_xf -> "__XF__"
	    | GCC_sd -> "__SD__"
	    | GCC_dd -> "__DD__"
	    | GCC_td -> "__TD__"
	    | GCC_tf -> "__TF__"
	    | GCC_qq -> "__QQ__"
	    | GCC_hq -> "__HQ__"
	    | GCC_sq -> "__SQ__"
	    | GCC_dq -> "__DQ__"
	    | GCC_tq -> "__TQ__"
	    | GCC_uqq-> "__UQQ__"
	    | GCC_uhq-> "__UHQ__"
	    | GCC_usq-> "__USQ__"
	    | GCC_udq-> "__UDQ__"
	    | GCC_utq-> "__UTQ__"
	    | GCC_ha -> "__HA__"
	    | GCC_sa -> "__SA__"
	    | GCC_da -> "__DA__"
	    | GCC_ta -> "__TA__"
	    | GCC_uha-> "__UHA__"
	    | GCC_usa-> "__USA__"
	    | GCC_uda-> "__UDA__"
	    | GCC_uta-> "__UTA__"
	    | GCC_cc -> "__CC__"
	    | GCC_word-> "word"
	    | GCC_byte-> "byte"
	    | GCC_pointer-> "pointer"
	  in
	  "__mode__(" ^ m ^ ")"
	end
    | GCC_may_alias -> "may_alias"
    | GCC_ms_struct -> "ms_struct"
    | GCC_gcc_struct -> "gcc_struct"
    | GCC_vector_size i -> "vector_size(" ^ (string_of_int i) ^ ")"
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

let pp_print_attributed_te_info fm (teid, attribute_list) = 
  pp_print_typ_id fm teid;
  pp_print_space fm ();
  pp_print_te_attribute_list fm attribute_list
      

let pp_print_typ_id_log: formatter -> te -> unit = 
  fun fm typ ->
    if !Mlite_config.enable_type_table_print then
      fprintf fm "/* T_%d */" (snd typ)

let pp_print_field_info_list: formatter -> field_info list -> unit =
  fun fm lst -> 
    let pp_print_field_info: formatter -> field_info -> unit =
      fun fm e ->
	pp_open_box fm 0;
	begin
	  pp_print_typ_id fm e.field_teid;
	  pp_print_space fm ();
	  match e.field_sname_opt with
	    | Some field_name -> pp_print_string fm field_name
	    | None -> ()
	end;
	pp_close_box fm ()
    in
    pp_open_vbox fm 0;
    begin
      pp_print_string fm "{";
      pp_open_vbox fm indent;
      pp_print_space fm ();
      begin
	Mlite_printer.pp_print_list fm
	  pp_print_field_info
	  pp_print_c_semicolon
	  lst;
      end;
      pp_close_box fm ();
      pp_print_c_semicolon fm;
      pp_print_string fm "}";
    end;
    pp_close_box fm ()
      

let pp_print_abs_fun_te_info: formatter -> abs_fun_te_info -> unit =
  fun fm abs_fun_te_info -> 
    pp_open_box fm 0;
    begin
      pp_print_typ_id fm abs_fun_te_info.aft_ret_teid;
      pp_print_space fm ();
      let _ = match abs_fun_te_info.aft_hidden_ret_param with
	| Some v -> 
	    pp_print_string fm "{";
	    pp_print_typ_id fm v;
	    pp_print_string fm "}"
	| None -> ()
      in
      let _ = pp_print_string fm "[";
	Mlite_printer.pp_print_list fm
	  pp_print_int
	  pp_print_c_comma
	  abs_fun_te_info.aft_muton_param_pos;
	pp_print_string fm "]"
      in
      pp_print_string fm "(";
      Mlite_printer.pp_print_list fm
	pp_print_typ_id
	pp_print_c_comma
	abs_fun_te_info.aft_param_teids;
      if abs_fun_te_info.aft_va_arg then
	pp_print_string fm ",...";
      pp_print_string fm ")";
    end;
    pp_close_box fm ()


let pp_print_crt_fun_te_info: formatter -> crt_fun_te_info -> unit = 
  fun fm crt_fun_te_info ->
    pp_open_box fm 0;
    begin
      pp_print_string fm "fn (";
      Mlite_printer.pp_print_list fm
	(fun fm v -> 
	  match v with
	    | THIS_PARAM str
	    | HIDDEN_RETURN str
	    | NORMAL_PARAM str -> 
		QNP.pp_print_qn_decl_str fm str
	    | SCALAR_PARAM qmuton 
	    | STRUCT_PARAM qmuton ->
		QNP.pp_print_qn_decl_str fm qmuton.orig;
		pp_print_string fm "->";
		QNP.pp_print_qn_decl_str fm qmuton.muton
		  
	) pp_print_c_comma
	crt_fun_te_info.m_crt_fun_in_params;
      pp_print_string fm ")";
      pp_print_string fm ":";
      pp_print_typ_id fm crt_fun_te_info.m_crt_fun_abs_teid;
    end;
    pp_close_box fm ()


let pp_print_array_cardi fm = function
  | ARRAY_FIXED size -> 
      pp_print_string fm (string_of_csize size);
  | ARRAY_VARIABLE -> ()

let pp_print_array_te_info: formatter -> array_te_info -> unit =
  fun fm array_te_info -> 
    pp_open_box fm 0;
    begin
      pp_print_typ_id fm array_te_info.m_elmt_teid;
      pp_print_space fm ();
      pp_print_string fm "[";
      pp_print_array_cardi fm array_te_info.m_array_cardi;
      pp_print_string fm "]"
    end;
    pp_close_box fm ()

let pp_print_pointer_te_info: formatter -> teid -> unit =
  fun fm pointer_te_info -> 
    pp_open_box fm 0;
    begin
      pp_print_typ_id fm pointer_te_info;
      pp_print_space fm ();
      pp_print_string fm "*";
    end;
    pp_close_box fm ()


let pp_print_bit_te_info: formatter -> bit_te_info -> unit =
  fun fm bit_te_info -> 
    pp_open_box fm 0;
    begin
      pp_print_typ_id fm bit_te_info.m_bit_base_teid;
      pp_print_space fm ();
      pp_print_string fm (string_of_csize bit_te_info.m_bit_size);
      pp_print_string fm ":";
      pp_print_string fm (string_of_csize bit_te_info.m_bit_lsb);
    end;
    pp_close_box fm ()

let pp_print_qual_te_info: formatter -> qual_te_info -> unit =
  fun fm qual_te_info -> 
    pp_open_box fm 0;
    begin
      let str = match qual_te_info.m_qualifier with
	| QUAL_CONST -> "const"
	| QUAL_VOLATILE -> "volatile"
	| QUAL_REGISTER -> "register"
	| QUAL_RESTRICT -> "restrict"
      in
      pp_print_string fm str;
      pp_print_space fm ();
      pp_print_typ_id fm qual_te_info.m_qualified_teid;
    end;
    pp_close_box fm ()


let pp_print_enum_te_info: formatter -> enum_item_info list -> unit =
  fun fm lst ->
    let pp_print_enum_item: formatter -> enum_item_info -> unit =
      fun fm e ->
	pp_open_box fm 0;
	begin
	  QNP.pp_print_qn_decl_str fm e.m_enum_item_name;
	  pp_print_space fm ();
	  pp_print_string fm "=";
	  pp_print_space fm ();
	  pp_print_int fm e.m_enum_item_value;
	end;
	pp_close_box fm ()
    in
    pp_open_vbox fm 0;
    begin
      pp_print_string fm "{";
      pp_open_vbox fm indent;
      pp_print_space fm ();
      begin
	Mlite_printer.pp_print_list fm
	  pp_print_enum_item
	  pp_print_c_comma
	  lst;
      end;
      pp_close_box fm ();
      pp_print_space fm ();
      pp_print_string fm "}";
    end;
    pp_close_box fm ()


let pp_print_typedef_te_info: formatter -> typedef_te_info -> unit =
  fun fm typedef_te_info ->
    pp_open_box fm 0;
    begin
      (*
      pp_print_string fm "typedef";
      pp_print_space fm ();
      *)
      pp_print_typ_id fm typedef_te_info.m_typedef_lhs_teid;
      (*
      pp_print_space fm ();
      Qual_name_printer.pp_print_qn_decl_str fm
	typedef_te_info.m_typedef_rhs_qname;
      *)
    end;
    pp_close_box fm ()

let pp_print_norm_table_entry: formatter -> norm_table_entry -> unit =
  fun fm norm_table_entry ->
    pp_open_box fm 0;
    begin
      pp_print_string fm "orig:";
      pp_print_typ_id fm norm_table_entry.m_norm_orig_teid;
      pp_print_string fm " norm:";
      pp_print_typ_id fm norm_table_entry.m_norm_norm_teid;
    end;
    pp_close_box fm ()
      

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

let pp_print_typ_size: formatter -> typ_size -> unit =
  fun fm typ_size ->
    match typ_size with
      | Byte_size i -> pp_print_string fm (Int64.to_string i); pp_print_string fm " bytes "
      | Incomplete -> pp_print_string fm "incomplete "
	  
let pp_print_te_info (fm:formatter) (te_tbl:te_tbl) (te_info:te_info) : unit =
  let v = te_info
  in
  pp_open_vbox fm 0;
  begin
    let _ = match v.m_kind with
      | Enum enum_te_info -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    pp_print_space fm ();
	    pp_print_enum_te_info fm enum_te_info.m_enum_item_infos
	  end

      | Enum_name ->
	  QNP.pp_print_qn_decl_str fm v.m_name
	    
      | Union field_table ->
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    pp_print_space fm ();
	    pp_print_field_info_list fm field_table.m_field_infos
	  end

      | Union_name ->
	  QNP.pp_print_qn_decl_str fm v.m_name
	    
      | Struct field_table ->
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    pp_print_space fm ();
	    pp_print_field_info_list fm field_table.m_field_infos
	  end
	   
      | Struct_name ->
	  QNP.pp_print_qn_decl_str fm v.m_name
	    
      | Crt_function crt_fun_te_info ->
	  pp_print_crt_fun_te_info fm crt_fun_te_info;
	    
      | Abs_function abs_fun_te_info ->
	  pp_print_abs_fun_te_info fm abs_fun_te_info
	    
      | Array array_te_info ->
	  pp_print_array_te_info  fm array_te_info
	
      | Pointer pointer_te_info -> 
	  pp_print_pointer_te_info  fm pointer_te_info
	    
      | Builtin -> 
	  QNP.pp_print_qn_decl_str fm v.m_name;
      
      | Typedef typedef_te_info ->
	  begin	    
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "typedef";
	      pp_print_space fm ();
	      pp_print_typedef_te_info fm typedef_te_info;
	      pp_print_space fm ();
	      QNP.pp_print_qn_decl_str fm v.m_name;
	    end;
	    pp_close_box fm ();
	  end
	    
      | Normal norm_table_entry ->
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_space fm ();
	      pp_print_norm_table_entry fm norm_table_entry;
	      pp_print_space fm ();
	      QNP.pp_print_qn_decl_str fm v.m_name;
	    end;
	    pp_close_box fm ();
	  end

      | Bits bit_te_info -> 
	  pp_print_bit_te_info fm bit_te_info 
	    
      | Qualified qual_te_info ->
	  pp_print_qual_te_info fm qual_te_info

      | Attribute (teid, attributes) ->
	  pp_print_attributed_te_info fm (teid, attributes)
	    
    in pp_print_te_attribute_list fm v.m_attribute_list
  end;
  pp_close_box fm ()



let pp_print_te_tbl (fm:formatter) (te_tbl:te_tbl): unit = 
  let pp_print_all_typ_ids () =
    let len = Int.Type_idHashtbl.length te_tbl.teid_to_te_info
    in
    let type_array = Array.make len
      { 
	m_id = 0;
	m_name = QN.default_qual_name;
	m_kind = Builtin;
	m_size = Incomplete;
	m_align = 0L;
	m_sizeof_computed = false;
	m_alignof_computed = false;
	m_runtime = true;
	m_ptr_teid_opt = None;
	m_has_alias = true;
	m_attribute_list = [];
      }
    in
    Int.Type_idHashtbl.iter
      (fun k b ->
	Array.set type_array k b
      ) te_tbl.teid_to_te_info;
    pp_open_vbox fm 0;
    begin
      if enable_m_name_debug then
	begin
	  pp_print_string fm 
	    "-TYPE---C---sizeof--alignof--m_name---------------------------------------";
	  pp_print_space fm ();
	  Mlite_printer.pp_print_array fm
	    (fun fm i v ->
	      pp_print_serno fm i;
	      pp_print_string fm "C ";
	      if v.m_sizeof_computed then
		pp_print_typ_size fm v.m_size
	      else
		pp_print_string fm "   ? bytes ";
	      
	      if v.m_alignof_computed then
		pp_print_align fm v.m_align
	      else
		pp_print_string fm " ? ";

	      fprintf fm " \"%s\"" (QNP.to_decl_str v.m_name);
	      if i = te_tbl.num_of_typs_in_files then
		begin
		  pp_print_space fm ();
		  pp_print_string fm 
		    "---------------------------above are typs in files------------------------";
		end
	    ) 
	    (fun fm -> pp_print_space fm ())
	    type_array;
	  pp_print_space fm ();
	  pp_print_string fm 
	    "--------------------------------------------------------------------------";
	  pp_print_space fm ();
	end;
      if enable_type_signature_debug then
	begin
	  pp_print_string fm 
	    "-TYPE-----type signature--------------------------------------------------";
	  pp_print_space fm ();
	  Mlite_printer.pp_print_array fm
	    (fun fm i v ->
	      pp_print_serno fm i;
	      if v.m_has_alias then
		pp_print_string fm " A "
	      else
		pp_print_string fm "   ";
	      pp_print_te_info fm te_tbl v
	    ) 
	    (fun fm -> pp_print_space fm ())
	    type_array;
	  pp_print_space fm ();
	  pp_print_string fm 
	    "--------------------------------------------------------------------------";
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

let print_te_tbl: te_tbl -> unit = (** can be invoked by ocaml debugger **)
  fun te_tbl ->
    let str = camlp4_macro_str_pp_print
	(fun fm ->
	  pp_open_box fm 0;
	  let _ = 
	    try
	      pp_print_te_tbl fm te_tbl;
	    with
	      Not_found ->
	      pp_print_string fm "error"
	  in
	  pp_close_box fm ())
    in
    print_string str


let pp_print_te fm ((te_tbl, typ_id):te) =
  try
    let te_info = Int.Type_idHashtbl.find te_tbl.teid_to_te_info typ_id
    in pp_print_te_info fm te_tbl te_info
  with
      Not_found ->
	fprintf fm "typ_id %d is not in the te_tbl" typ_id
	  
	  
let print_te te = 
  let str = camlp4_macro_str_pp_print
    (fun fm ->
      pp_open_box fm 0;
      pp_print_te fm te;
      pp_close_box fm ())
  in
  print_string str


