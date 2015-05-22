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

open Tent
open Tent_op
module QN = Qual_name
module QNO = Qual_name_op
module T = Tent
module TO = Tent_op

module HashTyp = 
struct 
  type t = te
  let equal (s1:t) (s2:t) = 
    let (db1, id1) = s1
    and (db2, id2) = s2
    in 
    db1 == db2 & id1 = id2
	
  let hash (s:t) =
    Hashtbl.hash s 
end
    
module TypHashtbl = Hashtbl.Make(HashTyp)   


type assumption_equiv = teid * teid
type typ_id = teid

type teid_maps =
    {
      local_te_tbl_name: string;
      gteid_to_lteid: teid list array;
      lteid_to_gteid: teid option array;
    }

let debug_global_teid = ref 0
let debug_import_teid = ref 0
let debug_field_name = ref None
let debug_new_teid = ref 0
      
let check_coinductive_assumption_equiv 
    (lst:assumption_equiv list) (v0:teid) (v1:teid): bool = 
  List.exists (fun (v0', v1') -> v0' = v0 & v1' = v1) lst

      
      
let rec add_assumption_equiv (lst:assumption_equiv list) 
    (te0:te) (te1:te) :assumption_equiv list = 
  let te_info0 = te_info_of te0
  and te_info1 = te_info_of te1
  in
  if te_info0.m_size <> te_info1.m_size or te_info0.m_align <> te_info1.m_align then
    lst
  else
    match te_info0.m_kind, te_info1.m_kind with
      | Struct _, Struct _
      | Union _, Union _ ->
	  (snd te0, snd te1)::lst
      | _, _ -> lst

	  
exception Not_builtin

let builtin_typ_equiv: te -> te -> bool =
  fun (typ_db0, teid0) (typ_db1, teid1) ->
    assert (typ_db0.max_preload_typ = typ_db1.max_preload_typ);
    if teid0 < typ_db0.max_preload_typ then
      teid0 = teid1
    else
      raise Not_builtin


let set_teid_maps 
    (teid_maps:teid_maps) 
    ~(global_typ_id:typ_id)
    ~(local_te_tbl_name:string)
    ~(local_typ_id:typ_id) : unit =
  if global_typ_id = !debug_global_teid & local_typ_id = !debug_import_teid then
    Debug.dummy_stop ();
  Array.set teid_maps.lteid_to_gteid local_typ_id (Some (global_typ_id));
  let local_typ_ids = 
    Array.get teid_maps.gteid_to_lteid global_typ_id
  in
  Array.set 
    teid_maps.gteid_to_lteid 
    global_typ_id (local_typ_id::local_typ_ids)

open Format
let pp_print_teid_maps: formatter -> teid_maps -> unit =
  fun fm teid_maps ->
    pp_open_vbox fm 0;
    pp_print_string fm "global -> locals";
    pp_print_space fm ();
    Array.iteri 
      (fun i local_typ_ids ->
	pp_open_vbox fm 2;
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  (fun fm (typ_id) -> fprintf fm "%d -> %d" i typ_id)
	  (fun fm -> pp_print_space fm ())
	  local_typ_ids;
	pp_close_box fm ();
	pp_print_space fm ();
      ) teid_maps.gteid_to_lteid;
    pp_print_space fm ();
    pp_print_string fm "local -> global";
    pp_print_space fm ();
    Array.iteri 
      (fun i typ_opt ->
	match typ_opt with
	  | Some (typ_id) -> 
	      fprintf fm "%d -> %d" i typ_id;
	      pp_print_space fm ();
	  | None -> ()
      ) teid_maps.lteid_to_gteid;
    pp_close_box fm ()

let assert_teid_maps (teid_maps:teid_maps) : unit = 
  let len = (Array.length teid_maps.lteid_to_gteid - 1)
  in
  for i = 0 to len do
    match Array.get teid_maps.lteid_to_gteid i with
      | Some global_typ_id -> 
	  begin
	    let local_typ_ids = 
	      Array.get teid_maps.gteid_to_lteid global_typ_id
	    in assert (List.exists (fun v -> v = i) local_typ_ids)
	  end
      | None -> ()
  done
      

type import_as_teid = 
    { 
      global_incomplete_teid: typ_id;
      local_complete_teid: typ_id;
    }

exception Found_equiv_typ of typ_id * import_as_teid list
      
let te_struct_eq (teid_maps:teid_maps) (assumption_equiv_list:assumption_equiv list)
    (typ0:te) (typ1:te) : bool * import_as_teid list = 
  let rec rec_te_struct_eq (teid_maps:teid_maps) (assumption_equiv_list:assumption_equiv list)
      (typ0:te) (typ1:te) : bool * import_as_teid list = 
    let (t0, v0) = typ0
    and (t1, v1) = typ1
    in
    let te0 = te_info_of (t0, v0)
    and te1 = te_info_of (t1, v1)
    in
    let compare () = 
      try
	(builtin_typ_equiv (t0, v0) (t1, v1), [])
      with
	| Not_builtin ->
	    let coinductive_equiv = 
	      check_coinductive_assumption_equiv 
		assumption_equiv_list v0 v1
	    in
	    if coinductive_equiv then
	      (true, [])
	    else
	      begin
		match te0.m_kind, te1.m_kind with
		  | Builtin, _ -> assert false
		  | Union_name, _ 
		  | Struct_name, _ 
		  | Enum_name, _ -> (* these cases should have been handled *)
		      assert false
			
		  | Typedef v0, Typedef v1 ->
		      rec_te_struct_eq teid_maps assumption_equiv_list
			(t0, v0.m_typedef_lhs_teid) 
			(t1, v1.m_typedef_lhs_teid)
			
		  | Normal v0, Normal v1 -> 
		      let (eq, lst) = 
			rec_te_struct_eq teid_maps assumption_equiv_list
			  (t0, v0.m_norm_orig_teid) 
			  (t1, v1.m_norm_orig_teid)
		      in
		      if (eq) then
			let (eq, lst0) = rec_te_struct_eq teid_maps assumption_equiv_list
			  (t0, v0.m_norm_norm_teid) 
			  (t1, v1.m_norm_norm_teid)
			in
			if (eq) then
			  (true, lst @ lst0)
			else
			  (false, [])
		      else
			(false, [])
			  
		  | Array v0, Array v1 -> 
		      begin
			if v0.m_array_cardi <> v1.m_array_cardi then
			  (false, [])
			else
			  rec_te_struct_eq teid_maps assumption_equiv_list
			    (t0, v0.m_elmt_teid) 
			    (t1, v1.m_elmt_teid)
		      end
			
		  | Abs_function e0, Abs_function e1 -> 
		      begin
			if List.length e0.aft_param_teids <> 
			  List.length e1.aft_param_teids or
			  e0.aft_va_arg <> e1.aft_va_arg
			then
			  (false, [])
			else 
			  begin 
			    let (out_typ_equiv, import_list0)  = 
			      rec_te_struct_eq teid_maps assumption_equiv_list
				(t0, e0.aft_ret_teid)
				(t1, e1.aft_ret_teid) 
			    in
			    if out_typ_equiv then
			      begin
				let (all_te_struct_eq, import_list1) = 
				  List.split
				    (List.map2 
				      (fun v0 v1 ->
					let (is_eq, lst) = 
					  rec_te_struct_eq 
					    teid_maps 
					    assumption_equiv_list 
					    (t0, v0) (t1, v1)
					in
					if not is_eq then
					  Debug.dummy_stop ();
					(is_eq, lst)
				      ) 
				      e0.aft_param_teids
				      e1.aft_param_teids
				    )
				in
				(List.for_all (fun v -> v) all_te_struct_eq, 
				import_list0 @ (List.flatten import_list1))
			      end
			    else
			      (false, [])
			  end
		      end
			
		  | Crt_function e0, Crt_function e1 ->
		      begin
			if List.length e0.m_crt_fun_in_params = 
			  List.length e1.m_crt_fun_in_params 
			then
			  begin
			    let all_equiv = 
			      List.map2
				(fun e0 e1 -> 
				  match e0, e1 with
				    | THIS_PARAM v0, THIS_PARAM v1 
				    | HIDDEN_RETURN v0, HIDDEN_RETURN v1
				    | NORMAL_PARAM v0, NORMAL_PARAM v1 ->
					QNO.equiv_param 
					  (fun teid0 teid1 ->
					    let (equiv, lst) = 
					      rec_te_struct_eq teid_maps
						assumption_equiv_list
						(t0, teid0) (t1, teid1)
					    in assert (lst = []);
					    equiv
					  ) v0 v1
					  
				    | SCALAR_PARAM v0, SCALAR_PARAM v1
				    | STRUCT_PARAM v0, STRUCT_PARAM v1 ->
					QNO.equiv_param 
					  (fun teid0 teid1 ->
					    let (equiv, lst) = 
					      rec_te_struct_eq teid_maps
						assumption_equiv_list
						(t0, teid0) (t1, teid1)
					    in assert (lst = []);
					    equiv
					  ) v0.orig v1.orig
				    | _, _ -> false
				)
				e0.m_crt_fun_in_params
				e1.m_crt_fun_in_params
			    in
			    if List.for_all (fun v -> v) all_equiv then
			      rec_te_struct_eq teid_maps 
				assumption_equiv_list
				(t0, e0.m_crt_fun_abs_teid)
				(t1, e1.m_crt_fun_abs_teid)
			    else
			      (false, [])
			  end
			else
			  (false, [])
		      end
			
		  | Pointer e0, Pointer e1 -> 
		      rec_te_struct_eq teid_maps assumption_equiv_list
			(t0, e0) (t1, e1)
			
		  | Struct field_infos0, Struct field_infos1  
		  | Union field_infos0, Union field_infos1 ->
		      begin
			if List.length field_infos0.m_field_infos <> 
			  List.length field_infos1.m_field_infos then
			    (false, [])
			else
			  let all_field_name_equiv = 
			    List.map2
			      (fun f0 f1 ->
				match f0.field_sname_opt with
				  | Some s0 ->
				      begin
					match f1.field_sname_opt with
					  | Some s1 -> s0 = s1
					  | None -> false
				      end
				  | None -> 
				      begin
					match f1.field_sname_opt with
					  | Some s1 -> false
					  | None -> true
				      end
			      ) field_infos0.m_field_infos 
			      field_infos1.m_field_infos
			  in
			  if List.for_all (fun v -> v) all_field_name_equiv then
			    begin
			      let assumption_equiv_list = 
				add_assumption_equiv 
				  assumption_equiv_list (t0, v0) (t1, v1)
			      in
			      let (all_field_te_struct_eq, import_list) = 
				List.split
				  (List.map2 
				    (fun f0 f1 ->
				      if f0.field_sname_opt = !debug_field_name then
					Debug.dummy_stop ();
				      let (is_eq, lst) =
					rec_te_struct_eq teid_maps
					  assumption_equiv_list
					  (t0, f0.field_teid) 
					  (t1, f1.field_teid) 
				      in
				      if not is_eq then
					Debug.dummy_stop ();
				      (is_eq, lst)
				    ) 
				    field_infos0.m_field_infos
				    field_infos1.m_field_infos
				  )
			      in
			      (List.for_all (fun v -> v) all_field_te_struct_eq, 
			      List.flatten import_list)
			    end
			  else
			    (false, [])
		      end
			
		  | Enum enum_te_info0, Enum enum_te_info1 ->
		      begin
			let e_lst0 = enum_te_info0.m_enum_item_infos
			and e_lst1 = enum_te_info1.m_enum_item_infos
			in
			if List.length e_lst0 <> List.length e_lst1 then
			  (false, [])
			else
			  let all_equiv = 
			    List.map2 
			      (fun e0 e1 -> 
				if (QN.eq e0.m_enum_item_name e1.m_enum_item_name) & 
				  e0.m_enum_item_value = e1.m_enum_item_value then
				    true
				else
				  false
			      ) e_lst0 e_lst1
			  in (List.for_all (fun v -> v) all_equiv,[])
		      end

		  | Bits e0, Bits e1 ->
		      begin
			if e0.m_bit_size = e1.m_bit_size &&
			  e0.m_bit_lsb = e1.m_bit_lsb &&
			  e0.m_bit_left = e1.m_bit_left &&
			  e0.m_bit_right = e1.m_bit_right &&
			  e0.m_bit_mask_typ = e1.m_bit_mask_typ
			then
			  rec_te_struct_eq teid_maps []
			    (t0, e0.m_bit_base_teid)
			    (t1, e1.m_bit_base_teid)
			else
			  (false, [])
		      end
			
		  | Qualified e0, Qualified e1 ->
		      if e0.m_qualifier = e1.m_qualifier then
			rec_te_struct_eq teid_maps 
			  assumption_equiv_list 
			  (t0, e0.m_qualified_teid) 
			  (t1, e1.m_qualified_teid)
		      else
			(false, [])

		  | Attribute (teid0, atts0), Attribute (teid1, atts1) ->
		      if atts0 = atts1 then
			rec_te_struct_eq teid_maps 
			  assumption_equiv_list (t0, teid0) (t1, teid1)
		      else
			(false, [])
			  
		  | _, _ -> (false, [])
	      end
    in
    if TO.is_alias_kind te0.m_kind || TO.is_alias_kind te1.m_kind ||
      (te0.m_kind = te1.m_kind && (is_bit_kind te0.m_kind || TO.is_norm_kind te0.m_kind ||
	is_abs_fun_kind te0.m_kind)) 
    then
      let (t, ls) = compare ()
      in
      let _ = if t then
	set_teid_maps teid_maps ~global_typ_id:te0.m_id 
	  ~local_te_tbl_name:t1.te_tbl_name
	  ~local_typ_id:te1.m_id
      in (t, ls)
    else 
      match Array.get teid_maps.lteid_to_gteid v1 with
	| Some (v1) -> (v1 = v0, [])
	| None -> 
	    let local_typ_ids = 
	      Array.get teid_maps.gteid_to_lteid v0 
	    in
	    if local_typ_ids <> [] then
	      (List.exists (fun v -> v = v1) local_typ_ids, [])
	    else
	      compare ()
  in rec_te_struct_eq teid_maps [] typ0 typ1
       
	    
let import_typ (teid_maps:teid_maps) ~(dest_tbl:te_tbl) ((src_tbl, typ_id):te):te =
  let rec import ~(src_tbl:te_tbl) typ_id :typ_id = 
    if typ_id > dest_tbl.max_preload_typ then
      begin
	match Array.get teid_maps.lteid_to_gteid typ_id with
	  | Some typ_id -> typ_id
	  | None ->
	      begin
		let e = te_info_of (src_tbl, typ_id)
		in
		match e.m_kind with
		  | Normal v -> 
		      begin
			import ~src_tbl v.m_norm_norm_teid
		      end
			
		  | Typedef _ ->
		      begin
			try
			  Int.Type_idHashtbl.iter
			    (fun k0 b0 ->
			      if k0 = !debug_global_teid then
				Debug.dummy_stop ();
			      if k0 > dest_tbl.max_preload_typ then
				let (is_equiv, import_list) =
				  te_struct_eq teid_maps [] (dest_tbl, k0) (src_tbl, typ_id)
				in
				if is_equiv then
				  raise (Found_equiv_typ (k0, import_list))
			    ) dest_tbl.teid_to_te_info;
			  add_typ ~src_tbl typ_id
			with
			    Found_equiv_typ (global_typ_id, import_list) -> 
			      begin
				set_teid_maps teid_maps ~global_typ_id 
				  ~local_te_tbl_name:src_tbl.T.te_tbl_name
				  ~local_typ_id:typ_id;
				List.iter
				  (fun import_as_teid ->
				    ignore
				      (import_typ_as
					~src_tbl 
					import_as_teid.local_complete_teid 
					~as_typ_id:import_as_teid.global_incomplete_teid)
				  ) import_list;
				global_typ_id
			      end
		      end
		  | _ ->
		      begin
			try
			  Int.Type_idHashtbl.iter
			    (fun k0 b0 ->
			      if k0 = !debug_global_teid then
				Debug.dummy_stop ();
			      if k0 > dest_tbl.max_preload_typ then
				let (is_equiv, import_list) =
				  te_struct_eq teid_maps [] (dest_tbl, k0) (src_tbl, typ_id)
				in
				if is_equiv then
				  raise (Found_equiv_typ (k0, import_list))
			    ) dest_tbl.teid_to_te_info;
			  add_typ ~src_tbl typ_id
			with
			    Found_equiv_typ (global_typ_id, import_list) -> 
			      begin
				set_teid_maps teid_maps ~global_typ_id 
				  ~local_te_tbl_name:src_tbl.T.te_tbl_name
				  ~local_typ_id:typ_id;
				List.iter
				  (fun import_as_teid ->
				    ignore
				      (import_typ_as
					~src_tbl 
					import_as_teid.local_complete_teid 
					~as_typ_id:import_as_teid.global_incomplete_teid)
				  ) import_list;
				global_typ_id
			      end
		      end
	      end
      end
    else
      typ_id

  and add_typ ~(src_tbl:te_tbl) typ_id :typ_id = 
    let new_typ_id = (alloc_teid dest_tbl)
    and e = te_info_of (src_tbl, typ_id)
    in
    if new_typ_id = !debug_new_teid then
      Debug.dummy_stop ();
    if typ_id = !debug_import_teid then
      Debug.dummy_stop ();
    set_teid_maps teid_maps ~global_typ_id:new_typ_id 
      ~local_te_tbl_name:src_tbl.T.te_tbl_name
      ~local_typ_id:typ_id;
    let _ = match e.m_ptr_teid_opt with
      | Some typ -> Some (import ~src_tbl typ)
      | None -> None
    in
    let (m_name, m_kind) = match e.m_kind with
      | Builtin -> assert false
      | Enum_name -> assert false
      | Typedef v -> 
	  begin
	    let new_v = 
	      { 
		m_typedef_lhs_teid = import ~src_tbl v.m_typedef_lhs_teid;
	      }
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_typedef_te_info fm new_v),
	    Typedef new_v)
	  end
	    
      | Normal _ -> assert false
      
      | Array v -> 
	  begin
	    let new_v = 
	      {
		m_elmt_teid = import ~src_tbl v.m_elmt_teid;
		m_array_cardi = v.m_array_cardi
	      }
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_array_te_info fm new_v), Array new_v)
	  end
	    
      | Abs_function v ->
	  begin
	    let new_v = 
	      {
		aft_ret_teid = import ~src_tbl 
		  v.aft_ret_teid;
		aft_param_teids = List.map
		  (import ~src_tbl) v.aft_param_teids;
		aft_va_arg = v.aft_va_arg;
		aft_hidden_ret_param = 
		  Mapping.map_opt (import ~src_tbl) v.aft_hidden_ret_param;
		aft_muton_param_pos = v.aft_muton_param_pos;
	      }
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_abs_fun_te_info fm new_v), 
	    Abs_function new_v)
	  end
	    
      | Crt_function v ->
	  begin
	    let new_v = 
	      {
		m_crt_fun_abs_teid = import ~src_tbl v.m_crt_fun_abs_teid;
		m_crt_fun_in_params = 
		  List.map 
		    (fun qn -> 
		      match qn with
			| THIS_PARAM qn -> 
			    THIS_PARAM (QNO.map_param (import ~src_tbl) qn)
			| HIDDEN_RETURN qn ->
			    HIDDEN_RETURN (QNO.map_param (import ~src_tbl) qn)
			| NORMAL_PARAM qn -> 
			    NORMAL_PARAM (QNO.map_param (import ~src_tbl) qn)
			| STRUCT_PARAM qn ->
			    STRUCT_PARAM
			      {
				orig = 
				  QNO.map_param (import ~src_tbl) qn.orig;
				muton = 
				  QNO.map_param (import ~src_tbl)
				    qn.muton;
			      }
			| SCALAR_PARAM qn ->
			    SCALAR_PARAM
			      {
				orig = 
				  QNO.map_param (import ~src_tbl) qn.orig;
				muton = 
				  QNO.map_param (import ~src_tbl)
				    qn.muton;
			      }
		    ) v.m_crt_fun_in_params
	      }
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_crt_fun_te_info fm new_v), Crt_function new_v)
	  end
	    
      | Pointer v ->
	  begin
	    let new_v = import ~src_tbl v
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_pointer_te_info fm new_v), Pointer new_v)
	  end
	    
      | Struct fields ->
	  let field_list = fields.m_field_infos
	  in
	  let new_field_list = List.map 
	    (fun f -> 
	      {
		field_sname_opt = f.field_sname_opt;
		field_serno = f.field_serno;
		field_teid = import ~src_tbl f.field_teid;
		field_te_has_def = f.field_te_has_def;
		field_offset = f.field_offset;
		field_is_bitfield = f.field_is_bitfield;
	      }
	    ) field_list
	  in (e.m_name.QN.qn_sname, Struct { T.m_field_infos = new_field_list; })
	    
      | Struct_name -> 
	  (e.m_name.QN.qn_sname, Struct_name)
	    
      | Union fields ->
	  let field_list = fields.m_field_infos
	  in
	  let new_field_list = List.map 
	    (fun f -> 
	      {
		field_sname_opt = f.field_sname_opt;
		field_serno = f.field_serno;
		field_teid = import ~src_tbl f.field_teid;
		field_te_has_def = f.field_te_has_def;
		field_offset = f.field_offset;
		field_is_bitfield = f.field_is_bitfield;
	      }
	    ) field_list
	  in (e.m_name.QN.qn_sname, Union { T.m_field_infos = new_field_list; })
	       
      | Union_name -> 
	  (e.m_name.QN.qn_sname, Union_name)
	    
      | Enum enum_item_info ->
	  begin
	    let enum_item_list = enum_item_info.m_enum_item_infos
	    in
	    let new_enum_item_list = List.map
	      (fun e ->
		{
		  m_enum_item_name = e.m_enum_item_name;
		  m_enum_item_serno = e.m_enum_item_serno;
		  m_enum_item_value = e.m_enum_item_value;
		}
	      ) enum_item_list
	    in
	    (e.m_name.QN.qn_sname, 
	    Enum 
	      { 
		T.m_enum_max_value = enum_item_info.T.m_enum_max_value;
		T.m_enum_item_infos = new_enum_item_list;
	      }
	    )
	  end
	    
      | Bits v ->
	  begin  
	    let new_v = 
	      {
		m_bit_base_teid = import ~src_tbl v.m_bit_base_teid;
		m_bit_size = v.m_bit_size;
		m_bit_lsb = v.m_bit_lsb;
		m_bit_right = v.m_bit_right;
		m_bit_left = v.m_bit_left;
		m_bit_mask_typ = v.m_bit_mask_typ;
		m_bit_mask = v.m_bit_mask;
		m_bit_emask = v.m_bit_emask;
		m_bit_bnot_emask = v.m_bit_bnot_emask;
	      }
	    in
	    (e.m_name.QN.qn_sname, Bits new_v)
	  end
	    
      | Qualified v ->
	  begin
	    let new_v = 
	      {
		m_qualified_teid = import ~src_tbl v.m_qualified_teid;
		m_qualifier = v.m_qualifier;
	      }
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_qual_te_info
		fm new_v), Qualified new_v)
	  end 
      | Attribute (teid, atts) ->
	  begin
	    let teid = import ~src_tbl teid
	    in
	    (camlp4_macro_str_pp_print
	      (fun fm -> Tent_printer.pp_print_attributed_te_info
		fm (teid, atts)), Attribute (teid, atts))
	  end
    in
    let new_e = 
      { 
	m_id = new_typ_id;
	m_name = Qual_name_op.alloc_type_name ([]) m_name;
	m_kind = m_kind;
	m_ptr_teid_opt = None;
	m_size = e.m_size;
	m_align = e.m_align;
	m_sizeof_computed = e.m_sizeof_computed;
	m_alignof_computed = e.m_alignof_computed;
	m_runtime = e.m_runtime;
	m_has_alias = e.m_has_alias;
	m_attribute_list = [];
      }
    in
    Int.Type_idHashtbl.replace dest_tbl.teid_to_te_info new_typ_id new_e;
    new_typ_id

  and import_typ_as ~(src_tbl:te_tbl) typ_id ~(as_typ_id:typ_id) : typ_id = 
    let new_typ_id = as_typ_id
    and e = te_info_of (src_tbl, typ_id)
    in
    if new_typ_id = !debug_new_teid then
      Debug.dummy_stop ();
    if typ_id = !debug_import_teid then
      Debug.dummy_stop ();
    set_teid_maps teid_maps ~global_typ_id:new_typ_id 
      ~local_te_tbl_name:src_tbl.T.te_tbl_name
      ~local_typ_id:typ_id;
    let _ = match e.m_ptr_teid_opt with
      | Some typ -> Some (import ~src_tbl typ)
      | None -> None
    in
    let (m_name, m_kind) = match e.m_kind with
      | Normal _ -> assert false
      | Builtin -> assert false
      | Array _ -> assert false
      | Enum_name 
      | Struct_name 
      | Union_name -> assert false
      | Abs_function _ -> assert false
      | Crt_function _ -> assert false
      | Pointer _ -> assert false
      | Struct fields ->
	  begin
	    let field_list = fields.m_field_infos 
	    in
	    let new_field_list = List.map 
	      (fun f -> 
                {
		  field_sname_opt = f.field_sname_opt;
		  field_serno = f.field_serno;
		  field_teid = import ~src_tbl f.field_teid;
		  field_te_has_def = f.field_te_has_def;
		  field_offset = f.field_offset;
		  field_is_bitfield = f.field_is_bitfield;
		}
	      ) field_list
	    in
	    (e.m_name, Struct { T.m_field_infos = new_field_list; })
	  end
	    
      | Union fields ->
	  begin
	    let field_list = fields.m_field_infos 
	    in
	    let new_field_list = List.map 
	      (fun f -> 
                {
		  field_sname_opt = f.field_sname_opt;
		  field_serno = f.field_serno;
		  field_teid = import ~src_tbl f.field_teid;
		  field_te_has_def = f.field_te_has_def;
		  field_offset = f.field_offset;
		  field_is_bitfield = f.field_is_bitfield;
		}
	      ) field_list
	    in
	    (e.m_name, Union { T.m_field_infos = new_field_list; })
	  end
	    
      | Enum enum_info ->
	  begin
	    let new_enum_item_list = List.map
	      (fun e ->
		{
		  m_enum_item_name = e.m_enum_item_name;
		  m_enum_item_serno = e.m_enum_item_serno;
		  m_enum_item_value = e.m_enum_item_value;
		}
	      ) enum_info.T.m_enum_item_infos
	    in
	    (e.m_name, Enum 
	      { 
		m_enum_max_value = enum_info.T.m_enum_max_value;
		m_enum_item_infos = new_enum_item_list;
	      })
	      
	  end

      | Attribute (teid, atts) -> assert false
      | Bits _ -> assert false
      | Qualified _ -> assert false
      | Typedef _ -> assert false

    in
    let new_e = 
      { 
	m_id = new_typ_id;
	m_name = m_name;
	m_kind = m_kind;
	m_ptr_teid_opt = None;
	m_size = e.m_size;
	m_align = e.m_align;
	m_sizeof_computed = e.m_sizeof_computed;
	m_alignof_computed = e.m_alignof_computed;
	m_runtime = e.m_runtime;
	m_has_alias = true;
	m_attribute_list = [];
      }
    in
    Int.Type_idHashtbl.replace
      dest_tbl.teid_to_te_info new_typ_id new_e;
    new_typ_id
  in
  assert (dest_tbl.max_preload_typ = src_tbl.max_preload_typ);
  (dest_tbl, import ~src_tbl typ_id)

      
let merge_typ_db ~(dest_tbl:te_tbl) ~(src_tbl:te_tbl) : teid_maps =
  if dest_tbl == src_tbl then
    begin
      let dest_len = Int.Type_idHashtbl.length dest_tbl.teid_to_te_info
      and src_len = Int.Type_idHashtbl.length src_tbl.teid_to_te_info
      in
      let teid_maps = 
	{ 
	  local_te_tbl_name = src_tbl.te_tbl_name;
	  gteid_to_lteid = Array.create dest_len [];
	  lteid_to_gteid = Array.create src_len None;
	}
      in
      for i = 0 to dest_len - 1 do
	Array.set teid_maps.lteid_to_gteid i (Some (i));
	Array.set teid_maps.gteid_to_lteid i [(i)]
      done;
      teid_maps
    end
  else
    begin
      assert (dest_tbl.max_preload_typ = src_tbl.max_preload_typ);
      let dest_len = Int.Type_idHashtbl.length dest_tbl.teid_to_te_info
      and src_len = Int.Type_idHashtbl.length src_tbl.teid_to_te_info
      in
      let teid_maps = 
	{ 
	  local_te_tbl_name = src_tbl.te_tbl_name;
	  gteid_to_lteid = Array.create (dest_len + src_len) [];
	  lteid_to_gteid = Array.create src_len None;
	}
      in
      for i = 0 to dest_tbl.max_preload_typ do
	begin
	  Array.set teid_maps.lteid_to_gteid i (Some (i));
	  Array.set teid_maps.gteid_to_lteid i [(i)];
	end
      done;
      for i = dest_tbl.max_preload_typ to (src_len - 1) do
	let e = te_info_of (src_tbl, i)
	in
	(*if not e.m_has_alias then*)
	ignore (import_typ teid_maps ~dest_tbl (src_tbl,i))
      done;
      teid_maps
    end

let teid_maps_sanity_check = assert_teid_maps
    
let typ_db_sanity_check = assert_uniqueness 
      
let find_typ (teid_maps:teid_maps)  ~(dest_tbl:te_tbl) ((src_tbl, teid):te) : te = 
  let src_e = te_info_of (src_tbl, teid)
  in 
  let teid = 
    if TO.is_norm_kind src_e.m_kind then
      match te_kind_of (src_tbl, teid) with
	| Normal v -> v.m_norm_norm_teid
	| _ -> assert false
    else
      teid
  in
  match Array.get teid_maps.lteid_to_gteid teid with
    | Some teid -> (dest_tbl, teid)
    | None -> 
	camlp4_macro_exception "the teid %d of %s is not merged"
	  teid src_tbl.te_tbl_name
