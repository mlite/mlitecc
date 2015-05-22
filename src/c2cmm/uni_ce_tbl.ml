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
open Cent_op
open Qual_name
module QN = Qual_name
module QNO = Qual_name_op
module T = Tent
module TO = Tent_op

type typ_id = ceid

type ceid_maps =
    {
      local_ce_tbl_name: string;
      gceid_to_lceid: ceid list array;
      lceid_to_gceid: ceid option array;
    }

let debug_global_ceid = ref 0
let debug_import_ceid = ref 0
let debug_new_ceid = ref 0


exception Not_builtin

let builtin_typ_equiv: ce -> ce -> bool =
  fun (typ_db0, ceid0) (typ_db1, ceid1) ->
    assert (typ_db0.max_preload_ceid = typ_db1.max_preload_ceid);
    if ceid0 < typ_db0.max_preload_ceid then
      ceid0 = ceid1
    else
      raise Not_builtin


let set_ceid_maps 
    (ceid_maps:ceid_maps) 
    ~(global_typ_id:typ_id)
    ~(local_ce_tbl_name:string)
    ~(local_typ_id:typ_id) : unit =
  if global_typ_id = !debug_global_ceid & local_typ_id = !debug_import_ceid then
    Debug.dummy_stop ();
  Array.set ceid_maps.lceid_to_gceid local_typ_id (Some (global_typ_id));
  let local_typ_ids = 
    Array.get ceid_maps.gceid_to_lceid global_typ_id
  in
  Array.set 
    ceid_maps.gceid_to_lceid 
    global_typ_id (local_typ_id::local_typ_ids)

open Format
let pp_print_ceid_maps: formatter -> ceid_maps -> unit =
  fun fm ceid_maps ->
    pp_open_vbox fm 0;
    pp_print_string fm "global -> locals";
    pp_print_space fm ();
    for i = 0 to Array.length ceid_maps.gceid_to_lceid - 1 do
      let local_typ_ids = Array.get ceid_maps.gceid_to_lceid i
      in
      pp_open_vbox fm 2;
      pp_print_space fm ();
      Mlite_printer.pp_print_list fm
	(fun fm (typ_id) -> 
	  fprintf fm "%d -> %d" i typ_id)
	(fun fm -> pp_print_space fm ())
	local_typ_ids;
      pp_close_box fm ();
      pp_print_space fm ();
      pp_print_flush fm ();
    done;
    pp_print_space fm ();
    pp_print_string fm "local -> global";
    pp_print_space fm ();
    for i = 0 to Array.length ceid_maps.lceid_to_gceid - 1 do
      let typ_opt = Array.get ceid_maps.lceid_to_gceid i
      in
      match typ_opt with
	| Some (typ_id) -> 
	    fprintf fm "%d -> %d" i typ_id;
	    pp_print_space fm ();
	    pp_print_flush fm ();
	| None -> ()
    done;
    pp_close_box fm ()

let assert_ceid_maps (ceid_maps:ceid_maps) : unit = 
  let len = (Array.length ceid_maps.lceid_to_gceid - 1)
  in
  for i = 0 to len do
    match Array.get ceid_maps.lceid_to_gceid i with
      | Some global_typ_id -> 
	  begin
	    let local_typ_ids = 
	      Array.get ceid_maps.gceid_to_lceid global_typ_id
	    in assert (List.exists (fun v -> v = i) local_typ_ids)
	  end
      | None -> ()
  done
      

exception Found_equiv_ce of ceid 
  
let ce_equiv (map_te:(te->te)) (ceid_maps:ceid_maps) (typ0:ce) (typ1:ce) : bool =
  let rec rec_ce_equiv (ceid_maps:ceid_maps) (typ0:ce) (typ1:ce) : bool =
    let (t0, v0) = typ0
    and (t1, v1) = typ1
    in
    let ce0 = ce_info_of (t0, v0)
    and ce1 = ce_info_of (t1, v1)
    in
    let compare () = 
      ce0.ce_qname = ce1.ce_qname & 
      (Tent_op.te_eq (map_te ce0.ce_te) (map_te ce1.ce_te))
    in
    match Array.get ceid_maps.lceid_to_gceid v1 with
      | Some (v1) -> v1 = v0
      | None -> 
	  let local_typ_ids = 
	    Array.get ceid_maps.gceid_to_lceid v0 
	  in
	  if local_typ_ids <> [] then
	    List.exists (fun v -> v = v1) local_typ_ids
	  else
	    compare ()
  in rec_ce_equiv ceid_maps typ0 typ1
       
	    
let import_ce (map_te:(te -> te)) (ceid_maps:ceid_maps) ~(dest_tbl:ce_tbl) ((src_tbl, typ_id):ce):ce =
  let rec import ~(src_tbl:ce_tbl) typ_id :ceid = 
    if typ_id > dest_tbl.max_preload_ceid then
      begin
	match Array.get ceid_maps.lceid_to_gceid typ_id with
	  | Some typ_id -> typ_id
	  | None ->
	      begin
		let e = ce_info_of (src_tbl, typ_id)
		in
		let _ = 
		  if !debug_global_ceid <> 0 then
		    let ge = ce_info_of (dest_tbl, !debug_global_ceid)
		    in
		    let result = QN.eq e.ce_qname ge.ce_qname
		    in ()
		in
		try
		  let dest_ce_info = QualNameHashtbl.find
		    dest_tbl.qname_to_ce_info
		    (QNO.map_param (fun i -> snd (map_te (fst e.ce_te, i)))
		      e.ce_qname)
		  in
		  let _ = 
		    if (not dest_ce_info.ce_is_real_ent) & e.ce_is_real_ent then
		      begin
			dest_ce_info.ce_te <- map_te e.ce_te;
			dest_ce_info.ce_is_real_ent <- true;
		      end
		    else if dest_ce_info.ce_is_real_ent & not e.ce_is_real_ent
		    then
		      ((* silence *))
		    else if not (Tent_op.typ_eq (map_te dest_ce_info.ce_te)
		      (map_te e.ce_te)) then
		      let old_te = camlp4_macro_str_pp_print 
			(fun fm -> Tent_c_printer.pp_print_c_type_only fm 
			  dest_ce_info.ce_te)
		      and new_te = camlp4_macro_str_pp_print 
			(fun fm -> Tent_c_printer.pp_print_c_type_only fm 
			  e.ce_te)
		      in
		      camlp4_macro_warning "ce '%s' has old_te '%s' and is added as new_te '%s'\n"
			(Qual_name_printer.to_decl_str e.ce_qname) old_te
			new_te    
		  in
		  set_ceid_maps ceid_maps ~global_typ_id:dest_ce_info.ce_id
		    ~local_ce_tbl_name:src_tbl.ce_tbl_name
		    ~local_typ_id:typ_id;
		  dest_ce_info.ce_id
		with
		  | Not_found ->
		      add_ce ~src_tbl typ_id
	      end
      end
    else
      typ_id
	
  and add_ce ~(src_tbl:ce_tbl) typ_id :typ_id = 
    let new_typ_id = (alloc_ceid dest_tbl)
    and e = ce_info_of (src_tbl, typ_id)
    in
    if new_typ_id = !debug_new_ceid then
      Debug.dummy_stop ();
    if typ_id = !debug_import_ceid then
      Debug.dummy_stop ();
    set_ceid_maps ceid_maps ~global_typ_id:new_typ_id 
      ~local_ce_tbl_name:src_tbl.ce_tbl_name
      ~local_typ_id:typ_id;
    let new_ce_info = 
      {
	ce_id = new_typ_id;
	ce_qname = QNO.map_param 
	  (fun i -> snd (map_te (fst e.ce_te, i))) e.ce_qname;
	ce_te = map_te e.ce_te;
	ce_used = e.ce_used;
	ce_is_real_ent = e.ce_is_real_ent;
	ce_addr_is_taken = e.ce_addr_is_taken;
	ce_is_register = e.ce_is_register;
      }
    in
    add_ce_info dest_tbl new_ce_info;
    new_typ_id
  in
  assert (dest_tbl.max_preload_ceid = src_tbl.max_preload_ceid);
  (dest_tbl, import ~src_tbl typ_id)

      
let merge_ce_tbl ~(map_te:(te -> te)) ~(dest_tbl:ce_tbl) ~(src_tbl:ce_tbl) : ceid_maps =
  if dest_tbl == src_tbl then
    begin
      let dest_len = dest_tbl.ce_count
      and src_len = src_tbl.ce_count
      in
      let ceid_maps = 
	{ 
	  local_ce_tbl_name = src_tbl.ce_tbl_name;
	  gceid_to_lceid = Array.create dest_len [];
	  lceid_to_gceid = Array.create src_len None;
	}
      in
      for i = 0 to dest_len - 1 do
	Array.set ceid_maps.lceid_to_gceid i (Some (i));
	Array.set ceid_maps.gceid_to_lceid i [(i)]
      done;
      ceid_maps
    end
  else
    begin
      assert (dest_tbl.max_preload_ceid = src_tbl.max_preload_ceid);
      let dest_len = dest_tbl.ce_count
      and src_len = src_tbl.ce_count
      in
      let ceid_maps = 
	{ 
	  local_ce_tbl_name = src_tbl.ce_tbl_name;
	  gceid_to_lceid = Array.create (dest_len + src_len) [];
	  lceid_to_gceid = Array.create src_len None;
	}
      in
      for i = 0 to dest_tbl.max_preload_ceid do
	begin
	  Array.set ceid_maps.lceid_to_gceid i (Some (i));
	  Array.set ceid_maps.gceid_to_lceid i [(i)];
	end
      done;
      for i = dest_tbl.max_preload_ceid to (src_len - 1) do
	try
	  let e = ce_info_of (src_tbl, i)
	  in 
	  ignore (import_ce map_te ceid_maps ~dest_tbl (src_tbl,i))
	with
	  | Not_found -> ()
      done;
      ceid_maps
    end


let ceid_maps_sanity_check = assert_ceid_maps

(*    
let typ_db_sanity_check = assert_uniqueness 
*)
      
let find_typ (ceid_maps:ceid_maps)  ~(dest_tbl:ce_tbl) ((src_tbl, ceid):ce) : ce = 
  let src_e = ce_info_of (src_tbl, ceid)
  in 
  match Array.get ceid_maps.lceid_to_gceid ceid with
    | Some ceid -> (dest_tbl, ceid)
    | None -> assert false
