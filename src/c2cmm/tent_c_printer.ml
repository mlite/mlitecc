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
open Tent

module QNP = Qual_name_printer

let indent = 2

let enable_typ_id_debug = false
let enable_typedef_debug = true
let disable_type_qualifier_print = ref false

let pp_print_c_comma: formatter -> unit =
  fun fm ->
    pp_print_string fm ",";
    pp_print_space fm ()
      
let pp_print_c_semicolon: formatter -> unit =
  fun fm ->
    pp_print_string fm ";";
    pp_print_space fm ()
      
let pp_print_typ_id (fm:formatter) (teid:teid) :unit = 
  if !Mlite_config.enable_type_table_print then
    begin
      pp_open_box fm 0;
      fprintf fm "/* T_%d */" teid;
      pp_close_box fm ()
    end
	
let rec pp_print_t (fm:formatter) ((typ_db, typ_id):te) 
    ~(print_def:bool)  ~(c_declarator_opt:string option) : unit = 
  let rec pp_print_string_opt fm id =
    match id with
      | Some s -> 
	  pp_print_space fm ();
	  pp_print_string fm s;
      | None -> ()
	  
  and pp_print_enum_item: formatter -> enum_item_info -> unit =
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
	
  and pp_print_enum_te_info (fm:formatter) (lst:enum_item_info list):unit =
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


  and pp_print_abs_fun_te_info: formatter -> abs_fun_te_info -> 
  declarator_opt:string option -> param_declarator_opts:string list option -> unit =
    fun fm abs_fun_te_info ~declarator_opt ~param_declarator_opts ->
      let serno = ref 0
      in
      let param_typ_ids = match param_declarator_opts with 
	| Some param_ids -> 
	    (List.map2 (fun typ id -> (typ, Some id) )
	      abs_fun_te_info.aft_param_teids param_ids)
	| None -> 
	    (List.map (fun typ -> incr serno; (typ, Some ""))
	      abs_fun_te_info.aft_param_teids
	    )
      in
      pp_open_box fm 0;
      begin
	let fun_param_interface fm = 
	  begin
	    pp_open_hvbox fm indent;
	    begin
	      let _ = match declarator_opt with
		| Some fun_name -> pp_print_string fm fun_name
		| None -> pp_print_string fm "($abs$)"
	      in
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      Mlite_printer.pp_print_list fm
		(fun fm (typ, c_declarator_opt) ->
		  pp_print_t fm (typ_db, typ) ~print_def:false ~c_declarator_opt
		)
		pp_print_c_comma
		param_typ_ids;
	      if abs_fun_te_info.aft_va_arg then
		pp_print_string fm ",...";
	      pp_print_string fm ")";
	    end;
	    pp_close_box fm ();
	  end
	in
	let fun_param_interface_txt =
	  camlp4_macro_str_pp_print fun_param_interface
	in
	pp_print_t 
	  fm (typ_db, abs_fun_te_info.aft_ret_teid) 
	  ~print_def:false 
	  ~c_declarator_opt:(Some (fun_param_interface_txt));
      end;
      pp_close_box fm ()

  and pp_print_crt_fun_te_info (fm:formatter) (crt_fun_te_info:crt_fun_te_info)
      ~(c_declarator_opt:string option) : unit = 
    let te_info = 
      Int.Type_idHashtbl.find typ_db.teid_to_te_info 
	crt_fun_te_info.m_crt_fun_abs_teid
    in
    let abs_fun_te_info = 
      match te_info.m_kind with
	| Abs_function v -> v
	| _ -> assert false
    in
    pp_print_abs_fun_te_info fm abs_fun_te_info c_declarator_opt
      (Some 
	(List.map 
	  (fun v -> 
	    match v with
	      | THIS_PARAM str
	      | HIDDEN_RETURN str
	      | NORMAL_PARAM str ->
		  QNP.to_decl_str str

	      | SCALAR_PARAM qmuton
	      | STRUCT_PARAM qmuton -> 
		  QNP.to_decl_str qmuton.muton
	  )crt_fun_te_info.m_crt_fun_in_params))

  and pp_print_field_info: formatter -> field_info -> unit =
    fun fm e ->
      pp_open_box fm 0;
      begin
	pp_print_t fm (typ_db, e.field_teid)
	  ~print_def:e.field_te_has_def ~c_declarator_opt:e.field_sname_opt
      end;
      pp_close_box fm ()

  and pp_print_field_info_list (fm:formatter) (lst:field_info list) :unit =
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

  and pp_print_typedef_te_info: formatter -> typedef_te_info
  -> c_declarator_opt:string option -> unit = 
    fun fm typedef_te_info ~c_declarator_opt ->
      pp_open_box fm 0;
      begin
	pp_print_t fm 
	  (typ_db, typedef_te_info.m_typedef_lhs_teid)
	  ~print_def:false ~c_declarator_opt
      end;
      pp_close_box fm ()

  and pp_print_norm_table_entry: formatter -> norm_table_entry 
  -> c_declarator_opt:string option -> unit = 
    fun fm norm_table_entry ~c_declarator_opt ->
      pp_open_box fm 0;
      begin
	pp_print_t fm 
	  (typ_db, norm_table_entry.m_norm_norm_teid)
	  ~print_def:false ~c_declarator_opt;
	pp_print_string fm "/* orig:";
	pp_print_t fm (typ_db, norm_table_entry.m_norm_orig_teid)
	  ~print_def:false ~c_declarator_opt;
	pp_print_string fm "*/"
      end;
      pp_close_box fm ()

  and pp_print_attributed_te_info (fm:formatter)
      ~(print_def:bool) (teid, attributes) ~(c_declarator_opt:string option): unit = 
    pp_open_box fm 0;
    pp_print_t fm (typ_db, teid) ~print_def ~c_declarator_opt;
    Tent_printer.pp_print_te_attribute_list fm attributes;
    pp_close_box fm ()

  and pp_print_array_te_info (fm:formatter)
      ~(print_def:bool) array_te_info ~(c_declarator_opt:string option): unit = 
    let array_cardi = 
      match array_te_info.m_array_cardi with
	| ARRAY_FIXED csize -> ("[" ^ (string_of_csize csize) ^ "]")
	| ARRAY_VARIABLE -> "[]"
    in
    pp_open_box fm 0;
    begin
      match c_declarator_opt with
	| Some fun_name ->
	    pp_print_t fm (typ_db, array_te_info.m_elmt_teid)  
	      ~print_def 
	      ~c_declarator_opt:(Some (fun_name ^ " " ^ array_cardi))
	      
	| None -> 
	    pp_print_t fm (typ_db, array_te_info.m_elmt_teid)
	      ~print_def 
	      ~c_declarator_opt:(Some (array_cardi))
    end;
    pp_close_box fm ()

  and pp_print_pointer_te_info: formatter -> 
  print_def:bool -> teid -> c_declarator_opt:string option -> unit = 
    fun fm ~print_def pointer_te_info ~c_declarator_opt ->
      let c_declarator_opt = 
	let (l, r) = 
	  let te_info = 
	    Int.Type_idHashtbl.find typ_db.teid_to_te_info pointer_te_info
	  in
	  match te_info.m_kind with
	    | Array _ 
	    | Crt_function _
	    | Abs_function _  -> ("(", ")")
	    | Qualified _ -> ("(", ")")
	    | _ -> ("", "")
	in
	match c_declarator_opt with
	  | Some s -> Some (l ^ "*" ^ s ^ r)
	  | None -> Some (l ^ "*" ^ r)
      in
      pp_open_box fm 0;
      begin
	match c_declarator_opt with
	  | Some fun_name -> 
	      pp_print_t fm (typ_db, pointer_te_info)
		~print_def ~c_declarator_opt
	  | None -> 
	      pp_print_t fm (typ_db, pointer_te_info)
		~print_def:false 
		~c_declarator_opt
      end;
      pp_close_box fm ()

  and pp_bit_te_info: formatter -> bit_te_info -> c_declarator_opt:string option -> unit = 
    fun fm bit_te_info ~c_declarator_opt ->
      pp_open_box fm 0;
      begin
	let txt = 
	  camlp4_macro_str_pp_print 
	    (fun fm ->
	      match c_declarator_opt with
		| Some c_declarator_opt ->
		    fprintf fm "%s:%s" c_declarator_opt 
		      (string_of_csize bit_te_info.m_bit_size)
		| None ->
		    fprintf fm ":%s" 
		      (string_of_csize bit_te_info.m_bit_size)
	    )
	in
	pp_print_t fm (typ_db, bit_te_info.m_bit_base_teid)
	  ~print_def:false 
	  ~c_declarator_opt:(Some txt)
      end;
      pp_close_box fm ()

  and pp_print_qual_te_info: formatter -> print_def:bool -> 
  qual_te_info -> c_declarator_opt:string option -> unit = 
    fun fm ~print_def qual_te_info ~c_declarator_opt ->
      pp_open_box fm 0;
      begin
	if not !disable_type_qualifier_print then
	  begin
	    let str = match qual_te_info.m_qualifier with
	      | QUAL_CONST -> "const"
	      | QUAL_VOLATILE -> "volatile"
	      | QUAL_REGISTER -> "register"
	      | QUAL_RESTRICT -> "restrict"
	    in
	    pp_print_string fm str;
	    pp_print_space fm ();
	  end;
	pp_print_t fm (typ_db, qual_te_info.m_qualified_teid)
	  ~print_def 
	  ~c_declarator_opt
      end;
      pp_close_box fm ()
  in
  pp_open_box fm 0;
  begin
    let v = Int.Type_idHashtbl.find typ_db.teid_to_te_info typ_id
    in
    let _ = match v.m_kind with
      | Enum enum_te_info ->
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    if print_def then
	      begin
		pp_print_space fm ();
		pp_print_space fm ();
		pp_print_enum_te_info fm
		  enum_te_info.m_enum_item_infos
	      end;
	    pp_print_string_opt fm c_declarator_opt
	  end
	    
      | Enum_name -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    match c_declarator_opt with
	      | Some declarator -> pp_print_string fm (" " ^ declarator)
	      | None -> ()
	  end
	    
      | Union field_info_list ->
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    if print_def then
	      begin
		pp_print_space fm ();
		pp_print_space fm ();
		pp_print_field_info_list fm field_info_list.m_field_infos
	      end;
	    pp_print_string_opt fm c_declarator_opt
	  end

      | Union_name -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    match c_declarator_opt with
	      | Some declarator -> pp_print_string fm (" " ^ declarator)
	      |  None -> ()
	  end
	    
      | Struct field_info_list -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    if print_def then
	      begin
		pp_print_space fm ();
		pp_print_space fm ();
		pp_print_field_info_list fm field_info_list.m_field_infos
	      end;
	    pp_print_string_opt fm c_declarator_opt
	  end

      | Struct_name -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    match c_declarator_opt with
	      | Some declarator -> pp_print_string fm (" " ^ declarator)
	      |  None -> ()
	  end
	    
      | Crt_function crt_fun_te_info ->
	  begin
	    pp_print_crt_fun_te_info fm crt_fun_te_info ~c_declarator_opt
	  end
      | Abs_function abs_fun_te_info ->
	  begin
	    pp_print_abs_fun_te_info fm  abs_fun_te_info
	      ~declarator_opt:c_declarator_opt ~param_declarator_opts:None
	  end
      | Array array_te_info ->
	  pp_print_array_te_info fm  ~print_def array_te_info ~c_declarator_opt 

      | Pointer pointer_te_info -> 
	  pp_print_pointer_te_info fm ~print_def pointer_te_info ~c_declarator_opt

      | Builtin -> 
	  begin
	    QNP.pp_print_qn_decl_str fm v.m_name;
	    pp_print_string_opt fm c_declarator_opt;
	  end
      | Typedef typedef_te_info ->
	  begin
	    if print_def then
	      begin
		let _ = match c_declarator_opt with
		  | Some x -> pp_print_string fm x
		  | None -> ()
		in
		pp_print_string fm "typedef";
		pp_print_space fm ();
		pp_print_typedef_te_info fm  
		  typedef_te_info  ~c_declarator_opt:(Some (QNP.to_decl_str v.m_name))
	      end
	    else
	      begin
		QNP.pp_print_qn_decl_str fm v.m_name;
		pp_print_string_opt fm c_declarator_opt
	      end
	  end
      | Bits bit_te_info -> 
	  begin
	    pp_bit_te_info fm bit_te_info ~c_declarator_opt
	  end
      | Qualified qual_te_info ->
	  begin
	    pp_print_qual_te_info fm 
	      ~print_def qual_te_info ~c_declarator_opt
	  end
      | Normal norm_table_entry -> 
	  pp_print_norm_table_entry fm norm_table_entry ~c_declarator_opt

      | Attribute (teid, atts) ->
	  pp_print_attributed_te_info fm 
	    ~print_def (teid, atts) ~c_declarator_opt
    in Tent_printer.pp_print_te_attribute_list fm v.m_attribute_list
  end;
  pp_close_box fm ()


let pp_print_c_type_only (fm:formatter) (te:te): unit = 
  pp_print_t fm te ~print_def:true ~c_declarator_opt:None
      

let print_c_type_only (te:te) :unit = (** can be invoked by ocaml debugger **)
  let str = camlp4_macro_str_pp_print
    (fun fm ->
      pp_open_box fm 0;
      pp_print_c_type_only fm te;
      pp_close_box fm ())
  in print_string str



let pp_print_c_type_name: formatter -> te -> unit = 
  fun fm typ ->
    pp_print_typ_id fm (snd typ);
    pp_open_box fm 0;
    begin
      pp_print_t fm typ ~print_def:false ~c_declarator_opt:None
    end;
    pp_close_box fm ()
      
let print_c_type_name: te -> unit =
  fun typ ->
    let str = camlp4_macro_str_pp_print
        (fun fm ->
          pp_open_box fm 0;
          pp_print_c_type_name fm typ;
          pp_close_box fm ())
    in
    print_string str


let pp_print_id_decl: formatter -> te -> string -> unit = 
  fun fm typ str ->
    pp_print_typ_id fm (snd typ);
    pp_open_box fm 0;
    begin
      pp_print_t fm typ
	~print_def:false ~c_declarator_opt:(Some str)
    end;
    pp_close_box fm ()


let pp_print_type_def: formatter -> te -> unit = 
  fun fm typ ->
    pp_print_typ_id fm (snd typ);
    pp_open_box fm 0;
    begin
      pp_print_t fm typ
	~print_def:true ~c_declarator_opt:None
    end;
    pp_close_box fm ()
      

let pp_print_c_type_decl: formatter -> te -> unit = 
  fun fm typ ->
    pp_print_typ_id fm (snd typ);
    pp_open_box fm 0;
    begin
      pp_print_t fm typ
	~print_def:true ~c_declarator_opt:None
    end;
    pp_close_box fm ()
      
      
let enable_type_qualifier_print: unit -> unit =
  fun () -> 
    disable_type_qualifier_print := false

let disable_type_qualifier_print: unit -> unit =
  fun () -> 
    disable_type_qualifier_print := true



