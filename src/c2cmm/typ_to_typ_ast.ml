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
open Typ

module O = Typ_ast
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
      
let convert_typ: typ -> print_def:bool -> c_declarator_opt:string option ->
  O.c_type = 
  fun (typ_db, typ_id) ~print_def ~c_declarator_opt -> 
    let rec pp_print_enum_item: enum_item_info -> O.enum_item =
      fun e -> (e.m_enum_item_name, e.m_enum_item_value)
	
    and pp_print_enum_te_info: typ_id -> O.enum_item list = 
      fun typ_id ->
	match te_kind_of (typ_db, typ_id) with
	  | Enum enum_te_info -> 
	      List.map pp_print_enum_item enum_te_info.m_enum_item_infos

    and pp_print_abs_fun_te_info: typ_id -> 
    declarator_opt:string option -> param_declarator_opts:string list option -> 
    O.c_type =
      fun typ_id ~declarator_opt ~param_declarator_opts ->
	let abs_fun_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Abs_function v -> abs_fun_te_info
	    | _ -> assert false
	and serno = ref 0
	in
	let param_typs = match param_declarator_opts with 
	  | Some param_ids -> 
	      List.map2 (fun typ_id id -> 
		(O.Labeled 
		  (convert_typ typ_id
		    ~print_def:false ~c_declarator_opt:None,id)))
		abs_fun_te_info.aft_param_teids param_ids
	  | None -> 
	      List.map (fun typ_id -> incr serno; 
		(O.Unlabeled 
		  (convert_typ typ_id 
		    ~print_def:false ~c_declarator_opt:None)))
		abs_fun_te_info.aft_param_teids
	in
	let out_typ = 
	  convert_typ 
	    abs_fun_te_info.aft_ret_teid
	    ~print_def:false ~c_declarator_opt:None
	in
	let ret_typ =
	  match declarator_opt with
	    | Some fun_name -> O.Labeled (out_typ, fun_name)
	    | None -> O.Unlabeled out_typ
	in
	let ftyp = 
	  if abs_fun_te_info.aft_va_arg then
	    O.Varadic (param_typs, ret_typ)
	  else if param_typs = [] then
	    O.Fixed ([O.Unlabeled (O.Primitive O.Void)], ret_typ)
	  else
	    O.Fixed (param_typs, ret_typ)
	in
	O.Function ftyp
	  
    and pp_print_crt_fun_te_info: typ_id -> 
    c_declarator_opt:string option -> O.c_type =
      fun typ_id ~c_declarator_opt ->
	let crt_fun_te_info = match te_kind_of (typ_db, typ_id) with
	  | Crt_function v -> v
	  | _ -> assert false
	in
	if not (crt_fun_te_info.m_crt_fun_complete) then
	  assert false
	else
	  begin
	    pp_print_abs_fun_te_info
	      crt_fun_te_info.m_crt_fun_abs_typ_id c_declarator_opt
	      (Some crt_fun_te_info.m_crt_fun_in_params)
	  end

    and pp_print_field_info: field_info -> O.field =
      fun e ->
	match e.field_sname_opt with
	  | Some name -> 
	      O.Named 
		(convert_typ e.field_teid
		  ~print_def:e.field_te_has_def 
		  ~c_declarator_opt:None, name, e.field_offset)
	  | None ->
	      O.Unamed
		(convert_typ e.field_teid
		  ~print_def:e.field_te_has_def 
		  ~c_declarator_opt:None, e.field_offset)
		
    and pp_print_field_info_list: typ_id -> O.field list =
      fun typ_id ->
	let lst = 
	  match te_kind_of (typ_db, typ_id) with
	    | Struct fields 
	    | Union fields -> 
		fields.m_field_infos
	    | _ -> assert false
	in
	List.map pp_print_field_info lst
	  
    and convert_typedef_te_info: 
	typ_id -> c_declarator:string -> O.c_type_decl =
      fun typ_id ~c_declarator ->
	let typedef_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Alias v -> v
	    | _ -> assert false
	in
	O.Typedef 
	  (convert_typ 
	    typedef_te_info.m_typedef_lhs_teid
	    ~print_def:false ~c_declarator_opt:None, c_declarator)
	  
    and pp_print_array_te_info: 
	print_def:bool -> typ_id -> c_declarator_opt:string option -> O.c_type = 
      fun ~print_def typ_id ~c_declarator_opt ->
	let array_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Array v -> v
	    | _ -> assert false
	in
	let array_cardi = 
	  Printf.sprintf "[%s]" 
	    (string_of_csize array_te_info.m_array_cardi)
	in
	match c_declarator_opt with
	  | Some fun_name ->
	      begin
		O.Array 
		  (convert_typ array_te_info.m_elmt_teid
		    ~print_def 
		    ~c_declarator_opt:
		    (Some (fun_name ^ " " ^ array_cardi)), 
		  array_te_info.m_array_cardi)
	      end
	  | None -> 
	      O.Array
		(convert_typ array_te_info.m_elmt_teid
		  ~print_def 
		  ~c_declarator_opt:(Some (array_cardi)),
	array_te_info.m_array_cardi)

    and pp_print_xarray_te_info: 
	print_def:bool -> typ_id -> c_declarator_opt:string option -> O.c_type = 
      fun ~print_def typ_id ~c_declarator_opt ->
	let xarray_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Xarray v -> v
	    | _ -> assert false
	in
	let array_cardi = "[]"
	in
	match c_declarator_opt with
	  | Some fun_name ->
	      O.Xarray
		(convert_typ xarray_te_info.m_xelmt_teid
		  ~print_def 
		  ~c_declarator_opt:
		  (Some (fun_name ^ " " ^ array_cardi)))
	  | None -> 
	      O.Xarray
		(convert_typ xarray_te_info.m_xelmt_teid
		  ~print_def 
		  ~c_declarator_opt:(Some (array_cardi)))

    and pp_print_darray_te_info: 
	print_def:bool -> typ_id -> c_declarator_opt:string option -> O.c_type = 
      fun ~print_def typ_id ~c_declarator_opt ->
	let darray_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Darray v -> v
	    | _ -> assert false
	in
	let array_cardi = "[]"
	in
	match c_declarator_opt with
	  | Some fun_name ->
	      O.Darray
		(convert_typ darray_te_info.m_delmt_teid
		  ~print_def 
		  ~c_declarator_opt:
		  (Some (fun_name ^ " " ^ array_cardi)))
	  | None -> 
	      O.Darray
		(convert_typ darray_te_info.m_delmt_teid
		  ~print_def 
		  ~c_declarator_opt:(Some (array_cardi)))
		
    and pp_print_pointer_te_info: 
	print_def:bool -> typ_id -> c_declarator_opt:string option -> O.c_type =
      fun ~print_def typ_id ~c_declarator_opt ->
	let pointer_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Pointer v -> v
	    | _ -> assert false
	in
	O.Pointer 
	  (convert_typ pointer_te_info.pt_elmt_teid
	    ~print_def ~c_declarator_opt:None)
	  
    and pp_bit_te_info: typ_id -> c_declarator_opt:string option
    -> O.c_type =
      fun typ_id ~c_declarator_opt ->
	let bit_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Bits v -> v
	    | _ -> assert false
	in
	let t = match convert_typ bit_te_info.m_bit_base_teid
	  ~print_def:false ~c_declarator_opt with
	    | O.Primitive t -> O.Primitive t
	    | O.Enum t -> O.Enum t
	    | O.Typename t -> O.Typename t
	    | t -> assert false
	in
	let str = bit_te_info.m_bit_mask
	in
	O.Bits (t, bit_te_info.m_bit_size, 
	bit_te_info.m_bit_lsb, str)

    and pp_print_qual_te_info: 
	print_def:bool -> typ_id -> c_declarator_opt:string option -> O.c_type =
      fun ~print_def typ_id ~c_declarator_opt ->
	let qual_te_info = 
	  match te_kind_of (typ_db, typ_id) with
	    | Qualified v -> v
	    | _ -> assert false
	in
	let qual = match qual_te_info.m_qualifier with
	  | "const" -> O.Const
	  | "restrict" -> O.Restrict
	  | "volatile" -> O.Volatile
	  | _ -> assert false
	in
	O.Qual
	  (convert_typ qual_te_info.m_qualified_teid
	    ~print_def ~c_declarator_opt, qual)
	  
    and convert_typ: typ_id -> print_def:bool -> c_declarator_opt:string option -> O.c_type = 
      fun typ_id ~print_def ~c_declarator_opt ->
	let v = Int.Type_idHashtbl.find typ_db.teid_to_te_info typ_id
	in
	match v.m_kind with
	  | Enum 
	  | Union
	  | Struct ->
	      begin
		if print_def then
		  if v.m_complete then
		    match v.m_kind with
		      | Enum _ -> 
			  O.Type_decl
			    (O.Enum_decl (v.m_name, 
			    pp_print_enum_te_info (typ_id)))
		      | Union _ ->
			  let size = match v.m_size with
			    | Byte_size s -> s
			    | _ -> -1L
			  in
			  O.Type_decl 
			    (O.Union_decl (v.m_name, size, v.m_align,
			    pp_print_field_info_list (typ_id)))
		      | Struct _ ->
			  let size = match v.m_size with
			    | Byte_size s -> s
			    | _ -> -1L
			  in
			  O.Type_decl
			    (O.Struct_decl (v.m_name, size, v.m_align,
			    pp_print_field_info_list (typ_id)))
		      | _ -> assert false
		  else
		    match v.m_kind with
		      | Enum _ -> 
			  O.Enum v.m_name
		      | Union _ ->
			  O.Union v.m_name
		      | Struct _ -> 
			  O.Struct v.m_name
		      | _ -> assert false
		else
		  match v.m_kind with
		    | Enum _ ->
			O.Enum v.m_name
		    | Union _ ->
			O.Union v.m_name
		    | Struct _ -> 
			O.Struct v.m_name
		    | _ -> assert false
	      end
		
	  | Crt_function _ ->
	      pp_print_crt_fun_te_info (typ_id) ~c_declarator_opt

	  | Abs_function _ ->
	      pp_print_abs_fun_te_info (typ_id) 
		~declarator_opt:c_declarator_opt ~param_declarator_opts:None
		
	  | Array _ ->
	      pp_print_array_te_info ~print_def (typ_id) ~c_declarator_opt 

	  | Darray _ ->
	      pp_print_darray_te_info ~print_def (typ_id) ~c_declarator_opt
		
	  | Xarray _ ->
	      pp_print_xarray_te_info ~print_def (typ_id) ~c_declarator_opt
		
	  | Pointer _ -> 
	      pp_print_pointer_te_info ~print_def typ_id ~c_declarator_opt
		
	  | Builtin -> 
	      let t = match v.m_name with
		| "void" -> O.Void
		| "char" -> O.Char
		| "signed char" -> O.Signed_Char
		| "unsigned char" -> O.Unsigned_Char
		| "short int" -> O.Short
		| "signed short int" -> O.Short
		| "unsigned short int" -> O.Unsigned_Short
		| "signed int" -> O.Int
		| "int" -> O.Int
		| "unsigned int" -> O.Unsigned_Int
		| "signed long int" -> O.Long
		| "long int" -> O.Long
		| "unsigned long int" -> O.Unsigned_Long
		| "signed long long int" -> O.Long_Long
		| "long long int" -> O.Long_Long
		| "unsigned long long int" -> O.Unsigned_Long_Long
		| "float" -> O.Float
		| "double" -> O.Double
		| "long double" -> O.Long_Double
		| "_Bool" -> O.Bool
		| "_Complex" -> O.Complex
		| "float _Complex" -> O.Float_Complex
		| "double _Complex" -> O.Double_Complex
		| "long double _Complex" -> O.Long_Double_Complex
		| "void *" -> O.Void_Ptr
		| "wchar_t" -> O.Int
		| "size_t" -> O.Int
		| "__builtin_va_list" -> O.Va_List
		| "?" -> O.Unknown
		| _ -> print_string (":" ^ v.m_name);
		    print_newline ();
		    assert false
	      in
	      O.Primitive t
		
	  | Alias _ ->
	      if print_def then
		O.Type_decl (convert_typedef_te_info
		  (typ_id) ~c_declarator:(v.m_name))
	      else
		O.Typename v.m_name

	  | Normal _ -> 
	      assert false
		  
	  | Bits _ -> 
	      pp_bit_te_info typ_id ~c_declarator_opt
		
	  | Qualified _ ->
	      pp_print_qual_te_info ~print_def typ_id
		~c_declarator_opt
    in
    convert_typ typ_id ~print_def ~c_declarator_opt	 



let convert_typ_db typ_db:O.c_type array = 
   let len = Int.Type_idHashtbl.length typ_db.teid_to_te_info
   in
   let type_array = Array.make len (O.Primitive O.Int)
   in
   Int.Type_idHashtbl.iter
     (fun k b ->
       Array.set type_array k 
	 (convert_typ (typ_db, k) ~print_def:true ~c_declarator_opt:None))
     typ_db.teid_to_te_info;
   (*
     let new_typ_db = Typ_op.create typ_db.type_count "abc"
     in
     Array.iter (fun typ -> 
     ignore(Typ_ast_op.eval_type_or_expr new_typ_db typ))
     type_array; 
   *)
   type_array
       
