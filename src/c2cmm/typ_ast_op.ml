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
open Int
open Collection
include Mach

module Type_ast = Typ_ast
module Type_ast_printer = Typ_ast_printer
module QN = Qual_name
module QNP = Qual_name_printer

exception Error of string

type typ = Typ.typ

type type_or_expr_ast = Type_ast.c_type

and type_or_expr_val = typ

    
let mk_clang_type_name str = 
  {
    QN.qn_namespace = QN.QN_CLANG;
    QN.qn_span = QN.QN_AUTO;
    QN.qn_class = QN.QN_TYPE_NAME;
    QN.qn_scopes = [];
    QN.qn_init = QN.QN_NULL;
    QN.qn_sname = str;
  }

let rec eval_type_or_expr: Typ.typ_db -> type_or_expr_ast -> type_or_expr_val =
  fun typ_db type_or_expr_ast ->
    let rec eval_type_val: Type_ast.c_type -> typ * bool =
      fun type_cabs ->
	let (typ_id, has_def) = match type_cabs with
	  | Type_ast.Primitive type_ast' -> 
	      begin
		let typ = match type_ast' with
		  | Type_ast.Unknown ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id 
			  typ_db ~type_name:(mk_clang_type_name "?")
		      in
		      typ

		  | Type_ast.Va_List -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id 
			  typ_db ~type_name:(mk_clang_type_name "__builtin_va_list")
		      in
		      typ
			
		  | Type_ast.Void -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db ~type_name:
			  (mk_clang_type_name "void")
		      in
		      typ

		  | Type_ast.Void_Ptr -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db ~type_name:
			  (mk_clang_type_name "void *")
		      in
		      typ
			
		  | Type_ast.Char -> 
		      begin
			let (typ, _, _) = 
			  Typ_op.rd_typ_id typ_db ~type_name:
			    (mk_clang_type_name "char")
			in
			typ
		      end
			
		  | Type_ast.Signed_Char -> 
		      begin
			let (typ, _, _) = 
			  Typ_op.rd_typ_id typ_db ~type_name:
			    (mk_clang_type_name "signed char")
			in
			typ
		      end
			
		  | Type_ast.Unsigned_Char ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "unsigned char")
		      in
		      typ

		  | Type_ast.Short -> 
		      begin
			let (typ, _, _) = 
			  Typ_op.rd_typ_id typ_db 
			    ~type_name:(mk_clang_type_name "short int")
			in
			typ
		      end
			
		  | Type_ast.Unsigned_Short -> 
		      begin
			let (typ, _, _) = 
			  Typ_op.rd_typ_id typ_db 
			    ~type_name:(mk_clang_type_name "unsigned short int")
			in
			typ
		      end
			
		  | Type_ast.Int -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "int")
		      in
		      typ

		  | Type_ast.Unsigned_Int -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "unsigned int")
		      in typ

		  | Type_ast.Long -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "long int")
		      in
		      typ

		  | Type_ast.Unsigned_Long -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "unsigned long int")
		      in
		      typ

		  | Type_ast.Long_Long -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "long long int")
		      in
		      typ

		  | Type_ast.Unsigned_Long_Long -> 
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  ~type_name:(mk_clang_type_name "unsigned long long int")
		      in
		      typ
			
		  | Type_ast.Float ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "float")
		      in
		      typ

		  | Type_ast.Double ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "double")
		      in
		      typ

		  | Type_ast.Long_Double ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "long double")
		      in
		      typ
			
		  | Type_ast.Float_Complex ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "float _Complex")
		      in
		      typ

		  | Type_ast.Double_Complex ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "double _Complex")
		      in
		      typ

		  | Type_ast.Long_Double_Complex ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "long double _Complex")
		      in
		      typ

		  | Type_ast.Bool ->
		      let (typ, _, _) = 
			Typ_op.rd_typ_id typ_db 
			  (mk_clang_type_name "_Bool")
		      in
		      typ

		  | Type_ast.Complex ->
		      let (typ, _, _) = Typ_op.rd_typ_id typ_db 
			(mk_clang_type_name "_Complex")
		      in
		      typ
		in
		(typ, false)
	      end

	  | Type_ast.Primary t -> 
	      let ((_, id), has_def) = eval_type_val t
	      in
	      (id, has_def)

	  | Type_ast.Array (elmt_type, size_value) ->
	      begin
		let (elmt_typ, has_def) = eval_type_val elmt_type
		in 
		let array_decl = 
		  { 
		    Typ.m_elmt_teid = snd elmt_typ;
		    Typ.m_array_cardi = Typ.ARRAY_FIXED size_value
		  }
		in
		(Typ_op.add_array_te typ_db array_decl, has_def)
	      end

	  | Type_ast.Xarray elmt_type ->
	      begin
		let (elmt_typ, has_def) = eval_type_val elmt_type
		in 
		let xarray_decl = 
		  { 
		    Typ.m_elmt_teid = snd elmt_typ;
		    Typ.m_array_cardi = Typ.ARRAY_VARIABLE; (*Typ_op.Incomplete;*)
		  }
		in
		(Typ_op.add_array_te typ_db xarray_decl, has_def)
	      end

	  | Type_ast.Darray (elmt_type) ->
	      begin
		let (elmt_typ, has_def) = eval_type_val elmt_type
		in 
		let xarray_decl = 
		  { 
		    Typ.m_elmt_teid = snd elmt_typ;
		    Typ.m_array_cardi = Typ.ARRAY_DYNAMIC (None, "");
		  }
		in
		(Typ_op.add_array_te typ_db xarray_decl, has_def)
	      end

	  | Type_ast.Pointer (elmt_type_cabs) ->
	      begin
		let (elmt_typ, has_def) = eval_type_val elmt_type_cabs
		in (** struct abc { struct abc * link; } * abc; **) 
		(snd (Typ_op.ptr_typ elmt_typ), has_def)
	      end

	  | Type_ast.Struct type_name' ->
	      begin
		let type_name = Printf.sprintf "%s" type_name'.QN.qn_sname
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name
		in
		let (typ, qualified_type_name, is_complete) = 
		  match type_info_opt with
		    | Some (typ', qualified_type_name, is_complete') -> 
			(typ', qualified_type_name, is_complete')
		    | None -> 
			let (typ', qualified_type_name) = 
			  Typ_op.add_struct_te 
			    typ_db ~type_name ~is_anonymous:false
			in
			(typ', qualified_type_name, false)
		in
		(typ, false)
	      end

	  | Type_ast.Union type_name' ->
	      begin
		let type_name = Printf.sprintf "%s" type_name'
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name
		in
		let (typ, is_complete) = match type_info_opt with
		  | Some (typ', _, is_complete') -> (typ', is_complete')
		  | None -> 
		      let (typ', _) = 
			Typ_op.add_union_te 
			  typ_db ~type_name ~is_anonymous:false
		      in
		      (typ', false)
		in
		(typ, false)
	      end

	  | Type_ast.Function typ -> 
	      convert_function_type typ

	  | Type_ast.Typename str -> 
	      begin
		let type_info_opt = 
		  Typ_op.find_te_info typ_db ~type_name:str 
		in
		match type_info_opt with
		  | Some (typ, _, _) -> (typ, false)
		  | None -> 
		      camlp4_macro_exception "undefined type '%s'\n" str
	      end

	  | Type_ast.Enum type_name' ->
	      begin
		let type_name = Printf.sprintf "%s" type_name'
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name 
		in
		let typ = match type_info_opt with
		  | Some (typ, qualified_type_name, is_complete) -> typ
		  | None -> 
		      let (typ, qualified_type_name) = 
			Typ_op.add_enum_te 
			  typ_db ~type_name ~is_anonymous:false
		      in
		      typ
		in
		(typ, false)
	      end
		
	  | Type_ast.Qual (c_type, type_qualifier) ->
	      begin
		let (typ, has_def) = eval_type_val c_type
		in
		let qualifier = match type_qualifier with
		  | Type_ast.Const -> "const"
		  | Type_ast.Restrict -> "restrict" (** todo **)
		  | Type_ast.Volatile -> "volatile"
		in
		let qual_te_info = 
		  { Typ.m_qual_teid = -1;
		  Typ.m_qualified_teid = (snd typ);
		  Typ.m_qualifier = qualifier;
		  }
		in
		let qualified_typ = 
		  Typ_op.add_qualified_type 
		    typ_db qual_te_info
		in
		(qualified_typ, has_def)
	      end
		
	  | Type_ast.Bits (typ, size, lsb, mask)  ->
	      let (base_typ, has_def) = eval_type_val typ
	      in
	      let typ_id = 
		Typ_op.add_bit_type_type0
		  typ_db ~base_typ:(snd(base_typ))
		  ~bits:(size) ~lsb
	      in
	      (typ_id, false)

	  | Type_ast.Type_decl type_decl ->
	      let (typ, has_def) = eval_type_decl type_decl
	      in
	      (snd(typ), has_def)
	in
	((typ_db, typ_id), has_def)

    and convert_function_type: Type_ast.function_type -> Typ_op.typ_id * bool =
      fun typ ->
	let eval_param = function
	  | Type_ast.Labeled (t, _) -> (eval_type_val t)
	  | Type_ast.Unlabeled t -> (eval_type_val t)
	in
	let convert_param_type_list: 
	    is_varadic:bool -> out_typ_id:Typ.typ_id -> Type_ast.param list -> 
	  Typ.abs_fun_te_info * string list option 
	  =
	  fun ~is_varadic ~out_typ_id param_type_list ->
	    let has_param_ids = ref true
	    in
	    let param_ids = 
	      List.map
		(fun v -> 
		  match v with
		    | Type_ast.Labeled (t, s) -> s
		    | Type_ast.Unlabeled t -> ""
		) param_type_list
	    and param_typ_ids = List.map 
	      (fun field -> snd(fst (eval_param field))) 
	      param_type_list;
	    in
	    if (List.length param_ids != List.length param_typ_ids) then
	      camlp4_macro_exception "type and param mismatch";
	    let abs_fun_te_info = 
	      {
		Typ.aft_teid = -1;
		Typ.aft_ret_teid = out_typ_id;
		Typ.aft_param_teids = param_typ_ids;
		Typ.aft_va_arg = is_varadic;
	      }
	    in
	    (abs_fun_te_info,
	    if !has_param_ids then Some param_ids else None)
	in
	match typ with
	  | Type_ast.Fixed (param_type_list, ret_typ) ->
	      begin
		let ret_type_id = fst (eval_param ret_typ)
		in
		let (abs_fun_decl, param_ids_opt) = 
		  convert_param_type_list ~is_varadic:false 
		    ~out_typ_id:(snd ret_type_id) 
		    param_type_list
		in
		let abs_type = 
		  Typ_op.add_abs_fun_type typ_db abs_fun_decl
		in
		let typ = match param_ids_opt with
		  | Some param_ids -> 
		      Typ_op.add_crt_fun_type0 
			typ_db ~abs_fun_typ_id_opt:(Some abs_type)
			~in_param_ids:param_ids
		  | None -> abs_type
		in
		(typ, false)
	      end
	  | Type_ast.Varadic (param_type_list, ret_typ) ->
	      let ret_type_id = fst (eval_param ret_typ)
	      in
	      let (abs_fun_decl, param_ids_opt) = 
		convert_param_type_list ~is_varadic:true
		  ~out_typ_id:(snd ret_type_id) 
		  param_type_list
	      in
	      let abs_type = 
		Typ_op.add_abs_fun_type typ_db abs_fun_decl
	      in
	      let typ = match param_ids_opt with
		| Some param_ids -> 
		    Typ_op.add_crt_fun_type0 
		      typ_db ~abs_fun_typ_id_opt:(Some abs_type)
		      ~in_param_ids:param_ids
		| None -> abs_type
	      in
	      (typ, false)
		
    and convert_field_list field_list is_union =
      let l = 
	Safe_list.map 
	  (fun field -> 
	    match field with
	      | Type_ast.Named (c_type, id, offset) ->  
		  let (typ, has_def) = eval_type_val c_type
		  in
		  (Some id, snd typ, has_def) 
	      | Type_ast.Unamed (c_type, offset) ->
		  let ((typ_db, typ_id), _) = eval_type_val c_type
		  in
		  (None, typ_id, false)
	  ) field_list
      in
      l

    and eval_type_decl: Type_ast.c_type_decl -> typ * bool =
      fun type_cabs ->
	let (typ_id, has_def) = match type_cabs with
	  | Type_ast.Struct_decl (id, size, align, field_list) ->
	      begin
		let type_name = Printf.sprintf "%s" id
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name
		in
		let typ = match type_info_opt with
		  | Some (typ, qualified_type_name, is_complete) -> 
		      if is_complete then
			begin
			  let scope_name = Typ_op.get_top_scope_name typ_db
			  in
			  if qualified_type_name = (type_name ^ scope_name) then
			    begin
			      if field_list <> [] then
				begin
				  let l = convert_field_list field_list false
				  in
				  let old = Typ_op.rd_fields typ_db typ
				  in
				  if List.length l = List.length old then
				    begin
				      let combine = 
					List.map2 (fun (s1,_,_) (s2, _, _) -> s1 = s2)
					  l old
				      in
				      if not (List.for_all (fun b -> b) combine) then
					camlp4_macro_exception 
					  "\nmultiple definitions of '%s' %d\n" 
					  type_name (t_int typ)
				    end
				  else
				    camlp4_macro_exception 
				      "\nmultiple definitions of '%s' %d\n" 
				      type_name (t_int typ)
				end;
			      typ
			    end
			  else
			    let (typ, qualified_type_name) = 
			      Typ_op.add_struct_te 
				typ_db ~type_name ~is_anonymous:false
			    in
			    typ
			end
		      else
			typ
		  | None -> 
		      let (typ, qualified_type_name) = 
			Typ_op.add_struct_te 
			  typ_db ~type_name ~is_anonymous:false
		      in
		      typ
		in
		if field_list <> [] then
		  let l = convert_field_list field_list false
		  in
		  let _ = 
		    Typ_op.insert_fields typ_db 
		      ~decl_id:typ ~component_list:l 
		  in
		  (typ, true)
		else (** GNU C allows declaration of struct without fields **)
		  (typ, false)
	      end

	  | Type_ast.Union_decl (id, size, align, field_list) ->
	      begin
		let type_name = Printf.sprintf "%s" id
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name
		in
		let typ = match type_info_opt with
		  | Some (typ, qualified_type_name, is_complete) -> 
		      if is_complete then
			begin
			  let scope_name = Typ_op.get_top_scope_name typ_db
			  in
			  if qualified_type_name = (type_name ^ scope_name) then
			    camlp4_macro_exception 
			      "\nmultiple definitions of '%s' %d\n" 
			      type_name (t_int typ)
			  else
			    let (typ, qualified_type_name) = 
			      Typ_op.add_union_te 
				typ_db ~type_name ~is_anonymous:false
			    in
			    typ
			end
		      else
			typ
		  | None -> 
		      let (typ, qualified_type_name) = 
			Typ_op.add_union_te 
			  typ_db ~type_name ~is_anonymous:false
		      in
		      typ
		in
		if field_list <> [] then
		  let l = convert_field_list field_list true 
		  in
		  let _ = Typ_op.insert_fields typ_db ~decl_id:typ
		    ~component_list:l
		  in
		  (typ, true)
		else (** GNU C allows declaration of struct without fields **)
		  (typ, false)
	      end

	  | Type_ast.Enum_decl (id, name_group_list) ->
	      begin
		assert (name_group_list <> []);
		let type_name = Printf.sprintf "%s" id
		in
		let type_info_opt = Typ_op.find_te_info typ_db ~type_name
		in
		let typ_id = match type_info_opt with
		  | Some (typ, qualified_type_name, is_complete) ->  
		      if is_complete then
			begin
			  let scope_name = Typ_op.get_top_scope_name typ_db
			  in
			  if qualified_type_name = (type_name ^ scope_name) then
			    camlp4_macro_exception 
			      "\nmultiple definitions of '%s' %d\n" 
			      type_name (t_int typ)
			  else
			    let (typ, qualified_type_name) = 
			      Typ_op.add_enum_te 
				typ_db ~type_name ~is_anonymous:false
			    in
			    typ
			end
		      else
			typ
		  | None -> 
		      let (typ, qualified_type_name) = 
			Typ_op.add_enum_te 
			  typ_db ~type_name ~is_anonymous:false
		      in
		      typ
		in
		let _ = List.iter
		  (fun (item_name, value) ->
		    let _ = Typ_op.insert_enum_item typ_db ~decl_id:typ_id
		      ~enum_item:(item_name, Some value)
		    in ()
		  ) name_group_list
		in
		let _ = Typ_op.complete_type_decl typ_db ~decl_id:typ_id
		in
		(typ_id, true)
	      end
	  | Type_ast.Typedef (typ_ast, alias) -> 
	      let type_info_opt = Typ_op.find_te_info typ_db ~type_name:alias
	      in
	      let typ_id = match type_info_opt with
		| Some (typ,_, _) -> 
		    begin
		      match alias with
			| "wchar_t"
			| "size_t" ->
			    (** wchar_t and size_t might be builtin or(and) typedef type **)
			    let (alias_typ, has_def) = eval_type_val typ_ast
			    in
			    if !Mlite_config.enable_Wtype then
			      camlp4_macro_warning "typedef %s as %d and then %d types" 
				alias typ (snd alias_typ);
			    let (typ, qualified_type_name) = 
			      Typ_op.replace_typedef_type 
				typ_db ~type_name:alias 
				~alias_typ_id:(snd alias_typ)
			    in
			    typ
			| _ ->
			    camlp4_macro_exception "typedef %s as different types" alias
		    end
		| None ->
		    begin
		      (*
		      let (synonyous_id, _) = Typ_op.create_synonymous_type
			typ_db ~type_name:alias
		      in
		      *)
		      let (alias_typ, has_def) = eval_type_val typ_ast
		      in
		      (*
		      let (typ, qualified_type_name) = 
			Typ_op.replace_typedef_type 
			  typ_db ~type_name:alias 
			  ~alias_typ_id:(snd alias_typ)
		      in
		      typ
		      *)
		      let (typ, qualified_type_name) = 
			Typ_op.add_typedef_te 
			  typ_db ~type_name:alias 
			  ~alias_typ_id:(snd alias_typ)
		      in
		      typ
		    end
	      in
	      (typ_id, false)
	in
	((typ_db, typ_id), has_def)
    in
    fst(eval_type_val type_or_expr_ast)
      
