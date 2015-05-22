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
open Collection
module C99 = Ast_aa_gram
module I = Ast_da_type
module IP = Ast_da_type_printer
module O = Ast_ea_expr
module S = C_semantics_symbol
module CA = Const_folding
module T = Tent
module TO = Tent_op
module E = Senv 
module EO = Senv_op
module TM = Typ_mem
module QN = Qual_name
module QNP = Qual_name_printer
module QNO = Qual_name_op
module CE = Cent
module CEO = Cent_op
module AEE = Ast_ea_expr_env

open Csize
open Safe_list


let init_local_array_as_zero = ref true
let disable_constant_expression_evaluation = ref false
let keep_string_literal = ref true


let enable_thread_storage = ref false
let enable_vararg = ref false
let enable_wchar = ref false

let user_opts = 
  [
    ("--init-local-array-as-zero",
    Arg.Set init_local_array_as_zero,
    "all uninitialized array elements should be initialized as zero");
    ("--disable-constant-evaluation",
    Arg.Set disable_constant_expression_evaluation,
    "all uninitialized array elements should be initialized as zero");
    ("--no-string-literal",
    Arg.Set keep_string_literal,
    "Dont keep string literal");    
    ("--thread", Arg.Unit (fun () -> enable_thread_storage := true), 
    "Allow __thread key word");
    ("--vararg", Arg.Unit (fun () -> enable_vararg := true), 
    "Allow variadic functions");
  ]


type te = Tent.te
and env = Senv.env
and lval_or_rval = 
  | LVAL of O.lval (* a->f f has a scalar type *)
  | RVAL of O.rval (* a->f f has an array type *)

(* temperary ast used during the translation, but it will not appear in the
   final ast *)
type c_tmp_local_declaration = 
  | TMPLocal_obj_decl of O.linkage * CE.ce
  | TMPLocal_obj_decl_init of O.linkage * CE.ce * c_tmp_initializer
  | TMPLocal_obj_init of O.expr
  | TMPLocal_type_def of O.c_type
  | TMPLocal_type_decl of O.c_type
  | TMPLocal_type_only of O.c_type
  | TMPLocal_darray_decl of O.c_stmt010 list * O.linkage * CE.ce * O.rexpr
  | TMPLocal_external_decl of CE.ce
      
and c_tmp_initializer = c_tmp_local_init_expression Typ_mem.t
    
and c_tmp_local_init_expression = 
  | TMPStatic_init_none
  | TMPDynamic_init_none
  | TMPStatic_init of O.cexpr
  | TMPDynamic_init of (O.c_stmt010 list * O.rexpr)


and cmpstate = 
    {
      env: Senv.env;
      eenv: O.expr_env;
      cstr_lit_hashtbl: (Cent.ce * Tent.te) C_syntax_symbol_op.CStrLitHashtbl.t;
      init_lst: O.c_declaration list ref;
    }
      
exception Dynamic of I.c_expr020
exception Cinit of I.c_expr020
exception CStmt of O.c_compound_stmt010
exception Finished of I.c_initializer_list
exception Next of I.c_initializer_list
exception Found_field of int * c_tmp_initializer
exception Darray of O.c_type * (O.c_stmt010 list) * O.rexpr option




let mk_ret_ce_info cmpstate (linkage:I.linkage) = fun (sname, te) -> 
  assert (not (TO.is_crt_function_typ te));
  let top_scope = (EO.get_top_scope_name cmpstate.env)
  in (QNO.alloc_data_regi top_scope sname, true)

let mk_ce_info cmpstate (linkage:I.linkage) = fun (sname, te) -> 
  let env = cmpstate.env
  in
  assert (not (TO.is_crt_function_typ te));
  let top_scope = (EO.get_top_scope_name cmpstate.env)
  in
  match linkage with
    | I.Static ->
	if TO.is_abs_function_typ te then
	  (QNO.alloc_static_code_addr top_scope sname, false)
	else 
	  (QNO.alloc_static_data_addr top_scope sname, true)
	    
    | I.Static_Inline ->
	if TO.is_abs_function_typ te then
	  (QNO.alloc_static_code_addr top_scope sname, false)
	else 
	  (QNO.alloc_static_data_addr top_scope sname, true)
	    
    | I.Extern ->
	if TO.is_abs_function_typ te then
	  (QNO.alloc_code_addr top_scope sname, false)
	else 
	  let top_scope = EO.get_file_scope_name env
	  in 
	  (QNO.alloc_data_addr top_scope sname, false)
	    
    | I.Default_extern ->
	if TO.is_abs_function_typ te then
	  (QNO.alloc_code_addr top_scope sname, false)
	else
	  (QNO.alloc_data_addr top_scope sname, true)
    | _ ->
	if TO.is_abs_function_typ te then
	  (QNO.alloc_code_addr top_scope sname, false)
	else 
	  (QNO.alloc_data_addr top_scope sname, true)


let mk_default_fun_ce_info cmpstate (linkage:I.linkage) = fun (sname, te) ->
  let env = cmpstate.env
  in
  let top_scope = (EO.get_top_scope_name env)
  in
  let qname = match linkage with
    | I.Static 
    | I.Static_Inline ->
	QNO.alloc_static_code_addr top_scope sname
    | I.Extern -> (* it's weird, you can prefix extern 
		     before a function definition *)
	QNO.alloc_code_addr top_scope sname
    | I.Default_extern ->
	QNO.alloc_code_addr top_scope sname
    | _ ->
	QNO.alloc_code_addr top_scope sname
  in (qname, true)
    
    
let mk_param_ce_info cmpstate = fun (sname, te) -> 
  let env = cmpstate.env
  in
  let top_scope = EO.get_top_scope_name env
  in
  let qname = 
    if TO.is_crt_function_typ te then
      assert false
    else
      QNO.alloc_param_name top_scope sname (snd te)
  in (qname, true)


let assert_link_time_constant init_expr c_expression = 
  let str = camlp4_macro_str_pp_print
    (fun fm -> IP.pp_print_c_expression fm ~need_paren:false
      c_expression)
  in
  match !init_expr with
    | TMPStatic_init_none ->
	camlp4_macro_exception "expr '%s' is not a link time cosntant." str
    | TMPDynamic_init_none -> ()
    | _ -> assert false

	

  
let mk_const_rval te str = O.Rconst (TO.make_type_const te str)
  
let mk_true_cond rval = O.NEQ_ZERO rval

let mk_cast cmpstate rexpr dest_typ = 
  let rexpr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rexpr
  in
  if (not (TO.is_cast_compatible dest_typ rexpr_O_rv_typ)) then
    if (TO.is_bool_typ dest_typ) then
      let const_zero = mk_const_rval (rexpr_O_rv_typ) "0"
      in O.Ne_cast (TO.bool_int_typ dest_typ, rexpr, const_zero)
    else
      O.Cast (dest_typ, rexpr)
  else
    O.Rval rexpr



let mk_cconst v c t=
  let t = match c with
    | Some (t, f) -> t
    | None -> t
  in O.Cconst (t, CA.cval_ext_of_cval v)


let recycle_tmp_ces cmpstate l exclusion =
  let exists v = 
    List.exists
      (fun rval -> 
	match rval with
	  | O.Rladdr (O.Nctn id)
	  | O.Rindir id -> (snd id) = (snd v)
	  | _ -> false
      ) exclusion
  in
  let lst = ref []
  in
  let f s = 
    let _ = if not (exists s) & CEO.is_tmp s then 
      let t = CEO.te_of s
      in
      lst := (t,s)::!lst
    in s
  in
  let _ = 
    Safe_list.iter (fun v -> 
      ignore(Ast_ea_expr_op.stmt_map_id f v)) l
  in EO.recycle_tmp_ces cmpstate.env !lst

    




let mk_tool_gen_decls l = 
  Safe_list.map
    (fun (t, ce) -> 
      if (CEO.is_register ce) then
	O.Local_register ce
      else
	O.Local_obj_decl (O.Auto, ce)
    ) l
    

let rec create_env (size:int) (name:string) 
    (builtin_function_list:I.function_type list)
    (gnu_builtin_function_list:I.function_type list)
    :cmpstate = 
  let lenv = 
    {
      env = EO.create_env name size;
      eenv = 
	{
	  O.rexpr_env = O.RexprHashtbl.create 17;
	  O.cexpr_env = O.CexprHashtbl.create 17;
	  O.rval_env = O.RvalHashtbl.create 17;
	  O.lval_env = O.LvalHashtbl.create 17;
	};
      cstr_lit_hashtbl = (C_syntax_symbol_op.CStrLitHashtbl.create 211);
      init_lst = ref [];
    }
  in
  let _ = init_env lenv
    (builtin_function_list) (gnu_builtin_function_list)
  in lenv

and init_env (cmpstate: cmpstate) 
    (builtin_function_list:I.function_type list)
    (gnu_builtin_function_list:I.function_type list) =
  let env = cmpstate.env
  in
  let typ = compile_c_type cmpstate
    (I.Function_type
      { 
	I.func_name = Some "int (void)";
	I.param_type = I.Param_type_fix [];
	I.formal_param = [];
	I.return_type = I.Primitive_type (I.Int);
	I.func_attributes = [];
      })
  in
  let _ = env.E.default_typ <- typ
  in
  Safe_list.iter
    (fun function_type ->
      let typ = compile_c_type cmpstate (I.Function_type function_type)
      in
      let fun_name = match function_type.I.func_name with
	| Some v -> v
	| None -> assert false
      in
      let mk_ce_info (fun_name, typ) = 
	{
	  CE.ce_id = CEO.alloc_ceid env.E.ce_tbl;
	  CE.ce_qname = QNO.alloc_cstd_code_addr 
	    (EO.get_top_scope_name env) fun_name;
	  CE.ce_te = typ;
	  CE.ce_used = false;
	  CE.ce_is_real_ent = false;
	  CE.ce_addr_is_taken = false;
	  CE.ce_is_register = false;
	}
      in
      ignore (EO.add_ce_info env 
	(fun (sname, te) -> 
	  (QNO.alloc_cstd_code_addr (EO.get_top_scope_name env) sname, false))  
	(fun_name, typ));
      let ce_info = 
	{
	  CE.ce_id = CEO.alloc_ceid env.E.ce_tbl;
	  CE.ce_qname = QNO.alloc_cstd_code_addr 
	    (EO.get_top_scope_name env) ("__builtin_" ^ fun_name);
	  CE.ce_te = typ;
	  CE.ce_used = false;
	  CE.ce_is_real_ent = false;
	  CE.ce_addr_is_taken = false;
	  CE.ce_is_register = false;
	}
      in
      ignore (EO.add_ce_info env (fun (sname, te) -> 
	(QNO.alloc_cstd_code_addr (EO.get_top_scope_name env) sname, false))
	(("__builtin_" ^ fun_name), typ));
    ) builtin_function_list;
  Safe_list.iter
    (fun function_type ->
      let typ = 
	compile_c_type cmpstate (I.Function_type function_type)
      in
      let fun_name = match function_type.I.func_name with
	| Some v -> v
	| None -> assert false
      in
      let ce_info (fun_name, typ) = 
	{
	  CE.ce_id = CEO.alloc_ceid env.E.ce_tbl;
	  CE.ce_qname = QNO.alloc_cstd_code_addr 
	    (EO.get_top_scope_name env) fun_name;
	  CE.ce_te = typ;
	  CE.ce_used = false;
	  CE.ce_is_real_ent = false;
	  CE.ce_addr_is_taken = false;
	  CE.ce_is_register = false;
	}
      in
      ignore (EO.add_ce_info env 
	(fun (sname, te) -> (QNO.alloc_cstd_code_addr 
	  (EO.get_top_scope_name env) sname, false))
	(fun_name, typ))
    ) gnu_builtin_function_list;
  TO.set_max_preload_typ env.E.te_tbl

and compile_attributes cmpstate lst = 
  List.fold_left 
    (fun (l0, l1) v -> 
      let (ce_atts, te_atts) = (compile_attribute cmpstate v)
      in (l0 @ ce_atts, l1 @ te_atts)
    ) ([], []) lst
    
and compile_attribute cmpstate (s, l) =
  match s with
    | "__attribute__" -> 
	List.fold_left 
	  (fun (ce_atts, te_atts) e -> match e with
	    | I.Variable str ->
		begin
		  match str with
		    | "transparent_union" -> 
			(ce_atts, te_atts @ [T.GCC_transparent_union])
		    | "aligned" ->
			(ce_atts, te_atts @ [T.GCC_aligned_max])
		    | "__packed__"
		    | "packed" ->
			(ce_atts, te_atts @ [T.GCC_packed])
		    | "weak" -> 
			(ce_atts @ [CE.GCC_weak], te_atts)
		    | "deprecated" ->
			(ce_atts, te_atts)
		    | "noreturn" ->
			(ce_atts @ [CE.GCC_noreturn], te_atts)
		    | "noinline" ->
			(ce_atts @ [CE.GCC_noinline], te_atts)
		    | "always_inline" ->
			(ce_atts @ [CE.GCC_always_inline], te_atts)
		    | "pure" ->
			(ce_atts @ [CE.GCC_pure], te_atts)
		    | "const" ->
			(ce_atts @ [CE.GCC_const], te_atts)
		    | "nothrow" ->
			(ce_atts @ [CE.GCC_nothrow], te_atts)
		    | "no_instrument_function" ->
			(ce_atts @ [CE.GCC_no_instrument_function], te_atts)
		    | "constructor" ->
			(ce_atts @ [CE.GCC_constructor], te_atts)
		    | "destructor" ->
			(ce_atts @ [CE.GCC_destructor], te_atts)
		    | "unused" ->
			(ce_atts @ [CE.GCC_unused], te_atts)
		    | "used" ->
			(ce_atts @ [CE.GCC_used], te_atts)
		    | "warn_unused_result" ->
			(ce_atts @ [CE.GCC_warn_unused_result], te_atts)
		    | "malloc" ->
			(ce_atts @ [CE.GCC_malloc], te_atts)
		    | _ -> assert false
		end
	    | I.Call (v, params) -> 
		begin
		  match v, params with
		    | I.Variable "aligned", [I.Constant c] 
		    | I.Variable "__aligned__", [I.Constant c] ->
			let ((_, (cval,_,_)), _) = compile_c_constant cmpstate c
			in
			let i = Const_folding.cval_to_int cval
			in (ce_atts, te_atts @ [T.GCC_aligned i])
		    | I.Variable "mode", [I.Variable v]
		    | I.Variable "__mode__", [I.Variable v] ->
			let mode = match v with
			  | "QI" 
			  | "__QI__" -> T.GCC_qi
			  | "DI"
			  | "__DI__" -> T.GCC_di
			  | "HI"
			  | "__HI__" -> T.GCC_hi
			  | "SI" 
			  | "__SI__" -> T.GCC_si
			  | "DF"
			  | "__DF__" -> T.GCC_df
			  | "HF"
			  | "__HF__" -> T.GCC_hf
			  | "SF"
			  | "__SF__" -> T.GCC_sf
			  | "word" -> T.GCC_word
			  | "byte" -> T.GCC_byte
			  | "pointer" -> T.GCC_pointer
			  | _ -> assert false
			in (ce_atts, te_atts @ [T.GCC_mode mode])
		    | _ -> 
			assert false
		end
	    | _ -> assert false
	  ) ([], []) l
    | _ -> assert false  


and find_cstr_lit cmpstate cstr =
  let env = cmpstate.env
  in
  let char_type env = function
    | C_syntax_symbol.String_literal str -> 
	let length = String.length str + 1
	in TO.create_array_type env.EO.char_typ (Int64.of_int length)
    | C_syntax_symbol.WString_literal lst -> 
	let length = List.length lst + 1
	in TO.create_array_type env.EO.wchar_t_typ (Int64.of_int length)
  and char_da_type = function 
    | C_syntax_symbol.String_literal _ ->
	I.Primitive_type I.Char
    | C_syntax_symbol.WString_literal _ ->
	I.Primitive_type I.WChar
  in
  try
    C_syntax_symbol_op.CStrLitHashtbl.find 
      cmpstate.cstr_lit_hashtbl cstr
  with
      Not_found->
	begin	
	  let ce_info = 
	    {
	      CE.ce_id = CEO.alloc_ceid env.E.ce_tbl;
	      CE.ce_qname = EO.alloc_cstr_id env;
	      CE.ce_te = char_type env cstr;
	      CE.ce_used = false;
	      CE.ce_is_real_ent = true;
	      CE.ce_addr_is_taken = false;
	      CE.ce_is_register = false;
	    }
	  in
	  let decl = 
	    O.Str_decl_init 
	      (O.Default_storage, (env.E.ce_tbl, ce_info.CE.ce_id), cstr)
	  in
	  cmpstate.init_lst := decl::!(cmpstate.init_lst);
	  let ty = ce_info.CE.ce_te
	  in
	  let _ = CEO.add_ce_info env.E.ce_tbl ce_info
	  in
	  let ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  C_syntax_symbol_op.CStrLitHashtbl.add 
	    cmpstate.cstr_lit_hashtbl cstr (ce, TO.ptr_of (TO.elmt_of ty));
	  (ce, TO.ptr_of (TO.elmt_of ty))
	end
	  
and find_cstr_lit_cexpr env cstr = 
  let (ce, te) = find_cstr_lit env cstr
  in O.Cvct ce
       
       
and find_cstr_lit_expr env cstr = 
  let (ce, te) = find_cstr_lit env cstr
  in O.Rval (O.Rvct ce)    
       
       
and mk_const_cexpr int_typ int_str = 
  O.Cconst (TO.make_type_const int_typ int_str)
    
    
and rexpr_to_rval cmpstate (rexpr:O.rexpr) = 
  let env = cmpstate.env
  in
  let rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
  in
  match rexpr with
    | O.Rval v -> ([], v)
    | _ -> 
	if TO.is_void_typ rexpr_O_re_typ then
	  ([], O.Rvoid)
	else
	  begin
	    let var = (EO.get_tmp_ce env rexpr_O_re_typ)
	    in
	    if (TO.is_native_assign_supported_typ rexpr_O_re_typ) then
	      ([c2s 
		(O.Assign 
		  (O.Lladdr (O.Nlbl var), rexpr))], O.Rladdr (O.Nctn var))
	    else
	      let int_typ = (env.E.te_tbl, Mach.cuint_id)
	      and s0 = TO.static_sizeof rexpr_O_re_typ
	      in
	      let expr0 = (O.Rladdr (O.Nlbl var))
	      and expr1 = (addrof_rexpr rexpr)
	      and csize = O.Cconst (TO.make_type_const int_typ (Int64.to_string s0))
	      in 
	      let _ = EO.find_ce_info env "memcpy"
	      in
	      ([c2s (O.Memcpy (expr0, expr1, csize))], O.Rladdr (O.Nctn var))
	  end

and cast_expr cmpstate e = function
  | Some (t,f) ->
      let e' = mk_cast cmpstate e t
      in rexpr_to_rval cmpstate e'
  | None -> ([], e)
      
and add_alias_typ (cmpstate:cmpstate) (alias:string) (cabs_typ:I.c_type):te =
  let env = cmpstate.env
  in
  let type_info_opt = EO.find_te_info env alias
  in
  let typ_id = match type_info_opt with
    | Some te_info -> 
	begin
	  match alias with
	    | "wchar_t"
	    | "size_t" ->
		(** wchar_t and size_t might be builtin or(and) 
		    typedef type **)
		let alias_typ = compile_c_type cmpstate cabs_typ
		in
		if !Mlite_config.enable_Wtype then
		  camlp4_macro_warning "typedef %s as %d and then %d types" 
		    alias te_info.T.m_id (snd alias_typ);
		let (typ, qualified_type_name) = 
		  EO.replace_typedef_type 
		    env ~type_name:te_info.T.m_name 
		    ~alias_teid:(snd alias_typ)
		in typ 
	    | _ ->
		camlp4_macro_exception 
		  "typedef %s as different types" alias
	end
    | None ->
	let alias_typ = compile_c_type cmpstate cabs_typ
	in EO.add_typedef_te env (snd alias_typ) alias
	     
  in (env.E.te_tbl, typ_id)  
       
and eval_expr_typ (cmpstate:cmpstate) (expr:I.c_expr020):te = 
  let env = cmpstate.env
  in
  let typ = match expr with
    | I.String _ -> assert false
    | I.Comma (se0, se1) -> eval_expr_typ cmpstate se1
	
    | I.Constant c_constant ->
	let (_, typ') = compile_c_constant cmpstate c_constant
	in typ'
	     
    | I.Call (se0, expr_list) -> 
	begin
	  let fun_typ = eval_expr_typ cmpstate se0
	  in
	  let arg_typs = 
	    Safe_list.map (eval_expr_typ cmpstate)
	      expr_list
	  in
	  let fun_typ' =
	    if TO.is_ptr_typ fun_typ then
	      TO.elmt_of fun_typ
	    else
	      fun_typ
	  in
	  let abs_fun_typ = 
	    if TO.is_abs_function_typ fun_typ' then
	      fun_typ'
	    else
	      TO.get_abs_fun_te fun_typ'
	  in
	  (TO.apply abs_fun_typ arg_typs)
	end
	  
    | I.Macro_va_start (se0, se1) -> env.EO.void_typ
	
    | I.Macro_va_arg (se0, c_type) ->  
	compile_c_type cmpstate c_type
	  
    | I.Macro_va_end se0 ->  env.EO.void_typ

    | I.Builtin_types_compatible (t0, t1) -> env.EO.int_typ
	
    | I.Builtin_constant_p e -> env.EO.int_typ
	
    | I.Builtin_expect (e0, e1) -> eval_expr_typ cmpstate e0

    | I.Gnu_block block -> assert false
	
    | I.Gnu_labeladdr str -> assert false
	
    | I.Variable str ->
	begin
	  let ce_info = EO.find_ce_info env str
	  in
	  if QNO.is_enum ce_info.CE.ce_qname then
	    (** C99 enumeration constants is explicity 
		defined as int **)
	    env.EO.int_typ
	  else
	    ce_info.CE.ce_te
	end
	  
    | I.Memberof (se0,f) ->
	let base_typ = eval_expr_typ cmpstate se0
	in TO.field_typ base_typ f
	     
    | I.Memberof_ptr (se0, f) ->
	let base_typ = eval_expr_typ cmpstate se0
	in TO.field_typ (TO.elmt_of base_typ) f
	     
    | I.Indexof (se0, se1) -> 
	let base_typ = eval_expr_typ cmpstate se0
	in TO.elmt_of base_typ
	     
    | I.Binary_arithm (bop, se0, se1) ->
	let ty0 = eval_expr_typ cmpstate se0
	and ty1 = eval_expr_typ cmpstate se1
	in
	let (_, _, rt) = TO.bin_arithm_typ bop ty0 ty1
	in rt
	     
    | I.Binary_predicate (brel, se0, se1) ->
	env.EO.int_typ
	  
    | I.Binary_logic (bop, se0, se1) ->
	env.EO.int_typ
	  
    | I.Unary_arithm (uop, se0) ->
	eval_expr_typ cmpstate se0
	  
    | I.Logic_not se0 -> 
	env.EO.int_typ
	  
    | I.Sizeof_expr expr0 ->
	env.EO.size_typ
	  
    | I.Sizeof_type (type_cabs, has_def) ->
	env.EO.size_typ
	  
    | I.Alignof_expr expr0 ->
	env.EO.size_typ
	  
    | I.Alignof_type (typ_cabs, has_def) ->
	env.EO.size_typ
	  
    | I.Cast (c_type_name, se0) -> 
	compile_c_type cmpstate c_type_name
	  
    | I.Cast_init (c_type_name, inits) -> assert false
	
    | I.Assign (se0, I.Cast_init (c_type_name, inits)) ->
	eval_expr_typ cmpstate se0
	  
    | I.Assign (se0, se1) ->
	eval_expr_typ cmpstate se0

    | I.Pre_decr se0
    | I.Pre_incr se0 
    | I.Post_decr se0
    | I.Post_incr se0 ->
	eval_expr_typ cmpstate se0
	  
    | I.Assign_arithm (bop, se0, se1) ->
	eval_expr_typ cmpstate se0
	  
    | I.Memof se0 -> 
	TO.elmt_of (eval_expr_typ cmpstate se0)
	  
    | I.Addrof se0 -> 
	let _ = mark_addr_is_taken cmpstate se0
	in TO.ptr_of (eval_expr_typ cmpstate se0)
	     
    | I.Question (se0, se_opt1, se2) ->
	begin
	  let ty0 = eval_expr_typ cmpstate se0
	  and ty1_opt = Mapping.map_opt (eval_expr_typ cmpstate) se_opt1
	  and ty2 = eval_expr_typ cmpstate se2
	  in
	  match ty1_opt with
	    | Some ty1 -> TO.lub [ty1;ty2]
	    | None -> TO.lub [ty0;ty2]
	end
  in typ

and compile_field_list cmpstate field_list =
  Safe_list.map 
    (fun field -> 
      match field with
	| I.Regular (c_type, id) ->  
	    let (typ, size_opt, (has_def, l)) = eval_type_te cmpstate c_type
	    in
	    assert (l = []);
	    assert (size_opt = None);
	    (Some id, snd typ, has_def) 
	| I.Bits (c_type, expr, id_opt) -> 
	    let size = evaluate_c_constant_expression_as_int cmpstate expr
	    in
	    let ((env, base_typ), size_opt, (_, l)) = eval_type_te cmpstate c_type
	    in
	    assert (l = []);
	    assert (size_opt = None);
	    let (_, base_typ') = 
	      TO.get_atomic_equiv_typ (env, base_typ)
	    in
	    let typ_id = TO.add_bit_te
	      env ~base_teid:base_typ' ~bits:(csize_of_int size)
	    in (id_opt, typ_id, false)
    ) field_list

and compile_enum_item_list cmpstate item_list =
  let ((serno, evalue), lst) = 
    List.fold_left 
      (fun ((serno, evalue), lst) (item_name, expr_opt) ->
	let (serno, evalue) = 
	  match expr_opt with
	    | None -> (serno, evalue)
	    | Some expr -> 
		let v = (evaluate_c_constant_expression_as_int cmpstate expr)
		in (serno, v)
	in ((serno + 1, evalue +1), (item_name, serno, evalue)::lst)
      ) ((0,0),[]) item_list
  in List.rev lst
       
and eval_type_te (cmpstate:cmpstate) (type_cabs:I.c_type):(te * O.rexpr option * (bool * O.c_stmt010 list)) =
  let env = cmpstate.env
  in
  let (l, size_opt, teid, has_def) = eval_type_teid cmpstate type_cabs []
  in
  let te = (env.E.te_tbl, teid)
  in
  if has_def then EO.add_new_type env te;
  (te, size_opt, (has_def, l))

and eval_static_type_te (cmpstate:cmpstate) (type_cabs:I.c_type) :(te * bool) =
  let (l, size_opt, teid, has_def) = eval_type_teid cmpstate type_cabs []
  in
  assert (l = []);
  assert (size_opt = None);
  ((cmpstate.env.E.te_tbl, teid), has_def)
    
and eval_type_teid (cmpstate:cmpstate) (type_cabs:I.c_type) (te_atts:T.te_attribute list) 
    :(O.c_stmt010 list * O.rexpr option * T.teid * bool) =
  let env = cmpstate.env
  in
  assert (te_atts = []);
  let (l, size_opt, teid, has_def) = match type_cabs with
    | I.Typeid i -> ([], None, i, false)
    | I.GCC_attribute_type (t, attribute) -> 
	let (ce_atts, te_atts) = compile_attributes cmpstate attribute
	in 
	let (l, expr_opt, teid, has_def) = eval_type_teid cmpstate t []
	in
	let teid = TO.add_attributed_te env.E.te_tbl (teid, te_atts)
	in (l, expr_opt, teid, has_def)
	     
    | I.Primitive_type v -> 
	begin
	  let teid = match v with
	    | I.Void -> Mach.cvoid_id
	    | I.Void_ptr -> Mach.cptr_id
	    | I.Char -> Mach.cchar_id
	    | I.Signed_Char -> Mach.cschar_id
	    | I.Unsigned_Char -> Mach.cuchar_id
	    | I.Short_Int -> Mach.cshort_id
	    | I.Signed_Short_Int -> Mach.cshort_id
	    | I.Unsigned_Short_Int -> Mach.cushort_id
	    | I.Int -> Mach.cint_id
	    | I.Default_int -> assert false
	    | I.Signed_Int -> Mach.cint_id
	    | I.Unsigned_Int -> Mach.cuint_id
	    | I.Signed -> Mach.cint_id
	    | I.Unsigned -> Mach.cuint_id
 	    | I.Long_Int -> Mach.clong_id
	    | I.Signed_Long_Int -> Mach.clong_id
	    | I.Unsigned_Long_Int -> Mach.culong_id
	    | I.Long_Long_Int -> Mach.cllong_id
	    | I.Signed_Long_Long_Int -> Mach.cllong_id
	    | I.Unsigned_Long_Long_Int -> Mach.cullong_id
	    | I.Float -> Mach.cfloat_id
	    | I.Double -> Mach.cdouble_id
	    | I.Long_Double -> Mach.cldouble_id
	    | I.Float_Complex -> Mach.cfcomplex_id
	    | I.Double_Complex -> Mach.cdcomplex_id
	    | I.Long_Double_Complex -> Mach.cldcomplex_id
	    | I.Bool -> Mach.cbool_id
	    | I.Complex -> Mach.ccomplex_id
	    | I.WChar -> Mach.cwchar_t_id
	  in
	  ([], None, teid, false)
	end

    | I.Array (elmt_type, expr) ->
	begin
	  let (elmt_typ, has_def) = eval_static_type_te cmpstate elmt_type
	  in
	  try
	    let size_value =
	      evaluate_c_constant_expression_as_int cmpstate expr
	    in 
	    let teid = 
	      TO.add_array_te env.E.te_tbl
		{ 
		  T.m_elmt_teid = snd elmt_typ;
		  T.m_array_cardi = T.ARRAY_FIXED (csize_of_int size_value)
		}
	    in ([], None, teid, has_def)
	  with
	    | Dynamic _ ->
		begin
		  let (l, rval)  = compile_c_expr020_rval cmpstate expr
		  in
		  let elmt_size = TO.static_sizeof elmt_typ
		  in
		  let size_rval = 
		    (mk_const_rval env.E.size_typ (string_of_csize elmt_size))
		  in
		  let byte_size = 
		    AEE.put_rexpr cmpstate.eenv (O.Binary_arithm (S.Mul, rval, size_rval))
		      (AEE.get_rval cmpstate.env cmpstate.eenv rval)
		  in
		  let teid = snd (TO.ptr_of elmt_typ)
		  in (l, Some byte_size, teid, has_def)
		end
	end
	  
    | I.Xarray elmt_type ->
	begin
	  let (elmt_typ, has_def) = eval_static_type_te cmpstate elmt_type
	  in 
	  let teid = 
	    TO.add_array_te env.E.te_tbl
	      { 
		T.m_elmt_teid = snd elmt_typ; 
		T.m_array_cardi = T.ARRAY_VARIABLE;
	      }
	  in ([], None, teid, has_def)
	end

    | I.Pointer (elmt_type_cabs) ->
	begin
	  let (elmt_typ, has_def) = eval_static_type_te cmpstate elmt_type_cabs
	  in (** struct abc { struct abc * link; } * abc; **) 
	  ([], None, snd (TO.ptr_of elmt_typ), has_def)
	end

    | I.Struct_type_name sname ->
	begin
	  let sname = "struct " ^ sname
	  in
	  match EO.find_te_info env sname with
	    | Some te_info -> ([], None, te_info.T.m_id, false)
	    | None -> ([], None, EO.add_struct_name_te env sname, false)
	end
	  
    | I.Struct_type (sname, field_list, I.C_struct) ->
	begin
	  let sname = "struct " ^ sname
	  and l = compile_field_list cmpstate field_list
	  in
	  let teid = 
	    try 
	      EO.add_struct_te env sname l
	    with
	      | EO.FoundTeInfo te_info ->
		  camlp4_macro_exception 
		    "\nmultiple definitions of '%s' %d\n" sname
		    te_info.T.m_id
	  in ([], None, teid, true)
	end

    | I.Struct_type (sname, field_list, layout) ->
	camlp4_macro_exception 
	  "\n struct %s has an unhandled struct layout\n" sname
	  
    | I.Union_type_name sname ->
	begin
	  let sname = "union " ^ sname
	  in
	  match EO.find_te_info env sname with
	    | Some te_info -> ([], None, te_info.T.m_id, false)
	    | None -> ([], None, EO.add_union_name_te env sname, false)
	end
	  
    | I.Union_type (sname, field_list) ->
	begin
	  let sname = "union " ^ sname
	  and l = compile_field_list cmpstate field_list
	  in
	  let teid = 
	    try
	      EO.add_union_te env sname l
	    with
	      | EO.FoundTeInfo te_info ->
		  camlp4_macro_exception 
		    "\nmultiple definitions of '%s' %d\n" sname
		    te_info.T.m_id
	  in ([], None, teid, true)
	end

    | I.Enum_type_name sname ->
	begin
	  let sname = "enum " ^ sname
	  in
	  match EO.find_te_info env sname with
	    | Some te_info -> ([], None, te_info.T.m_id, false)
	    | None -> ([], None, EO.add_enum_name_te env sname, false)
	end
	  
    | I.Enum_type (sname, name_group_list) ->
	begin
	  let sname = "enum " ^ sname
	  and l = compile_enum_item_list cmpstate name_group_list
	  in
	  assert (l <> []);
	  let teid = 
	    try
	      EO.add_enum_te env sname l
	    with
	      | EO.FoundTeInfo te_info ->
		  camlp4_macro_exception 
		    "\nmultiple definitions of '%s' %d\n" sname
		    te_info.T.m_id
	  in ([], None, teid, true)
	end	     	
	  
    | I.Function_type proto ->
	begin
	  let scopes = EO.get_top_scope_name env
	  in
	  let convert_param_type_list param_type_list id_opt_list =
	    if (List.length param_type_list != List.length id_opt_list) then
	      camlp4_macro_exception "type and param mismatch"
	    else
	      let param_teids = 
		List.fold_left
		  (fun lst c_type -> 
		    let ty = fst(eval_static_type_te cmpstate c_type)
		    in
		    let ty = 
		      if TO.is_array_typ ty then TO.ptr_of (TO.elmt_of ty)
		      else ty
		    in 
		    if TO.is_void_typ ty then
		      lst
		    else
		      lst @ [snd ty]
		  ) [] param_type_list
	      and in_param_ids =
		List.fold_left 
		  (fun l v -> 
		    match v with
		      | None -> l
		      | Some v -> l @ [v]
		  ) [] id_opt_list
	      in
	      let in_param_ids_opt = 
		if List.length in_param_ids = List.length param_teids then 
		  Some 
		    (List.map2 (fun teid sname -> 
		      fst (mk_param_ce_info cmpstate (sname, (env.E.te_tbl, teid)))
		    ) param_teids in_param_ids)
		else None
	      in (param_teids, in_param_ids_opt)
	  in
	  let ret_te =
	    fst (eval_static_type_te cmpstate proto.I.return_type)
	  and ((param_teids, in_param_ids_opt), is_va_arg) = 
	    match proto.I.param_type with
	      | I.Param_type_fix param_type_list ->
		  (convert_param_type_list param_type_list 
		    proto.I.formal_param, false)
	      | I.Param_type_va param_type_list -> 
		  (convert_param_type_list param_type_list 
		    proto.I.formal_param, true)
	      | I.Param_list l -> assert false
	  in
	  let abs_fun_teid = 
	    TO.add_abs_fun_type env.E.te_tbl 
	      {
		T.aft_ret_teid = snd ret_te;
		T.aft_param_teids = param_teids;
		T.aft_va_arg = is_va_arg;
		T.aft_hidden_ret_param = None;
		T.aft_muton_param_pos = [];
	      }
	  in
	  let teid = match in_param_ids_opt with
	    | Some in_param_ids -> 
		TO.add_crt_fun_te env.E.te_tbl ~abs_fun_teid ~in_param_ids
	    | None -> abs_fun_teid
	  in
	  ([], None, teid, false)
	end

    | I.Function_type_ex (proto, decl_list) ->
	begin
	  let scopes = EO.get_top_scope_name env
	  and ret_te =
	    fst(eval_static_type_te cmpstate proto.I.return_type)
	  in
	  let teid = match proto.I.param_type with
	    | I.Param_type_fix param_type_list -> assert false
	    | I.Param_type_va param_type_list -> assert false
	    | I.Param_list id_list -> 
		begin
		  if (List.length decl_list != List.length id_list) then
		    camlp4_macro_exception "type and param mismatch";
		  let hashtbl = StringHashtbl.create 17
		  in
		  Safe_list.iter 
		    (fun (typ, str) -> 
		      StringHashtbl.add hashtbl str typ) 
		    decl_list;
		  let (param_teids, in_param_ids) = 
		    Safe_list.split 
		      (Safe_list.map
			(fun id -> 
			  let c_type = StringHashtbl.find hashtbl id
			  in
			  let typ = fst (eval_static_type_te cmpstate c_type)
			  in
			  let (qname, _) = mk_param_ce_info cmpstate (id, typ) 
			    (*QNO.alloc_param_name scopes id*)
			  in (snd typ, qname)
			) id_list
		      )
		  in
		  let abs_fun_teid = 
		    TO.add_abs_fun_type env.EO.te_tbl 
		      {
			T.aft_ret_teid = snd ret_te;
			T.aft_param_teids = param_teids;
			T.aft_va_arg = false;
			T.aft_hidden_ret_param = None;
			T.aft_muton_param_pos = [];
		      }
		  in
		  TO.add_crt_fun_te
		    env.EO.te_tbl ~abs_fun_teid ~in_param_ids
		end
	  in
	  ([], None, teid, false)
	end
	  
    | I.Typename str -> 
	begin
	  match EO.find_te_info env str with
	    | Some te_info -> ([], None, te_info.T.m_id, false)
	    | None -> 
		camlp4_macro_exception "undefined type '%s'\n" str
	end
	  
    | I.Typeof expr -> 
	let (_, teid) = eval_expr_typ cmpstate expr
	in ([], None, teid, false)
	     
	     
    | I.Qualified_type (type_qualifier, c_type) ->
	begin
	  let (l, size_opt, teid, has_def) = eval_type_teid cmpstate c_type te_atts
	  in
	  let qualifier = match type_qualifier with
	    | I.Const -> T.QUAL_CONST
	    | I.Restrict -> T.QUAL_RESTRICT
	    | I.Volatile -> T.QUAL_VOLATILE
	  in
	  let qualified_teid = 
	    TO.add_qualified_type env.E.te_tbl 
	      { 
		T.m_qualified_teid = teid;
		T.m_qualifier = qualifier;
	      }
	  in (l, size_opt, qualified_teid, has_def)
	end
	  
    | I.Incomplete_qualified_type (c_type_opt, type_qualifier) ->
	begin
	  match c_type_opt with
	    | Some c_type -> 
		let (te, has_def) = eval_static_type_te cmpstate c_type
		in ([], None, snd te, has_def)
	    | None -> assert false
	end
	  
    | I.Incomplete_gnu_attribute_type (c_type_opt, type_qualifier) ->
	begin
	  assert false
	end
  in (l, size_opt, teid, has_def)
       
and compile_c_constant cmpstate (const:Ast_ca_expr.c_constant):(O.c_const040 * te) =
  let env = cmpstate.env
  in
  let (typ_id, cv) = match const with
    | Ast_ca_expr.Constant_value (teid, cval_ext) -> (teid, cval_ext)
    | Ast_ca_expr.Constant_enumeration v -> 
	let ce_info = EO.find_ce_info env v
	in
	let i = QNO.enum_const ce_info.CE.ce_qname
	in
	(Mach.cint_id, Const_folding.cval_ext_of_cval 
	  (Const_folding.cint_cval_of_string (string_of_int i)))
  in (((env.EO.te_tbl, typ_id), cv), (env.EO.te_tbl, typ_id))
       
and evaluate_c_constant_expression_as_int cmpstate (expr:I.c_expr020):int =
  let c = evaluate_c_constant_expression cmpstate expr
  in
  match c with
    | O.Cconst (t,(v,s,sopt)) ->
	let v' = CA.cval_to_CINT v
	in CA.cval_to_int v'
    | _ -> assert false

and c2s s = O.COMPUTATION s
  
and make_param_cast_type c = 
  if TO.is_array_typ c or TO.is_xarray_typ c then
    let elmt_typ = TO.elmt_of c
    in
    TO.ptr_of elmt_typ
  else if TO.is_crt_function_typ c then
    let abs_fun_typ = TO.get_abs_fun_te c
    in TO.ptr_of abs_fun_typ
  else if TO.is_abs_function_typ c then
    TO.ptr_of c
  else
    c
      
and compile_c_expr020_fun cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.rval) = 
  match expr with
    | I.Variable id ->
	let ce_info = EO.find_ce_info cmpstate.env id
	in
	let te = ce_info.CE.ce_te
	in
	let ce = (cmpstate.env.E.ce_tbl, ce_info.CE.ce_id)
	in
	if QNO.is_enum ce_info.CE.ce_qname then
	  assert false
	else if TO.is_crt_function_typ te then
	  ([], O.Rfun ce)
	else if TO.is_abs_function_typ te then
	  ([], O.Rladdr (O.Nctn ce))
	else if TO.is_ptr_typ te then
	  let elmt_typ = TO.elmt_of te
	  in	
	  let elmt_typ = TO.stripoff_const_qualifier elmt_typ
	  in
	  if TO.is_crt_function_typ elmt_typ 
	    or TO.is_abs_function_typ elmt_typ then
	      ([], O.Rladdr (O.Nctn ce))
	  else
	    assert false
	else
	  assert false
    | _ -> 
	compile_c_expr020_rval cmpstate expr

and compute_rexpr_address cmpstate op e offset ptr_typ =
  let env = cmpstate.env
  in
  let ptr_typ = 
    if TO.is_ptr_typ ptr_typ then ptr_typ else 
      TO.ptr_of (TO.elmt_of ptr_typ)
  in
  let elmt_typ = TO.elmt_of ptr_typ
  in
  let base_typ = 
    if TO.is_bit_te elmt_typ then 
      TO.bit_type_base elmt_typ
    else
      elmt_typ
  in
  let base_ptr_typ = TO.ptr_of base_typ
  in
  let base_ptr_typ = TO.ptrize base_ptr_typ
  in
  let (l0, opd0) = rexpr_to_rval cmpstate (O.Cast (env.E.cptr_cuint_typ, e))    
  in
  let (l1, opd1) = rexpr_to_rval cmpstate
    (AEE.put_rexpr cmpstate.eenv (O.Binary_arithm (op, opd0, offset)) 
      env.E.cptr_cuint_typ)    
  in
  (l0 @$ l1, O.Cast (base_ptr_typ, opd1))
    
and compute_cexpr_address cmpstate op e offset ptr_typ =
  let env = cmpstate.env
  in
  let ptr_typ = 
    if TO.is_ptr_typ ptr_typ then ptr_typ else 
      TO.ptr_of (TO.elmt_of ptr_typ)
  in
  let elmt_typ = TO.elmt_of ptr_typ
  in
  let base_typ = 
    if TO.is_bit_te elmt_typ then 
      TO.bit_type_base elmt_typ
    else
      elmt_typ
  in
  let base_ptr_typ = TO.ptr_of base_typ
  in
  let base_ptr_typ = TO.ptrize base_ptr_typ
  in
  let opd0 = O.CCast (TO.ptr_of env.E.char_typ, e)    
  in
  let opd1 = AEE.put_cexpr cmpstate.eenv 
    (O.CBinary_arithm (op, opd0, offset)) env.E.cptr_cuint_typ    
  in O.CCast (base_ptr_typ, opd1)

and compute_address cmpstate (base:O.rval) (offset:O.rval) ptr_typ = 
  let env = cmpstate.env
  in
  let cptr_cuint_typ = env.EO.cptr_cuint_typ
  in
  let (l0, base_ptr) = rexpr_to_rval cmpstate
    (O.Cast (cptr_cuint_typ, base))    
  in
  let (l1, addr_rval) = 
    rexpr_to_rval cmpstate
      (AEE.put_rexpr cmpstate.eenv 
	(O.Binary_arithm (S.Add, base_ptr, offset)) cptr_cuint_typ)
  in (l0 @$ l1, O.Cast (ptr_typ, addr_rval))

       
and compute_constant_address cmpstate (base:O.cexpr) (offset:O.cexpr) ptr_typ = 
  let env = cmpstate.env
  in
  let base_ptr = O.CCast (env.EO.cptr_cuint_typ, base)    
  in
  let addr_expr = AEE.put_cexpr cmpstate.eenv 
    (O.CBinary_arithm (S.Add, base_ptr, offset)) env.EO.cptr_cuint_typ
  in O.CCast (ptr_typ, addr_expr)

and compile_c_expr020_lval cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.lval) = 
  let env = cmpstate.env
  in
  let new_expr = match expr with
    | I.Comma (se0, se1) -> 
	let (se0', _) = compile_c_expr020_rexpr cmpstate se0
	and (se1', v) = compile_c_expr020_lval cmpstate se1
	in (se0' @$ se1', v)
	     
    | I.Constant c_constant -> assert false
    | I.String cstr -> assert false
    | I.Call (se0, expr_list) -> assert false
    | I.Macro_va_start (se0, se1) -> assert false
    | I.Macro_va_arg (se0, c_type) -> assert false
    | I.Macro_va_end se0 -> assert false
    | I.Builtin_types_compatible _ -> assert false
    | I.Builtin_constant_p _ -> assert false
    | I.Builtin_expect _ -> assert false
    | I.Gnu_block block -> assert false
    | I.Gnu_labeladdr block -> assert false
    | I.Variable id ->
	begin
	  let ce_info = EO.find_ce_info env id
	  in
	  let ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  and te = ce_info.CE.ce_te
	  in
	  let (l, lval) =
	    if QNO.is_enum ce_info.CE.ce_qname then
	      assert false
	    else if TO.is_array_typ te then
	      let var_typ = TO.ptr_of (TO.elmt_of te)
	      in
	      let var = EO.get_tmp_ce env var_typ
	      in
	      let lval = O.Lladdr (O.Nlbl var)
	      and rexpr = O.Rval (O.Rvct ce)
	      in ([c2s (O.Assign (lval, rexpr))], O.Lladdr (O.Nctn var))
	    else if TO.is_crt_function_typ te
	      (* or TO.is_abs_function_typ expr_typ*)
	    then
	      assert false
	    else
	      ([], mk_lhs ce)
	  in (l, lval)
	end
	  
    | I.Memberof (se0, f) ->
	begin
	  let (l, lval) = compile_memberof_lval_or_rval cmpstate (se0, f)
	  in 
	  match lval with
	    | LVAL lval -> (l, lval)
	    | _ -> assert false
	end
	  
    | I.Memberof_ptr (se0, f) ->
	begin
	  let (l, lval) = compile_memberof_ptr_lval_or_rval cmpstate (se0, f)
	  in 
	  match lval with
	    | LVAL lval -> (l, lval)
	    | _ -> assert false
	end
	  
    | I.Indexof (se0, se1) -> 
	begin
	  let (l, lval) = compile_indexof_lval_or_rval cmpstate (se0, se1)
	  in 
	  match lval with
	    | LVAL lval -> (l, lval)
	    | _ -> assert false
	end
	  
    | I.Binary_arithm (bop, se0, se1) -> assert false
    | I.Binary_predicate (brel, se0, se1) -> assert false
    | I.Binary_logic (bop, se0, se1) -> assert false
    | I.Unary_arithm (uop, se0) -> assert false
    | I.Logic_not se0 -> assert false
    | I.Sizeof_expr _ 
    | I.Sizeof_type _ 
    | I.Alignof_expr _
    | I.Alignof_type _ -> assert false
    | I.Cast (c_type_name, se0) -> assert false
    | I.Cast_init (c_type_name, inits) -> assert false
    | I.Assign (se0, se1) -> assert false
    | I.Pre_decr se0
    | I.Pre_incr se0 
    | I.Post_decr se0
    | I.Post_incr se0 -> assert false
    | I.Assign_arithm (bop, se0, se1) -> assert false
    | I.Memof se0 -> 
	begin
	  let (l, rexpr) = compile_c_expr020_rexpr cmpstate se0
	  in	
	  let rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
	  in
	  let ptr_typ = rexpr_O_re_typ
	  in
	  let elmt_typ = TO.elmt_of ptr_typ
	  in
	  let (lx, var) = match rexpr with
	    | O.Rval rv -> 
		begin
		  match rv with
		    | O.Rladdr (O.Nctn id) -> ([], id)
		    | O.Rladdr laddr -> assert false
		    | O.Rreg _
		    | O.Rvct _
		    | O.Rindir _ ->
			let var = (EO.get_tmp_ce env ptr_typ)
			in
			(normalized_assign_stmts cmpstate
			  (O.Lladdr (O.Nlbl var), rexpr), var)
		    | O.Rfun _ -> assert false
		    | O.Rconst _ -> assert false
		    | O.Rsizeof _ -> assert false
		    | O.Ralignof _ -> assert false
		    | O.Rvoid -> assert false
		    | O.Rcode_label _ -> assert false
		    | O.Rbyte_value _ -> assert false
		    | O.Rcexpr _ -> assert false
		end
		  
	    | _ -> 
		begin
		  let var = (EO.get_tmp_ce env ptr_typ)
		  in
		  (normalized_assign_stmts cmpstate
		    (O.Lladdr (O.Nlbl var), rexpr), var)
		end
	  in
	  (l @$ lx, O.Lladdr (O.Nctn var))
	end
	  
    | I.Addrof se0 -> assert false
    | I.Question _ ->
	let (l, rexpr) = compile_c_expr020_addr cmpstate expr
	in
	let rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
	in
	let ptr_typ = rexpr_O_re_typ
	in
	let elmt_typ = TO.elmt_of ptr_typ
	in
	let var = (EO.get_tmp_ce env ptr_typ)
	in
	(l @$ 
	  (normalized_assign_stmts cmpstate
	    (O.Lladdr (O.Nlbl var), rexpr)), O.Lladdr (O.Nctn var))
  in new_expr

and compile_memberof_lval_or_rval cmpstate (se0, field)
    :(O.c_stmt010 list * lval_or_rval) = 
  let env = cmpstate.env
  in
  let (l0, base_addr_rexpr) = compile_c_expr020_addr_rval cmpstate se0
  in
  let base_addr_rexpr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv
    base_addr_rexpr
  in
  let size_typ = env.EO.size_typ
  in
  let base_typ = TO.elmt_of base_addr_rexpr_O_rv_typ
  in
  let field_typ = TO.field_typ base_typ field
  and offset = TO.field_offset base_typ field
  in
  let offset_rval = 
    (mk_const_rval size_typ (string_of_csize offset))
  in
  if TO.is_array_typ field_typ || TO.is_xarray_typ field_typ then
    let ptr_typ = TO.ptr_of (TO.elmt_of field_typ)
    in
    let (l1, rexpr) = compute_address 
      cmpstate base_addr_rexpr offset_rval ptr_typ
    in
    let (l2, rval) = rexpr_to_rval cmpstate rexpr
    in (l0 @$ l1 @$ l2, RVAL rval)
  else
    let ptr_typ = TO.ptr_of field_typ
    in
    let (l1, rexpr) = compute_address 
      cmpstate base_addr_rexpr offset_rval ptr_typ
    in
    let (l2, rval) = rexpr_to_rval cmpstate rexpr
    in
    let (l3, lval) = mk_rval_lval cmpstate rval
    in (l0 @$ l1 @$ l2 @$ l3, LVAL lval)

and compile_memberof_ptr_lval_or_rval cmpstate (se0, field)
    :(O.c_stmt010 list * lval_or_rval) = 
  let env = cmpstate.env
  in
  let (l0, rexpr) = compile_c_expr020_rval cmpstate se0
  and size_typ = env.EO.size_typ
  in
  let rexpr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rexpr
  in
  let base_vtyp = TO.elmt_of rexpr_O_rv_typ
  in
  let (field_typ, offset) = TO.field_info base_vtyp field
  in
  let offset_rval = 
    (mk_const_rval size_typ (string_of_csize offset))
  in
  if TO.is_array_typ field_typ || TO.is_xarray_typ field_typ then
    let ptr_typ = TO.ptr_of (TO.elmt_of field_typ)
    in
    let (l1, rexpr) = compute_address 
      cmpstate rexpr offset_rval ptr_typ
    in
    let (l2, rval) = rexpr_to_rval cmpstate rexpr
    in (l0 @$ l1 @$ l2, RVAL rval)
  else
    let ptr_typ = TO.ptr_of field_typ
    in
    let (l1, rexpr) = compute_address 
      cmpstate rexpr offset_rval ptr_typ
    in
    let (l2, rval) = rexpr_to_rval cmpstate rexpr
    in
    let (l3, lval) = mk_rval_lval cmpstate rval
    in (l0 @$ l1 @$ l2 @$ l3, LVAL lval)

and compile_indexof_lval_or_rval cmpstate (se0, se1) 
    :(O.c_stmt010 list * lval_or_rval) = 
  let env = cmpstate.env
  in
  let (l0, base_addr) = match se0 with
    | I.Variable id ->
	let ce_info = EO.find_ce_info env id
	in
	let te = ce_info.CE.ce_te
	and ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	in
	let addr = 
	  if QNO.is_enum ce_info.CE.ce_qname then
	    assert false
	  else if TO.is_array_typ te then
	    O.Rvct ce
	  else if TO.is_xarray_typ te then
	    O.Rladdr (O.Nctn ce)
	  else 
	    O.Rladdr (O.Nctn ce)
	in ([], addr)
    | _ -> 
	compile_c_expr020_rval cmpstate se0
  in
  let (l1, idx) = compile_c_expr020_rval cmpstate se1
  and size_typ = env.EO.size_typ
  in
  let base_addr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv base_addr
  in
  let elmt_typ = TO.elmt_of base_addr_O_rv_typ
  in
  let elmt_size = TO.static_sizeof elmt_typ
  in
  let size_rexpr = 
    (mk_const_rval size_typ (string_of_csize elmt_size))
  in  
  (*
  let offset_rexpr = 
    AEE.put_rexpr cmpstate.eenv 
      (O.Binary_arithm (S.Mul, idx, size_rexpr)) size_typ
  in
  *)
  let (l2, offset_rval) = rexpr_to_rval cmpstate (O.Binary_arithm (S.Mul, idx, size_rexpr))
  in
  let (l2', offset_rval) = 
    rexpr_to_rval cmpstate (mk_cast cmpstate offset_rval size_typ)
  in
  if TO.is_array_typ elmt_typ then
    let ptr_typ = TO.ptr_of (TO.elmt_of elmt_typ)
    in
    let (l3, rexpr) = compute_address 
      cmpstate base_addr offset_rval ptr_typ
    in
    let (l4, rval) = rexpr_to_rval cmpstate rexpr
    in (l0 @$ l1 @$ l2 @$ l2' @$ l3 @$ l4, RVAL rval)
  else
    let ptr_typ = TO.ptr_of elmt_typ
    in
    let (l3, rexpr) = compute_address 
      cmpstate base_addr offset_rval ptr_typ
    in
    let (l4, rval) = rexpr_to_rval cmpstate rexpr
    in
    let (l5, lval) = mk_rval_lval cmpstate rval
    in (l0 @$ l1 @$ l2 @$ l2' @$ l3 @$ l4 @$ l5, LVAL lval)

and mark_addr_is_taken cmpstate e = 
  let env = cmpstate.env
  in
  match e with
    | I.Variable id -> 
	let ce_info = EO.find_ce_info env id
	in ce_info.CE.ce_addr_is_taken <- true
    | I.Memberof (base, _) -> mark_addr_is_taken cmpstate base
    | I.Indexof (base, _) -> mark_addr_is_taken cmpstate base
    | _ -> ()
	
and compile_c_expr020_addr cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.rexpr) = 
  let env = cmpstate.env
  in
  let new_expr = match expr with
    | I.Comma (se0, se1) -> 
	let (l0, _) = compile_c_expr020_rexpr cmpstate se0
	and (l1, v) = compile_c_expr020_addr cmpstate se1
	in (l0 @$ l1, v)
	     
    | I.Constant c_constant -> assert false
    | I.String cstr -> assert false
    | I.Call (se0, expr_list) ->
	let (l, rexpr) = compile_c_expr020_rexpr cmpstate expr
	in
	let rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
	in
	let var = (EO.get_tmp_ce env rexpr_O_re_typ)
	and stack_loc = (EO.add_fresh_storage env rexpr_O_re_typ false)
	in
	let lval = O.Lladdr (O.Nlbl var)
	and rval = O.Rladdr (O.Nctn var)
	and stack_loc = O.Lladdr (O.Nlbl stack_loc)
	in
	(l @$ (normalized_assign_stmts cmpstate (lval, rexpr)) @$ 
	  (normalized_assign_stmts cmpstate (stack_loc, O.Rval rval)), 
	O.Rval (addrof_lval stack_loc))
	  
    | I.Macro_va_start (se0, se1) -> assert false
    | I.Macro_va_arg (se0, c_type) -> assert false
    | I.Macro_va_end se0 -> assert false
    | I.Builtin_types_compatible _ -> assert false
    | I.Builtin_constant_p _ -> assert false
    | I.Builtin_expect _ -> assert false
    | I.Gnu_block block -> assert false
    | I.Gnu_labeladdr str -> assert false
    | I.Variable id ->
	begin
	  let ce_info = EO.find_ce_info env id
	  in
	  let te = ce_info.CE.ce_te
	  and ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  let rexpr = 
	    if QNO.is_enum ce_info.CE.ce_qname then
	      assert false
	    else if TO.is_array_typ te then
	      O.Rval (O.Rladdr (O.Nlbl ce))
	    else if TO.is_crt_function_typ te
	      (* or TO.is_abs_function_typ decl_typ*)
	    then
	      O.Rval (O.Rfun ce)
	    else
	      O.Rval (O.Rladdr (O.Nlbl ce))
	  in ([], rexpr)
	end
	  
    | I.Memberof (se0, field) ->
	let (l, base_addr_rexpr) = compile_c_expr020_addr_rval cmpstate se0
	in
	let base_addr_rexpr_O_rv_typ = 
	  AEE.get_rval cmpstate.env cmpstate.eenv base_addr_rexpr
	in
	let size_typ = env.EO.size_typ
	in
	let base_typ = TO.elmt_of base_addr_rexpr_O_rv_typ
	in
	let field_typ = TO.field_typ base_typ field
	and offset = TO.field_offset base_typ field
	in
	let offset_rval = 
	  (mk_const_rval size_typ (string_of_csize offset))
	in
	let ptr_typ = TO.ptr_of field_typ
	in
	let (l0, rexpr) = compute_address 
	  cmpstate base_addr_rexpr offset_rval ptr_typ
	in (l @$ l0, rexpr)

    | I.Memberof_ptr (se0, field) ->
	let (l, rexpr) = compile_c_expr020_rval cmpstate se0
	and size_typ = env.EO.size_typ
	in
	let rexpr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rexpr
	in
	let base_typ = TO.elmt_of rexpr_O_rv_typ
	in
	let field_typ = TO.field_typ base_typ field
	and offset = TO.field_offset base_typ field
	in
	let offset_rval = 
	  (mk_const_rval size_typ (string_of_csize offset))
	in
	let ptr_typ = TO.ptr_of field_typ
	in
	let (l0, rexpr) = compute_address cmpstate rexpr offset_rval ptr_typ
	in (l @$ l0, rexpr)
	     
    | I.Indexof (se0, se1) ->
	let (l0, base_addr) = 
	  match se0 with
	    | I.Variable id ->
		let ce_info = EO.find_ce_info env id
		in
		let te = ce_info.CE.ce_te
		and ce = (env.E.ce_tbl, ce_info.CE.ce_id)
		in
		let addr = 
		  if QNO.is_enum ce_info.CE.ce_qname then
		    assert false
		  else if TO.is_array_typ te then
		    O.Rvct ce
		  else if TO.is_xarray_typ te then
		    O.Rladdr (O.Nctn ce)
		  else 
		    O.Rladdr (O.Nctn ce)
		in
		([], addr)
	    | _ -> 
		compile_c_expr020_rval cmpstate se0
	in
	let (l1, idx) = compile_c_expr020_rval cmpstate se1
	and size_typ = env.EO.size_typ
	in
	let base_addr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv base_addr
	in
	let elmt_typ = TO.elmt_of base_addr_O_rv_typ
	in
	let elmt_size = TO.static_sizeof elmt_typ
	in
	let size_rexpr = 
	  (mk_const_rval size_typ (string_of_csize elmt_size))
	in
	let offset_rexpr = 
	  AEE.put_rexpr cmpstate.eenv 
	    (O.Binary_arithm (S.Mul, idx, size_rexpr)) size_typ
	in
	let (l2, offset_rval) = rexpr_to_rval cmpstate offset_rexpr
	in
	let ptr_typ = TO.ptr_of elmt_typ
	in
	let (l3, rexpr) = compute_address 
	  cmpstate base_addr offset_rval ptr_typ
	in (l0 @$ l1 @$ l2 @$ l3, rexpr)
	     
    | I.Binary_arithm (bop, se0, se1) -> assert false
    | I.Binary_predicate (brel, se0, se1) -> assert false
    | I.Binary_logic (bop, se0, se1) -> assert false
    | I.Unary_arithm (uop, se0) -> assert false
    | I.Logic_not se0 -> assert false
    | I.Sizeof_expr _ 
    | I.Sizeof_type _ 
    | I.Alignof_expr _
    | I.Alignof_type _ -> assert false
    | I.Cast (c_type_name, se0) -> 
	begin
	  let ty = compile_c_type cmpstate c_type_name
	  in
	  let (l0, rexpr) = compile_c_expr020_addr cmpstate se0
	  in
	  let (l1, rval) = rexpr_to_rval cmpstate rexpr
	  in (l0 @$ l1, O.Cast (ty, rval))
	end

    | I.Cast_init (c_type_name, inits) -> 
	begin
	  let cast_typ = compile_c_type cmpstate c_type_name
	  in		
	  let (l1, cast_typ, var) = 
	    let memory = alloc_top_c_initializer I.Auto cast_typ
	    in
	    let _ = 
	      compile_top_c_initializer cmpstate 
		memory (I.Initializer_2 inits)
	    in
	    let cast_typ = 
	      match memory with
		| Typ_mem.Xarray (_,_,_, s) ->
		    let elmt_typ = TO.elmt_of cast_typ
		    in TO.array_of elmt_typ (csize_of_int !s)
		| _ -> cast_typ
	    in
	    let var = EO.add_fresh_storage env cast_typ false
	    in
	    (make_asign_init cmpstate (O.Lladdr (O.Nlbl var))
	      memory, cast_typ, var)
	  in
	  if TO.is_array_typ cast_typ then
	    (l1, O.Rval (O.Rvct var))
	  else
	    (l1, O.Rval (O.Rladdr (O.Nlbl var)))
	end
	  
    | I.Assign (se0, se1) -> assert false
    | I.Pre_decr se0
    | I.Pre_incr se0 
    | I.Post_decr se0
    | I.Post_incr se0 -> assert false
    | I.Assign_arithm (bop, se0, se1) -> assert false
    | I.Memof se0 -> compile_c_expr020_rexpr cmpstate se0
    | I.Addrof se0 -> assert false
    | I.Question (se0, None, se2) -> assert false
    | I.Question (se0, Some se1, se2) ->
	begin
	  let (l0, se0') = compile_c_expr020_rval cmpstate se0
	  and (l1, se1_addr) = compile_c_expr020_addr cmpstate se1
	  and (l2, se2_addr) = compile_c_expr020_addr cmpstate se2
	  in
	  let se1_addr_O_re_typ = 
	    AEE.get_rexpr cmpstate.env cmpstate.eenv se1_addr
	  in
	  let var = (EO.get_tmp_ce env se1_addr_O_re_typ)
	  in
	  let lval = O.Lladdr (O.Nlbl var)
	  in
	  (l0 @$ 
	    [O.IF (mk_true_cond se0', 
	    O.COMPOUND (Some "?", O.BLOCK ([],[], l1 @$ 
	      (normalized_assign_stmts cmpstate (lval, se1_addr)))), 
	    O.COMPOUND (Some "?", O.BLOCK ([],[], l2 @$ 
	      (normalized_assign_stmts cmpstate (lval, se2_addr)))))], 
	  O.Rval (fetch_rval_from_lval cmpstate lval))
	end
  in
  new_expr

and convertion cmpstate rval dest_typ = 
  let rexpr = mk_cast cmpstate rval dest_typ
  in
  let (l, rval) = rexpr_to_rval cmpstate rexpr
  in (l, rval)
       
and make_asign cmpstate (lval:O.lval) (rexpr:O.rexpr) =
  let env = cmpstate.env
  in
  let (l, rexpr') = 
    let rval = fetch_rval_from_lval cmpstate lval
    in      
    let rval_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rval
    and rexpr_O_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
    in
    if TO.is_bit_te rval_O_rv_typ then
      let bit_typ_info = TO.get_bit_type_decl rval_O_rv_typ
      in
      let fieldright = 
	let v = CA.cuint_cval_of_string 
	  (string_of_csize bit_typ_info.T.m_bit_right)
	in
	CA.cval_ext_of_cval v
      and (mask_teid, fieldmask, mask, bnot_mask) = 
	TO.bit_type_masks bit_typ_info
      in
      let mask_typ = (env.E.te_tbl, mask_teid)
      and cuint_typ = env.EO.cuint_typ
      in
      let (l1, rexpr) = rexpr_to_rval cmpstate rexpr
      in
      let (ct0, ct1, dt) = 
	Tent_op.bin_arithm_typ S.Band rval_O_rv_typ mask_typ
      in
      let (l2, se0'') = cast_expr cmpstate rval ct0 
      and (l3, se1'') = cast_expr cmpstate (O.Rconst (mask_typ, bnot_mask)) ct1
      in
      let (l4, rval0) = 
	rexpr_to_rval cmpstate (O.Binary_arithm (S.Band, se0'', se1''))
      in
      let (ct0, ct1, dt) = 
	Tent_op.bin_arithm_typ S.Shl mask_typ (*rexpr_O_typ*) cuint_typ
      in
      let (l5, se2) = cast_expr cmpstate rexpr ct0
      and (l6, se3) = cast_expr cmpstate (O.Rconst (cuint_typ, fieldright)) ct1
      in
      (*
      let (l0'', rval0') = convertion cmpstate rexpr mask_typ
      in
      *)
      let (l7, rval1) = rexpr_to_rval cmpstate (O.Binary_arithm (S.Shl, se2, se3))
      in
      let (ct0, ct1, dt) = 
	Tent_op.bin_arithm_typ 
	  S.Band (AEE.get_rval cmpstate.env cmpstate.eenv rval1) mask_typ
      in
      let (l8, se4) = cast_expr cmpstate rval1 ct0
      and (l9, se5) = cast_expr cmpstate (O.Rconst (mask_typ, mask)) ct1
      in
      let (l10, rval2) = 
	rexpr_to_rval cmpstate (O.Binary_arithm (S.Band, se4, se5))
      in 
      let (ct0, ct1, dt) = 
	Tent_op.bin_arithm_typ
	  S.Bor 
	  (AEE.get_rval cmpstate.env cmpstate.eenv rval0) 
	  (AEE.get_rval cmpstate.env cmpstate.eenv rval2)
      in
      let (l11, se6) = cast_expr cmpstate rval0 ct0
      and (l12, se7) = cast_expr cmpstate rval2 ct1
      in 
      (l1 @$ l2 @$ l3  @$ l4 @$ l5 @$ l6 @$ l7 @$ l8 @$ l9 @$ l10 @$ l11 @$ l12,
      O.Binary_arithm (S.Bor, se6, se7))
    else
      let (lx, rexpr) = rexpr_to_rval cmpstate rexpr
      in
      let lval_O_lv_typ = AEE.get_lval cmpstate.env cmpstate.eenv lval
      in
      let rexpr = mk_cast cmpstate rexpr lval_O_lv_typ
      in
      let (ly, rexpr) = rexpr_to_rval cmpstate rexpr
      in
      (lx @ ly, O.Rval rexpr)
  in
  l @$ (normalized_assign_stmts cmpstate (lval, rexpr'))
    
and get_bitfield_rval cmpstate (rval:O.rval) = 
  let (l, rexpr) = get_bitfield_rexpr cmpstate rval
  in
  let (l1, rval) = rexpr_to_rval cmpstate rexpr
  in (l @$ l1, rval)
       
and get_bitfield_rexpr cmpstate (rval:O.rval) =
  let env = cmpstate.env
  in
  match rval with
    | O.Rindir str ->
	begin
	  let rval_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rval
	  in
	  let vtyp = rval_O_rv_typ
	  in
	  if not (TO.is_bit_te vtyp) then
	    ([], O.Rval rval)
	  else
	    begin
	      let bit_typ_info = TO.get_bit_type_decl vtyp
	      in
	      let shift_left = 
		let v = CA.cuint_cval_of_string 
		  (string_of_csize bit_typ_info.T.m_bit_left)
		in CA.cval_ext_of_cval v
	      and shift_right = 
		let v = CA.cuint_cval_of_string 
		  (string_of_csize 
		    (bit_typ_info.T.m_bit_right +$ bit_typ_info.T.m_bit_left))
		in CA.cval_ext_of_cval v
	      in
	      let base_typ = (env.E.te_tbl, bit_typ_info.T.m_bit_base_teid)
	      and cuint_typ = env.E.cuint_typ
	      in
	      let (l, op0) = 
		rexpr_to_rval cmpstate 
		  (AEE.put_rexpr cmpstate.eenv 
		    (O.Binary_arithm
		      (S.Shl, rval, O.Rconst (cuint_typ, shift_left))) base_typ)
	      in
	      (l, (AEE.put_rexpr cmpstate.eenv
		(O.Binary_arithm 
		  (S.Shr,op0, O.Rconst (cuint_typ, shift_right))) base_typ))
	    end
	end
    | _ -> ([], O.Rval rval)
	
and dref_rval cmpstate rval = 
  let env = cmpstate.env
  in
  let rval_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rval
  in
  let dt = TO.elmt_of rval_O_rv_typ
  in
  let (l, e) = match rval with
    | O.Rreg s -> 
	let v = (EO.get_tmp_ce env rval_O_rv_typ)
	in
	(normalized_assign_stmts cmpstate
	  (O.Lladdr (O.Nlbl v), O.Rval rval), O.Rindir v)

    | O.Rladdr (O.Nlbl s) -> 
	([], O.Rladdr (O.Nctn s))

    | O.Rindir string -> 
	let v = (EO.get_tmp_ce env rval_O_rv_typ)
	in
	(normalized_assign_stmts cmpstate
	  (O.Lladdr (O.Nlbl v),O.Rval rval), O.Rindir v)
	  
    | O.Rfun string -> assert false
    | O.Rvct string -> 
	let ptr_typ = TO.ptr_of dt
	in
	let v = (EO.get_tmp_ce env ptr_typ)
	in
	(normalized_assign_stmts cmpstate
	  (O.Lladdr (O.Nlbl v), O.Rval rval), O.Rladdr (O.Nctn v))

    | O.Rladdr (O.Nctn string) -> 
	([], O.Rindir string)
	  
    | O.Ralignof _ 
    | O.Rsizeof _ -> assert false
    | O.Rconst c_const040 -> assert false
    | O.Rcode_label _ -> assert false
    | O.Rbyte_value _ -> assert false
    | O.Rvoid -> assert false
    | O.Rcexpr _ -> assert false
  in (Safe_list.map (fun v -> v) l, e)
       
and fetch_rval_from_lval cmpstate lval = 
  let lval_0_lv_typ =  AEE.get_lval cmpstate.env cmpstate.eenv lval
  in
  match lval with
    | O.Lladdr (O.Nlbl s) -> O.Rladdr (O.Nctn s)
    | O.Lladdr (O.Nctn s) -> O.Rindir s
    | O.Lreg s -> O.Rreg s
	
and addrof_lval lval:O.rval = 
  match lval with
    | O.Lladdr (O.Nlbl s) -> 
	let qname = CEO.qname_of s
	in
	assert (not (QNO.is_register qname));
	O.Rladdr (O.Nlbl s)
	  
    | O.Lladdr (O.Nctn s) -> O.Rladdr (O.Nctn s)
	  
    | O.Lreg _ -> assert false

and addrof_rval e =
  match e with
    | O.Rindir s -> O.Rladdr (O.Nctn s)
    | O.Rladdr (O.Nctn ce) -> O.Rladdr (O.Nlbl ce)
    | _ -> assert false
	
and addrof_rexpr e = 
  match e with
    | O.Rval rval -> (addrof_rval rval)
    | O.Cast (t, rval) -> (addrof_rval rval)
    | _ -> assert false
	
and voidexpr = O.Rval O.Rvoid
  
and mk_rval var typ = O.Rladdr (O.Nctn var)
    
and mk_lval var typ = O.Lladdr (O.Nlbl var)

and mk_rval_lval cmpstate rval =
  match rval with
    | O.Rladdr c -> ([], O.Lladdr c)
    | O.Rreg str -> assert false
    | O.Rindir _ 
    | O.Rfun _
    | O.Rvct _
    | O.Rconst _
    | O.Rsizeof _
    | O.Ralignof _
    | O.Rcode_label _ -> assert false
    | O.Rvoid -> assert false
    | O.Rbyte_value _ -> assert false
    | O.Rcexpr _ -> assert false
	
and mk_call_expr cmpstate rval rexprs ret_ty = 
  let env = cmpstate.env
  in
  if TO.is_void_typ ret_ty then
    ([O.CALL (None, rval, rexprs)], O.Rvoid)
  else
    let var = (EO.get_tmp_ce cmpstate.env ret_ty)
    in
    ([O.CALL 
      (Some (O.Lladdr (O.Nlbl var)), rval, rexprs)], O.Rladdr (O.Nctn var))

and make_bin_expr cmpstate bop (rexpr1:O.rval) (rexpr2:O.rval) = 
  let env = cmpstate.env
  in
  let se0' = rexpr1 
  and se1' = rexpr2
  in
  let se0_O_rv_typ' = AEE.get_rval cmpstate.env cmpstate.eenv se0'
  and se1_O_rv_typ' = AEE.get_rval cmpstate.env cmpstate.eenv se1'
  in
  let (ct0, ct1, rt) = 
    TO.bin_arithm_typ bop se0_O_rv_typ' se1_O_rv_typ'
  in
  let (l0, se0'') = cast_expr cmpstate se0' ct0 
  and (l1, se1'') = cast_expr cmpstate se1' ct1
  in
  let se0_O_rv_typ'' = AEE.get_rval cmpstate.env cmpstate.eenv se0''
  and se1_O_rv_typ'' = AEE.get_rval cmpstate.env cmpstate.eenv se1''
  in
  let (l2, re) = 
    match bop with
      | S.Add ->
	  begin
	    if ((TO.is_ptr_typ se0_O_rv_typ'') or (TO.is_array_typ se0_O_rv_typ'')) 
 	      & not (TO.is_ptr_typ se1_O_rv_typ'' or TO.is_array_typ se1_O_rv_typ'')
	    then
	      let elmt_typ = TO.elmt_of se0_O_rv_typ''
	      in
	      let elmt_siz = TO.static_sizeof elmt_typ
	      in
	      let size_expr = 
		(mk_const_rval (env.E.size_typ) 
		  (string_of_csize elmt_siz))
	      in
	      let offset_expr = 
		AEE.put_rexpr cmpstate.eenv 
		  ((O.Binary_arithm (S.Mul, se1'', size_expr))) env.E.size_typ
	      in
	      let (l0, offset_rval) = rexpr_to_rval cmpstate offset_expr
	      in
	      let (l1, e) = compute_rexpr_address 
		cmpstate bop se0'' offset_rval se0_O_rv_typ''
	      in
	      (l0 @$ l1, e)
	    else
	      ([], AEE.put_rexpr cmpstate.eenv
		(O.Binary_arithm (bop, se0'', se1'')) rt)
	  end
      | S.Sub ->
	  begin
	    if ((TO.is_ptr_typ se0_O_rv_typ'') or (TO.is_array_typ se0_O_rv_typ'')) 
 	      & not (TO.is_ptr_typ se1_O_rv_typ'' or TO.is_array_typ se1_O_rv_typ'')
	    then
	      let elmt_typ = TO.elmt_of se0_O_rv_typ''
	      in
	      let elmt_siz = TO.static_sizeof elmt_typ
	      in
	      let size_expr = 
		(mk_const_rval (env.E.size_typ) 
		  (string_of_csize elmt_siz))
	      in
	      let offset_expr = AEE.put_rexpr cmpstate.eenv 
		(O.Binary_arithm (S.Mul, se1'', size_expr)) env.E.size_typ
	      in
	      let (l0, offset_rval) = rexpr_to_rval cmpstate offset_expr
	      in
	      let (l1, e) = compute_rexpr_address 
		cmpstate bop se0'' offset_rval se0_O_rv_typ''
	      in
	      (l0 @$ l1, e)
	    else if TO.is_ptr_or_array_typ se0_O_rv_typ'' & TO.is_ptr_or_array_typ se1_O_rv_typ''
	    then
	      begin
		let elmt_typ0 = TO.elmt_of se0_O_rv_typ''
		and elmt_typ1 = TO.elmt_of se1_O_rv_typ''
		in
		assert (TO.compatible_unqual_tes elmt_typ0 elmt_typ1);
		let elmt_siz = TO.static_sizeof elmt_typ0
		in
		let size_expr = 
		  (mk_const_rval (env.E.size_typ) (string_of_csize elmt_siz))
		in
		let char_ptr0 = O.Cast (TO.ptr_of env.E.char_typ, se0'')
		and char_ptr1 = O.Cast (TO.ptr_of env.E.char_typ, se1'')
		in 
		let (l0, char_rval0) = rexpr_to_rval cmpstate char_ptr0
		and (l1, char_rval1) = rexpr_to_rval cmpstate char_ptr1
		in
		let sub_expr = AEE.put_rexpr cmpstate.eenv 
		  (O.Binary_arithm (bop, char_rval0, char_rval1)) rt
		in
		let (l2, sub_expr) = rexpr_to_rval cmpstate sub_expr
		in		  
		(l0 @ l1 @ l2, AEE.put_rexpr cmpstate.eenv
		  (O.Binary_arithm (S.Div, sub_expr, size_expr)) rt)
	      end
	    else
	      ([], AEE.put_rexpr cmpstate.eenv 
		(O.Binary_arithm (bop, se0'', se1'')) rt)
	  end
      | _ -> 
	  if ((TO.is_ptr_typ se0_O_rv_typ'') or 
	    (TO.is_array_typ se0_O_rv_typ'')) & 
	    not (TO.is_ptr_typ se1_O_rv_typ'' or 
	      TO.is_array_typ se1_O_rv_typ'')
	  then
	    assert false
	  else
	    ([], AEE.put_rexpr cmpstate.eenv
	      (O.Binary_arithm (bop, se0'', se1'')) rt)
  in (l0 @$ l1 @$ l2, re)

and compile_c_expr020_rval cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.rval) = 
  let (l, rexpr) = compile_c_expr020_rexpr cmpstate expr
  in
  let (l0, rval) = rexpr_to_rval cmpstate rexpr
  in (l@$l0, rval)

and compile_c_expr020_addr_rval cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.rval) = 
  let (l, rexpr) = compile_c_expr020_addr cmpstate expr
  in
  let (l0, rval) = rexpr_to_rval cmpstate rexpr
  in (l@$l0, rval)

and cexpr_to_rexpr cexpr = O.Rval (O.Rcexpr cexpr)
    
and make_asign_init cmpstate (lhs:O.lval) (memory:c_tmp_initializer)
    :(O.c_stmt010 list) = 
  let env = cmpstate.env
  in
  let has_val = function
    | TMPStatic_init_none -> false
    | TMPDynamic_init_none -> false 
    | _ -> true
  in
  let l = match memory with
    | Typ_mem.Bits (c_typ, init_expr) ->
	begin
	  match !init_expr with
	    | TMPStatic_init c -> 
		make_asign cmpstate lhs (cexpr_to_rexpr c)
		  
	    | TMPDynamic_init (l0, rexpr) ->
		l0 @$ (make_asign cmpstate lhs rexpr)
		  
	    | TMPStatic_init_none -> []
	    | TMPDynamic_init_none -> []
	end
	  
    | Typ_mem.Null ->
	(normalized_assign_stmts cmpstate (lhs, 
	O.Rval (mk_const_rval (env.E.int_typ) "0")))
	  
    | Typ_mem.Scalar (c_typ, expr) ->
	begin
	  match !expr with
	    | TMPStatic_init c -> 
		(normalized_assign_stmts cmpstate (lhs, cexpr_to_rexpr c))
		  
	    | TMPDynamic_init (l0, rexpr) ->
		l0 @$ (normalized_assign_stmts cmpstate (lhs, rexpr))
		  
	    | TMPStatic_init_none
	    | TMPDynamic_init_none -> (** 20000801-4.c **)
		if TO.is_char_typ c_typ then
		  let zero = mk_const_rval (env.E.char_typ) "0"
		  in
		  (normalized_assign_stmts cmpstate (lhs, O.Rval zero))
		else if TO.is_wchar_typ c_typ then
		  let zero = mk_const_rval (env.E.wchar_t_typ) "0"
		  in
		  (normalized_assign_stmts cmpstate (lhs, O.Rval zero))
		else
		  []
	end
	  
    | Typ_mem.Array (c_typ, elmt_mem_array) ->
	begin
	  let lst = ref []
	  and elmt_typ = TO.elmt_of c_typ
	  in
	  let var_typ = TO.ptr_of elmt_typ
	  in
	  let var = (EO.get_tmp_ce env var_typ)
	  in
	  let size = TO.static_sizeof elmt_typ
	  in
	  Array.iteri 
	    (fun i (elmt_mem) -> 
	      let has_val = match elmt_mem with
		| Typ_mem.Scalar (t, c) ->
		    begin
		      match !c with
			| TMPStatic_init_none -> true
			| TMPDynamic_init_none -> false
			| _ -> true
		    end
		| _ -> true
	      in
	      if i = 958 then
		Debug.dummy_stop ();
	      
	      if has_val or !init_local_array_as_zero then
		begin
		  let offset = 
		    (mk_const_rval (env.E.int_typ) 
		      (string_of_csize ((csize_of_int i) *$ size)))
		  in
		  let (l, addr) = 
		    compute_address cmpstate (addrof_lval lhs)
		      offset (TO.ptr_of elmt_typ)
		  in
		  lst := 
		    !lst @$ l @$
		      (normalized_assign_stmts cmpstate
			(O.Lladdr (O.Nlbl var), addr)) @$
		      (make_asign_init 
			cmpstate
			(O.Lladdr (O.Nctn var)) elmt_mem);
		  recycle_tmp_ces cmpstate l [];
		end
	    ) elmt_mem_array;
	  !lst
	end

    | Typ_mem.Xarray (c_typ,_, elmt_list, xlen) -> 
	let lst = ref []
	and elmt_typ = TO.elmt_of c_typ
	in
	let var_typ = TO.ptr_of elmt_typ
	in
	let var = (EO.get_tmp_ce env var_typ)
	in
	let size = TO.static_sizeof elmt_typ
	in
	let len = !xlen - 1
	in
	let _ = for i = 0 to len do
	  begin
	    let offset =
	      (mk_const_rval (env.E.int_typ) 
		(string_of_csize ((csize_of_int i) *$ size)))
	    and (idx, elmt_mem) = Safe_list.nth !elmt_list i
	    in
	    assert (idx = i);
	    let (l, addr) = 
	      compute_address cmpstate (addrof_lval lhs)
		offset (TO.ptr_of elmt_typ)
	    in
	    lst := 
	      !lst @$ l @$
		(normalized_assign_stmts cmpstate
		  (O.Lladdr (O.Nlbl var), addr)) @$
		(make_asign_init cmpstate 
		  (O.Lladdr (O.Nctn var)) elmt_mem);
	    recycle_tmp_ces cmpstate l [];
	  end
	done
	in !lst
	     
    | Typ_mem.Struct (c_typ, field_array, ref_e) ->
	let lst = match !ref_e with
	  | TMPStatic_init_none -> assert false
	  | TMPDynamic_init_none ->
	      let lst = ref []
	      in
	      let _ = Array.iter 
		(fun (offset, field_opt, field_mem) -> 
		  match field_opt with
		    | Some field ->
			let field_typ = TO.field_typ c_typ field
			and offset = TO.field_offset c_typ field
			in
			let field_ptr_typ = TO.ptr_of field_typ
			in
			let var = (EO.get_tmp_ce 
			  env (TO.ptr_of field_typ))
			in
			let offset = 
			  (mk_const_rval (env.E.int_typ) 
			    (string_of_csize offset))
			in
			let (l, addr) = 
			  compute_address cmpstate 
			    (addrof_lval lhs) offset field_ptr_typ
			in
			lst := 
			  !lst @$ l @$
			    (normalized_assign_stmts cmpstate
			      (O.Lladdr (O.Nlbl var), addr)) @$
			    (make_asign_init cmpstate
			      (O.Lladdr (O.Nctn var)) field_mem);
			recycle_tmp_ces cmpstate l [];
		    | None -> ()
		) field_array
	      in !lst
		   
	  | TMPStatic_init _
	  | TMPDynamic_init _ ->
	      begin
		let (lst, e) = match !ref_e with
		  | TMPStatic_init e -> (ref [], cexpr_to_rexpr e)
		  | TMPDynamic_init (l, e) -> (ref l, e)
		  | _ -> assert false
		in
		lst := !lst @$ (normalized_assign_stmts cmpstate (lhs, e));
		!lst
	      end
	in lst
	     
    | Typ_mem.Union (c_typ, field_array, ref_e) ->
	let lst = match !ref_e with
	  | TMPStatic_init_none -> assert false
	  | TMPDynamic_init_none -> 
	      let comma_expr = ref []
	      and has_initialized = ref false (** only initialize once **)
	      in
	      let _ = Array.iter 
		(fun (offset, field_opt, field_mem) ->
		  if not !has_initialized then
		    match field_opt with
		      | Some field ->
			  if Typ_mem_op.rec_has_values has_val field_mem then
			    has_initialized := true;
			  
			  let offset = TO.field_offset c_typ field
			  and field_typ = TO.field_typ c_typ field
			  in
			  let field_ptr_typ = TO.ptr_of field_typ
			  in
			  let var = (EO.get_tmp_ce 
			    env (TO.ptr_of field_typ))
			  in
			  let offset =
			    (mk_const_rval (env.E.int_typ) 
			      (string_of_csize offset))
			  in
			  let (l, addr) = 
			    compute_address cmpstate 
			      (addrof_lval lhs) offset field_ptr_typ
			  in
			  comma_expr :=
			    !comma_expr @$ l @$
			      (normalized_assign_stmts cmpstate
				(O.Lladdr (O.Nlbl var), addr)) @$
			      (make_asign_init cmpstate 
				(O.Lladdr (O.Nctn var)) field_mem);
			  recycle_tmp_ces cmpstate l [];
		      | None -> ()
		) field_array
	      in !comma_expr
		   
	  | TMPStatic_init _ 
	  | TMPDynamic_init _ ->
	      let (lst, e) = match !ref_e with
		| TMPStatic_init e -> (ref [], cexpr_to_rexpr e)
		| TMPDynamic_init (l, e) -> (ref l, e)
		| _ -> assert false
	      in
	      let _ = lst := !lst @$ (normalized_assign_stmts cmpstate (lhs, e))
	      in !lst
	in lst
  in l

and normalized_assign_stmts cmpstate (lval, rexpr) = 
  let env = cmpstate.env
  in
  let lval_O_lv_typ = AEE.get_lval cmpstate.env cmpstate.eenv lval
  and rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
  in
  let t0 = lval_O_lv_typ
  and t1 = rexpr_O_re_typ
  in
  let t0 = TO.normalize_bit_typ t0
  and t1 = TO.normalize_bit_typ t1
  in
  let s0 = TO.static_sizeof t0
  and s1 = TO.static_sizeof t1
  in
  if (TO.is_native_assign_supported_typ t0) || 
    (TO.is_native_assign_supported_typ t1) then
      begin
	if (TO.is_cast_compatible t0 t1) then
	  [c2s (O.Assign (lval, rexpr))]
	else
	  let (l, rexpr_) = rexpr_to_rval cmpstate rexpr
	  in
	  let rexpr = mk_cast cmpstate rexpr_ t0 
	  in l @$ [c2s (O.Assign (lval, rexpr))]
      end
  else 
    let int_typ = (fst t0, Mach.cuint_id)
    in
    let expr0 = (addrof_lval lval)
    and expr1 = (addrof_rexpr rexpr)
    and csize = O.Cconst (TO.make_type_const int_typ (Int64.to_string s0))
    and _ = EO.find_ce_info env "memcpy"
    in [c2s (O.Memcpy (expr0, expr1, csize))]
	 
and compile_c_expr020_rexpr cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.rexpr) = 
  let env = cmpstate.env
  in
  let (l, new_expr) = match expr with
    | I.String str ->
	([], find_cstr_lit_expr cmpstate str)
	  
    | I.Comma (se0, se1) -> 
	let (l0, _) = compile_c_expr020_rexpr cmpstate se0
	and (l1, se1') = compile_c_expr020_rexpr cmpstate se1
	in (l0 @$ l1, se1')
	     
    | I.Constant c_constant ->
	let (c_constant', typ') = compile_c_constant cmpstate c_constant
	in ([], O.Rval (O.Rconst c_constant'))
	     
    | I.Call (se0, expr_list) -> 
	begin
	  let se0 = match se0 with
	    | I.Memof v -> v
	    | _ -> se0
	  in
	  let (l0, se0') = compile_c_expr020_fun cmpstate se0
	  in
	  let se0_O_rv_typ' = AEE.get_rval cmpstate.env cmpstate.eenv se0'
	  in
	  let fun_typ = TO.get_fun_typ se0_O_rv_typ'
	  in	    
	  let fun_typ' =
	    if TO.is_ptr_typ fun_typ then
	      TO.elmt_of fun_typ
	    else
	      fun_typ
	  in
	  let abs_fun_typ = TO.get_abs_fun_te fun_typ'
	  and hidden_ret_opt = ref None
	  in
	  let posHash = TO.get_abs_function_muton_pos abs_fun_typ
	  and lst = ref []
	  and idx = ref 0
	  in
	  let (arg_exprs, arg_typs) = 
	    Safe_list.split 
	      (Safe_list.map 
		(fun e ->
		  incr idx;
		  let (l, e') = 		    
		    try
		      let _ = IntHashtbl.find posHash (!idx - 1)
		      in compile_c_expr020_addr_rval cmpstate e
		    with
		      | Not_found -> 
			  compile_c_expr020_rval cmpstate e
		  in
		  let e_O_rv_typ' = AEE.get_rval cmpstate.env cmpstate.eenv e'
		  in
		  lst := !lst @$ l;
		  (e', e_O_rv_typ')
		) expr_list)
	  in
	  let arg_exprs = 
	    if TO.abs_function_has_hidden_ret abs_fun_typ then
	      let tmp_te = TO.abs_function_get_hidden_ret abs_fun_typ 
	      in
	      let ce = EO.add_fresh_storage env tmp_te false
	      in
	      let tmp_rval = O.Rladdr (O.Nlbl ce)
	      in 
	      let _ = hidden_ret_opt := Some (ce, tmp_te)
	      in tmp_rval::arg_exprs
	    else
	      arg_exprs
	  in
	  let (cvts, arg_expr_exs) = 
	    if not (TO.is_variant_fun abs_fun_typ) then
	      let param_typs = TO.get_in_param_types abs_fun_typ
	      in
	      let param_typs = 
		if param_typs = [] then 
		  Safe_list.map 
		    (fun e -> 
		      let e_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv e
		      in
		      if TO.is_integer_typ e_O_rv_typ then
			(env.E.int_typ)
		      else if TO.is_real_typ e_O_rv_typ then
			(env.E.int_typ)
		      else
			(env.E.int_typ)
		    ) arg_exprs 
		    (* todo double check this *)
		else
		  param_typs
	      in
	      let casts = ref []
	      in		
	      let params = 
		Safe_list.map2 
		  (fun e t -> 
		    let e_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv e
		    in
		    let e_ty = TO.get_atomic_equiv_typ 
		      (TO.unqualified_typ e_O_rv_typ)
		    and t_ty = TO.get_atomic_equiv_typ
		      (TO.unqualified_typ t)
		    in
		    if snd e_ty <> snd t_ty then
		      let (l, v) = 
			rexpr_to_rval cmpstate (mk_cast cmpstate e
			  (make_param_cast_type t_ty))
		      in
		      let _ = casts := !casts @$ l
		      in v
		    else
		      e
		  ) arg_exprs param_typs
	      in (!casts, params)
	    else
	      ([], arg_exprs)
	  in
	  let (l2, rval) = 
	    match !hidden_ret_opt with
	      | None -> 
		  mk_call_expr cmpstate se0' arg_expr_exs 
		    (TO.apply abs_fun_typ arg_typs)
	      | Some (ce, te) ->
		  let (l2, _) =
		    mk_call_expr cmpstate se0' arg_expr_exs 
		      (TO.apply abs_fun_typ arg_typs)
		  in (l2, O.Rladdr (O.Nctn ce))
	  in
	  (l0 @$ !lst @$ cvts @$ l2, O.Rval rval)
	end
	  
    | I.Macro_va_start (se0, se1) ->
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	and (l1, se1') = compile_c_expr020_rval cmpstate se1
	in
	(l0 @$ l1 @$ [c2s 
	  (O.Macro_va_start (se0', se1'))], voidexpr)
	  
    | I.Macro_va_arg (se0, c_type) -> 
	let (l0, rval) = compile_c_expr020_rval cmpstate se0
	and typ = compile_c_type cmpstate c_type
	in
	let var = (EO.get_tmp_ce env typ)
	in
	(l0 @$ 
	  [c2s (O.Macro_va_arg
	    (mk_lval var typ, rval, typ))], 
	O.Rval (mk_rval var typ))
	  
    | I.Macro_va_end se0 -> 
	let (l, se0) = compile_c_expr020_rval cmpstate se0
	in
	(l @$ [c2s (O.Macro_va_end se0)],  voidexpr)

    | I.Builtin_types_compatible (t0, t1) -> 
	let typ0 = compile_c_type cmpstate t0
	and typ1 = compile_c_type cmpstate t1
	in
	let v = TO.gnu_builtin_types_compatible typ0 typ1
	in
	let rval = 
	  O.Rconst 
	    (env.E.int_typ, Const_folding.cval_ext_of_cval 
	      (Const_folding.cint_cval_of_string (string_of_int v)))
	in ([], O.Rval rval)

    | I.Builtin_constant_p e -> 
	let rval = O.Rconst (gnu_builtin_constant_p cmpstate e)
	in ([], O.Rval rval)

    | I.Builtin_expect (se0, se1) ->
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	in (l0, O.Rval se0')
	     
    | I.Gnu_block block -> 
	let var = EO.get_tmp_ce env (env.E.non_typ)
	in
	let (stmt', typ') = compile_c_compound_stmt010 cmpstate block 
	in
	let (assign_stmt, rv_typ) = 
	  extract_c_compound_stmt010 cmpstate var stmt'
	in
	let _ = 
	  if (TO.is_void_typ rv_typ) then
	    EO.remove_qual_name env var
	  else
	    let ce_info = CEO.ce_info_of var 
	    in EO.update_variable_type env var ce_info.CE.ce_te
	in
	([O.COMPOUND (None, assign_stmt)], O.Rval (mk_rval var rv_typ))
	  
    | I.Gnu_labeladdr str -> ([], O.Rval (O.Rcode_label str))
	
    | I.Variable id ->
	begin
	  let ce_info = EO.find_ce_info env id
	  in
	  let te = ce_info.CE.ce_te
	  and ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  if QNO.is_enum ce_info.CE.ce_qname then
	    let i = QNO.enum_const ce_info.CE.ce_qname
	    in
	    (** C99 enumeration constants is explicity 
		defined as int **)
	    ([], O.Rval (mk_const_rval env.E.int_typ (string_of_int i)))
	  else if TO.is_array_typ te || TO.is_xarray_typ te then
	    let var_typ = TO.ptr_of (TO.elmt_of te)
	    in
	    let var = EO.get_tmp_ce env var_typ
	    in
	    let lval = O.Lladdr (O.Nlbl var)
	    and  rexpr = O.Rval (O.Rvct ce)
	    in
	    (make_asign cmpstate lval rexpr, O.Rval (O.Rladdr (O.Nctn var)))
	  else if TO.is_crt_function_typ te then
	    ([], O.Rval (O.Rfun ce))
	  else if TO.is_abs_function_typ te then
	    ([], O.Rval (O.Rladdr (O.Nctn ce)))
	  else
	    ([], O.Rval (O.Rladdr (O.Nctn ce)))
	end
	  
    | I.Memberof (se0,f) ->
	begin
	  let (l0, addr_rexpr) = 
	    compile_memberof_lval_or_rval cmpstate (se0, f)
	  in
	  match addr_rexpr with
	    | LVAL addr_rexpr ->
		let rval = fetch_rval_from_lval cmpstate addr_rexpr
		in
		let (l1, rexpr) = get_bitfield_rexpr cmpstate rval
		in (l0 @$ l1, rexpr)
	    | RVAL addr_rexpr -> 
		(l0, O.Rval addr_rexpr)
	end
	  
    | I.Memberof_ptr (se0, f) ->
	begin
	  let (l0, addr_rexpr) = 
	    compile_memberof_ptr_lval_or_rval cmpstate (se0, f)
	  in
	  match addr_rexpr with
	    | LVAL addr_rexpr -> 
		let rval = fetch_rval_from_lval cmpstate addr_rexpr
		in
		let (l1, rexpr) = get_bitfield_rexpr cmpstate rval
		in (l0 @$ l1, rexpr)
	    | RVAL addr_rexpr ->
		(l0, O.Rval addr_rexpr)
	end
	  
    | I.Indexof (se0, se1) -> 
	begin
	  let (l0, addr_rexpr) = 
	    compile_indexof_lval_or_rval cmpstate (se0, se1)
	  in
	  match addr_rexpr with
	    | LVAL addr_rexpr -> 
		let rval = fetch_rval_from_lval cmpstate addr_rexpr
		in (l0, O.Rval rval)
	    | RVAL addr_rexpr ->
		(l0, O.Rval addr_rexpr)
	end
	  
    | I.Binary_arithm (bop, se0, se1) ->
	begin
	  let (l0, se0') = compile_c_expr020_rval cmpstate se0
	  and (l1, se1') = compile_c_expr020_rval cmpstate se1
	  in
	  let (l2, result_expr) = 
	    make_bin_expr cmpstate bop se0' se1'
	  in (l0 @$ l1 @$ l2, result_expr)
	end
	  
    | I.Binary_predicate (brel, se0, se1) ->
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	and (l1, se1') = compile_c_expr020_rval cmpstate se1
	in (l0 @$ l1, O.Binary_predicate (brel, se0', se1'))
	     
    | I.Binary_logic (bop, se0, se1) ->
	let var = EO.get_tmp_ce env (env.E.int_typ)
	in
	let lval = O.Lladdr (O.Nlbl var)
	in
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	in
	let _ = EO.push_compiler_gen_scope env
	in
	let (l1, se1') = compile_c_expr020_rval cmpstate se1
	in
	let se1_O_rv_typ' = AEE.get_rval cmpstate.env cmpstate.eenv se1'
	in
	let se1_rexpr = 
	  let v = TO.make_type_const se1_O_rv_typ' "0"
	  in
	  let vc = O.Rconst v
	  in O.Binary_predicate (S.Ne, se1', vc)
	in
	let (new_tmp_syms, new_typs) = EO.pop_compiler_gen_scope env
	in
	let new_decls = mk_tool_gen_decls new_tmp_syms
	and new_ty_decls = 
	  Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
	in
	let if_stmt = 
	  match bop with
	    | S.And -> 
		O.IF 
		  (mk_true_cond se0', 
		  O.COMPOUND 
		    (Some "logic_and", 
		    O.BLOCK 
		      ([],new_ty_decls @$ new_decls, l1
			@$ (normalized_assign_stmts cmpstate (lval, se1_rexpr)))), 
		  O.COMPOUND
		    (Some "logic_and",
		    O.BLOCK
		      ([], [], 
		      normalized_assign_stmts cmpstate
			(lval, O.Rval 
			  (mk_const_rval (env.E.int_typ) "0")))))
		  
	    | S.Or -> 
		O.IF 
		  (mk_true_cond se0', 
		  O.COMPOUND
		    (Some "logic_or",
		    O.BLOCK
		      ([], [],
		      normalized_assign_stmts cmpstate
			(lval, O.Rval (mk_const_rval (env.E.int_typ) "1")))),
		  O.COMPOUND 
		    (Some "logic_or", 
		    O.BLOCK ([],new_ty_decls @$ new_decls, l1 
		      @$ 
		      (normalized_assign_stmts cmpstate 
			(lval, se1_rexpr)))))
	in
	(l0 @$ [if_stmt],  O.Rval (fetch_rval_from_lval cmpstate lval))
	  
    | I.Unary_arithm (uop, se0) ->
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	in (l0, O.Unary_arithm (uop, se0'))
	     
    | I.Logic_not se0 -> 
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	in (l0, O.Logic_not se0')
	     
    | I.Sizeof_expr expr0 ->
	begin
	  let size = match expr0 with
	    | I.String c_string_literal ->
		string_of_csize
		  (TO.sizeof_c_string_literal 
		    env.E.te_tbl c_string_literal)
	    | _ -> 
		let typ = eval_expr_typ cmpstate expr0
		in
		let size = TO.static_sizeof typ
		in string_of_csize size
	  in
	  ([], O.Rval (O.Rconst (TO.make_type_const (env.E.size_typ) size)))
	end
	  
    | I.Sizeof_type (type_cabs, has_def) ->
	let typ = compile_c_type cmpstate type_cabs
	in
	let size = TO.static_sizeof typ
	in 
	([], O.Rval (O.Rsizeof (typ, TO.make_type_const (env.E.size_typ) (string_of_csize size))))
	  
    | I.Alignof_expr expr0 ->
	let typ = eval_expr_typ cmpstate expr0
	in
	let size = TO.alignof typ
	in
	([], O.Rval (O.Ralignof (typ, TO.make_type_const (env.E.size_typ) (string_of_csize size))))
	  
    | I.Alignof_type (typ_cabs, has_def) ->
	let typ = compile_c_type cmpstate typ_cabs
	in
	let size = TO.alignof typ
	in
	([], O.Rval (O.Ralignof (typ, TO.make_type_const (env.E.size_typ) (string_of_csize size))))
	  
    | I.Cast (c_type_name, se0) -> 
	let cast_typ = compile_c_type cmpstate c_type_name
	and (l0, se0') = compile_c_expr020_rval cmpstate se0
	in
	(l0, mk_cast cmpstate se0' cast_typ)

    | I.Cast_init (c_type_name, inits) -> 
	begin
	  let cast_typ = compile_c_type cmpstate c_type_name
	  in
	  if TO.is_xarray_typ cast_typ or TO.is_ptr_typ cast_typ then
	    let (l0, addr) = 
	      compile_c_expr020_addr cmpstate 
		(I.Cast_init (c_type_name, inits))
	    in
	    let addr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv addr
	    in
	    let var = EO.get_tmp_ce env addr_O_re_typ
	    in
	    let lval = O.Lladdr (O.Nlbl var)
	    in
	    (l0 @$ (normalized_assign_stmts cmpstate (lval, addr)), 
	    O.Rval (fetch_rval_from_lval cmpstate lval))
	  else
	    let memory = alloc_top_c_initializer I.Auto cast_typ
	    in
	    let _ = 
	      compile_top_c_initializer cmpstate 
		memory (I.Initializer_2 inits)
	    in
	    let cast_typ = 
	      match memory with
		| Typ_mem.Xarray (_, _,_, s) ->
		    let elmt_typ = TO.elmt_of cast_typ
		    in TO.array_of elmt_typ (csize_of_int !s)
		| _ -> cast_typ
	    in
	    let var = EO.add_fresh_storage env cast_typ false
	    in
	    let lval = O.Lladdr (O.Nlbl var)
	    in
	    (make_asign_init cmpstate lval memory, 
	    O.Rval (fetch_rval_from_lval cmpstate lval))
	end
	  
    | I.Assign (se0, I.Cast_init (c_type_name, inits)) ->
	begin
	  let (l0, lval) = compile_c_expr020_lval cmpstate se0
	  and cast_typ = compile_c_type cmpstate c_type_name
	  in
	  if TO.is_xarray_typ cast_typ or 
	    TO.is_ptr_typ cast_typ then
	      let (l0, addr) = 
		compile_c_expr020_addr cmpstate 
		  (I.Cast_init (c_type_name, inits))
	      in
	      (l0 @$ (normalized_assign_stmts cmpstate (lval, addr)), 
	      O.Rval
		(fetch_rval_from_lval cmpstate lval))
	  else
	    let memory = alloc_top_c_initializer I.Auto cast_typ
	    in
	    let _ = 
	      compile_top_c_initializer cmpstate memory (I.Initializer_2 inits)
	    in
	    let cast_typ = 
	      match memory with
		| Typ_mem.Xarray (_, _,_, s) ->
		    let elmt_typ = TO.elmt_of cast_typ
		    in TO.array_of elmt_typ (csize_of_int !s)
		| _ -> cast_typ
	    in
	    (l0 @$ make_asign_init cmpstate lval memory, 
	    O.Rval (fetch_rval_from_lval cmpstate lval))
	end
	  
    | I.Assign (se0, se1) ->
	begin
	  let (l0, lval) = compile_c_expr020_lval cmpstate se0
	  and (l1, rexpr) = compile_c_expr020_rexpr cmpstate se1
	  in
	  let lval_O_lv_typ = AEE.get_lval cmpstate.env cmpstate.eenv lval
	  and rexpr_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr
	  in
	  let t0 = lval_O_lv_typ
	  and t1 = rexpr_O_re_typ
	  in
	  (l0 @$ l1 @$ (make_asign cmpstate lval rexpr), 
	  O.Rval (fetch_rval_from_lval cmpstate lval))
	end
	  
    | I.Pre_decr se0
    | I.Pre_incr se0 
    | I.Post_decr se0
    | I.Post_incr se0 ->
	begin
	  let (l0, lval) = compile_c_expr020_lval cmpstate se0
	  in
	  let rval = fetch_rval_from_lval cmpstate lval
	  in
	  let (l1, rexpr) = get_bitfield_rval cmpstate rval
	  in
	  let opcode = match expr with
	    | I.Post_decr _ -> S.Sub
	    | I.Post_incr _ -> S.Add
	    | I.Pre_decr _ -> S.Sub
	    | I.Pre_incr _ -> S.Add
	    | _ -> assert false
	  in
	  let (l2, bin_exp_ex) = make_bin_expr cmpstate opcode rexpr
	    (mk_const_rval (env.E.int_typ) "1")
	  in
	  let rexpr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv rexpr
	  in
	  match expr with
	    | I.Pre_decr _
	    | I.Pre_incr _ ->
		(l0 @$ l1 @$ l2 @$ (make_asign cmpstate lval bin_exp_ex), 
		O.Rval rexpr)
		  
	    | I.Post_decr _
	    | I.Post_incr _ ->
		let tmp = EO.get_tmp_ce env rexpr_O_rv_typ
		in
		(l0 @$ l1 @$ l2 @$
		  ([c2s (O.Assign (O.Lladdr (O.Nlbl tmp), O.Rval rexpr))] @$
		    make_asign cmpstate lval bin_exp_ex),
		O.Rval (O.Rladdr (O.Nctn tmp)))
	    | _ -> assert false
	end
	  
    | I.Assign_arithm (bop, se0, se1) ->
	let (l0, lval) = compile_c_expr020_lval cmpstate se0
	and (l1, rexpr1) = compile_c_expr020_rval cmpstate se1
	in
	let rval = (fetch_rval_from_lval cmpstate lval)
	in
	let (l2, rexpr0) = get_bitfield_rval cmpstate rval
	in
	let (l3, bin_expr) = make_bin_expr cmpstate bop rexpr0 rexpr1
	in
	(l0 @$ l1 @$ l2 @$ l3 @$ (make_asign cmpstate lval bin_expr), 
	O.Rval rexpr0)
	  
    | I.Memof se0 -> 
	let (l0, v0) = compile_c_expr020_rval cmpstate se0
	in
	let (l1, v1) = dref_rval cmpstate v0
	in (l0 @$ l1, O.Rval v1)
	     
    | I.Addrof se0 -> 
	let _ = mark_addr_is_taken cmpstate se0
	in
	let (l0, v0) = compile_c_expr020_addr cmpstate se0
	in (l0, v0)
	     
    | I.Question (se0, se_opt1, se2) ->
	begin
	  let (l0, se0') = compile_c_expr020_rval cmpstate se0
	  in
	  let (if_stmt, rval) = match se_opt1 with
	    | Some se1 ->
		begin
		  let _ = EO.push_compiler_gen_scope env
		  in
		  let (l1, se1') = compile_c_expr020_rexpr cmpstate se1
		  in
		  let (new_tmp_sym1s, new_typs1) = EO.pop_compiler_gen_scope env
		  in
		  let _ = EO.push_compiler_gen_scope env
		  in
		  let (l2, se2') = compile_c_expr020_rexpr cmpstate se2
		  in
		  let (new_tmp_sym2s, new_typs2) = EO.pop_compiler_gen_scope env
		  in
		  let new_decl1 = mk_tool_gen_decls new_tmp_sym1s
		  and new_decl2 = mk_tool_gen_decls new_tmp_sym2s
		  and new_ty_decls1 = 
		    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs1
		  and new_ty_decls2 = 
		    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs2
		  in
		  let se1_O_re_typ' = 
		    AEE.get_rexpr cmpstate.env cmpstate.eenv se1'
		  and se2_O_re_typ' = 
		    AEE.get_rexpr cmpstate.env cmpstate.eenv se2'
		  in
		  let typ = TO.lub [se1_O_re_typ'; se2_O_re_typ']
		  in
		  let (asign_stmt1, asign_stmt2, ret) = 
		    if (TO.is_void_typ typ) then
		      ([], [], O.Rvoid)
		    else
		      let var = EO.get_tmp_ce env typ
		      in		    
		      (normalized_assign_stmts cmpstate
			(O.Lladdr (O.Nlbl var), se1'),
		      normalized_assign_stmts cmpstate
			(O.Lladdr (O.Nlbl var), se2'),
		      O.Rladdr (O.Nctn var))
		  in
		  let l1stmt = l1
		  and l2stmt = l2
		  in
		  let if_stmt = 
		    O.IF (mk_true_cond se0', 
		    O.COMPOUND 
		      (Some "?", 
		      O.BLOCK ([], new_ty_decls1 @$ new_decl1, l1stmt @$
			asign_stmt1)),
		    O.COMPOUND 
		      (Some "?", 
		      O.BLOCK ([], new_ty_decls2 @$ new_decl2, l2stmt @$
			asign_stmt2)))
		  in (if_stmt, ret)
		end
	    | None ->
		begin
		  let _ = EO.push_compiler_gen_scope env
		  in
		  let (l2, se2') = compile_c_expr020_rexpr cmpstate se2
		  in
		  let (new_tmp_sym2s, new_typs) = EO.pop_compiler_gen_scope env
		  in
		  let new_decl2 = mk_tool_gen_decls new_tmp_sym2s
		  and new_ty_decls = 
		    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
		  in
		  let se0_O_rv_typ' = 
		    AEE.get_rval cmpstate.env cmpstate.eenv se0'
		  and se2_O_re_typ' = 
		    AEE.get_rexpr cmpstate.env cmpstate.eenv se2'
		  in
		  let typ = TO.lub [se0_O_rv_typ'; se2_O_re_typ']
		  in
		  let (asign_stmt1, asign_stmt2, ret) = 
		    if (TO.is_void_typ typ) then
		      ([], [], O.Rvoid)
		    else
		      let var = EO.get_tmp_ce env typ
		      in		    
		      (normalized_assign_stmts cmpstate
			(O.Lladdr (O.Nlbl var), O.Rval se0'),
		      normalized_assign_stmts cmpstate
			(O.Lladdr (O.Nlbl var), se2'),
		      O.Rladdr (O.Nctn var))
		  in
		  let l2stmt = l2
		  in
		  let if_stmt = 
		    O.IF (mk_true_cond se0', 
		    O.COMPOUND (Some "?", O.BLOCK ([], [], asign_stmt1)),
		    O.COMPOUND (Some "?", O.BLOCK ([], 
		    new_ty_decls @$ new_decl2, l2stmt @$ asign_stmt2)))
		  in
		  (if_stmt, ret)
		end
	  in
	  (l0 @$ [if_stmt], O.Rval rval)
	end
  in (l, new_expr)

and compile_c_expr020_true_cond cmpstate (expr:I.c_expr020)
    :(O.c_stmt010 list * O.true_cond) = 
  let (l, new_expr) = match expr with
    | I.Comma (se0, se1) -> 
	let (l0, _) = compile_c_expr020_rexpr cmpstate se0
	and (l1, se1') = compile_c_expr020_true_cond cmpstate se1
	in (l0 @$ l1, se1')
	     
    | I.Binary_predicate (brel, se0, se1) ->
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	and (l1, se1') = compile_c_expr020_rval cmpstate se1
	in (l0 @$ l1,  O.PRED (brel, se0', se1'))
	     
    | I.Logic_not se0 -> 
	let (l0, se0') = compile_c_expr020_rval cmpstate se0
	in (l0, O.EQ_ZERO se0')
	     
    | I.Constant _ | I.Builtin_types_compatible _
    | I.Builtin_constant_p _ | I.Sizeof_expr _
    | I.Sizeof_type _  | I.Alignof_expr _
    | I.Alignof_type _ | I.String _
    | I.Call _ | I.Macro_va_start _
    | I.Macro_va_arg _ | I.Macro_va_end _
    | I.Builtin_expect _ | I.Gnu_block _
    | I.Gnu_labeladdr _  | I.Variable _
    | I.Memberof _ | I.Memberof_ptr _
    | I.Indexof _ | I.Binary_arithm _
    | I.Binary_logic _ | I.Unary_arithm _ 
    | I.Cast _ | I.Cast_init _
    | I.Assign _ | I.Pre_decr _
    | I.Pre_incr _ | I.Post_decr _
    | I.Post_incr _ | I.Assign_arithm _
    | I.Memof _  | I.Addrof _
    | I.Question _ ->
	let (l, rval) = compile_c_expr020_rval cmpstate expr
	in (l, O.NEQ_ZERO rval)
  in (l, new_expr)
       
and compile_c_expression cmpstate (expr:I.c_expression) =
  compile_c_expr020_rexpr cmpstate expr
    
and compile_c_constant_address cmpstate
    (expr:I.c_constant_expression):O.c_constant_expression = 
  let env = cmpstate.env
  in
  match expr with
    | I.Variable id ->
	begin
	  let ce_info = EO.find_ce_info env id
	  in
	  let ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  if CEO.is_enum ce then
	    assert false
	  else if CEO.is_global ce || CEO.is_static ce then
	    O.Cvar_lbl ce
	  else
	    raise (Dynamic expr)
	end
	  
    | I.Memberof (I.Variable id, field) ->
	begin
	  let ce_info = EO.find_ce_info env id
	  in
	  let ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  if CEO.is_enum ce then
	    assert false
	  else if not (CEO.is_global ce || CEO.is_static ce) then
	    raise (Dynamic expr)
	  else
	    let se0' = O.Cvar_lbl ce
	    in
	    let se0_O_ce_typ' = AEE.get_cexpr cmpstate.env cmpstate.eenv se0'
	    in
	    let field_typ = TO.field_typ 
	      (TO.elmt_of se0_O_ce_typ') field
	    and offset = TO.field_offset 
	      (TO.elmt_of se0_O_ce_typ') field
	    in
	    let offset_rval = 
	      (mk_const_cexpr env.E.size_typ 
		(string_of_csize offset))
	    in
	    AEE.put_cexpr cmpstate.eenv 
	      (O.CBinary_arithm (S.Add, se0', offset_rval)) 
	      (TO.ptr_of field_typ)
	end
	  
    | I.Memberof (se0, field) ->
	let se0' = compile_c_constant_address cmpstate se0
	in
	let se0_O_ce_typ' = AEE.get_cexpr cmpstate.env cmpstate.eenv se0'
	in
	let field_typ = TO.field_typ (TO.elmt_of se0_O_ce_typ') field
	and offset = TO.field_offset (TO.elmt_of se0_O_ce_typ') field
	in
	let offset_rval = 
	  (mk_const_cexpr env.E.size_typ (string_of_csize offset))
	in 
	compute_constant_address cmpstate se0' offset_rval (TO.ptr_of field_typ)
	  
    | I.Memberof_ptr (se0, field) ->
	let se0' = evaluate_c_constant_expression cmpstate se0
	in
	let se0_O_ce_typ' = AEE.get_cexpr cmpstate.env cmpstate.eenv se0'
	in
	let field_typ = TO.field_typ (TO.elmt_of se0_O_ce_typ') field
	and offset = TO.field_offset (TO.elmt_of se0_O_ce_typ') field
	in
	let offset_rval = 
	  (mk_const_cexpr env.E.size_typ (string_of_csize offset))
	in compute_constant_address cmpstate se0' offset_rval
	     (TO.ptr_of field_typ)
	     
    | I.Indexof (se0, se1) -> 
	let se0 = evaluate_c_constant_expression cmpstate se0
	and se1 = evaluate_c_constant_expression cmpstate se1
	in
	let se0_te = AEE.get_cexpr cmpstate.env cmpstate.eenv se0
	in
	let elmt_typ = TO.elmt_of se0_te
	in
	let elmt_size = TO.static_sizeof elmt_typ
	in
	let size_rexpr = 
	  (mk_const_cexpr env.E.size_typ (string_of_csize elmt_size))
	in
	let offset_rexpr = 
	  O.CBinary_arithm (S.Mul, se1, size_rexpr)
	in compute_constant_address cmpstate se0 offset_rexpr 
	     (TO.ptr_of elmt_typ)
	     
    | _ -> raise (Dynamic expr)

	
and make_bin_cexpr cmpstate bop (rexpr1:O.cexpr) (rexpr2:O.cexpr) = 
  let env = cmpstate.env
  in
  let mk_cast rexpr dest_typ_opt = 
    match dest_typ_opt with
      | Some (dest_te, _) -> 
	  let (_, did) = dest_te
	  and (_, sid) = AEE.get_cexpr cmpstate.env cmpstate.eenv rexpr
	  in
	  if (did <> sid) then
	    O.CCast (dest_te, rexpr)
	  else
	    rexpr
      | None -> rexpr
  in
  let se0' = rexpr1 
  and se1' = rexpr2
  in
  let se0_O_ce_typ' = AEE.get_cexpr cmpstate.env cmpstate.eenv se0'
  and se1_O_ce_typ' = AEE.get_cexpr cmpstate.env cmpstate.eenv se1'
  in
  let (ct0, ct1, rt) = 
    TO.bin_arithm_typ bop se0_O_ce_typ' se1_O_ce_typ'
  in
  let se0'' = mk_cast se0' ct0 
  and se1'' = mk_cast se1' ct1
  in
  let se0_O_ce_typ'' = AEE.get_cexpr cmpstate.env cmpstate.eenv se0''
  and se1_O_ce_typ'' = AEE.get_cexpr cmpstate.env cmpstate.eenv se1''
  in
  match bop with
    | S.Add ->
	begin
	  if ((TO.is_ptr_typ se0_O_ce_typ'') or (TO.is_array_typ se0_O_ce_typ'')) 
 	    & not (TO.is_ptr_typ se1_O_ce_typ'' or TO.is_array_typ se1_O_ce_typ'')
	  then
	    let elmt_typ = TO.elmt_of se0_O_ce_typ''
	    in
	    let elmt_siz = TO.static_sizeof elmt_typ
	    in
	    let size_expr = 
	      (mk_const_cexpr (env.E.size_typ) (string_of_csize elmt_siz))
	    in
	    let offset_cexpr = 
	      AEE.put_cexpr cmpstate.eenv 
		(O.CBinary_arithm (S.Mul, se1'', size_expr)) env.E.size_typ
	    in
	    compute_cexpr_address cmpstate bop se0'' offset_cexpr se0_O_ce_typ''
	  else
	    AEE.put_cexpr cmpstate.eenv
	      (O.CBinary_arithm (bop, se0'', se1'')) rt
	end
    | S.Sub ->
	begin
	  if ((TO.is_ptr_typ se0_O_ce_typ'') or (TO.is_array_typ se0_O_ce_typ''))
 	    & not (TO.is_ptr_typ se1_O_ce_typ'' or TO.is_array_typ se1_O_ce_typ'')
	  then
	    let elmt_typ = TO.elmt_of se0_O_ce_typ''
	    in
	    let elmt_siz = TO.static_sizeof elmt_typ
	    in
	    let size_expr = 
	      (mk_const_cexpr (env.E.size_typ) (string_of_csize elmt_siz))
	    in
	    let offset_cexpr = 
	      AEE.put_cexpr cmpstate.eenv 
		(O.CBinary_arithm (S.Mul, se1'', size_expr)) env.E.size_typ
	    in 
	    compute_cexpr_address cmpstate bop se0'' offset_cexpr se0_O_ce_typ''
	  else if TO.is_ptr_or_array_typ se0_O_ce_typ'' & TO.is_ptr_or_array_typ se1_O_ce_typ''
	  then
	    begin
	      let elmt_typ0 = TO.elmt_of se0_O_ce_typ''
	      and elmt_typ1 = TO.elmt_of se1_O_ce_typ''
	      in
	      assert (snd elmt_typ0 == snd elmt_typ1);
	      let elmt_siz = TO.static_sizeof elmt_typ0
	      in
	      let size_expr = 
		(mk_const_cexpr (env.E.size_typ) (string_of_csize elmt_siz))
	      in
	      let char_cexpr0 = O.CCast (TO.ptr_of env.E.char_typ, se0'')
	      and char_cexpr1 = O.CCast (TO.ptr_of env.E.char_typ, se1'')
	      in 
	      let sub_expr = AEE.put_cexpr cmpstate.eenv
		(O.CBinary_arithm (bop, char_cexpr0, char_cexpr1)) rt
	      in
	      AEE.put_cexpr cmpstate.eenv
		(O.CBinary_arithm (S.Div, sub_expr, size_expr)) rt
	    end
	  else
	    AEE.put_cexpr cmpstate.eenv
	      (O.CBinary_arithm (bop, se0'', se1'')) rt
	end
    | _ -> 
	if ((TO.is_ptr_typ se0_O_ce_typ'') or (TO.is_array_typ se0_O_ce_typ'')) & 
	  not (TO.is_ptr_typ se1_O_ce_typ'' or TO.is_array_typ se1_O_ce_typ'')
	then
	  assert false
	else
	  AEE.put_cexpr cmpstate.eenv (O.CBinary_arithm (bop, se0'', se1'')) rt
	    
and compile_c_constant_expression cmpstate (expr:I.c_constant_expression):O.c_constant_expression = 
  let env = cmpstate.env
  in
  let new_expr = match expr with
    | I.String str -> find_cstr_lit_cexpr cmpstate str
	
    | I.Variable id ->
	begin
	  (** todo check if string is a static or global variable **)
	  let ce_info = EO.find_ce_info env id
	  in
	  let ce = (env.E.ce_tbl, ce_info.CE.ce_id)
	  in
	  let is_const = (CEO.is_global ce) || (CEO.is_static ce) || 
	    (not (CEO.is_var ce))
	  and te = ce_info.CE.ce_te
	  in
	  if CEO.is_enum ce then 
	    let i = CEO.enum_const ce
	    in
	    (** C99 enumeration constants is explicity defined as int **)
	    mk_const_cexpr env.E.int_typ (string_of_int i);
	  else if TO.is_array_typ te && is_const then
	    O.Cvct ce
	  else if TO.is_xarray_typ te && is_const then
	    O.Cvct ce
	  else if (TO.is_crt_function_typ te
	  or TO.is_abs_function_typ te) && is_const 
	  then
	    O.Cfun_lbl ce
	  else
	    raise (Dynamic expr)
	end
	  
    | I.Sizeof_expr expr0 ->
	let size = match expr0 with
	  | I.String c_string_literal ->
	      string_of_csize
		(TO.sizeof_c_string_literal 
		  env.E.te_tbl c_string_literal)
	  | _ -> 
	      let typ = eval_expr_typ cmpstate expr0
	      in
	      let size = TO.static_sizeof typ
	      in string_of_csize size
	in 
	O.Cconst (TO.make_type_const (env.E.size_typ) size)
	  
    | I.Sizeof_type (type_cabs, has_def) ->
	let typ = compile_c_type cmpstate type_cabs
	in
	let size = TO.static_sizeof typ
	in 
	O.Csizeof 
	  (typ, TO.make_type_const (env.E.size_typ) 
	    (string_of_csize size))
	  
    | I.Alignof_expr expr0 ->
	let typ = eval_expr_typ cmpstate expr0
	in
	let size = TO.alignof typ
	in 
	O.Calignof 
	  (typ, TO.make_type_const (env.E.size_typ) 
	    (string_of_csize size))
	  
    | I.Alignof_type (typ_cabs, has_def) ->
	let typ = compile_c_type cmpstate typ_cabs
	in
	let size = TO.alignof typ
	in 
	O.Calignof 
	  (typ, TO.make_type_const (env.E.size_typ) 
	    (string_of_csize size))

    | I.Constant c_constant ->
	let (c_constant', typ') = compile_c_constant cmpstate c_constant
	in O.Cconst c_constant'
	     
    | I.Comma (se0, se1) -> 
	let _ = compile_c_constant_expression cmpstate se0
	in compile_c_constant_expression cmpstate se1
	     
    | I.Binary_arithm (bop, se0, se1) -> 
	let se0 = compile_c_constant_expression cmpstate se0
	and se1 = compile_c_constant_expression cmpstate se1
	in
	make_bin_cexpr cmpstate bop se0 se1
	  
    | I.Binary_predicate (bop, se0, se1) ->
	let se0 = compile_c_constant_expression cmpstate se0
	and se1 = compile_c_constant_expression cmpstate se1
	in O.CBinary_predicate (bop, se0, se1)
	     
    | I.Binary_logic (bop, se0, se1) ->
	let se0 = compile_c_constant_expression cmpstate se0
	and se1 = compile_c_constant_expression cmpstate se1
	in O.CBinary_logic (bop, se0, se1)
	     
    | I.Unary_arithm (uop, se0) ->
	let se0 = compile_c_constant_expression cmpstate se0
	in O.CUnary_arithm (uop, se0)
	     
    | I.Logic_not se0 ->
	let se0 = compile_c_constant_expression cmpstate se0
	in O.CLogic_not se0
	     
    | I.Memof e ->
	begin
	  match e with
	    | I.Addrof e -> 
		let _ = mark_addr_is_taken cmpstate e
		in compile_c_constant_expression cmpstate e
	    | _ -> raise (Dynamic expr)
	end
	  
    | I.Addrof e -> 
	let _ = mark_addr_is_taken cmpstate e
	in compile_c_constant_address cmpstate e
	     
    | I.Question (se0, se1_opt, se2) ->
	begin
	  let se0 = compile_c_constant_expression cmpstate se0
	  in
	  let se1 = match se1_opt with
	    | Some se1 -> compile_c_constant_expression cmpstate se1
	    | None -> se0
	  and se2 = compile_c_constant_expression cmpstate se2
	  in
	  let se0_te = AEE.get_cexpr cmpstate.env cmpstate.eenv se0
	  and se1_te = AEE.get_cexpr cmpstate.env cmpstate.eenv se1
	  in 
	  O.CQuestion (se0, se1, se2)
	end

    | I.Builtin_constant_p (e) ->
	O.Cconst (gnu_builtin_constant_p cmpstate e)

    | I.Builtin_expect _ -> assert false
	
    | I.Builtin_types_compatible (t0, t1) ->
	assert false
	  (* error *)

    | I.Cast (t,e) ->
	let t = compile_c_type cmpstate t
	and e = compile_c_constant_expression cmpstate e
	in O.CCast (t, e)
	     
    | I.Memberof _ 
    | I.Memberof_ptr _
    | I.Indexof _
    | I.Assign _
    | I.Post_decr _
    | I.Post_incr _
    | I.Pre_decr _
    | I.Pre_incr _
    | I.Assign_arithm _
    | I.Call _
    | I.Macro_va_start _
    | I.Macro_va_arg _
    | I.Macro_va_end _
    | I.Gnu_block _
    | I.Gnu_labeladdr _ -> 
	raise (Dynamic expr)
    | I.Cast_init _ ->  
	raise (Cinit expr)
  in new_expr

and evaluate_c_constant_expression cmpstate (expr:I.c_expr020):O.cexpr =
  let env = cmpstate.env
  in
  let c = compile_c_constant_expression cmpstate expr
  in
  if !disable_constant_expression_evaluation then
    c
  else
    begin
      try
	let (v,s,sopt) = Ast_ea_expr_env.eval_cexpr env cmpstate.eenv c
	in
	let te = AEE.get_cexpr cmpstate.env cmpstate.eenv c
	in O.Cconst (te, CA.cval_ext_of_cval v)
      with
	| Ast_ea_expr_env.LinkConst c -> c
    end
      
(** the following are standard copy translation **)
and compile_c_type cmpstate (typ:I.c_type): te = 
  let (typ, size_opt, (has_def, l)) = eval_type_te cmpstate typ
  in
  if l <> [] then
    raise (Darray (typ, l, size_opt))
  else typ

and compile_local_c_type cmpstate (typ:I.c_type) : (te * O.rexpr option * (O.c_stmt010 list)) =
  let (c, size_opt,  (def, l)) = eval_type_te cmpstate typ
  in (c, size_opt, l)
       
and compile_linkage = function
  | I.Default_extern -> O.Default_extern
  | I.Default -> O.Default_storage
  | I.Extern -> O.Extern
  | I.Extern_Inline -> O.Extern_Inline
  | I.Static_Inline -> O.Static_Inline
  | I.Auto -> O.Auto
  | I.Static -> O.Static
  | I.Register -> O.Register
  | I.Inline -> O.Inline
  | I.Type_alias -> O.Type_alias
  | I.Thread -> 
      if !enable_thread_storage then
	O.Thread
      else
	camlp4_macro_exception "__thread is not supported\n"
	  
  | I.Extern_Thread -> 
      if !enable_thread_storage then
	O.Extern_Thread
      else
	camlp4_macro_exception "__thread is not supported\n"
	  
  | I.Static_Thread -> 
      if !enable_thread_storage then
	O.Static_Thread
      else
	camlp4_macro_exception "__thread is not supported\n"

and set_ce_is_register ce linkage = 
  match linkage with
    | I.Register -> 
	let ce_info = CEO.ce_info_of ce
	in ce_info.CE.ce_is_register <- true
    | _ -> ()

and compile_local_c_declaration_ex cmpstate (expr:I.c_declaration_ex):
    (c_tmp_local_declaration list) = 
  let env = cmpstate.env
  in
  let (expr, _) = expr
  in
  match expr with
    | I.Obj_decl (linkage, c_type, sname) ->
	begin
	  let (typ, size_opt, l) = compile_local_c_type cmpstate c_type
	  in
	  let typ = TO.rec_norm_function_te typ
	  in
	  let typ = TO.stripoff_crt_function_typ typ
	  in
	  let ce = EO.add_ce_info env (mk_ce_info cmpstate linkage) (sname, typ)
	  in 
	  let _ = set_ce_is_register ce linkage
	  in
	  match size_opt with
	    | None -> 
		[TMPLocal_obj_decl (compile_linkage linkage, ce)]
	    | Some size -> 
		[TMPLocal_darray_decl (l, compile_linkage linkage, ce, size)]
	end
	  
    | I.Obj_decl_init (linkage, c_type_name, sname, 
      I.Initializer_1 (I.Cast_init(cast_type, inits))) ->
	begin
	  let tmp_sname = EO.alloc_fresh_sname env 
	  in
	  let decls = 
	    [
	      (I.Obj_decl_init (linkage, cast_type, tmp_sname, I.Initializer_2
		inits), ref false);
	      (I.Obj_decl_init (linkage, c_type_name, sname, I.Initializer_1
		(I.Variable tmp_sname)), ref false)
	    ]
	  in
	  List.flatten (List.map (compile_local_c_declaration_ex cmpstate) decls);
	end
	  
    | I.Obj_decl_init (linkage, c_type, sname, c_initializer) ->
	begin
	  let typ = compile_c_type cmpstate c_type
	  in
	  let typ = TO.rec_norm_function_te typ
	  in
	  let typ = TO.stripoff_const_qualifier typ  (* local initialization
							is translated to
							explicit assignments,
							so we have to stripoff
							all const qualifers *)
	  in
	  let ce = EO.add_ce_info env (mk_ce_info cmpstate linkage) (sname, typ)
	  in
	  let _ = set_ce_is_register ce linkage
	  in
	  let memory = alloc_top_c_initializer linkage typ
	  in
	  let _ = 
	    compile_top_c_initializer cmpstate memory c_initializer
	  in
	  let typ = 
	    match memory with
	      | Typ_mem.Xarray (_, _, _, s) ->
		  let elmt_typ = TO.elmt_of typ
		  in TO.array_of elmt_typ (csize_of_int !s)
	      | _ -> typ
	  in
	  let _ = EO.update_ce_info env ce typ
	  in	  
	  ignore(TO.sizeof typ);
	  [TMPLocal_obj_decl_init (compile_linkage linkage, ce, memory)]
	end

    | I.Typedef (c_type, str) -> 
	let typ = add_alias_typ cmpstate str c_type
	in
	EO.remove_new_type env typ;
	[TMPLocal_type_def typ]

    | I.Type_decl c_type ->
	let typ = compile_c_type cmpstate c_type
	in
	EO.remove_new_type env typ;
	[TMPLocal_type_decl typ]
	  
    | I.Type_only c_type ->
	let typ = compile_c_type cmpstate c_type
	in
	EO.remove_new_type env typ;
	[TMPLocal_type_only typ]

and convert_initializer init = 
  let c = function
    | TMPStatic_init_none -> O.Static_init_none
    | TMPDynamic_init_none -> assert false
    | TMPStatic_init rexpr -> O.Static_init rexpr
    | TMPDynamic_init _ -> assert false
  in Typ_mem_op.compile c init

and compile_global_c_declaration_one cmpstate 
    (expr:I.c_declaration_ex):O.c_declaration = 
  let l = compile_global_c_declaration_ex cmpstate expr
  in
  assert (Safe_list.length l = 1);
  Safe_list.hd l
    
and compile_global_c_declaration_ex cmpstate (expr:I.c_declaration_ex)
    :(O.c_declaration list) = 
  let env = cmpstate.env
  in
  let (expr, _) = expr
  in
  match expr with
    | I.Obj_decl (linkage, c_type, sname) ->
	let typ = compile_c_type cmpstate c_type
	in
	let typ = TO.rec_norm_function_te typ
	in
	let typ = TO.stripoff_crt_function_typ typ
	in
	let ce = EO.add_ce_info env (mk_ce_info cmpstate linkage) (sname, typ)
	in 
	let _ = set_ce_is_register ce linkage
	in
	[O.Obj_decl (compile_linkage linkage, ce)]
	  
    | I.Obj_decl_init (linkage, c_type_name, sname,
      I.Initializer_1 (I.Cast_init(cast_type, inits))) ->
	begin
	  let tmp_sname = EO.alloc_fresh_sname env 
	  in
	  let decls = 
	    [
	      (I.Obj_decl_init (linkage, cast_type, tmp_sname, I.Initializer_2
		inits), ref false);
	      (I.Obj_decl_init (linkage, c_type_name, sname, I.Initializer_1
		(I.Variable tmp_sname)), ref false)
	    ]
	  in 
	  List.flatten (List.map (compile_global_c_declaration_ex cmpstate) decls)
	end
	  
    | I.Obj_decl_init (linkage, c_type, string, c_initializer) ->
	begin
	  let typ = compile_c_type cmpstate c_type
	  in	    
	  let typ = TO.rec_norm_function_te typ
	  in
	  let ce = EO.add_ce_info env (mk_ce_info cmpstate linkage) 
	    (string, typ)
	  in
	  let _ = set_ce_is_register ce linkage
	  in
	  let memory = alloc_top_c_initializer I.Static typ
	  in
	  let _ = 
	    compile_top_c_initializer cmpstate memory c_initializer
	  in
	  let typ = 
	    match memory with
	      | Typ_mem.Xarray (_, _, _, s) ->
		  let elmt_typ = TO.elmt_of typ
		  in TO.array_of elmt_typ (csize_of_int !s)
	      | _ -> typ
	  in
	  let _ = EO.update_ce_info env ce typ
	  in
	  ignore(TO.sizeof typ);
	  [O.Obj_decl_init (compile_linkage linkage, ce, convert_initializer memory)]
	end

    | I.Typedef (c_type, str) -> 
	let typ = add_alias_typ cmpstate str c_type
	in
	EO.remove_new_type env typ;
	[O.Type_def typ]
	  
    | I.Type_decl c_type ->
	let typ = compile_c_type cmpstate c_type
	in
	EO.remove_new_type env typ;
	[O.Type_decl typ]

    | I.Type_only c_type ->
	let typ = compile_c_type cmpstate c_type
	in
	EO.remove_new_type env typ;
	[O.Type_only typ]
	  

and alloc_top_c_initializer (linkage) (te:te):c_tmp_initializer =
  match linkage with
    | I.Static -> 
	Typ_mem_op.alloc_top te ~init_val:TMPStatic_init_none
    | _ -> 
	Typ_mem_op.alloc_top te ~init_val:TMPDynamic_init_none
	  
and alloc_c_initializer (te:te) (init_expr) :c_tmp_initializer =
  Typ_mem_op.alloc te ~init_val:init_expr
    
and compile_top_c_initializer cmpstate (memory:c_tmp_initializer)
    (expr:I.c_initializer):unit =
  let env = cmpstate.env
  in
  match expr with
    | I.Initializer_1 c_expression ->
	begin
	  match compile_c_initializer cmpstate memory expr with
	    | Some _ -> assert false
	    | None -> ()
	end
    | I.Initializer_2 c_initializer_list  ->
	begin
	  match memory with
	    | Typ_mem.Struct (_, a, _) ->
		compile_top_c_initializer_list_in_struct cmpstate
		  a c_initializer_list
		  
	    | Typ_mem.Union (_, a, _) ->
		let first_elmt_array = Array.create 1 (Array.get a 0)
		in
		compile_top_c_initializer_list_in_struct cmpstate 
		  first_elmt_array c_initializer_list
		  
	    | Typ_mem.Array (_, a)  ->
		compile_top_c_initializer_list_in_array cmpstate
		  a c_initializer_list
		  
	    | Typ_mem.Xarray (_, _, _, _) -> 
		compile_top_c_initializer_list_in_xarray cmpstate
		  memory c_initializer_list
		  
	    | Typ_mem.Scalar _ ->
		begin
		  try
		    begin
		      compile_c_designator_list_in_scalar cmpstate
			memory (ref 0) c_initializer_list;
		      assert false
		    end
		  with
		    | Finished remainder 
		    | Next remainder ->
			begin
			  if remainder <> [] then
			    camlp4_macro_exception 
			      "excess elements in initializer\n"
			end
		end
		  
	    | Typ_mem.Bits _ ->
		begin
		  assert false
		end

	    | Typ_mem.Null -> assert false
	end	  
	  
and compile_c_initializer cmpstate (memory:c_tmp_initializer)
    (expr:I.c_initializer):(I.c_initializer option) = 
  let rec set_scalar_variable_char (memory:c_tmp_initializer) (char:char):unit =
    let char_code = Int64.of_int (Char.code char) in
    match memory with
      | Typ_mem.Scalar (te, init_expr) ->
	  init_expr := 
	    TMPStatic_init (mk_const_cexpr te (string_of_csize char_code))

      | Typ_mem.Bits _ -> assert false
      | _ -> assert false
	  
  and set_scalar_variable_wchar (memory:c_tmp_initializer) (char_code:int64):unit = 
    match memory with
      | Typ_mem.Scalar (te, init_expr) ->
	  init_expr := 
	    TMPStatic_init (mk_const_cexpr te (string_of_csize char_code))
	      
      | Typ_mem.Bits _ -> assert false
      | _ -> assert false
	  
  and set_scalar_variable_val (memory:c_tmp_initializer) (c_expression:I.c_expr020):unit = 
    match memory with
      | Typ_mem.Scalar (c_typ, init_expr) ->
	  begin
	    try 
	      init_expr := TMPStatic_init
		(evaluate_c_constant_expression cmpstate c_expression)
	    with
	      | Dynamic _ -> 
		  let _ = assert_link_time_constant init_expr c_expression
		  in
		  let c_expression = compile_c_expression cmpstate c_expression
		  in init_expr := TMPDynamic_init c_expression
	  end
      | Typ_mem.Bits (c_typ, init_expr) ->
	  assert false
      | _ -> assert false
	  
  and set_memory_block_val (memory:c_tmp_initializer) 
      (var_val:I.c_expr020):unit = 
    match memory with
      | Typ_mem.Union (c_typ, field_array, ref_e) ->
	  begin
	    try
	      ref_e := TMPStatic_init
		(evaluate_c_constant_expression cmpstate var_val)
	    with
	      | Dynamic _ ->
		  let var_val = compile_c_expression cmpstate var_val
		  in
		  ref_e := TMPDynamic_init var_val
	  end
      | Typ_mem.Struct (c_typ, field_array, ref_e) ->
	  begin
	    match  var_val with
	      | I.Cast_init (t, inits) ->
		  (* assert t = c_typ *)
		  compile_top_c_initializer 
		    cmpstate memory (I.Initializer_2 inits)
	      | _ ->
		  begin
		    try
		      ref_e := TMPStatic_init
			(evaluate_c_constant_expression cmpstate var_val)
		    with
		      | Dynamic _ ->
			  let var_val = compile_c_expression cmpstate var_val
			  in
			  ref_e := TMPDynamic_init var_val
		  end
	  end
      | Typ_mem.Array (c_typ, elmt_array) ->
	  begin
	    let elmt_typ = TO.elmt_of c_typ
	    in
	    let is_ptr = TO.is_ptr_typ elmt_typ
	    in
	    Array.iteri
	      (fun i (elmt_mem) ->
		let elmt_val = 
		  (I.Indexof 
		    (var_val, 
		    I.Constant
		      (Ast_ca_expr.Constant_value 
			(Mach.cuint_id, (Const_folding.cval_ext_of_cval
			  (Const_folding.cuint_cval_of_string (string_of_int i)))))))
		in
		if is_ptr then
		  set_scalar_variable_val elmt_mem elmt_val
		else
		  set_memory_block_val elmt_mem elmt_val
	      ) elmt_array
	  end
      | Typ_mem.Scalar (c_typ, init_expr) -> 
	  set_scalar_variable_val memory var_val
      | Typ_mem.Bits (c_typ, init_expr) -> assert false
      | _ -> assert false
  in  
  try let ret = match expr with
    | I.Initializer_1 c_expression -> 
	let ret = match c_expression with
	  | I.String (C_syntax_symbol.String_literal c_string_literal) -> 
	      let ret = match memory with
		| Typ_mem.Array (c_typ, char_array) ->
		    begin
		      if TO.is_array_typ c_typ then
			begin
			  let elmt_typ = TO.elmt_of c_typ
			  and len = Array.length char_array
			  in
			  if TO.is_char_typ elmt_typ then
			    ignore(List.fold_left
			      (fun index c -> 
				let _ = 
				  if index >= len then
				    if c = '\000' then ()
				    else
				      camlp4_macro_exception 
					"string overflow '%s'\n" 
					c_string_literal
				  else
				    let (elmt_mem) = 
				      Array.get char_array index 
				    in set_scalar_variable_char elmt_mem c
				in index + 1
			      ) 0 (C_str.to_chars ~c_string:c_string_literal))
			  else
			    assert false
			end;
		      None
		    end
		| Typ_mem.Xarray 
		    (c_typ, init_expr, char_mem_list, max_len) 
		  ->
		    begin
		      if TO.is_xarray_typ c_typ then
			begin
			  let elmt_typ = TO.elmt_of c_typ
			  in
			  if TO.is_char_typ elmt_typ then
			    ignore(List.fold_left
			      (fun index c -> 
				let elmt_mem = 
				  alloc_c_initializer elmt_typ init_expr
				in
				set_scalar_variable_char elmt_mem c;
				char_mem_list := !char_mem_list @$ [(index, elmt_mem)];
				max_len := max (index+1) !max_len;
				index + 1
			      ) 0 (C_str.to_chars ~c_string:c_string_literal))
			  else
			    assert false
			end;
		      None
		    end
		| Typ_mem.Scalar (c_typ, init_expr) -> 
		    begin
		      if TO.is_ptr_typ c_typ then
			begin
			  let elmt_typ = TO.elmt_of c_typ
			  in
			  if TO.is_char_typ elmt_typ then
			    let cstr = find_cstr_lit_cexpr cmpstate
			      (C_syntax_symbol.String_literal 
				c_string_literal)
			    in
			    init_expr := TMPStatic_init cstr
			  else if TO.is_void_typ elmt_typ then
			    let _ = camlp4_macro_warning 
			      "warning: type mismatch\n"
			    in
			    let cstr = find_cstr_lit_cexpr cmpstate
			      (C_syntax_symbol.String_literal 
				c_string_literal)
			    in
			    init_expr := TMPStatic_init cstr
			  else
			    assert false;
			  None
			end
		      else if TO.is_char_typ c_typ then
			begin
			  begin
			    let str_len = 
			      String.length c_string_literal
			    in
			    let (has_done, hd, tail) = 
			      if str_len = 0 then
				(true, Char.chr 0, "")
			      else if str_len = 1 then
				(true, 
				String.get c_string_literal 0, "")
			      else if str_len > 1 then
				(false, String.get c_string_literal 0,
				String.sub 
				  c_string_literal 1 (str_len - 1))
			      else
				assert false
			    in
			    set_scalar_variable_char memory hd;
			    if has_done then None
			    else 
			      Some 
				(I.Initializer_1 
				  (I.String 
				    (C_syntax_symbol.String_literal tail)))
			  end;
			end
		      else
			assert false
		    end
		| Typ_mem.Bits (c_typ, init_expr) ->
		    assert false
		| _ -> assert false
	      in ret

	  | I.String (C_syntax_symbol.WString_literal c_string_literal) -> 
	      let ret = match memory with
		| Typ_mem.Array (c_typ, wchar_array) ->
		    begin
		      if TO.is_array_typ c_typ then
			begin
			  let elmt_typ = TO.elmt_of c_typ
			  and len = Array.length wchar_array
			  in
			  if TO.is_wchar_typ elmt_typ then
			    ignore(List.fold_left
			      (fun index c -> 
				let _ = if index >= len then
				  if c = 0L then ()
				  else
				    camlp4_macro_exception 
				      "string overflow \n" 
				else
				  let (elmt_mem) = 
				    Array.get wchar_array index 
				  in set_scalar_variable_wchar elmt_mem c
				in index + 1
			      ) 0 (c_string_literal @$ [0L]))
			  else
			    assert false
			end;
		      None
		    end
		| Typ_mem.Xarray 
		    (c_typ, init_expr, char_mem_list, max_len) 
		  ->
		    begin
		      if TO.is_xarray_typ c_typ then
			begin
			  let elmt_typ = TO.elmt_of c_typ
			  in
			  if TO.is_wchar_typ elmt_typ then
			    ignore(List.fold_left
			      (fun index c -> 
				let elmt_mem = 
				  alloc_c_initializer elmt_typ init_expr
				in
				set_scalar_variable_wchar elmt_mem c;
				char_mem_list := 
				  !char_mem_list @$ [(index, elmt_mem)];
				max_len := max (index+1) !max_len;
				index + 1
			      ) 0 (c_string_literal @$ [0L]))
			  else
			    assert false
			end;
		      None
		    end
		| Typ_mem.Scalar (c_typ, init_expr) -> 
		    begin
		      if TO.is_ptr_typ c_typ then
			begin
			  let elmt_typ = 
			    TO.elmt_of c_typ
			  in
			  if TO.is_char_typ elmt_typ then
			    begin
			      let cstr = find_cstr_lit_cexpr cmpstate
				(C_syntax_symbol.WString_literal 
				  c_string_literal)
			      in
			      init_expr := TMPStatic_init cstr
			    end
			  else if TO.is_void_typ elmt_typ
			  then
			    begin
			      camlp4_macro_warning 
				"warning: type mismatch\n";
			      let cstr = find_cstr_lit_cexpr cmpstate
				(C_syntax_symbol.WString_literal 
				  c_string_literal)
			      in
			      init_expr := TMPStatic_init cstr
			    end
			  else
			    assert false;
			  None
			end
		      else if TO.is_wchar_typ c_typ then
			begin
			  begin
			    let str_len = 
			      Safe_list.length c_string_literal
			    in
			    let (has_done, hd, tail) = 
			      if str_len = 0 then
				(true, 0L, [])
			      else if str_len = 1 then
				(true, Safe_list.hd c_string_literal, [])
			      else if str_len > 1 then
				(false, Safe_list.hd c_string_literal,
				Safe_list.tl c_string_literal)
			      else
				assert false
			    in
			    set_scalar_variable_wchar memory hd;
			    if has_done then None
			    else 
			      Some 
				(I.Initializer_1 
				  (I.String 
				    (C_syntax_symbol.WString_literal tail)))
			  end;
			end
		      else
			assert false
		    end
		| Typ_mem.Bits (c_typ, init_expr) ->
		    assert false
		| _ -> assert false
	      in ret
		   
	  | _ -> 
	      begin
		let _ = match memory with
		  | Typ_mem.Bits (c_typ, init_expr) ->
		      begin
			try
			  init_expr := 
			    TMPStatic_init
			      (evaluate_c_constant_expression cmpstate c_expression)
			with
			  | Dynamic _->
			      let _ = assert_link_time_constant init_expr c_expression
			      in
			      init_expr :=
				let c_expression = 
				  compile_c_expression cmpstate c_expression
				in
				TMPDynamic_init c_expression
		      end
		  | Typ_mem.Scalar (c_typ, init_expr) ->
		      begin
			try
			  init_expr := 
			    TMPStatic_init
			      (evaluate_c_constant_expression cmpstate c_expression)
			with
			  | Dynamic _->
			      let _ = assert_link_time_constant init_expr c_expression
			      in
			      init_expr :=
				let c_expression = 
				  compile_c_expression cmpstate c_expression
				in
				TMPDynamic_init c_expression
		      end
		  | Typ_mem.Array (c_typ, a) ->
		      begin
			if !Mlite_config.enable_log then
			  camlp4_macro_warning 
			    "incomplete right hand side\n";
			let (first_elmt_mem) = Array.get a 0
			in
			let v = 
			  compile_c_initializer cmpstate first_elmt_mem expr
			in
			match v with
			  | Some _ -> ()
			  | None -> ()
		      end
		  | _ ->
		      set_memory_block_val memory c_expression
		in None
	      end
	in ret
	     
    | I.Initializer_2 c_initializer_list ->
	let _ = match memory with
	  | Typ_mem.Struct (_, a, _) ->
	      compile_top_c_initializer_list_in_struct cmpstate
		a c_initializer_list
	  | Typ_mem.Union (_, a, _) ->
	      let first_elmt_array = Array.create 1 (Array.get a 0)
	      in
	      compile_top_c_initializer_list_in_struct cmpstate
		first_elmt_array c_initializer_list
	  | Typ_mem.Array (_, a) ->
	      compile_top_c_initializer_list_in_array cmpstate
		a c_initializer_list
	  | Typ_mem.Xarray _ -> assert false
	  | Typ_mem.Scalar _ ->
	      let _ = try
		let _ = 
		  compile_c_designator_list_in_scalar cmpstate
		    memory (ref 0) c_initializer_list
		in assert false
	      with
		| Next remainder 
		| Finished remainder ->
		    if remainder <> [] then
		      camlp4_macro_exception 
			"excess elements in initializer\n"
	      in ()
		   
	  | Typ_mem.Bits (c_typ, init_expr) ->
	      assert false
	  | Typ_mem.Null -> assert false
	in None
  in ret
  with 
    | Next v 
    | Finished v -> assert false
	
and compile_top_c_initializer_list_in_struct cmpstate
    (memory:(Csize.csize * string option * c_tmp_initializer) array) 
    (c_initializer_list:I.c_initializer_list):unit = 
  let tail = ref c_initializer_list
  and index = ref 0
  in
  while !tail <> [] do
    try
      compile_c_designator_list_in_struct cmpstate (false, memory) index !tail;
      assert false
    with
      | Finished remainder 
      | Next remainder -> 
	  begin
	    incr index;
	    tail := remainder;
	  end
  done
    
and compile_top_c_initializer_list_in_array cmpstate
    (memory:c_tmp_initializer array) 
    (c_initializer_list:I.c_initializer_list):unit = 
  let tail = ref c_initializer_list
  and index = ref 0
  in
  while !tail <> [] do
    try
      begin
	compile_c_designator_list_in_array cmpstate
	  (false, memory) index !tail;
	assert false
      end
    with
      | Finished remainder 
      | Next remainder -> 
	  begin
	    incr index;
	    tail := remainder;
	  end
  done

and compile_top_c_initializer_list_in_xarray cmpstate
    (memory:c_tmp_initializer) 
    (c_initializer_list:I.c_initializer_list):unit = 
  let tail = ref c_initializer_list
  and index = ref 0
  in
  while !tail <> [] do
    try
      begin
	compile_c_designator_list_in_xarray cmpstate
	  memory index !tail;
	assert false
      end
    with
      | Finished remainder 
      | Next remainder -> 
	  begin
	    incr index;
	    tail := remainder;
	  end
  done
    
and compile_c_initializer_list cmpstate (memory:c_tmp_initializer)
    (c_initializer_list:I.c_initializer_list):unit =
  let tail = ref c_initializer_list
  in
  try let _ = match memory with
    | Typ_mem.Null -> assert false
    | Typ_mem.Bits (c_typ, init_expr) ->
	compile_c_designator_list_in_scalar cmpstate
	  memory (ref 0) !tail
    | Typ_mem.Scalar _ -> 
	compile_c_designator_list_in_scalar cmpstate
	  memory (ref 0) !tail
    | Typ_mem.Union  (_, field_array, _) ->
	compile_c_designator_list_in_struct cmpstate
	  (!(ref true), field_array) (ref 0) !tail
    | Typ_mem.Struct (c_typ, field_array, _) -> 
	let field_index = ref 0
	and is_top = ref true
	in
	let _ = 
	  while !tail <> [] do
	    try 
	      let _ =
		compile_c_designator_list_in_struct cmpstate
		  (!is_top, field_array) field_index !tail
	      in assert false
	    with
	      | Next remainder ->
		  begin
		    is_top := false;
		    incr field_index;
		    tail := remainder;
		  end
	      | Finished remainder ->
		  raise (Next remainder)
	  done
	in raise (Next [])
	     
    | Typ_mem.Array (c_typ, elmt_mem_array) -> 
	begin
	  let elmt_typ = TO.elmt_of c_typ
	  in
	  if TO.is_char_typ elmt_typ then
	    begin
	      let (c_designator_list, c_initializer) = 
		Safe_list.hd c_initializer_list
	      in
	      match c_initializer with
		| I.Initializer_1 _ ->  
		    begin
		      assert (c_designator_list = []);
		      let v = 
			compile_c_initializer cmpstate
			  memory c_initializer
		      in
		      match v with
			| Some v -> assert false
			| None -> 
			    raise (Finished (Safe_list.tl c_initializer_list))
		    end
		| I.Initializer_2 lst ->
		    begin
		      let index = ref 0
		      and tail = ref lst
		      in
		      match !tail with
			| [a] -> 
			    begin
			      let (c_designator_list, c_initializer) = a
			      in
			      assert (c_designator_list = []);
			      let v = 
				compile_c_initializer cmpstate
				  memory c_initializer
			      in
			      match v with
				| Some v -> assert false
				| None -> 
				    raise 
				      (Finished 
					(Safe_list.tl c_initializer_list))
			    end
			| _ -> 
			    begin
			      while !tail <> [] do
				try
				  let _ = 
				    compile_c_designator_list_in_array cmpstate
				      (false, elmt_mem_array) index !tail
				  in assert false
				with
				  | Finished remainder 
				  | Next remainder -> 
				      begin
					incr index;
					tail := remainder;
				      end
			      done;
			      raise (Finished [])
			    end
		    end
	    end
	  else
	    begin
	      let elmt_index = ref 0
	      and is_top = ref true
	      in
	      while !tail <> [] do
		try 
		  let _ = compile_c_designator_list_in_array cmpstate
		    (!is_top, elmt_mem_array)
		    elmt_index !tail;
		  in assert false
		with
		  | Next remainder ->
		      begin
			is_top := false;
			incr elmt_index;
			tail := remainder;
		      end
		  | Finished remainder ->
		      raise (Next remainder)
	      done;
	      raise (Next [])
	    end
	end
    | Typ_mem.Xarray _ -> assert false
  in ()
  with
      Finished tail -> raise (Next tail)

and compile_c_designator_list_in_struct cmpstate
    ((is_top,field_array):bool * (Csize.csize * string option * c_tmp_initializer) array)
    (used_index: int ref) (tail:I.c_initializer_list):unit = 
  let (c_designator_list ,c_initializer) = Safe_list.hd tail
  in
  match c_designator_list with
    | [] -> 
	if !used_index < Array.length field_array then
	  let rec get_field_memory () =
	    let (offset, field_opt, field_memory) = 
	      Array.get field_array !used_index
	    in
	    if field_opt = None then
	      begin
		incr used_index;
		if !used_index < Array.length field_array then
		  get_field_memory ()
		else
		  raise (Finished tail)
	      end
	    else
	      field_memory
	  in
	  let field_memory = get_field_memory ()
	  in
	  match c_initializer with
	    | I.Initializer_1 _ ->
		compile_c_initializer_list cmpstate field_memory tail
	    | I.Initializer_2 lst ->
		if is_top then
		  let _ = 
		    compile_top_c_initializer_list_in_struct cmpstate
		      field_array lst;
		  in raise (Finished (Safe_list.tl tail))
		else
		  compile_c_initializer_list cmpstate field_memory tail
	else
	  raise (Finished tail)
	    
    | a::tl -> (* just locate the memory position *)
	begin
	  let (used_mem, actual_index) = 
	    match a with
	      | I.Designator_1 c_constant_expression -> 
		  raise (Finished tail)
	      | I.Designator_2 s -> 
		  begin
		    try 
		      let _ = 
			Array.iteri 
			  (fun i (offset, f_opt, mem) ->
			    match f_opt with
			      | Some f -> 
				  if s = f then raise (Found_field (i, mem))
			      | None -> ()
			  ) field_array
		      in
		      raise (Finished tail)
		    with
			Found_field (i, mem) ->
			  (mem, i)
		  end
	      | I.Designator_gnu_range (e0, e1) -> assert false
	  in 
	  used_index := actual_index;
	  compile_c_initializer_list cmpstate
	    used_mem 
	    ((tl, c_initializer)::(Safe_list.tl tail))
	end


and compile_c_designator_list_in_array cmpstate
    ((is_top, elmt_mem_array):bool * c_tmp_initializer array) 
    (used_index: int ref)
    (tail:I.c_initializer_list):unit = 
  let (c_designator_list ,c_initializer) = Safe_list.hd tail
  in
  match c_designator_list with
    | [] -> 
	begin
	  if !used_index < Array.length elmt_mem_array then
	    begin
	      let elmt_mem = Array.get elmt_mem_array !used_index
	      in
	      let compile_elmt () = 
		compile_c_initializer_list cmpstate elmt_mem tail
	      in
	      match c_initializer with
		| I.Initializer_1 _ -> compile_elmt ()
		| I.Initializer_2 lst ->
		    begin
		      if is_top then
			begin
			  compile_top_c_initializer_list_in_array cmpstate
			    elmt_mem_array lst;
			  raise (Finished (Safe_list.tl tail))
			end
		      else
			compile_elmt ()
		    end
	    end
	  else
	    raise (Finished tail)
	end
    | a::tl -> (* just locate the memory position *)
	begin
	  let (used_mem, actual_index) = 
	    match a with
	      | I.Designator_gnu_range (e0, e1) -> 
		  begin
		    let idx0 = 
		      evaluate_c_constant_expression_as_int cmpstate e0
		    and idx1 = 
		      evaluate_c_constant_expression_as_int cmpstate e1
		    in
		    begin
		      if idx0 < Array.length elmt_mem_array &
			idx1 < Array.length elmt_mem_array
		      then
			begin
			  let elmt_mem = Array.get elmt_mem_array idx0
			  in
			  for i = idx0 + 1 to idx1 do
			    Array.set elmt_mem_array i elmt_mem
			  done;
			  (elmt_mem, idx1)
			end
		      else
			assert false
		    end
		  end
	      | I.Designator_1 c_constant_expression -> 
		  begin
		    let index = 
		      evaluate_c_constant_expression_as_int cmpstate
			c_constant_expression
		    in
		    begin
		      if index < Array.length elmt_mem_array then
			begin
			  let elmt_mem = Array.get elmt_mem_array index
			  in
			  (elmt_mem, index)
			end
		      else
			assert false
		    end
		  end
	      | I.Designator_2 _ -> raise (Finished tail)
	  in 
	  used_index := actual_index;
	  compile_c_initializer_list cmpstate
	    used_mem ((tl, c_initializer)::(Safe_list.tl tail))
	end

and compile_c_designator_list_in_xarray cmpstate
    (memory:c_tmp_initializer) (used_index: int ref) 
    (tail:I.c_initializer_list):unit = 
  if tail <> [] then
    begin
      let (c_designator_list ,c_initializer) = Safe_list.hd tail
      in
      match c_designator_list with
	| [] -> 
	    begin
	      let _ = match c_initializer with
		| I.Initializer_2 [] -> 
		    raise (Finished (Safe_list.tl tail));
		| I.Initializer_2 _ 
		| I.Initializer_1 _ -> ()
	      in
	      match memory with
		| Typ_mem.Xarray (c_typ, init_expr, elmt_mem_list, max_len) ->
		    begin
		      let elmt_mem = 
			try
			  let (_, elmt_mem) = 
			    Safe_list.find 
			      (fun (i, _) -> i = !used_index) 
			      !elmt_mem_list
			  in
			  elmt_mem
			with
			    Not_found ->
			      begin
				let elmt_typ = TO.elmt_of c_typ
				in
				let elmt_mem = 
				  alloc_c_initializer elmt_typ init_expr
				in
				elmt_mem_list := 
				  !elmt_mem_list @$ [(!used_index, elmt_mem)];
				max_len := max (!used_index+1) !max_len;
				elmt_mem
			      end
		      in
		      compile_c_initializer_list cmpstate elmt_mem tail
		    end
		| _ -> assert false
	    end
	| a::tl -> (* just locate the memory position *)
	    begin
	      let (used_mem, actual_index) = 
		match a with
		  | I.Designator_1 c_constant_expression -> 
		      begin
			let index = 
			  evaluate_c_constant_expression_as_int cmpstate
			    c_constant_expression
			in
			match memory with
			  | Typ_mem.Null -> assert false
			  | Typ_mem.Bits (c_typ, init_expr) ->
			      assert false
			  | Typ_mem.Scalar _ -> assert false
			  | Typ_mem.Struct _ -> raise (Finished tail)
			  | Typ_mem.Union _ -> raise (Finished tail)
			  | Typ_mem.Array _ -> raise (Finished tail)
			  | Typ_mem.Xarray 
			      (c_typ, init_expr, elmt_mem_list, max_len) 
			    ->
			      begin
				let (actual_index, elmt_mem) = 
				  try
				    Safe_list.find 
				      (fun (i, _) -> i = index) 
				      !elmt_mem_list
				  with
				      Not_found ->
					begin
					  let elmt_typ = TO.elmt_of c_typ
					  in
					  let elmt_mem = 
					    alloc_c_initializer elmt_typ init_expr
					  in
					  elmt_mem_list := 
					    !elmt_mem_list @$ 
					      [(index, elmt_mem)];
					  max_len := max (index+1) !max_len;
					  (index, elmt_mem)
					end
				in
				(elmt_mem, actual_index)
			      end
		      end
		  | I.Designator_gnu_range (e0, e1) ->  
		      begin
			let idx0 = 
			  evaluate_c_constant_expression_as_int cmpstate e0
			and idx1 = 
			  evaluate_c_constant_expression_as_int cmpstate e1
			in
			match memory with
			  | Typ_mem.Null -> assert false
			  | Typ_mem.Bits (c_typ, init_expr) ->
			      assert false
			  | Typ_mem.Scalar _ -> assert false
			  | Typ_mem.Struct _ -> raise (Finished tail)
			  | Typ_mem.Union _ -> raise (Finished tail)
			  | Typ_mem.Array _ -> raise (Finished tail)
			  | Typ_mem.Xarray 
			      (c_typ, init_expr, elmt_mem_list, max_len)
			    ->
			      begin
				let (actual_index, elmt_mem) = 
				  try
				    Safe_list.find 
				      (fun (i, _) -> i = idx0) 
				      !elmt_mem_list
				  with
				      Not_found ->
					begin
					  let elmt_typ = TO.elmt_of c_typ
					  in
					  let elmt_mem = 
					    alloc_c_initializer
					      elmt_typ init_expr
					  in
					  elmt_mem_list := 
					    !elmt_mem_list @$ 
					      [(idx0, elmt_mem)];
					  max_len := max (idx1) !max_len;
					  (idx1, elmt_mem)
					end
				in
				(elmt_mem, actual_index)
			      end
		      end
		  | I.Designator_2 _ -> assert false
		      
	      in 
	      used_index := actual_index;
	      compile_c_initializer_list cmpstate used_mem 
		((tl, c_initializer)::(Safe_list.tl tail))
	    end
    end
  else
    raise (Next [])


and compile_c_designator_list_in_scalar cmpstate
    (memory:c_tmp_initializer) (used_index: int ref)
    (tail:I.c_initializer_list): unit = 
  let (c_designator_list ,c_initializer) = Safe_list.hd tail
  in
  match c_designator_list with
    | [] -> 
	begin
	  match memory with
	    | Typ_mem.Null  -> assert false
	    | Typ_mem.Bits _ 
	    | Typ_mem.Scalar _ -> 
		begin
		  let v = 
		    compile_c_initializer cmpstate memory c_initializer
		  in
		  match v with
		    | Some c_initializer -> 
			(* must be initialization of characters **)
			raise (Next 
			  (([], c_initializer)::(Safe_list.tl tail)))
		    | None ->
			raise (Finished (Safe_list.tl tail))
		end
	    | Typ_mem.Struct _ -> assert false
	    | Typ_mem.Union _ -> assert false
	    | Typ_mem.Array _ -> assert false
	    | Typ_mem.Xarray _ -> assert false
	end
    | a::tl -> 
	raise (Finished tail)

	  
and compile_c_stmt010 cmpstate (expr:I.c_stmt010): O.c_stmt010 * te =
  let env = cmpstate.env
  in
  match expr with
    | I.STMT_AT (coord, stmt) -> 
	let (stmt, te) = compile_c_stmt010 cmpstate stmt
	in (O.STMT_AT(coord, stmt), te)
	     
    | I.NOP -> (O.NOP, env.E.non_typ)
	
    | I.SEQUENCE (txt_opt, c_stmt010_list) ->
	let (stmts, typs) = 
	  Safe_list.split 
	    (Safe_list.map (compile_c_stmt010 cmpstate) c_stmt010_list)
	in
	let get_type cmpstate typs =
	  let rec get_typ = function
	    | typ::l' -> 
		if typ == env.E.non_typ then
		  get_typ l'
		else typ
	    | [] -> env.E.non_typ
	  in
	  get_typ (Safe_list.rev typs)
	in (O.SEQUENCE (txt_opt, stmts), get_type cmpstate typs)
	     
    | I.COMPUTATION (c_expression) ->
	let str = camlp4_macro_str_pp_print
	  (fun fm -> 
	    IP.pp_print_c_expression fm ~need_paren:false c_expression) 
	in
	let (stmts, expr) = compile_c_expr020_rval cmpstate c_expression
	in
	let expr_O_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv expr
	in
	recycle_tmp_ces cmpstate stmts [];
	(O.SEQUENCE (Some str, stmts @$
	  [O.COMPUTATION (O.Rexpr (O.Rval expr))]), expr_O_rv_typ)
	  
    | I.COMPOUND (txt_opt, c_compound_stmt010) ->
	let (stmt', typ') = compile_c_compound_stmt010 
	  cmpstate c_compound_stmt010
	in (O.COMPOUND (txt_opt, stmt'), env.E.void_typ)
	     
    | I.IF (c_expression, then_c_stmt010, else_c_stmt010) ->
	let str = camlp4_macro_str_pp_print
	  (fun fm -> 
	    IP.pp_print_c_expression fm ~need_paren:false c_expression) 
	in
	let (lstmt, expr) =  compile_c_expr020_true_cond cmpstate c_expression
	in
	let _ = recycle_tmp_ces cmpstate lstmt []
	in
	let (then_stmt', then_typ') = compile_c_stmt010 cmpstate then_c_stmt010 
	and (else_stmt', else_typ') = compile_c_stmt010 cmpstate else_c_stmt010
	in
	let (s, t) = 
	  if lstmt <> [] then
	    (O.SEQUENCE (None, lstmt @$ 
	      [O.IF (expr, then_stmt', else_stmt')]), env.E.void_typ)
	  else
	    (O.IF (expr, then_stmt', else_stmt'), env.E.void_typ)
	in (O.STMT_SPAN (str, s), t)
	     
    | I.WHILE (c_expression, c_stmt010) -> 
	let str = camlp4_macro_str_pp_print
	  (fun fm -> 
	    IP.pp_print_c_expression fm ~need_paren:false c_expression) 
	in
	let (lstmt, expr) = compile_c_expr020_true_cond cmpstate c_expression
	in
	let _ = recycle_tmp_ces cmpstate lstmt []
	in
	let (stmt, typ') = compile_c_stmt010 cmpstate c_stmt010
	in
	let (s, t) = 
	  if lstmt <> [] then
	    (O.LOOP (O.COMPOUND 
	      (None, O.BLOCK ([], [], [O.SEQUENCE (None, lstmt
		@$ [O.IF (expr, stmt, O.BREAK)])]))), 
	    env.E.void_typ)
	  else
	    (O.WHILE (expr, stmt), env.E.void_typ)
	in (O.STMT_SPAN (str, s), t)
	     
    | I.LOOP (c_stmt010) -> 
	let (stmt', typ') = compile_c_stmt010 cmpstate c_stmt010
	in 
	(O.LOOP stmt', env.E.non_typ)
	  
    | I.BREAK -> (O.BREAK, env.E.non_typ)
	
    | I.CONTINUE -> (O.CONTINUE, env.E.non_typ)
	
    | I.RETURN_VALUE (c_expression) -> 
	let (ret_opt, ret_lbl) = EO.get_ret_info env
	in	  
	let str = camlp4_macro_str_pp_print
	  (fun fm -> 
	    IP.pp_print_c_expression fm ~need_paren:false c_expression) 
	in
	let (s, t) = match c_expression with
	  | I.Cast_init (c_type_name, inits) ->
	      let ret_lval = match ret_opt with
		| E.HIDDEN_PARAM ce -> O.Lladdr (O.Nctn ce)
		| E.NORMAL_RETURN ce -> mk_lhs ce
		| E.VOID_RETURN -> assert false
	      in
	      let cast_typ = compile_c_type cmpstate c_type_name
	      in
	      if TO.is_xarray_typ cast_typ or TO.is_ptr_typ cast_typ then
		assert false (* reference escapes the current scope *)
	      else
		let memory = alloc_top_c_initializer I.Auto cast_typ
		in
		let _ = 
		  compile_top_c_initializer 
		    cmpstate memory (I.Initializer_2 inits)
		and var = EO.add_fresh_storage env cast_typ false
		in
		let l = make_asign_init 
		  cmpstate (O.Lladdr (O.Nlbl var)) memory
		in
		let ret_stmts = 
		  normalized_assign_stmts cmpstate
		    (ret_lval, O.Rval (O.Rladdr (O.Nctn var)))
		in
		(O.SEQUENCE (None, l @$ ret_stmts @$ [O.GOTO ret_lbl]),
		env.E.void_typ)
	  | _ ->
	      let (lstmt, expr) = compile_c_expression cmpstate c_expression
	      in
	      let ret_stmts = match ret_opt with
		| E.HIDDEN_PARAM ce ->
		    normalized_assign_stmts cmpstate 
		      (O.Lladdr (O.Nctn ce), expr)
		| E.NORMAL_RETURN ce ->
		    normalized_assign_stmts cmpstate
		      (mk_lhs ce, expr)
		| E.VOID_RETURN -> []
	      in
	      (O.SEQUENCE (None, lstmt @$ ret_stmts @$ [O.GOTO ret_lbl]),
	      env.E.void_typ)
	in (O.STMT_SPAN (str, s), t)
	     
    | I.RETURN -> 
	let (ret_opt, ret_lbl) = EO.get_ret_info env
	in (O.GOTO ret_lbl, env.E.void_typ)
	     
    | I.SWITCH (c_expression, c_stmt010) ->
	let str = camlp4_macro_str_pp_print
	  (fun fm -> 
	    IP.pp_print_c_expression fm ~need_paren:false c_expression) 
	in
	let (lstmt, expr) = compile_c_expr020_rval cmpstate c_expression
	in
	let _ = recycle_tmp_ces cmpstate lstmt []
	in
	let (stmt, typ') = compile_c_stmt010 cmpstate c_stmt010
	in
	let (s, t) =
	  if lstmt <> [] then
	    (O.SEQUENCE (None, lstmt @$ [O.SWITCH (expr, stmt)]), 
	    env.E.void_typ)
	  else
	    (O.SWITCH (expr, stmt), env.E.void_typ)
	in (O.STMT_SPAN (str, s), t)
	     
    | I.CASE (c_constant_expression, c_stmt010) ->
	let (stmt', typ') = compile_c_stmt010 cmpstate c_stmt010
	in
	(O.CASE (evaluate_c_constant_expression cmpstate c_constant_expression, 
	stmt'), typ')

    | I.CASE_RANGE (e0, e1, c_stmt010) ->
	let (stmt', typ') = compile_c_stmt010 cmpstate c_stmt010
	in
	(O.CASE_RANGE 
	  (evaluate_c_constant_expression cmpstate e0,
	  evaluate_c_constant_expression cmpstate e1,
	  stmt'), typ')
	  
    | I.DEFAULT (c_stmt010) ->
	let (stmt', typ') = compile_c_stmt010 cmpstate c_stmt010
	in (O.DEFAULT (stmt'), typ')
	  
    | I.LABEL (string, c_stmt010) ->
	let (stmt', typ') = compile_c_stmt010 cmpstate c_stmt010
	in (O.LABEL (string, stmt'), typ')
	  
    | I.GOTO (string) -> 
	(O.GOTO (string), env.E.void_typ)

    | I.GCC_GOTO expr ->
	let (stmts, expr) = compile_c_expr020_rval cmpstate expr
	in (O.SEQUENCE (None, stmts @$ [O.GCC_GOTO expr]), env.E.void_typ)

    | I.ASM (str_list, asm_details_opt) ->
	let lst = ref []
	in
	let asm_details_opt = match asm_details_opt with
	  | Some asm_details ->
	      let fl (so, s, e) = 
		let (l, e) = compile_c_expr020_lval cmpstate e
		in
		let _ = lst := !lst @$ l
		in (so, s, e)
	      and fv (so, s, e) = 
		let (l, e) = compile_c_expr020_rval cmpstate e
		in
		let _ = lst := !lst @$ l
		in (so, s, e)
	      in
	      Some 
		({
		  O.asm_outputs = Safe_list.map fl asm_details.I.asm_outputs;
		  O.asm_inputs = Safe_list.map fv asm_details.I.asm_inputs;
		  O.asm_clobbers = asm_details.I.asm_clobbers;
		})
	  | None -> None
	in
	(O.SEQUENCE (None, !lst @$ [O.ASM (str_list, asm_details_opt)]), 
	env.E.void_typ)
       
and extract_c_stmt010 cmpstate var stmt = 
  match stmt with
    | O.STMT_SPAN (str, c_stmt010) ->
	let (s, e) = extract_c_stmt010 cmpstate var c_stmt010
	in (O.STMT_SPAN (str, s), e)
	     
    | O.STMT_AT (loc, c_stmt010) -> 
	let (s, e) = extract_c_stmt010 cmpstate var c_stmt010
	in (O.STMT_AT (loc, s), e)
	     
    | O.NOP -> assert false
	
    | O.COMPUTATION expr ->
	begin
	  match expr with
	    | O.Rexpr e -> 
		begin
		  let e_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv e
		  in
		  if (TO.is_void_typ e_O_re_typ) then
		    (O.NOP, e_O_re_typ)
		  else
		    (O.SEQUENCE 
		      (None, normalized_assign_stmts cmpstate
			(mk_lhs var, e)), e_O_re_typ)
		end
	    | _ -> assert false
	end
	  
    | O.SESE expr_list ->
	begin
	  let rec last = function 
	    | [a] -> a
	    | a::l' -> last l'
	    | [] -> assert false
	  in
	  let expr = last expr_list
	  in
	  match expr with
	    | O.Rexpr e -> 
		let e_O_re_typ = AEE.get_rexpr cmpstate.env cmpstate.eenv e
		in
		(O.SEQUENCE 
		  (None, normalized_assign_stmts cmpstate
		    (mk_lhs var, e)), e_O_re_typ)
	    | _ -> assert false
	end

    | O.CALL _ -> assert false
	
    | O.SEQUENCE (v, stmts) -> 
	let (l, e) = extract_c_stmt010_list cmpstate var stmts
	in (O.SEQUENCE (v, l), e)
	     
    | O.COMPOUND (v, cstmt) ->
	let (c, e) = extract_c_compound_stmt010 cmpstate var cstmt
	in (O.COMPOUND (v, c), e)
	     
    | O.IF (cond, then_stmt, else_stmt) ->
	let (l0, t0) = extract_c_stmt010 cmpstate var then_stmt
	and (l1, t1) = extract_c_stmt010 cmpstate var else_stmt
	in (O.IF (cond, l0, l1), TO.lub [t0;t1]) 
	     
    | O.WHILE _  -> assert false
    | O.LOOP _  -> assert false
    | O.BREAK  -> assert false
    | O.CONTINUE -> assert false
    | O.RETURN_VALUE _ -> assert false
    | O.RETURN -> assert false
	
    | O.SWITCH (e0, stmt) -> 
	let (l0, t0) = extract_c_stmt010 cmpstate var stmt
	in (O.SWITCH (e0, l0), t0)
	     
    | O.CASE (e0, stmt) -> 
	let (l0, t0) = extract_c_stmt010 cmpstate var stmt
	in (O.CASE (e0, l0), t0)
	     
    | O.CASE_RANGE (e0, e1,stmt) -> 
	let (l0, t0) = extract_c_stmt010 cmpstate var stmt
	in (O.CASE_RANGE (e0, e1, l0), t0)
	     
    | O.DEFAULT stmt -> 
	let (l0, t0) = extract_c_stmt010 cmpstate var stmt
	in (O.DEFAULT l0, t0)

    | O.LABEL (str, stmt) -> 
	let (s, e) = extract_c_stmt010 cmpstate var stmt
	in (O.LABEL (str, s), e)
	     
    | O.GOTO str -> assert false
    | O.EPI _ -> assert false
    | O.GCC_GOTO _ -> assert false
    | O.ASM _ -> assert false
	
and extract_c_stmt010_list cmpstate var stmts =
  match stmts with
    | [a] -> 
	let (s, e) = extract_c_stmt010 cmpstate var a
	in ([s],e)
    | a::l -> 
	let (l', e) = extract_c_stmt010_list cmpstate var l
	in
	(a::l', e)
    | [] -> assert false
	
and extract_c_compound_stmt010 cmpstate var (O.BLOCK (labels, decls, stmts)) = 
  let (l, e) = extract_c_stmt010_list cmpstate var stmts
  in (O.BLOCK (labels, decls, l), e)
       
and convert_to_local_decl cmpstate = function
  | TMPLocal_obj_decl (l, ce) -> O.Local_obj_decl (l, ce)
  | TMPLocal_obj_decl_init (l, ce, c) -> 
      O.Local_obj_decl_init (l, ce, convert_initializer c)
  | TMPLocal_obj_init e -> assert false
  | TMPLocal_type_def t -> O.Local_type_def t
  | TMPLocal_type_decl t -> O.Local_type_decl t
  | TMPLocal_type_only t -> O.Local_type_only t
  | TMPLocal_darray_decl (slst, l, ce, s) -> assert false
  | TMPLocal_external_decl _ -> assert false

and mk_lhs ce = 
  let ce_info = CEO.ce_info_of ce
  in
  if ce_info.CE.ce_is_register then
    O.Lreg ce
  else
    O.Lladdr (O.Nlbl ce)
      
and compile_c_compound_stmt010 cmpstate (expr:I.c_compound_stmt010)
    :(O.c_compound_stmt010 * te) = 
  let env = cmpstate.env
  in
  let compile (expr:I.c_compound_stmt010) 
      :(O.c_compound_stmt010 * te) = 
    let rec normalize (start:int) (decls: I.c_declaration_ex array) 
	(stmts:I.c_stmt010 list) :O.c_compound_stmt010 =
      try
	let tmp_list = ref []
	in
	let _ = EO.push_compiler_gen_scope env
	in
	for i = start to (Array.length decls - 1) do
	  let (decl, b) = Array.get decls i
	  in
	  let decls' = compile_local_c_declaration_ex cmpstate (decl, b)
	  in
	  let (hd, decl') = match decls' with
	    | [a] -> (None, a)
	    | [a;b] -> (Some a, b)
	    | _ -> assert false
	  in
	  match decl' with
	    | TMPLocal_obj_decl_init (O.Static, ce, v2) ->
		begin
		  let decl = match hd with
		    | Some a -> tmp_list := !tmp_list @$
			[convert_to_local_decl cmpstate a]
		    | None -> ()
		  in
		  tmp_list := !tmp_list @$ 
		    [O.Local_obj_decl_init (O.Static, ce, convert_initializer v2)]
		end		   
		  
	    | TMPLocal_darray_decl (l, linkage, ce, size)->
		begin
		  assert (hd == None);
		  let this_decl = O.Local_obj_decl (linkage,ce)
		  and cstmt = normalize (i+1) decls stmts
		  in
		  let (new_tmp_syms, new_typs) = EO.pop_compiler_gen_scope env
		  in
		  let new_decls = mk_tool_gen_decls new_tmp_syms
		  and new_ty_decls = 
		    List.map (fun t -> O.Local_type_decl t) new_typs
		  in
		  let c_type = (CEO.ce_info_of ce).CE.ce_te
		  in
		  let cstmt = 
		    O.BLOCK ([], !tmp_list @$ [this_decl] @$ new_ty_decls @$ new_decls, 
		    l @$ [O.COMPUTATION (O.Alloca (mk_lhs ce, c_type, size));O.COMPOUND (None, cstmt)])
		  in
		  raise (CStmt cstmt)		      
		end
		  
	    | TMPLocal_obj_decl_init (linkage, ce, c_initializer)
	      ->
		begin
		  let c_type = (CEO.ce_info_of ce).CE.ce_te
		  and string = ce
		  in
		  match hd with 
		    | None ->
			begin
			  tmp_list := !tmp_list @$ [O.Local_obj_decl (linkage,ce)];
			  let l = 
			    make_asign_init cmpstate 
			      (mk_lhs ce) c_initializer
			  in
			  recycle_tmp_ces cmpstate l [];
			  let cstmt = normalize (i+1) decls stmts
			  in
			  let (new_tmp_syms, new_typs) = EO.pop_compiler_gen_scope env
			  in
			  let new_decls = mk_tool_gen_decls new_tmp_syms
			  and new_ty_decls = 
			    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
			  in
			  raise (CStmt 
			    (O.BLOCK ([], !tmp_list @$ new_ty_decls @$ new_decls, 
			    l @$ [O.COMPOUND (None, cstmt)])))
			end
		    | Some a ->
			begin
			  match a with
			    | TMPLocal_obj_decl_init (linkage0, ce0, c_initializer0) ->
				begin
				  let c_type0 = (CEO.ce_info_of ce0).CE.ce_te
				  in
				  tmp_list := !tmp_list @$ [O.Local_obj_decl
				    (linkage0, ce0)];
				  let l0 = 
				    make_asign_init cmpstate (mk_lhs ce0) c_initializer0
				  in	
				  recycle_tmp_ces cmpstate l0 [];
				  let _ = EO.push_compiler_gen_scope env
				  in
				  let l = 
				    make_asign_init cmpstate (mk_lhs string) c_initializer
				  in
				  recycle_tmp_ces cmpstate l [];
				  let cstmt = normalize (i+1) decls stmts
				  in				    
				  let (new_tmp_syms, new_typs) = EO.pop_compiler_gen_scope env
				  in
				  let new_decls = mk_tool_gen_decls new_tmp_syms
				  and new_ty_decls = 
				    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
				  in
				  let (new_tmp_syms0, new_typs0) = EO.pop_compiler_gen_scope env
				  in
				  let new_decls0 = mk_tool_gen_decls new_tmp_syms0
				  and new_ty_decls0 = 
				    Safe_list.map (fun t -> O.Local_type_decl t)  new_typs0
				  in
				  raise 
				    (CStmt 
				      (O.BLOCK ([], !tmp_list @$ new_ty_decls0 @$ new_decls0, 
				      l0 @$ [O.COMPOUND
					(None, (O.BLOCK ([], 
					(O.Local_obj_decl(linkage,string))::
					  new_ty_decls 
					@$ new_decls, 
					l @$ [O.COMPOUND (None, cstmt)])))])))
				end
			    | _ -> assert false
			end
		end
	    | TMPLocal_external_decl _ -> assert (hd == None)
	    | _ -> 
		let _ = assert (hd == None)
		in tmp_list := 
		     !tmp_list @$ [convert_to_local_decl cmpstate decl']
	done;
	let (stmts', typs) = 
	  Safe_list.split (Safe_list.map (compile_c_stmt010 cmpstate) stmts)
	in
	let (new_tmp_syms, new_typs) = EO.pop_compiler_gen_scope env
	in
	let new_decls = mk_tool_gen_decls new_tmp_syms
	and new_ty_decls = 
	  Safe_list.map (fun t -> O.Local_type_decl t)  new_typs
	in O.BLOCK ([], !tmp_list @$ new_ty_decls @$ new_decls, stmts')
      with
	| CStmt s -> s
    in
    let I.BLOCK (labels, c_declaration_list, c_stmt010s) = expr
    in
    let decl_array = Array.create (Safe_list.length c_declaration_list) 
      (I.Obj_decl (I.Default, I.Primitive_type I.Void, "?"), ref false)
    in
    let i = ref 0
    in
    let _ = Safe_list.iter
      (fun v ->
	Array.set decl_array !i v;
	incr i) c_declaration_list
    in
    if labels <> [] then
      (O.BLOCK (labels, [], 
      [O.COMPOUND (None, normalize 0 decl_array c_stmt010s)]), 
      env.E.non_typ)
    else
      (normalize 0 decl_array c_stmt010s, env.E.non_typ)
  in
  let _ = EO.begin_block env
  in
  let v = compile expr
  in
  let (new_tmp_syms, new_typs) = EO.end_block env
  in 
  let _ = assert (new_tmp_syms = [])
  and _ = assert (new_typs = [])
  in v
       
and gnu_builtin_constant_p cmpstate expr =
  let env = cmpstate.env
  in
  let v = 
    if (compute_builtin_constant_p cmpstate expr) then "1"
    else "0"
  in
  (env.E.int_typ, Const_folding.cval_ext_of_cval 
    (Const_folding.cint_cval_of_string v))
    
and compute_builtin_constant_p cmpstate (expr:I.c_expr020):bool =
  let env = cmpstate.env
  in
  match expr with
    | I.Variable str ->
	let ce_info = EO.find_ce_info env str
	in
	if QNO.is_enum ce_info.CE.ce_qname then true
	else false
	  
    | I.Sizeof_expr expr0 -> true
    | I.Sizeof_type (type_cabs, has_def) -> true
    | I.Alignof_expr expr0 -> true
    | I.Alignof_type (typ_cabs, has_def) -> true
    | I.Constant c_constant -> true
    | I.String str -> true
    | I.Comma (se0, se1) -> 
	compute_builtin_constant_p cmpstate se0
	  
    | I.Binary_arithm (bop, se0, se1) -> 
	(compute_builtin_constant_p cmpstate se0) &&
	  (compute_builtin_constant_p cmpstate se1)
	  
    | I.Binary_predicate (bop, se0, se1) ->
	(compute_builtin_constant_p cmpstate se0) &&
	  (compute_builtin_constant_p cmpstate se1)
	  
    | I.Binary_logic (bop, se0, se1) ->
	(compute_builtin_constant_p cmpstate se0) &&
	  (compute_builtin_constant_p cmpstate se1)
	  
    | I.Unary_arithm (uop, se0) ->
	compute_builtin_constant_p cmpstate se0
	  
    | I.Logic_not se0 ->
	compute_builtin_constant_p cmpstate se0
	  
    | I.Memof se0 -> false

    | I.Addrof se0 -> false
	
    | I.Question (se0, se1_opt, se2) -> assert false
	
    | I.Builtin_constant_p (e) -> true
	
    | I.Builtin_expect _ -> assert false
	
    | I.Builtin_types_compatible (t0, t1) -> true
    | I.Cast (t,e) -> compute_builtin_constant_p cmpstate e

    | I.Memberof _ 
    | I.Memberof_ptr _
    | I.Indexof _
    | I.Assign _
    | I.Post_decr _
    | I.Post_incr _
    | I.Pre_decr _
    | I.Pre_incr _
    | I.Assign_arithm _
    | I.Call _
    | I.Macro_va_start _
    | I.Macro_va_arg _
    | I.Macro_va_end _
    | I.Gnu_block _
    | I.Gnu_labeladdr _ -> false
    | I.Cast_init _ -> false

and compile_c_translation_unit cmpstate = function
  | I.Translation_unit l ->	
      let l' = Safe_list.map (compile_c_external_declaration cmpstate) l
      in 
      O.Translation_unit 
	(((O.External_declaration_2 (Safe_list.rev !(cmpstate.init_lst)))::l'), cmpstate.eenv)
	
and compile_c_external_declaration cmpstate expr = 
  let env = cmpstate.env
  in
  match expr with
    | I.External_declaration_at (coord, expr) -> 
	O.External_declaration_at 
	  (coord, compile_c_external_declaration cmpstate expr)

    | I.External_declaration_1 (c_function_definition) ->
	O.External_declaration_1 
	  (compile_c_function_definition cmpstate c_function_definition)
 	  
    | I.External_declaration_2 (c_declaration) ->
	O.External_declaration_2 
	  (Safe_list.flatten 
	    (Safe_list.map 
	      (compile_global_c_declaration_ex cmpstate) c_declaration))

    | I.External_declaration_string (typ, id, cstr) ->
	begin
	  let v = 
	    if !keep_string_literal then
	      let char_type = 
		match cstr with
		  | C_syntax_symbol.String_literal str -> 
		      let length = String.length str + 1
		      in TO.create_array_type 
			   env.E.char_typ (Int64.of_int length)
			   
		  | C_syntax_symbol.WString_literal lst -> 
		      let length = List.length lst + 1
		      in TO.create_array_type 
			   env.E.wchar_t_typ (Int64.of_int length)
	      in
	      let ce = EO.add_ce_info env (mk_ce_info cmpstate I.Auto) (id, char_type)
	      in
	      O.Str_decl_init (O.Default_storage, ce, cstr)
	    else
	      let char_da_type =
		match cstr with
		  | C_syntax_symbol.String_literal _ ->
		      I.Primitive_type I.Char
		  | C_syntax_symbol.WString_literal _ ->
		      I.Primitive_type I.WChar
	      in
	      compile_global_c_declaration_one cmpstate
		(I.Obj_decl_init 
		  (I.Default, I.Xarray char_da_type, 
		  id, I.Initializer_1 (I.String cstr)), ref false)
	  in
	  O.External_declaration_2 [v]
	end
	  
and compile_c_function_definition cmpstate (expr:I.c_function_definition):
    O.c_function_definition =
  let env = cmpstate.env
  in
  let I.Function_definition 
      (linkage, fun_type, sname, c_compound_stmt010) = expr
  in
  let ce = EO.add_ce_info env 
    (mk_default_fun_ce_info cmpstate linkage) (sname, env.E.default_typ)
  in
  let qname = CEO.qname_of ce
  and ce_info = CEO.ce_info_of ce
  in
  let _ = EO.begin_function "_mlitecc_ea_" env sname
  in
  let fun_te = compile_c_type cmpstate fun_type
  in
  let _ = if (not !enable_vararg) && (TO.is_variant_fun (TO.get_abs_fun_te fun_te)) then
    camlp4_macro_exception "variadic function %s is not supported\n" sname
  in
  let fun_te = TO.mk_crt_function_typ fun_te
  in
  let fun_te = TO.norm_crt_function_xarray_params fun_te
  in
  let (fun_te, hidden_ptr_opt) = TO.norm_crt_function_typ fun_te
  in
  let _ = EO.set_return env (mk_ret_ce_info cmpstate I.Auto) fun_te
  and _ = ce_info.CE.ce_te <- fun_te
  and _ = ce_info.CE.ce_is_real_ent <- true
  in
  let formal_params = TO.formal_params_of fun_te
  and struct_decls = ref []
  and init_stmts = ref []
  in
  List.iter 
    (fun (te, id) -> 
      let (ce, is_hidden_ret) = match id with
	| T.HIDDEN_RETURN id -> 
	    (EO.add_ce_info env (mk_param_ce_info cmpstate) 
	      (id.QN.qn_sname, te), true)
	      
	| T.THIS_PARAM id 
	| T.NORMAL_PARAM id -> 
	    (EO.add_ce_info env (mk_param_ce_info cmpstate) 
	      (id.QN.qn_sname, te), false)
	      
	| T.STRUCT_PARAM qname_muton -> 
	    let ce = EO.add_ce_info env 
	      (mk_ce_info cmpstate I.Auto) 
	      (qname_muton.T.orig.QN.qn_sname, TO.elmt_of te)
	    and ce_ptr = EO.add_ce_info env 
	      (mk_param_ce_info cmpstate) 
	      (qname_muton.T.muton.QN.qn_sname, te)
	    in
	    let int_typ = (fst te, Mach.cuint_id)
	    and s0 = TO.static_sizeof (TO.elmt_of te)
	    in
	    let csize = O.Cconst (TO.make_type_const int_typ (Int64.to_string s0))
	    in
	    let _ = EO.find_ce_info env "memcpy"
	    and _ = struct_decls := 
	      (O.Local_obj_decl (O.Auto, ce))::!struct_decls
	    and _ = 
	      init_stmts := 
		(c2s 
		  (O.Memcpy
		    (O.Rladdr (O.Nlbl ce), 
		    (O.Rladdr (O.Nctn ce_ptr)), csize)))::!init_stmts
	    in (ce_ptr, false)
		 
	| T.SCALAR_PARAM qname_muton -> assert false
      in 
      if is_hidden_ret then
	EO.set_hidden_return env ce
    ) formal_params;
  let (c_compound_stmt010, typ') = 
    compile_c_compound_stmt010 cmpstate c_compound_stmt010
  in
  let O.BLOCK (strlist, decl_list, stmt_list) = c_compound_stmt010
  in
  let (fun_te, _) = TO.transfer_crt_function_te 
    (fun v -> 
      let ce_info = EO.find_ce_info env v.QN.qn_sname
      in ce_info.CE.ce_addr_is_taken
    ) fun_te 
  in
  let formal_params = TO.formal_params_of fun_te
  in
  let _ = 
    List.iter 
      (fun (te, id) ->
	match id with
	  | T.HIDDEN_RETURN id 
	  | T.THIS_PARAM id 
	  | T.NORMAL_PARAM id -> ()
	  | T.STRUCT_PARAM qname_muton -> ()
	  | T.SCALAR_PARAM qname_muton ->
	      let orig_ce_info = EO.stacknize_ce env 
		qname_muton.T.orig.QN.qn_sname
	      and muton_ce = EO.add_ce_info env 
		(mk_param_ce_info cmpstate) 
		(qname_muton.T.muton.QN.qn_sname, te)
	      in
	      let orig_ce = (env.E.ce_tbl, orig_ce_info.CE.ce_id)
	      in
	      let _ = struct_decls := 
		(O.Local_obj_decl (O.Auto, orig_ce))::!struct_decls
	      and _ = init_stmts := 
		(c2s 
		  (O.Assign
		    (mk_lhs orig_ce, 
		    O.Rval (O.Rladdr (O.Nctn muton_ce)))))::!init_stmts
	      in ()
      ) formal_params
  in
  let (ret_lbl, ret_opt) = EO.end_function env
  in
  let (decls, ret_stmt) = match ret_opt with
    | E.NORMAL_RETURN ce ->
	([O.Local_register ce], O.EPI (Some (O.Rladdr (O.Nctn ce))))
    | E.HIDDEN_PARAM _ 
    | E.VOID_RETURN -> 
	([], O.EPI None)
  in    
  O.Function_definition 
    (compile_linkage linkage, fun_te, qname,
    O.BLOCK (strlist, !struct_decls @ decls @$ decl_list, 
    !init_stmts @ stmt_list @ [O.LABEL (ret_lbl, ret_stmt)]))

and compile
    (builtin_function_list:I.function_type list)
    (gnu_builtin_function_list:I.function_type list)
    (basename:string) 
    (c_translation_unit:I.c_translation_unit)
    :(O.c_translation_unit * E.env) = 
  let cmpstate = create_env 100 
    basename builtin_function_list gnu_builtin_function_list 
  in
  let _ = EO.begin_file cmpstate.env basename 
  in
  let c_translation_unit = 
    compile_c_translation_unit cmpstate c_translation_unit
  in
  let (import_funs, (new_tmp_syms, new_typs)) = EO.end_file cmpstate.env
  in
  let import_decls = 
    Safe_list.map (fun s -> O.Obj_decl (O.Extern, s)) import_funs
  in
  let new_decls = 
    Safe_list.map (fun (t, s) -> O.Obj_decl (O.Auto, s)) new_tmp_syms
  and new_ty_decls = 
    Safe_list.map (fun t -> O.Type_decl t)  new_typs
  in
  let O.Translation_unit (c_extern_list, eenv) = c_translation_unit
  in
  let c_translation_unit = 
    O.Translation_unit 
      ((O.External_declaration_2 import_decls)::c_extern_list, cmpstate.eenv)
  in
  (** todo print out the new_ty decls into a file *)
  let _ = TO.canonicalize cmpstate.env.E.te_tbl
  in
  (*
    let type_array = Typ_to_typ_ast.convert_env env
    in
    let out_chan = open_out (basename ^ ".typ_ast")
    in
    let fm = Format.formatter_of_out_channel out_chan
    in
    Typ_printer.pp_print_env fm env;
    Format.pp_print_space fm ();
    Format.pp_open_vbox fm 0;
    let _ = Mlite_printer.pp_print_array fm
    (fun fm i typ_ast ->
    Format.fprintf fm "T_%d " i;
    Typ_ast_printer.pp_print_t fm typ_ast)
    (fun fm -> Format.pp_print_space fm ())
    type_array
    in
    Format.pp_close_box fm ();
    Format.pp_print_flush fm ();
    close_out out_chan;
  *)
  (c_translation_unit, cmpstate.env)
