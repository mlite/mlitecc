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
include Senv
open Csize
open Tent
open Qual_name
open Cent

open Const_folding
module CA = Const_folding
module QNP = Qual_name_printer
module QNO = Qual_name_op
module TEO = Tent_op
module TE = Tent
module CEO = Cent_op
module CE = Cent
open Tent_op

exception FoundTeInfo of te_info
exception FoundSeInfo of ce_info
  
let make_sname str : string =
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


let create size file_name : env = 
  let (te_tbl, typ_db_idx_by_sname) = 
    TEO.create size file_name
  in
  let stack = Stack.create ()
  in
  let builtin_scope = 
    {
      builtin_name = [];
      builtin_qname_to_te_info = typ_db_idx_by_sname;
      builtin_symtbl = StringHashtbl.create 13;
      builtin_cnt = 0;
    }
  in
  let _ = Stack.push (BUILTIN_SCOPE builtin_scope) stack 
  in
  {       
    anonymous_count = 0;
    te_tbl = te_tbl;
    ce_tbl = CEO.create size file_name;
    stack = stack;
    scope_serno = 0;
    (* import from syms *)    
    current_file_name = file_name;
    current_file_scope = 
      { 
	file_name = [QN.QN_SCOPE_FILE ""];
	file_qname_to_te_info = StringHashtbl.create 3;
	file_symtbl = StringHashtbl.create 3;
	file_cnt = 0;
      };
    current_func_scope = 
      { 
	fun_name = [];
	fun_qname_to_te_info = StringHashtbl.create 13;
	fun_symtbl = StringHashtbl.create 13;
	fun_cnt = 0;
	is_global = false;
	tmp_symtbl_scope = Stack.create ();
	temp_pool = 
	  {
	    id_decls = ref [];
	    te_decls = ref [];
	    id_pools = TeHashtbl.create 17;
	  };
	var_prefix = "_dreg";
	tmp_serno = 0;
	ret_opt = VOID_RETURN;
	ret_lbl = "";
      };
    undefined = StringHashtbl.create 12;
    used_builtin = QualNameHashtbl.create 12;
    
    (* import from sym_env *)
    fscope = Stack.create ();
    anonymous_id_serno = ref 0;
    cstr_id_serno = 0;
    cstr_id_tag = Random_prefix.get 
      ((make_sname file_name) ^ "_cstr_");
    default_typ = (te_tbl, Mach.cnon_id);
    void_typ = (te_tbl, Mach.cvoid_id);
    non_typ = (te_tbl, Mach.cnon_id);
    char_typ = (te_tbl, Mach.cchar_id);
    wchar_t_typ = (te_tbl, Mach.cwchar_t_id);
    int_typ = (te_tbl, Mach.cint_id);
    cuint_typ = (te_tbl, Mach.cint_id);
    size_typ = (te_tbl, Mach.csize_t_id);
    unsigned_char_typ = (te_tbl, Mach.cuchar_id);
    cptr_cuint_typ = (te_tbl, Mach.cptr_uint_id);
    bool_typ = (te_tbl, Mach.cbool_id);
  }


let create_empty_env size file_name : env = 
  let (te_tbl, typ_db_idx_by_sname) = 
    TEO.create_empty_te_tbl size file_name
  in
  let qname_to_te_info = StringHashtbl.create 
    (int_of_float ((float_of_int size) *. 1.5));
  in
  let stack = Stack.create ()
  in
  let builtin_scope = 
    {
      builtin_name = [];
      builtin_qname_to_te_info = qname_to_te_info;
      builtin_symtbl = StringHashtbl.create 13;
      builtin_cnt = 0;
    }
  in
  let _ = Stack.push (BUILTIN_SCOPE builtin_scope) stack 
  in
  {       
    anonymous_count = 0;
    te_tbl = te_tbl;
    ce_tbl = CEO.create size file_name;
    stack = stack;
    scope_serno = 0;
    (* import from syms *)    
    current_file_name = file_name;
    current_file_scope = 
      { 
	file_name = [QN.QN_SCOPE_FILE ""];
	file_qname_to_te_info = StringHashtbl.create 3;
	file_symtbl = StringHashtbl.create 3;
	file_cnt = 0;
      };
    current_func_scope = 
      { 
	fun_name = [];
	fun_qname_to_te_info = StringHashtbl.create 13;
	fun_symtbl = StringHashtbl.create 13;
	fun_cnt = 0;
	is_global = false;
	tmp_symtbl_scope = Stack.create ();
	temp_pool = 
	  {
	    id_decls = ref [];
	    te_decls = ref [];
	    id_pools = TeHashtbl.create 17;
	  };
	var_prefix = "_dreg";
	tmp_serno = 0;
	ret_opt = VOID_RETURN;
	ret_lbl = "";
      };
    undefined = StringHashtbl.create 12;
    used_builtin = QualNameHashtbl.create 12;

    (* import from sym_env *)
    fscope = Stack.create ();
    anonymous_id_serno = ref 0;
    cstr_id_serno = 0;
    cstr_id_tag = Random_prefix.get 
      ((make_sname file_name) ^ "_cstr_");
    default_typ = (te_tbl, Mach.cnon_id);
    void_typ = (te_tbl, Mach.cvoid_id);
    non_typ = (te_tbl, Mach.cnon_id);
    char_typ = (te_tbl, Mach.cchar_id);
    wchar_t_typ = (te_tbl, Mach.cwchar_t_id);
    int_typ = (te_tbl, Mach.cint_id);
    cuint_typ = (te_tbl, Mach.cint_id);
    size_typ = (te_tbl, Mach.csize_t_id);
    unsigned_char_typ = (te_tbl, Mach.cuchar_id);
    cptr_cuint_typ = (te_tbl, Mach.cptr_uint_id);
    bool_typ = (te_tbl, Mach.cbool_id);
  }


let scope_to_ce_info_list scope =
  let lst = ref []
  in
  let _ = match scope with
    | BUILTIN_SCOPE (builtin_scope) ->
	StringHashtbl.iter
	  (fun k ce_info ->
	    lst := (ce_info.ce_te, ce_info.ce_qname)::!lst
	  ) builtin_scope.builtin_symtbl
    | FILE_SCOPE (file_scope) ->
	StringHashtbl.iter
	  (fun k ce_info ->
	    lst := (ce_info.ce_te, ce_info.ce_qname)::!lst
	  ) file_scope.file_symtbl
    | FUN_SCOPE (fun_scope) ->
	StringHashtbl.iter
	  (fun k ce_info ->
	    lst := (ce_info.ce_te, ce_info.ce_qname)::!lst
	  ) fun_scope.fun_symtbl
    | BLOCK_SCOPE (block_scope) ->
	StringHashtbl.iter
	  (fun k ce_info ->
	    lst := (ce_info.ce_te, ce_info.ce_qname)::!lst
	  ) block_scope.scope_symtbl
  in !lst


let get_top_scope_name (env:env) :QN.qn_scope list = 
  match Stack.top env.stack with
    | BLOCK_SCOPE b -> b.block_name
    | FUN_SCOPE f -> f.fun_name
    | FILE_SCOPE f -> f.file_name
    | BUILTIN_SCOPE f -> f.builtin_name

exception FoundFileScopeName of QN.qn_scope list
  
let get_file_scope_name (env:env) :QN.qn_scope list = 
  env.current_file_scope.file_name
	  
	  

let get_next_top_scope_cnt: scope Stack.t -> int =
  fun stack ->
    match Stack.top stack with
      | BLOCK_SCOPE f -> 
	  f.scope_cnt <- f.scope_cnt + 1; f.scope_cnt
      | FUN_SCOPE f -> 
	  f.fun_cnt <- f.fun_cnt + 1; f.fun_cnt
      | FILE_SCOPE f -> 
	  f.file_cnt <- f.file_cnt + 1; f.file_cnt
      | BUILTIN_SCOPE f -> 
	  f.builtin_cnt <- f.builtin_cnt + 1; f.builtin_cnt


let get_top_scope_symtbl (stack:scope Stack.t) : ce_info StringHashtbl.t =
  match Stack.top stack with
    | BLOCK_SCOPE b -> b.scope_symtbl
    | FUN_SCOPE f -> f.fun_symtbl
    | FILE_SCOPE f -> f.file_symtbl
    | BUILTIN_SCOPE f -> f.builtin_symtbl

let alloc_cstr_id env = 
  let cstr_id = env.anonymous_count;
  in env.anonymous_count <- env.anonymous_count + 1;
  let cstr_id_expr = make_sname (env.current_file_name) ^ (string_of_int cstr_id)
  in
  QNO.alloc_strn_cnst 
    (get_top_scope_name env) cstr_id_expr

let alloc_fresh_sname env = 
  let cstr_id = env.anonymous_count;
  in env.anonymous_count <- env.anonymous_count + 1;
  let cstr_id_expr = make_sname (env.current_file_name) ^ (string_of_int cstr_id)
  in cstr_id_expr


let mk_te_qname env str = 
  {
    QN.qn_namespace = QN.QN_DEFAULT;
    QN.qn_span = QN.QN_AUTO;
    QN.qn_class = QN.QN_TYPE_NAME;
    QN.qn_scopes = get_top_scope_name env;
    QN.qn_init = QN.QN_NULL;
    QN.qn_sname = str;
  }

let get_top_sname_to_te_info: scope Stack.t -> te_info StringHashtbl.t = 
  fun stack ->
    match Stack.top stack with
      | BLOCK_SCOPE b -> b.block_qname_to_te_info 
      | FUN_SCOPE f -> f.fun_qname_to_te_info
      | FILE_SCOPE f -> f.file_qname_to_te_info
      | BUILTIN_SCOPE f -> f.builtin_qname_to_te_info
	  

let add_te_info_on_stack env te_info =
  let sname_to_te_info = get_top_sname_to_te_info env.stack
  in
  try
    let te_info = StringHashtbl.find 
      sname_to_te_info te_info.TE.m_name.QN.qn_sname
    in ((*print out a warning?*))
  with
    | Not_found -> 
	StringHashtbl.add 
	  sname_to_te_info te_info.TE.m_name.QN.qn_sname te_info
	    
	  
let replace_typedef_type: env -> type_name:QN.t -> alias_teid:teid->
  teid * QN.t = 
  fun env ~type_name ~alias_teid ->
    let te_info = TEO.replace_typedef_type env.te_tbl ~type_name ~alias_teid
    in
    let sname_to_te_info = get_top_sname_to_te_info env.stack
    in
    StringHashtbl.replace
    sname_to_te_info te_info.TE.m_name.QN.qn_sname te_info;
    (te_info.m_id, te_info.m_name)
      

let add_struct_te env sname lst : teid =
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_struct_te env.te_tbl qname lst
  in
  add_te_info_on_stack env te_info;
  te_info.m_id
    
let add_struct_name_te env sname : teid =
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_struct_name_te env.te_tbl qname
  in
  add_te_info_on_stack env te_info;
  te_info.m_id
    
let add_union_te env sname lst: teid =
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_union_te env.te_tbl qname lst
  in
  add_te_info_on_stack env te_info;
  te_info.m_id

let add_union_name_te env sname: teid =
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_union_name_te env.te_tbl qname 
  in
  add_te_info_on_stack env te_info;
  te_info.m_id
    
let add_enum_te env sname lst : teid =
  let qname = mk_te_qname env sname
  in
  let (te_info, lst) = TEO.add_enum_te env.te_tbl qname lst
  in
  add_te_info_on_stack env te_info;
  let ce_symtbl = get_top_scope_symtbl env.stack
  in
  List.iter
    (fun enum_item ->
      let ce_info = 
	{
	  CE.ce_id = CEO.get_ceid env.ce_tbl enum_item.enum_name;
	  CE.ce_qname = enum_item.enum_name;
	  CE.ce_te = (env.te_tbl, te_info.m_id);
	  CE.ce_used = false;
	  CE.ce_is_real_ent = true;
	  CE.ce_addr_is_taken = false;
	  CE.ce_is_register = false;
	}
      in CEO.add_ce_info env.ce_tbl ce_info;
      StringHashtbl.add ce_symtbl enum_item.enum_name.QN.qn_sname ce_info;
    ) lst;
  te_info.m_id
    
let add_enum_name_te env sname : teid =
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_enum_name_te env.te_tbl qname
  in
  add_te_info_on_stack env te_info;
  te_info.m_id

let add_typedef_te env lhs_teid sname: teid = 
  let qname = mk_te_qname env sname
  in
  let te_info = TEO.add_typedef_te env.te_tbl lhs_teid qname 
  in
  add_te_info_on_stack env te_info;
  te_info.m_id


let find_te_info te_tbl sname : te_info option =
  try
    let _ = Stack.iter
      (fun scope ->
	let qname_to_te_info = 
	  match scope with
	    | BLOCK_SCOPE v -> v.block_qname_to_te_info
	    | FUN_SCOPE v -> v.fun_qname_to_te_info
	    | FILE_SCOPE v -> v.file_qname_to_te_info
	    | BUILTIN_SCOPE v -> v.builtin_qname_to_te_info
	in
	try
	  let te_info = StringHashtbl.find qname_to_te_info sname
	  in raise (FoundTeInfo te_info)
	with
	  | Not_found -> ()
      )  te_tbl.stack
    in None
  with
    | FoundTeInfo te_info -> Some te_info
      
    

let replace_ce_info ce_info: bool =
  not (Tent_op.is_complete_te ce_info.ce_te)


let update_ce_info (env:env) (ce:CE.ce) (te:te) : unit =
  let old_ce_info = CEO.ce_info_of ce
  in old_ce_info.ce_te <- te

	
let te_of_ce_info ce_info : te = ce_info.ce_te
  
let add_ce_info (env:env) (mk_qname:(string * te -> QN.t * bool)) (sname, te) : Cent.ce = 
  let symtbl = get_top_scope_symtbl env.stack
  and (qname, is_real_ent) = mk_qname (sname, te)
  in
  let mk_ce_info qname te = 
    {
      CE.ce_id = CEO.get_ceid env.ce_tbl qname;
      CE.ce_qname = qname;
      CE.ce_te = te;
      CE.ce_used = false;
      CE.ce_is_real_ent = is_real_ent;
      CE.ce_addr_is_taken = false;
      CE.ce_is_register = false;
    }
  in
  assert (not is_real_ent || (qname.QN.qn_scopes = get_top_scope_name env));
  try 
    let ce_info = StringHashtbl.find symtbl qname.QN.qn_sname
    in 
    let _ = 
      if is_real_ent then
	if ce_info.ce_is_real_ent then
	  if TEO.is_function_te ce_info.ce_te then
	    camlp4_macro_exception
	      "error: redeclaration of '%s' as type %d\n" 
	      qname.QN.qn_sname (snd ce_info.ce_te)
	  else if not (TEO.is_complete_te ce_info.ce_te) & TEO.is_complete_te te then
	    ce_info.ce_te <- te
	  else
	    (* C allows you to declare the same variable multiple times,
	       we have to let it go *)
	    camlp4_macro_warning
	      "warning: redeclaration of '%s' as type %d\n" 
	      qname.QN.qn_sname (snd ce_info.ce_te)
	else
	  begin
	    ce_info.ce_te <- te;
	    ce_info.ce_is_real_ent <- true;
	  end
      else
	if !Mlite_config.enable_log then
	  camlp4_macro_warning 
	    "warning: redeclaration of '%s' as type %d\n" 
	    qname.QN.qn_sname (snd ce_info.ce_te)
    in (env.ce_tbl, ce_info.CE.ce_id)
  with
      Not_found -> 
	let ce_info = mk_ce_info qname te
	in
	CEO.add_ce_info env.ce_tbl ce_info;
	let _ = StringHashtbl.add symtbl qname.QN.qn_sname ce_info
	in (env.ce_tbl, ce_info.CE.ce_id)
	  

let add_used_builtin te_tbl is_builtin s ty =
  if is_builtin then
    try 
      ignore (QualNameHashtbl.find te_tbl.used_builtin s)
    with
      | Not_found -> 
	  QualNameHashtbl.add te_tbl.used_builtin s ty
  
let find_ce_info (env:env) (name:string) : ce_info = 
  try
    begin
      let _ = Stack.iter
	(fun scope ->
	  let (is_builtin, is_global, symtbl) = match scope with
	    | BLOCK_SCOPE block_scope -> (false, false, block_scope.scope_symtbl)
	    | FUN_SCOPE fun_scope -> (false, false, fun_scope.fun_symtbl)
	    | FILE_SCOPE file_scope -> (false, true, file_scope.file_symtbl)
	    | BUILTIN_SCOPE builtin_scope -> 
		(true, true, builtin_scope.builtin_symtbl)
	  in
	  try
	    let ce_info = StringHashtbl.find symtbl name
	    in
	    add_used_builtin env is_builtin ce_info.ce_qname (env.ce_tbl, ce_info.ce_id);
	    raise (FoundSeInfo ce_info)
	  with
	      Not_found -> ()
	) env.stack
      in
      let _ = 
	try 
	  let _ = StringHashtbl.find env.undefined name
	  in ()
	with
	    Not_found ->
	      begin
		StringHashtbl.add env.undefined name ();
		if !Mlite_config.enable_werror then
		  camlp4_macro_exception "\nundefined symbol %s\n" name
		else
		  camlp4_macro_warning "\nundefined symbol %s\n" name
	      end
      in 
      (* In C, an undefined symbol is treated as a global function with 
	 default prototype
      *)
      let ce_qname = 
	{ 
	  QN.qn_namespace = QN.QN_DEFAULT;
	  QN.qn_span = QN.QN_AUTO; (*QN.QN_EXTERN;*)
	  QN.qn_class = QN.QN_CODE_ADDR;
	  QN.qn_scopes = [];
	  QN.qn_init = QN.QN_NULL;
	  QN.qn_sname = name;
	}
      in
      let ce_info =
	{
	  ce_id = CEO.get_ceid env.ce_tbl ce_qname;
	  ce_te = env.default_typ;
	  ce_qname = ce_qname;	  
	  ce_used = false;
	  ce_is_real_ent = false;
	  ce_addr_is_taken = false;
	  ce_is_register = false;
	}
      in 
      ignore(CEO.add_ce_info env.ce_tbl ce_info);
      ce_info
    end
  with
      FoundSeInfo ce_info -> ce_info

let stacknize_ce env sname = 
  let ce_info = find_ce_info env sname
  in 
  let _ = QNO.stacknize_param_qname ce_info.ce_qname
  in ce_info


let push_compiler_gen_scope env = 
  let func_scope = Stack.top env.fscope
  in
  Stack.push 
    { 
      id_decls = ref []; 
      te_decls = ref []; 
      id_pools = TeHashtbl.create 100; 
    }
    func_scope.tmp_symtbl_scope
    
let pop_compiler_gen_scope env = 
  let func_scope = Stack.top env.fscope
  in
  let local_scope = Stack.pop func_scope.tmp_symtbl_scope
  in
  (Safe_list.rev !(local_scope.id_decls), 
  Safe_list.rev !(local_scope.te_decls))


let begin_block (env:env) :QN.qn_scope list = 
  let _ = env.scope_serno <- env.scope_serno + 1
  in
  let scope_name = 
    (get_top_scope_name env) @
      [QN.QN_SCOPE_BLOCK env.scope_serno]
  in
  let block_scope = 
    { 
      block_name = scope_name;
      block_qname_to_te_info = StringHashtbl.create 13;
      scope_symtbl = StringHashtbl.create 13;
      scope_cnt = 0;
    }
  in Stack.push (BLOCK_SCOPE block_scope) env.stack;
  push_compiler_gen_scope env;
  scope_name
    
let end_block (env:env) =
  let scope = Stack.pop env.stack
  in
  let env_hashtbl = 
    match scope with
      | BUILTIN_SCOPE s -> s.builtin_qname_to_te_info
      | FILE_SCOPE s -> s.file_qname_to_te_info
      | FUN_SCOPE s -> s.fun_qname_to_te_info
      | BLOCK_SCOPE s -> s.block_qname_to_te_info
  in
  let v = pop_compiler_gen_scope env
  in v


(* import from sym_env *)
let push_compiler_gen_scope env = 
  let func_scope = Stack.top env.fscope
  in
  Stack.push 
    { 
      id_decls = ref []; 
      te_decls = ref []; 
      id_pools = TeHashtbl.create 100; 
    }
    func_scope.tmp_symtbl_scope
    
let pop_compiler_gen_scope env = 
  let func_scope = Stack.top env.fscope
  in
  let local_scope = Stack.pop func_scope.tmp_symtbl_scope
  in
  (Safe_list.rev !(local_scope.id_decls), 
  Safe_list.rev !(local_scope.te_decls))
    

let begin_function prefix env fun_name: unit =
  let _ = env.scope_serno <- 0
  and fun_scope = 
    { 
      fun_name = (get_top_scope_name env) @ [QN.QN_SCOPE_FUN fun_name];
      fun_qname_to_te_info = StringHashtbl.create 13;
      fun_symtbl = StringHashtbl.create 13;
      fun_cnt = 0;
      is_global = false;
      tmp_symtbl_scope = Stack.create ();
      temp_pool = 
	{
	  id_decls = ref [];
	  te_decls = ref [];
	  id_pools = TeHashtbl.create 17;
	};
      var_prefix = prefix ^ "_tmp_ce_";
      tmp_serno = 0;
      ret_opt = VOID_RETURN;
      ret_lbl = fun_name ^ "ret_lbl";
    }
  in  
  Stack.push (FUN_SCOPE fun_scope) env.stack;  
  Stack.push fun_scope env.fscope


let set_return env mk_ce_info te: unit =
  let ret_te = TEO.return_te_of te
  in
  match Stack.top env.stack with
    | FUN_SCOPE fun_scope ->
	if not (TEO.is_void_typ ret_te) then
	  begin
	    let ce = add_ce_info env mk_ce_info ("ret", ret_te)
	    in fun_scope.ret_opt <- NORMAL_RETURN ce
	  end
    | _ -> assert false


let set_hidden_return env ce: unit =
  match Stack.top env.stack with
    | FUN_SCOPE fun_scope ->
	fun_scope.ret_opt <- HIDDEN_PARAM ce
    | _ -> assert false


let end_function (env: env) =
  let scope = Stack.pop env.stack
  in 
  let l = scope_to_ce_info_list scope
  in
  let func_scope = Stack.pop env.fscope
  in (func_scope.ret_lbl, func_scope.ret_opt)



let begin_file (env: env) (file_name:string) : unit =
  assert (Stack.length env.stack = 1); 
  (** only has builtin scope on stack **)
  let _ = env.scope_serno <- 0
  in
  let file_scope = 
    { 
      file_name = (get_top_scope_name env) @ 
	[QN.QN_SCOPE_FILE file_name];
      file_qname_to_te_info = StringHashtbl.create 13;
      file_symtbl = StringHashtbl.create 2217;
      file_cnt = 0;      
    }
  in
  let _ = env.current_file_scope <- file_scope
  in Stack.push (FILE_SCOPE file_scope) env.stack;
  let fun_scope = 
    { 
      fun_name = (get_top_scope_name env);
      fun_qname_to_te_info = StringHashtbl.create 13;
      fun_symtbl = StringHashtbl.create 13;
      fun_cnt = 0;
      is_global = true;
      tmp_symtbl_scope = Stack.create ();
      temp_pool = 
	{
	  id_decls = ref [];
	  te_decls = ref [];
	  id_pools = TeHashtbl.create 17;
	};
      var_prefix = "_keep_this_in_begin_file_";
      tmp_serno = 0;
      ret_opt = VOID_RETURN;
      ret_lbl = "";
    }
  in
  Stack.push fun_scope env.fscope;
  push_compiler_gen_scope env


let end_file env: ce list * 'a =
  assert (Stack.length env.stack = 2);
  ignore (Stack.pop env.stack);
  env.te_tbl.num_of_typs_in_files <- 
    Int.Type_idHashtbl.length env.te_tbl.teid_to_te_info;
  let _ = 
    Int.Type_idHashtbl.iter
      (fun i b -> 
	match b.m_kind with
	  | Struct_name 
	  | Union_name 
	  | Enum_name -> ()
	  | Typedef v ->
	      begin
		let kind = te_kind_of (env.te_tbl, v.m_typedef_lhs_teid)
		in
		match kind with
		  | Struct_name
		  | Union_name -> ()
		  | _ -> 
		      ignore(compute_alignof (env.te_tbl, i))
	      end
	  | Qualified v ->
	      begin
		let kind = te_kind_of (env.te_tbl, v.m_qualified_teid)
		in
		match kind with
		  | Struct_name 
		  | Union_name -> ()
		  | _ -> 
		      ignore(compute_alignof (env.te_tbl, i))
	      end
	  | Bits _ -> ()
	  | Abs_function _ 
	  | Crt_function _ -> ()
	  | Normal _ -> ()
	  | _ -> 
	      begin
		let size = TEO.sizeof (env.te_tbl, i)
		in
		if !Mlite_config.enable_log then
		  begin
		    print_string "type: ";
		    print_int (i);
		    print_string (" name:{" ^ (QNP.to_decl_str b.m_name) ^ "}");
		    print_string " size: ";
		    let _ = match size with
		      | Byte_size v -> 
			  print_string (string_of_csize v); 
			  print_string " bytes"
		      | Incomplete -> print_string "?"
		    in ()
		  end
		else if !Mlite_config.enable_log then
		  print_string (" is incomplete");
		if !Mlite_config.enable_log then
		  print_newline ();
	      end
      ) env.te_tbl.teid_to_te_info
  in
  let l = ref []
  and builtin_lst = ref []
  in 
  let import_list = QualNameHashtbl.iter
    (fun str ce -> builtin_lst := ce::!builtin_lst) env.used_builtin
  in
  let ret = 
    if StringHashtbl.length env.undefined <> 0 then
      begin
	let out_chan = open_out (env.current_file_name ^ ".undef.sh")
	in
	let filename = env.current_file_name ^ ".undef"
	in
	print_string 
	  "\nDefintions of the following symbols are not found, we treat them as type 'int (void)'\n";
	Printf.fprintf out_chan "rm -rf %s\n" filename;
	StringHashtbl.iter
	  (fun str _ ->
	    Printf.fprintf out_chan 
	      "grep -h -w '%s' *.cproto >> %s\n" str filename;
	    Printf.fprintf stderr 
	      "`%s' undeclared (first use in this function)\n" str;
	  ) env.undefined;
	close_out out_chan;
	if !Mlite_config.allow_undefined_symbols then
	  begin
	    camlp4_macro_warning "\nundefined symbols\n";
	    !l
	  end
	else
	  camlp4_macro_exception "\nundefined symbols\n" 
      end
    else
      !builtin_lst
  in
  let v = pop_compiler_gen_scope env
  in
  ignore(Stack.pop env.fscope);
  (ret, v)



(* import from sym_env *)
module I = Ast_da_type
open Safe_list

let create_env (name:string) (size:int) : env =
  let env = create size name
  in env


let update_variable_type env var te = 
  let c_type = te
  in
  assert (not (is_void_typ c_type));
  let func_scope = Stack.top env.fscope
  in
  let local_scope = Stack.top func_scope.tmp_symtbl_scope
  in
  local_scope.id_decls := 
    Safe_list.map 
      (fun (c, name) -> 
	if (snd name = snd var) then (c_type, name) 
	else (c,name))
      !(local_scope.id_decls);
  ignore(update_ce_info env var te)


let get_fresh_tmp_qname env c_type is_reg = 
  assert (not (is_void_typ c_type));
  let func_scope = Stack.top env.fscope
  in
  let qname = 
    { 
      QN.qn_namespace = QN.QN_TMP;
      QN.qn_span = QN.QN_AUTO;
      QN.qn_class = 
	if is_reg & Tent_op.is_native_assign_supported_typ c_type then
	  QN.QN_DATA_REGI
	else
	  QN.QN_DATA_ADDR;
      QN.qn_scopes = get_top_scope_name env;
      QN.qn_init = QN.QN_NULL;
      QN.qn_sname = func_scope.var_prefix 
	^ (string_of_int func_scope.tmp_serno);
    }
  in
  func_scope.tmp_serno <- func_scope.tmp_serno + 1;
  qname
      
let add_fresh_storage env c_type is_reg = 
  assert (not (is_void_typ c_type));
  let name = get_fresh_tmp_qname env c_type is_reg
  in
  let ce_info = 
    {
      CE.ce_id = CEO.alloc_ceid env.ce_tbl;
      CE.ce_qname = name;
      CE.ce_te = c_type;
      CE.ce_used = false;
      CE.ce_is_real_ent = true;
      CE.ce_addr_is_taken = false;
      CE.ce_is_register = false;
    }
  in
  CEO.add_ce_info env.ce_tbl ce_info;
  let func_scope = Stack.top env.fscope
  in
  let ce = (env.ce_tbl, ce_info.ce_id)
  in
  if (func_scope.is_global) then
    begin (* we can allocate variable in global scope *)
      ce
    end
  else
    begin
      let local_scope = Stack.top func_scope.tmp_symtbl_scope
      in
      local_scope.id_decls := (c_type, ce)::!(local_scope.id_decls);
      ce
    end

let remove_qual_name env var = 
  let name = var
  in
  let func_scope = Stack.top env.fscope
  in
  if (func_scope.is_global) then
    begin (* we can allocate variable in global scope *)
      ();
    end
  else
    begin
      let local_scope = Stack.top func_scope.tmp_symtbl_scope
      in
      let l = 
	List.fold_left
	  (fun l0 (t,n) ->
	    if (snd n) <> (snd name) then (t,n)::l0
	    else l0
	  ) [] !(local_scope.id_decls)
      in
      local_scope.id_decls := (List.rev l);
    end
      
exception Done of Cent.ce
exception End
  
let recycle_tmp_ces env lst =
  let lst = ref lst
  in
  try
    Stack.iter
      (fun func_scope ->
	if (func_scope.is_global) then
	  begin (* we can allocate variable in global scope *)
	    ()
	  end
	else
	  Stack.iter 
	    (fun local_scope ->
	      lst := 
		List.fold_left
		  (fun l (c_typ, ce) -> 
		    try
		      let _ = List.find 
			(fun (t, n) -> typ_eq t c_typ & (snd n) = (snd ce)) 
			!(local_scope.id_decls)
		      in TeHashtbl.replace local_scope.id_pools c_typ ce;
		      l
		    with
		      | Not_found -> 
			  (c_typ,ce)::l
		  ) [] !lst;
	      raise End
	    )  func_scope.tmp_symtbl_scope
      ) env.fscope
  with
    | End -> ()


let get_tmp_ce env te = 
  let rec unqual te = 
    match TEO.te_kind_of te with 
      | TE.Pointer elmt_teid -> 
	  let elmt_te = unqual (fst te, elmt_teid)
	  in TEO.ptr_of elmt_te
      | TE.Qualified v ->
	  unqual (fst te, v.m_qualified_teid)
      | TE.Attribute (teid, atts) ->
	  unqual (fst te, teid)
      | _ -> te
  in
  let te = unqual te
  in
  if TEO.typ_eq te env.non_typ then
    let c_type = get_scalar_typ te
    in  add_fresh_storage env te true
  else
    let func_scope = Stack.top env.fscope
    in
    if (func_scope.is_global) then
      add_fresh_storage env te true
    else
      begin
	try
	  Stack.iter
	    (fun local_scope -> 
	      try
		let v = TeHashtbl.find local_scope.id_pools te
		in
		let _ = TeHashtbl.remove local_scope.id_pools te
		in 
		raise (Done v)
	      with
		| Not_found -> ()
	    ) func_scope.tmp_symtbl_scope;
	  raise Not_found 
	with
	  | Not_found ->
	      add_fresh_storage env te true
	  | Done n -> 
	      n
      end
	
let add_new_type env c_type = 
  let func_scope = Stack.top env.fscope
  in
  let local_scope = Stack.top func_scope.tmp_symtbl_scope
  in local_scope.te_decls := c_type::!(local_scope.te_decls)
       
let remove_new_type env c_type = 
  let func_scope = Stack.top env.fscope
  in
  let local_scope = Stack.top func_scope.tmp_symtbl_scope
  in 
  let lst = 
    List.fold_left
      (fun lst v ->
	if snd v <> snd c_type then v::lst
	else lst
      ) [] !(local_scope.te_decls)
  in local_scope.te_decls := Safe_list.rev lst
    


let get_ret_info env = 
  let func_scope = Stack.top env.fscope
  in (func_scope.ret_opt, func_scope.ret_lbl)


