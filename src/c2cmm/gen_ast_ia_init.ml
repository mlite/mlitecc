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
module GA = Ast_ga_code
module I = Ast_ha_graf
module EB = Ast_eb_expr
module EBop = Ast_eb_expr_op
module O = Ast_ia_init
module S = C_semantics_symbol
module CA = Const_folding
module E = Senv
module EO = Senv_op
module TM = Typ_mem
module T = Tent
module TO = Tent_op
module QN = Qual_name
module CEO = Cent_op
module CE = Cent
module QNP = Qual_name_printer
module AEE = Gen_ast_eb_expr

open Csize
open Safe_list


let user_opts = []

exception LinkConst of O.cexpr

let get_union_id orig =
  let str = QNP.to_decl_str orig
  in str

let mk_union_init_te (env:E.env) ~(var_mem:(O.c_type * Qual_name.t))
    ~(struct_mem:(string * O.c_type)) :O.c_type * string =
  let union_wrapper_name =
    Printf.sprintf "_union_init_%s" (QNP.to_decl_str (snd var_mem))
  in
  let union_name = "union " ^ union_wrapper_name
  in
  let l = 
    [(Some ((snd var_mem).QN.qn_sname), snd (fst var_mem), false);
    (Some (fst struct_mem), snd (snd struct_mem), true)]
  in
  let wrapper_typ = EO.add_union_te env union_name l
  in ((env.E.te_tbl, wrapper_typ), union_wrapper_name)
	
type cmpstate =
    {
      union_init_tes: bool Int.Type_idHashtbl.t;
      eenv: Ast_eb_expr.expr_env;
      env: E.env;
    }
  

let rec get_size = (fun v -> let s = Typ_mem_op.sizeof v in Int64.to_int s)
  
and eval_const_expr cmpstate (idx:int) (dest_typ:I.c_type) 
    (expr:EB.cexpr) :O.mem_cell =
  let size = Int64.to_int (TO.static_sizeof dest_typ)
  and env = cmpstate.env
  and eenv = cmpstate.eenv
  in
  let rec eval_int (expr:EB.cexpr) : CA.cval_ext =
    match expr with
      | EB.Cconst (t, (v, s, sopt)) -> (v, s, sopt)
      | _ -> raise (LinkConst expr)
  in
  try
    let (cval, str, str_opt) = eval_int expr
    in 
    let (_, d_tid) = TO.convert_to_builtin_typ dest_typ
    and (_, v_tid) = TO.convert_to_builtin_typ 
      (AEE.get_cexpr env eenv expr) (*.EB.ce_typ*)
    in
    let v = 
      if (v_tid != d_tid) then
	let converter = Converter.get v_tid d_tid
	in
	converter cval
      else
	cval
    in
    let bsa = CA.bsa_of_cval v
    in
    let arr =
      if Mach.little_endian then
	Array.map (fun s -> O.I8Const s) bsa
      else
	assert false
    in
    let addr = idx
    in
    {
      O.addr = addr;
      O.offset_name = "offset_" ^ (string_of_int addr);
      O.cell = arr;
      O.size = size;
      O.typ = TO.create_array_type 
	env.E.unsigned_char_typ (Int64.of_int size)
    }
  with
    | LinkConst e -> 
	begin
	  let addr = idx
	  in
	  {
	    O.addr = addr;
	    O.offset_name = "offset_" ^ (string_of_int addr);
	    O.cell = Array.create 1 (O.IXCexpr e);
	    O.size = size;
	    O.typ = TO.create_array_type dest_typ 1L
	  }
	end

and convert_c_init_expression cmpstate idx t size expr = 
  let env = cmpstate.env
  in 
  match expr with
    | EB.Static_init_none -> 
	let arr = Array.create size O.I8Space
	and addr = idx
	in
	{
	  O.addr = addr;
	  O.offset_name = "offset_" ^ (string_of_int addr);
	  O.size = size;
	  O.cell = arr;
	  O.typ = TO.create_array_type 
	    env.E.unsigned_char_typ (Int64.of_int size)
	}
    | EB.Static_init cexpr ->
	eval_const_expr cmpstate idx t cexpr
	  
and compile_c_initializer cmpstate ~(orig_typ:I.c_type) ~(orig_var:Qual_name.t)
    (c_initializer:EB.c_initializer) : (O.c_type * string * O.c_initializer) = 
  let env = cmpstate.env
  in
  let size = Int64.to_int (TO.static_sizeof orig_typ)
  and a = ref []
  in
  let convert addr = function 
    | TM.Null -> assert false
    | TM.Bits (t, a) -> 
	camlp4_macro_exception 
	  "static initialization of bit fields is not supported yet"

    | TM.Scalar (t, s) -> 
	begin
	  let size = Int64.to_int (TO.static_sizeof t)
	  in
	  let mem_cell = 
	    convert_c_init_expression cmpstate (Int64.to_int addr) t size !s
	  in a := mem_cell::!a
	end
    | TM.Union (t, a, ref_e) 
    | TM.Struct (t, a, ref_e) -> assert false
    | TM.Array (t, a) -> assert false
    | TM.Xarray (t, a, l, n) -> assert false
  in
  let _ = Typ_mem_op.fold_offset convert 0L c_initializer
  in 
  let init_list = (List.rev !a)
  in
  let init_typ = make_packaged_struct_type env ~init_list 
  in 
  let (typ, new_id) = 
    mk_union_init_te env ~var_mem:(orig_typ, orig_var) 
      ~struct_mem:("init", init_typ)
  in 
  let _ = Int.Type_idHashtbl.add cmpstate.union_init_tes (snd typ) false
  in (typ, new_id, ("init", init_list))
       
and make_packaged_struct_type (env:E.env) ~(init_list:O.mem_cell list) :O.c_type =
  let sname = Printf.sprintf "struct _init_%s" (EO.alloc_fresh_sname env)
  and prev_addr = ref 0
  in
  let ls = 
    List.fold_left
      (fun lst mem_cell ->
	let nelmt = mem_cell.O.addr - !prev_addr
	in
	let lst = 
	  if nelmt > 0 then
	    let at = TO.create_array_type env.E.unsigned_char_typ (Int64.of_int nelmt)
	    in lst @ [(Some ("padding_" ^ (string_of_int !prev_addr)), snd at,false)]
	  else if nelmt = 0 then lst
	  else assert false
	in
	let lst = lst @ [(Some (mem_cell.O.offset_name), snd mem_cell.O.typ, false)]
	and _ = prev_addr := mem_cell.O.addr + mem_cell.O.size
	in lst
      ) [] init_list
  in
  let typ = EO.add_struct_te env sname ls
  in (env.E.te_tbl, typ)

and compile_scope_ctrl cmpstate = function
  | GA.Begin_fun (c_local_decls, i) ->
      O.Begin_fun 
	(List.flatten (List.map (compile_c_local_declaration cmpstate) c_local_decls), i)
  | GA.End_fun ->
      O.End_fun
  | GA.Begin_decl (c_local_decls, i) ->
      O.Begin_decl 
	(List.flatten (List.map (compile_c_local_declaration cmpstate) c_local_decls), i)
  | GA.End_decl ->
      O.End_decl

and compile_c_code cmpstate expr =
  match expr with
    | GA.Scope scope_ctrl -> O.Scope (compile_scope_ctrl cmpstate scope_ctrl)
    | GA.Call c -> O.Call c
    | GA.Flow f -> O.Flow f
    | GA.Sese l -> O.Sese l
    | GA.Join -> O.Join
    | GA.Nop -> O.Nop
    | GA.Epi r -> O.Epi r

and compile_node cmpstate v = 
  { 
    O.label_att = v.I.label_att;
    O.coord_opt = v.I.coord_opt;
    O.preds = v.I.preds;
    O.code = compile_c_code cmpstate v.I.code;
    O.succs = v.I.succs;
    O.idom = v.I.idom;
    O.domfrontiers = v.I.domfrontiers;
    O.dominated_nodes = v.I.dominated_nodes;
    O.scope_begin_pcs = v.I.scope_begin_pcs;
  }

and compile_c_declaration cmpstate (expr:I.c_declaration) :O.c_declaration list = 
  match expr with
    | EB.Str_decl_init (linkage, ce, str_literal) ->
	[O.Str_decl_init (linkage, ce, str_literal)]
	  
    | EB.Obj_decl (linkage, ce) ->
	[O.Obj_decl (linkage, ce)]
	  
    | EB.Obj_decl_init (linkage, ce, c_initializer) 
      ->
	begin
	  let c_type = CEO.te_of ce
	  and orig_id = CEO.qname_of ce
	  and ce_info = CEO.ce_info_of ce
	  in
	  let (decl_typ, union_var, c_initializer) = 
	    compile_c_initializer cmpstate ~orig_typ:c_type
	      ~orig_var:orig_id c_initializer
	  in
	  let _ = orig_id.QN.qn_init <- (QN.QN_INIT union_var)
	  and _ = ce_info.CE.ce_te <- decl_typ
	  in	    
	  [
	    O.Type_decl decl_typ;
	    O.Dat_decl_init 
	      { 
		O.sto_linkage = linkage;
		O.orig_type = c_type;
		O.orig_id = orig_id;
		O.union_type = decl_typ; 
		O.union_id = get_union_id orig_id;
		O.init = c_initializer;
	      }
	  ]
	end
	  
    | EB.Type_def c_type ->
	let rec get_elmt_typ typ = 
	  if TO.is_ptr_typ typ then
	    get_elmt_typ (TO.elmt_of typ)
	  else
	    typ
	in
	let base_typ = get_elmt_typ c_type
	in
	if TO.maybe_typedef_predeclaration base_typ
	then [O.Type_only base_typ]
	else []
	  
    | EB.Type_decl c_type -> [O.Type_decl c_type]
	
    | EB.Type_only c_type -> [O.Type_only c_type]
	
and compile_c_local_declaration cmpstate
    (expr:I.c_local_declaration):O.c_local_declaration list = 
  match expr with
    | EB.Local_obj_decl (linkage, ce) ->
	[O.Local_obj_decl (linkage, ce)]
	  
    | EB.Local_obj_decl_init (linkage, ce, c_initializer) ->
	begin
	  let c_type = CEO.te_of ce
	  and orig_id = CEO.qname_of ce
	  in
	  let (decl_typ, union_var, c_initializer) = 
	    compile_c_initializer cmpstate ~orig_typ:c_type
	      ~orig_var:orig_id c_initializer
	  in
	  let _ = orig_id.QN.qn_init <- (QN.QN_INIT union_var)
	  in	    
	  [
	    O.Local_type_decl decl_typ;
	    O.Local_dat_decl_init 
	      { 
		O.sto_linkage = linkage;
		O.orig_type = c_type;
		O.orig_id = orig_id;
		O.union_type = decl_typ; 
		O.union_id = get_union_id orig_id;
		O.init = c_initializer;
	      }
	  ]
	end

    | EB.Local_type_def c_type ->
	let rec get_elmt_typ typ = 
	  if TO.is_ptr_typ typ then
	    get_elmt_typ (TO.elmt_of typ)
	  else
	    typ
	in
	let base_typ = get_elmt_typ c_type
	in
	if TO.maybe_typedef_predeclaration base_typ
	then [O.Local_type_only base_typ]
	else []
	  
    | EB.Local_type_decl c_type ->
	[O.Local_type_decl c_type]
	  
    | EB.Local_type_only c_type ->
	[O.Local_type_only c_type]

    | EB.Local_register (c_type, str) ->
	[O.Local_register (c_type, str)]

and compile_c_translation_unit env = function
  | I.Translation_unit (l, eenv) ->
      let cmpstate = 
	{
	  union_init_tes = Int.Type_idHashtbl.create 211;
	  eenv = eenv;
	  env = env;
	}
      in
      let l' = Safe_list.map (compile_c_external_declaration cmpstate) l
      in (cmpstate, O.Translation_unit (l', eenv))
	   
and compile_c_external_declaration cmpstate = function
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
	    (compile_c_declaration cmpstate) c_declaration))

and compile_c_function_definition cmpstate (expr:I.c_function_definition) = 
  {
    O.linkage = expr.I.linkage;
    O.c_type = expr.I.c_type;
    O.name = expr.I.name;
    O.code_array = Array.map (compile_node cmpstate) expr.I.code_array;
    O.jmp_tbl = expr.I.jmp_tbl;
  }

and relocate_union_wrapper_decl cmpstate ctn = 
  let rec relocate_decl_translation_unit cmpstate expr =
    match expr with
      | O.Translation_unit (l, eenv) ->
	  let l' = Safe_list.map (relocate_decl_external_declaration cmpstate) l
	  in O.Translation_unit (l', eenv)
	     
  and relocate_decl_external_declaration cmpstate expr = 
    match expr with
      | O.External_declaration_at (coord, expr) -> 
	  O.External_declaration_at 
	    (coord, relocate_decl_external_declaration cmpstate expr)
	    
      | O.External_declaration_1 (c_function_definition) ->
	  O.External_declaration_1 (c_function_definition)
 	    
      | O.External_declaration_2 (c_declaration) ->
	  O.External_declaration_2 
	    (Safe_list.flatten 
	      (Safe_list.map (relocate_decl_declaration cmpstate) c_declaration))
	    
  and relocate_decl_declaration cmpstate (expr:O.c_declaration) :O.c_declaration list = 
    match expr with
      | O.Str_decl_init (linkage, ce, str_literal) -> [expr]
	  
      | O.Obj_decl (linkage, ce) -> 
	  begin
	    let ce_info = Cent_op.ce_info_of ce
	    in
	    let te = ce_info.Cent.ce_te
	    in
	    try
	      let decl = Int.Type_idHashtbl.find cmpstate.union_init_tes (snd te)
	      in
	      if decl then
		[expr]
	      else
		begin
		  Int.Type_idHashtbl.replace cmpstate.union_init_tes (snd te) true;
		  [O.Type_decl te;expr]
		end
	    with
	      | Not_found -> 
		  [expr]
	  end
	    
      | O.Dat_decl_init v -> [expr]

      | O.Type_def c_type -> [expr]
	  
      | O.Type_decl te -> 
	  begin
	    try
	      let decl = Int.Type_idHashtbl.find cmpstate.union_init_tes (snd te)
	      in
	      if decl then
		[]
	      else
		begin
		  Int.Type_idHashtbl.replace cmpstate.union_init_tes (snd te) true;
		  [expr]
		end
	    with
	      | Not_found -> 
		  [expr]
	  end
	    
      | O.Type_only c_type -> [expr]
	  
  in relocate_decl_translation_unit cmpstate ctn
       
and compile (basename:string) (c_translation_unit:I.c_translation_unit)
    (env:E.env) :O.c_translation_unit =  
  let _ = EO.begin_file env basename   
  in
  let (cmpstate, c_translation_unit) = 
    compile_c_translation_unit env c_translation_unit
  in 
  let c_translation_unit =
    relocate_union_wrapper_decl cmpstate c_translation_unit
  in
  let (new_tmp_syms, new_typs) = EO.end_file env
  in c_translation_unit 

