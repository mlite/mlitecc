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

open Ast_ha_graf
open Int
open Collection

module M = Ast_eb_expr_op 
module C = Ast_ga_code
module T = Tent
module TO = Tent_op
module E = Senv
module QNP = Qual_name_printer

let current_coord = ref ("", 0, 0)
  

let link ~(basename:string) ~(global:E.env) ~(local:E.env) c_translation_unit:c_translation_unit = 
  let teid_maps = ref 
    { 
      Uni_typ_db.local_te_tbl_name = "<NONE>";
      Uni_typ_db.gteid_to_lteid = Array.create 0 [];
      Uni_typ_db.lteid_to_gteid = Array.create 0 None;
    }
  and ceid_maps = ref 
    { 
      Uni_ce_tbl.local_ce_tbl_name = "<NONE>";
      Uni_ce_tbl.gceid_to_lceid = Array.create 0 [];
      Uni_ce_tbl.lceid_to_gceid = Array.create 0 None;
    }
  in
  let rec link_scope_ctrl = function
    | C.Begin_fun (decls, i) ->
	let decls = Safe_list.map (M.c_local_declaration_map map_te map_ce) decls
	in C.Begin_fun (decls, i)
	     
    | C.End_fun -> C.End_fun
	
    | C.Begin_decl (decls, i) ->
	C.Begin_decl (Safe_list.map 
	  (M.c_local_declaration_map map_te map_ce) decls, i)
	  
    | C.End_decl -> C.End_decl
	
  and link_flow_ctrl = function
    | C.Jmp int -> C.Jmp int
    | C.Jmp_join int -> C.Jmp_join int
    | C.Jmp_join_backward int -> C.Jmp_join_backward int
    (*| C.Cmp_zero_jmp (c_val, int) -> 
	C.Cmp_zero_jmp (M.rval_map map_te map_ce c_val, int)*)
    | C.Cmp_jmp (c_val, int) -> 
	C.Cmp_jmp (M.true_cond_map map_te map_ce c_val, int)
    | C.Tbl_jmp (c_val, int) -> 
	C.Tbl_jmp (M.rval_map map_te map_ce c_val, int)
	  
  and link_sese_code = function
    | C.Computing elst ->
	C.Computing (Safe_list.map (M.expr_map map_te map_ce) elst)
	  
    | C.Pop_args args -> 
	C.Pop_args ((Safe_list.map (fun (t,v) -> (map_te t, v))) args)
	  
    | C.Asm (s, v) -> C.Asm (s,v)
	
  and link_c_code080 = function
    | C.Nop -> C.Nop 
    | C.Scope expr -> C.Scope (link_scope_ctrl expr)
    | C.Call expr -> C.Call (M.call_ctrl_map map_te map_ce expr)
    | C.Flow expr -> C.Flow (link_flow_ctrl expr)
    | C.Sese exprs -> C.Sese (Safe_list.map link_sese_code exprs)
    | C.Join -> C.Join
    | C.Epi str_opt -> C.Epi str_opt
	
  and map_ce expr = 
    let (ce_tbl, _) = expr
    in
    if ce_tbl == global.E.ce_tbl then
      expr
    else
      Uni_ce_tbl.find_typ !ceid_maps ~dest_tbl:global.E.ce_tbl expr

  and map_te (expr:c_type) : c_type = 
    let (te_tbl, _) = expr
    in
    if te_tbl == global.E.te_tbl then
      expr
    else
      Uni_typ_db.find_typ !teid_maps ~dest_tbl:global.E.te_tbl expr
	
  and link_c_translation_unit = function
    | Translation_unit (l, eenv) ->
	Translation_unit 
	  (Safe_list.map
	    (fun external_declaration -> 
	      link_c_external_declaration external_declaration
	    ) l, eenv)
	  
  and link_c_external_declaration e = 
    match e with
      | External_declaration_at (coord, c_external_declaration) ->
	  let _ = current_coord := coord
	  in
	  External_declaration_at 
	    (coord, link_c_external_declaration c_external_declaration)
	    
      | External_declaration_1 c_function_definition ->
	  External_declaration_1 
	    (link_c_function_definition c_function_definition)
	    
      | External_declaration_2 c_declaration ->
	  let lst = Safe_list.map (M.c_declaration_map map_te map_ce) c_declaration
	  in External_declaration_2 lst
	       
  and link_jmp_tbl 
      (old_array:((C.c_constant_expression * C.c_constant_expression) option * int) list array) 
      :((C.c_constant_expression * C.c_constant_expression) option * int) list array =
    let new_array = Array.create (Array.length old_array) []
    in
    Array.iteri
      (fun i old_lst -> 
	let new_lst = Safe_list.map
	  (fun (e_opt, v) ->
	    match e_opt with
	      | Some (e0, e1) ->
		  (Some (M.cexpr_map map_te map_ce e0, 
		  M.cexpr_map map_te map_ce e1), v)
	      | None ->
		  (None, v)
	  ) old_lst
	in
	Array.set new_array i new_lst
      ) old_array;
    new_array
      
  and link_c_function_definition (expr:c_function_definition):c_function_definition =
    let len = Array.length expr.code_array
    in
    let code_array = Array.create len 
      { 
	label_att = Code_label.SEQ (ref None);
	coord_opt = None;
	preds = [];
	code = C.Nop;
	succs = [];
	idom = None;
	domfrontiers = [];
	dominated_nodes = IntSet.empty;
	scope_begin_pcs = IntSet.empty;
      }
    in
    for i = 0 to len - 1 do
      let node = Array.get expr.code_array i
      in
      let new_node = 
	{
	  label_att = node.label_att;
	  coord_opt = node.coord_opt;
	  preds = node.preds;
	  code = link_c_code080 node.code;
	  succs = node.succs;
	  idom = node.idom;
	  domfrontiers = node.domfrontiers;
	  dominated_nodes = node.dominated_nodes;
	  scope_begin_pcs = node.scope_begin_pcs;
	}
      in
      Array.set code_array i new_node
    done;
    let jmp_tbl = link_jmp_tbl expr.jmp_tbl
    in
    if expr.name.Qual_name.qn_sname = "newstring" then
      print_string (QNP.to_decl_str expr.name)
    else
      ();
    { 
      linkage = expr.linkage;
      c_type = map_te expr.c_type;
      name = expr.name;
      code_array = code_array;
      jmp_tbl = jmp_tbl;
    }
  in
  let _ = 
    teid_maps := 
      Uni_typ_db.merge_typ_db ~dest_tbl:global.E.te_tbl 
	~src_tbl:local.E.te_tbl
  in
  let _ = 
    ceid_maps :=
      Uni_ce_tbl.merge_ce_tbl ~map_te
	~dest_tbl:global.E.ce_tbl
	~src_tbl:local.E.ce_tbl
  in
  let out_chan = open_out (basename ^ ".teid_map")
  in
  let fm = Format.formatter_of_out_channel out_chan
  in
  Uni_typ_db.pp_print_teid_maps fm !teid_maps;
  Format.pp_print_flush fm ();
  close_out out_chan;
  let out_chan = open_out (basename ^ ".ceid_map")
  in
  let fm = Format.formatter_of_out_channel out_chan
  in
  Uni_ce_tbl.pp_print_ceid_maps fm !ceid_maps;
  Format.pp_print_flush fm ();
  close_out out_chan;
  Typ_db_log.doit global.E.te_tbl global.E.ce_tbl "check_this_if_failed";
  let _ = Uni_typ_db.teid_maps_sanity_check !teid_maps
  and _ = Uni_typ_db.typ_db_sanity_check global.E.te_tbl
  in link_c_translation_unit c_translation_unit
