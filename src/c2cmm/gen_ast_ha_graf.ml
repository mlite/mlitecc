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

module I   = Ast_ga_code
module O   = Ast_ha_graf


open Int
open Collection
let enable_log = ref false
    
let user_opts = [("--ast-graf110-enable-log",
		  Arg.Set enable_log,
		  "enable dominance computation log ")
		]

type gnode =
    { 
      mutable preds: IntSet.t;
      mutable succs: IntSet.t
    }

let current_coord = ref ("", 0, 0)
  
let compile: string -> I.c_translation_unit -> O.c_translation_unit = 
  fun basename c_translation_unit ->
    let rec compile_c_code080 is_reachable = function 
      | I.Scope expr -> I.Scope expr
      | I.Call expr -> 
	  if is_reachable then
	    I.Call expr
	  else
	    I.Nop
	      
      | I.Flow expr ->
	  if is_reachable then
	    I.Flow expr
	  else
	    I.Nop
	      
      | I.Sese exprs ->
	  if is_reachable then
	    I.Sese exprs
	  else
	    I.Nop

      | I.Join -> 
	  if is_reachable then
	    I.Join
	  else
	    I.Nop
	      
      | I.Nop -> I.Nop

      | I.Epi str_opt -> 
	  I.Epi str_opt

    and is_flow_join = function
      | I.Scope expr -> false
      | I.Call expr -> false
      | I.Flow expr -> false
      | I.Sese exprs -> false
      | I.Join -> true
      | I.Nop -> false
      | I.Epi str_opt -> false
	    
    and generate_gnode_array jmp_tbl code_array =
      let get_succs (pc, expr) =
	match expr with
	| I.Scope e -> 
	    begin
	      match e with
	      | I.End_fun -> IntSet.empty
	      | _ -> IntSet.singleton (pc+1)
	    end
	| I.Sese _ -> IntSet.singleton (pc+1)
	| I.Call _ -> IntSet.singleton (pc+1)
	| I.Nop  -> IntSet.singleton (pc+1)
	| I.Join -> IntSet.singleton (pc+1)
	| I.Epi _ -> IntSet.singleton (pc+1)
	| I.Flow expr ->
	    begin
	      match expr with
	      | I.Jmp int -> IntSet.singleton int
	      | I.Jmp_join int -> IntSet.singleton int
	      | I.Jmp_join_backward int -> IntSet.singleton int
	      (*| I.Cmp_zero_jmp (c_val, int) -> 
		  let set = IntSet.singleton (pc+1)
		  in IntSet.add int set*)

	      | I.Cmp_jmp (c_val, int) -> 
		  let set = IntSet.singleton (pc+1)
		  in IntSet.add int set
		       
	      | I.Tbl_jmp (c_val, idx) -> 
		  let lst = Array.get jmp_tbl idx
		  in
		  List.fold_left 
		    (fun set (_, x) -> IntSet.add x set)
		    IntSet.empty lst
	    end
      in
      let len = Array.length code_array
      in
      let gnode_array = 
	Array.create len 
	  { 
	    preds = IntSet.empty;
	    succs = IntSet.empty;
	  }
      in
      for i = 0 to len - 1 do
	Array.set gnode_array i 
	  { 
	    preds = IntSet.empty;
	    succs = IntSet.empty;
	  }
      done;
      let _ = 
	Array.iteri
	  (fun i (b, old_code, coord_opt) -> 
	    let succs = get_succs (i, old_code)
	    in 
	    IntSet.iter
	      (fun s ->
		let snode = Array.get gnode_array s
		in
		snode.preds <- IntSet.add i snode.preds
	      ) succs;
	    let old_gnode = Array.get gnode_array i
	    in
	    Array.set gnode_array i { preds = old_gnode.preds;
				      succs = succs
				    }
	  ) code_array
      in
      gnode_array

    and mark_reachable_nodes nodes = 
      let len = Array.length nodes
      in
      let reachables = Array.create len false
      in
      let rec rec_dfs i = 
	if Array.get reachables i = false then
	  let _ = Array.set reachables i true
	  and node = Array.get nodes i in
	  IntSet.iter rec_dfs node.succs
      in
      let _ = rec_dfs 0
      in
      reachables
	
    and rev_post_order: gnode array -> int list =
      fun nodes -> 
	let count = Array.length nodes in
	let visited = Array.create count false 
	and rpo = ref [] in 
	let rec dfs n = 
	  if Array.get visited n = false then 
	    begin
	      Array.set visited n true;
	      let node = Array.get nodes n 
	      in
	      IntSet.iter dfs node.succs;
	      rpo := n::!rpo
	    end
	in
	dfs 0;
	if !enable_log then
	  Safe_list.iter
	    (fun n -> 
	      Printf.printf "rpo %d\n"  (t_int n))
	    !rpo;
	!rpo

    and idom_of_reachable_node: int option array -> int -> int = 
      fun idoms rn -> 
	match Array.get idoms rn with 
	| None -> 
	    camlp4_macro_exception "reachable node %d has no idom" rn 
	| Some rn_idom -> rn_idom

    and compute_idom: gnode array -> int option array = 
      fun nodes -> 
	let node_size = Array.length nodes in 
	let idoms = Array.create node_size None 
	in 
	let rpo = rev_post_order nodes 
	in
	let rec common_dominator: preds:int list -> int = 
	  fun ~preds -> 
	    let common_ancestor (v0:int) (v1:int) :int = 
	      let a = ref v0
	      and b = ref v1
	      in 
	      while !a <> !b do
		while !a < !b do
		  if !enable_log then
		    Printf.printf "crazy loop1 !a %d !b %d\n" (t_int !a) (t_int !b);
		  b := idom_of_reachable_node idoms !b
		done;
		while !a > !b do
		  if !enable_log then
		    Printf.printf "crazy loop2 !b %d !a %d\n" (t_int !b) (t_int !a);
		  a := idom_of_reachable_node idoms !a
		done
	      done;
	      assert (!a >= 0);
	      !a
	    in
	    match preds with
	    | [] -> 
		camlp4_macro_exception 
		  "common_dominator is called with an emtpy preds\n";
	    | a::preds' -> 
		if !enable_log then
		  Printf.printf "get idom of node %d\n" (t_int a);
		match Array.get idoms a with 
		| Some aidom -> 
		    List.fold_left 
		      (fun new_idom p ->
			match Array.get idoms p with
			  | Some pidom -> common_ancestor p new_idom
			  | None -> new_idom (* Haven't compute the idom of the 
						predecessor yet *)
		      ) a preds';
		| None -> 
		    (** 
		       this recursive call intends to fix the error in 
		       Cooper's algorithm 
		       in "A simple, Fast Dominance Algorithm" 
		       the original algorithm fails in the case 
		       pred[0] = {}
		       pred[1] = {3, 0}
		       pred[2] = {1} 
		       pred[3] = {2}
		       The oringial algorithm will end up with 
		       idom[0] = 0
		       idom[1] = 3 
		       idom[2] = 1
		       idom[3] = 2
		       The procedure will go into endless loop
		     *)
		    if !enable_log then
		      Printf.printf "Warning:recusivelly call common_dominator\n";
		    common_dominator ~preds:preds' 
		      
	in
	let changed = ref true 
	and _ = Array.set idoms 0 (Some 0)
	in 
	while !changed do
	  let _ = changed := false in
	  Safe_list.iter
	    (fun nid -> 
	      if t_int nid = 0 then 
		()
	      else
		begin
		  if !enable_log then
		    Printf.printf "processing node %d\n" (t_int nid);
		  let preds = (Array.get nodes nid).preds
		  in
		  if (IntSet.cardinal preds) > 0 then 
		    begin
		      let new_idom = common_dominator (IntSet.elements preds) in 
		      match Array.get idoms nid with 
		      | Some old_idom -> 
			  if old_idom <> new_idom then 
			    let _ = Array.set idoms nid (Some new_idom) in
			    changed := true
		      | None -> 
			  let _ = Array.set idoms nid (Some new_idom) in 
			  changed := true
		    end
		  else if t_int nid > 0 then
		    camlp4_macro_exception 
		      "reachable node %d has empty preds" nid
		end
	    )
	    rpo
	done;
	idoms

    and compute_domfrontiers: gnode array -> int option array -> bool array -> int list array = 
      fun nodes idoms reachables ->
	let dom_fronts = Array.make (Array.length idoms) [] in 
	let _ = 
	  Array.iteri 
	    (fun nid node ->
	      if Array.get reachables nid then
		let nid_idom = idom_of_reachable_node idoms nid 
		in IntSet.iter
		  (fun pnode -> 
		    if Array.get reachables pnode then
		      let runner = ref pnode in
		      while !runner <> nid_idom do
			let runner_df = 
			  Array.get dom_fronts !runner 
			in 
			let _ = 
			  Array.set dom_fronts !runner (nid::runner_df) 
			in 
			runner := idom_of_reachable_node idoms !runner
		      done
		  )
		  node.preds
	    )
	    nodes
	in
	dom_fronts

    and compile_c_function_definition: I.c_function_definition -> 
      O.c_function_definition =
      fun expr ->
	let coord = !current_coord
	in
	let changed = ref false
	in
	let gnode_array = generate_gnode_array expr.I.jmp_tbl expr.I.code_array
	in
	let idom_array = compute_idom gnode_array
	and reachables = mark_reachable_nodes gnode_array
	in
	let dom_frontiers = compute_domfrontiers gnode_array idom_array reachables
	in
	let len = Array.length gnode_array
	in
	let code_array = Array.create len
	  { 
	    O.label_att = Code_label.SEQ (ref None);
	    O.coord_opt = None;
	    O.preds = [];
	    O.code = I.Nop;
	    O.succs = [];
	    O.idom = None;
	    O.domfrontiers = [];
	    O.dominated_nodes = IntSet.empty;
	    O.scope_begin_pcs = IntSet.empty;
	  }
	in
	let is_reachable n =
	  Array.get reachables n 
	in
	let scope_pc_stack = Stack.create ()
	in
	for i = 0 to len - 1 do
	  begin
	    let reachable = is_reachable i
	    and gnode = Array.get gnode_array i 
	    and (label_att, code, coord_opt) = Array.get expr.I.code_array i
	    in
	    let preds = List.filter is_reachable (IntSet.elements gnode.preds)
	    in
	    let label_att = 
	      if reachable then 
		match label_att with
		| Code_label.TARGET ->
		    if List.length preds > 1 then
		      begin
			assert (is_flow_join code);
			Code_label.JOIN (List.length preds)
		      end
		    else
		      Code_label.TARGET
		| Code_label.JOIN _ ->
		    assert (is_flow_join code);
		    Code_label.JOIN (List.length preds)
		| Code_label.SEQ _ -> 
		    label_att 
	      else 
		Code_label.SEQ (ref None)
	    in
	    let scope_begin_pcs = ref IntSet.empty
	    in
	    Stack.iter
	      (fun pc -> 
		scope_begin_pcs := IntSet.add pc !scope_begin_pcs
	      ) scope_pc_stack;
	    let _ = match code with
	    | I.Scope scope_ctrl ->
		begin
		  match scope_ctrl with
		  | I.Begin_fun _ 
		  | I.Begin_decl _ ->
		      Stack.push i scope_pc_stack
		  | _ -> 
		      ignore(Stack.pop scope_pc_stack)
		end
	    | _ -> ()
	    in
	    let node = 
	      { 
		O.label_att = 
		  if reachable then label_att else (Code_label.SEQ (ref None));
		O.coord_opt = coord_opt;
		O.preds = List.filter is_reachable (IntSet.elements gnode.preds);
		O.code = compile_c_code080 reachable code;
		O.succs = List.filter is_reachable (IntSet.elements gnode.succs);
		O.idom = Array.get idom_array i;
		O.domfrontiers = Array.get dom_frontiers i;
		O.dominated_nodes = IntSet.empty;
		O.scope_begin_pcs = !scope_begin_pcs;
	      }
	    in
	    Array.set code_array i node
	  end
	done;
	changed := true;
	while !changed do
	  begin
	    changed := false;
	    for i = len - 1 to 0 do
	      begin
		let node = Array.get code_array i
		in
		let idom_i_opt = node.O.idom
		in
		match idom_i_opt with
		| Some idom_i -> 
		    begin
		      let idom_node = Array.get code_array idom_i
		      in
		      let dominated_nodes = 
			IntSet.add i
			  (IntSet.union node.O.dominated_nodes 
			     idom_node.O.dominated_nodes)
		      in
		      let diff = IntSet.diff dominated_nodes idom_node.O.dominated_nodes
		      in
		      if not (IntSet.is_empty diff) then
			begin
			  idom_node.O.dominated_nodes <- dominated_nodes;
			  changed := true;
			end
		    end
		| None -> ()
	      end
	    done;
	  end;
	done;
	{ 
	  O.linkage = expr.I.linkage;
	  O.c_type = expr.I.c_type;
	  O.name = expr.I.name;
	  O.code_array = code_array;
	  O.jmp_tbl = expr.I.jmp_tbl;
	}
	    
    and compile_c_external_declaration = function
      | I.External_declaration_at (coord, expr) ->
	  let _ = current_coord := coord
	  in O.External_declaration_at 
	       (coord, compile_c_external_declaration expr)
	    
      | I.External_declaration_1 c_function_definition ->
	  O.External_declaration_1 
	    (compile_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 c_declaration ->
	  O.External_declaration_2 c_declaration
		  
    and compile_c_translation_unit (c_translation_unit:I.c_translation_unit):O.c_translation_unit = 
      match c_translation_unit with
	| I.Translation_unit (l, eenv) ->
	    O.Translation_unit 
	      (List.map
		(fun external_declaration -> 
		  compile_c_external_declaration external_declaration
		) l, eenv)
    in compile_c_translation_unit c_translation_unit
