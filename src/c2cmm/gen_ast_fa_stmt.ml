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

open Int
open Collection
open Safe_list


module C99 = Ast_aa_gram
module I = Ast_ec_expr
module O = Ast_fa_stmt
module QN = Qual_name
module T = Tent
module TO = Tent_op
module CS = C_semantics_symbol

exception Error 

module HashCoordinate = 
  struct 
    type t = Coordinate.t
    let equal (s1:t) (s2:t) = 
      let (file1, row1, col1) = s1
      and (file2, row2, col2) = s2
      in 
      file1 = file2 & row1 = row2 & col1 = col2
	
    let hash (s:t) =
      let (file, row, col) = s 
      in 
      Hashtbl.hash (file ^ (string_of_int row) ^ (string_of_int col))
  end
    
module CoordinateHashtbl = Hashtbl.Make(HashCoordinate)   

let current_coord = ref ("", 0, 0)
let assert_no_return_inst = ref false

let user_opts = 
  [
    ("--assert-no-return-inst",
    Arg.Set assert_no_return_inst,
    "assert all return instruction should be normalized as jmp")
  ]
    
type function_data = 
    { 
      node_serno: int ref;
      mutable epi_join_node: int;
      mutable ret_variable: O.rval option;
      switch_head_stack: 
	(((O.c_constant_expression * O.c_constant_expression) option * int) list ref) Stack.t;
      continue_target_stack: (int * bool ref) Stack.t;
      break_target_stack: (int * bool ref) Stack.t;
      label_tbl: int Collection.StringHashtbl.t;
      line_tbl: int CoordinateHashtbl.t;
      used_lab: bool Collection.IntHashtbl.t;
      mutable is_toplevel_function_scope: bool;
      mutable formal_params: (O.c_type * QN.t) list;
    }
      
let patch_lab: int StringHashtbl.t -> bool IntHashtbl.t -> O.c_stmt090 list -> 
  O.c_lab_stmt090 list =
  fun goto_label_tbl target_label_tbl c_stmt_list -> 
    let is_target l = 
      try
	let _ = IntHashtbl.find target_label_tbl l
	in
	Code_label.TARGET
      with
	Not_found -> (Code_label.SEQ (ref None))
    in
    Safe_list.map 
      (fun stmt -> 
	match stmt with 
	| O.GOTO (g0, lb) ->
	    let t = StringHashtbl.find goto_label_tbl lb
	    in
	    (is_target g0, O.JMP (g0, Some lb, t))
	| O.JOIN (g0, _, _) 
	| O.SESE_SEQUENCE (g0, _, _, _)
	| O.CALL (g0, _, _)
	| O.ASM (g0, _, _, _)
	(*| O.CMP_ZERO_JMP (g0, _) *)
	| O.CMP_JMP (g0, _) 
	| O.TBL_JMP (g0, _) 
	| O.JMP (g0, _, _)
	| O.JMP_JOIN (g0, _, _)
	| O.JMP_JOIN_BACKWARD (g0, _, _)
	| O.BEGIN_DECL (g0, _, _, _)
	| O.END_DECL (g0, _)
	| O.POP_ARGS (g0, _, _) 
	| O.BEGIN_FUNCTION (g0, _, _, _)
	| O.EPI (g0,_,_) 
	| O.END_FUNCTION g0
	  -> 
	    (is_target g0, stmt)
      ) c_stmt_list
      

let fusion_stmts (stmts:I.c_stmt010 list) : I.c_stmt010 list =
  let lst = ref []
  and new_stmts = ref []
  in
  let rec fusion lst s =
    match s with
      | I.COMPUTATION s -> lst := !lst @$ [s]
      | I.SESE l -> lst := !lst @$ l
      | I.SEQUENCE (str_opt, l) -> Safe_list.iter (fusion lst) l
      | I.STMT_AT (_, s) -> fusion lst s
      | I.STMT_SPAN (_, s) -> fusion lst s
      | _ -> 
	  let _ = new_stmts := !new_stmts @$ [I.SESE !lst;s]
	  in lst := []
  in
  let _ = Safe_list.iter (fusion lst) stmts
  in 
  if !lst <> [] then
    !new_stmts @$ [I.SESE !lst]
  else
    !new_stmts
    

	
let compile (basename:string) (c_translation_unit:I.c_translation_unit) : O.c_translation_unit = 
  let function_scope = Stack.create ()
  in
  let begin_function () = 
    let function_data = 
      { 
	node_serno = ref (int_t 0);
	epi_join_node = -1;
	ret_variable = None;
	switch_head_stack = Stack.create ();
	continue_target_stack = Stack.create ();
	break_target_stack = Stack.create ();
	label_tbl  = Collection.StringHashtbl.create 211;
	line_tbl = CoordinateHashtbl.create 211;
	used_lab = Collection.IntHashtbl.create 13;
	is_toplevel_function_scope = true;
	formal_params = [];
      }
    in
    Stack.push function_data function_scope
      
  and end_function () = 
    ignore (Stack.pop function_scope)
  in
  let rec new_node () :Int.gnode_id =
    let function_data = Stack.top function_scope
    in
    let v = !(function_data.node_serno)
    in
    incr function_data.node_serno;
    v
	
  and new_node_for_coord (coord:Coordinate.t) :Int.gnode_id =
    let function_data = Stack.top function_scope
    in
    let nid = new_node ()
    in
    let _ = CoordinateHashtbl.add function_data.line_tbl coord nid 
    in nid
  in
  (** the following are standard copy translation **)
  let rec compile_c_stmt010_list: followup:Int.gnode_id -> 
    I.c_stmt010 list -> O.c_stmt090 list * Int.gnode_id =
    fun  ~followup c_stmt010_list ->
      begin
	let rev_c_stmt010_list = List.rev c_stmt010_list
	in
	let current_followup = ref followup
	in
	let rev_c_stmt090_list_list = 
	  Safe_list.map
	    (fun stmt ->
	      let (stmt, followup) = 
		compile_c_stmt010 ~followup:!current_followup stmt
	      in
	      current_followup := followup;
	      stmt
	    ) rev_c_stmt010_list;
	in
	let c_stmt090_list_list = List.rev rev_c_stmt090_list_list
	in
	(Safe_list.flatten c_stmt090_list_list, !current_followup)
      end

  and not_true_cond = function
    | Ast_eb_expr.NEQ_ZERO v -> Ast_eb_expr.EQ_ZERO v
    | Ast_eb_expr.EQ_ZERO v -> Ast_eb_expr.NEQ_ZERO v
    | Ast_eb_expr.IPRED (rel, bits, v0, v1) -> 
	let rel = match rel with
	  | Ast_eb_expr.EQ -> Ast_eb_expr.NE
	  | Ast_eb_expr.NE -> Ast_eb_expr.EQ
	  | Ast_eb_expr.LT -> Ast_eb_expr.GE
	  | Ast_eb_expr.GT -> Ast_eb_expr.LE
	  | Ast_eb_expr.LE -> Ast_eb_expr.GT
	  | Ast_eb_expr.GE -> Ast_eb_expr.LT
	  | Ast_eb_expr.GTU -> Ast_eb_expr.LEU
	  | Ast_eb_expr.GEU -> Ast_eb_expr.LTU
	  | Ast_eb_expr.LTU -> Ast_eb_expr.GEU
	  | Ast_eb_expr.LEU -> Ast_eb_expr.GTU
	in Ast_eb_expr.IPRED (rel, bits, v0, v1)
	     
    | Ast_eb_expr.FPRED (rel, bits, v0, v1) -> 
	(* some floating pointer numbers such as NaN 
	   are not comparable, so we cannot simply flip
	   predicate relations
	*)
	Ast_eb_expr.FNOTPRED (rel, bits, v0, v1)

    | Ast_eb_expr.FNOTPRED (rel, bits, v0, v1) -> 
	(* some floating pointer numbers such as NaN 
	   are not comparable, so we cannot simply flip
	   predicate relations
	*)
	Ast_eb_expr.FPRED (rel, bits, v0, v1)
	  
  and compile_c_stmt010: followup:Int.gnode_id -> I.c_stmt010 ->  
  O.c_stmt090 list * Int.gnode_id =
    fun ~followup expr  ->
      let function_data = Stack.top function_scope
      in
      let use_lab lab =
	IntHashtbl.replace function_data.used_lab lab true
      in
      match expr with
	| I.NOP -> ([], followup)

	| I.ASM (strlst, asm) ->
	    let nid = new_node ()
	    in ([O.ASM (nid, strlst, asm, followup)], nid)

	| I.COMPUTATION expr -> assert false
	| I.SESE exprs ->
	    let nid = new_node ()
	    in ([O.SESE_SEQUENCE (nid, None, exprs, followup)], nid)

	| I.CALL (lval_opt, expr0, expr_list) -> 
	    let nid = new_node ()
	    in ([O.CALL (nid, (lval_opt, expr0, expr_list), followup)], nid)

	| I.STMT_AT (coord, cstmt) ->
	    compile_c_stmt010 ~followup cstmt
	      
	| I.STMT_SPAN (str, cstmt) ->
	    compile_c_stmt010 ~followup cstmt
	      
	| I.RETURN_VALUE _ -> assert false
	| I.GCC_GOTO _ -> assert false
	    
	| I.SEQUENCE (txt_opt, c_stmt010_list) ->
	    compile_c_stmt010_list ~followup c_stmt010_list 
	      
	| I.COMPOUND (txt_opt, c_stmt010_list) ->
	    compile_c_stmt010_list ~followup c_stmt010_list 
	      
	| I.IF (c_val, then_c_stmt090, else_c_stmt090) ->
	    begin
	      let join = new_node ()
	      in
	      let jmp0 = new_node ()
	      and jmp1 = new_node ()
	      in
	      let (then_c_stmt090, followup0) = 
		compile_c_stmt010 ~followup:jmp0 then_c_stmt090 
	      and (else_c_stmt090, followup1) = 
		compile_c_stmt010 ~followup:jmp1 else_c_stmt090 
	      in
	      let cond = new_node_for_coord !current_coord
	      in
	      use_lab (join);
	      use_lab (followup1);
	      let stmts = Safe_list.concat 
		((O.CMP_JMP (cond, 
		(not_true_cond c_val, followup1)))::then_c_stmt090)
		((O.JMP_JOIN (jmp0, None, join))::else_c_stmt090)
	      in
	      let stmts = Safe_list.concat
		stmts [O.JMP_JOIN 
		  (jmp1, None, join);O.JOIN (join, None, followup)]
	      in (stmts, cond)
	    end

	| I.LOOP sl0 -> 
	    begin
	      let head_join = new_node ()
	      in
	      let end_join = new_node ()
	      in
	      let jmp_node = new_node ()
	      in
	      let jmp_from_end_join = O.JOIN (end_join, None, followup)
	      in
	      use_lab (followup);
	      let followup = end_join
	      in
	      let _ = Stack.push (followup, ref false) 
		function_data.break_target_stack
	      and _ = Stack.push (head_join, ref false) 
		function_data.continue_target_stack 
	      in
	      let (sl0', loop_start) = 
		compile_c_stmt010 ~followup:jmp_node sl0
	      in
	      let _ = Stack.pop function_data.continue_target_stack
	      and _ = Stack.pop function_data.break_target_stack 
	      in 
	      let loop_cond = new_node_for_coord !current_coord
	      in
	      use_lab (head_join);
	      use_lab (followup);
	      let stmts = Safe_list.concat
		[O.JOIN (head_join, None, loop_cond);] sl0'
	      in
	      let stmts = Safe_list.concat
		stmts [O.JMP_JOIN_BACKWARD (jmp_node, None, head_join);
		jmp_from_end_join]
	      in 
	      (stmts, head_join)
	    end
	      
	| I.WHILE (expr, sl0) -> 
	    begin
	      let head_join = new_node ()
	      in
	      let end_join = new_node ()
	      in
	      let jmp_node = new_node ()
	      in
	      let jmp_from_end_join = O.JOIN (end_join, None, followup)
	      in
	      use_lab (followup);
	      let followup = end_join
	      in
	      let _ = Stack.push (followup, ref false) 
		function_data.break_target_stack
	      and _ = Stack.push (head_join, ref false) 
		function_data.continue_target_stack 
	      in
	      let (sl0', loop_start) = 
		compile_c_stmt010 ~followup:jmp_node sl0
	      in
	      let _ = Stack.pop function_data.continue_target_stack
	      and _ = Stack.pop function_data.break_target_stack 
	      in 
	      let loop_cond = new_node_for_coord !current_coord
	      in
	      use_lab (head_join);
	      use_lab (followup);
	      let stmts = Safe_list.concat
		([O.JOIN (head_join, None, loop_cond);
		O.CMP_JMP (loop_cond, (not_true_cond expr, followup))])
		sl0'
	      in
	      let stmts = Safe_list.concat
		stmts [O.JMP_JOIN_BACKWARD (jmp_node, None, head_join);
		jmp_from_end_join]
	      in
	      (stmts, head_join)
	    end
	      
	| I.BREAK -> 
	    begin
	      (** 6.8.6.3  The break statement
		  Constraints
		  [#1]  A  break statement shall appear only in or as a switch
		  body or loop body.
		  
		  Semantics
		  [#2] A break statement terminates execution of the smallest
		  enclosing switch or iteration statement.
	      **)
	      if Stack.is_empty function_data.break_target_stack then 
		camlp4_macro_exception 
		  "break statement at line %s is not in switch body or loop
		 body" (Coordinate.to_str !current_coord)
	      else
		let nid = new_node ()
		and (tnid, is_used) = Stack.top function_data.break_target_stack 
		in 
		is_used := true;
		([O.JMP (nid, Some "break", tnid)], nid)
	    end
	      
	| I.CONTINUE -> 
	    begin
	      (** 6.8.6.2  The continue statement
		  
		  Constraints
		  [#1]  A continue statement shall appear only in or as a loop
		  body.
		  
		  Semantics
		  [#2] A  continue  statement  causes  a  jump  to  the  loop-
		  continuation  portion  of  the  smallest enclosing iteration
		  statement; that is, to the  end  of  the  loop  body.   More
		  precisely, in each of the statements
		  
		  while (e) {  
		  ...
		  continue;    
		  ...
		  contin: ;    
		  }            
		  
		  do {
		  ...
		  continue;
		  ...
		  contin: ;
		  } while (e)
		  
		  for (;;) {
		  ...
		  continue;
		  ...
		  contin: ;
		  }
	      **)
	      if Stack.is_empty function_data.continue_target_stack then 
		camlp4_macro_exception 
		  "continue statement at line %s is not in a loop body" 
		  (Coordinate.to_str !current_coord)
	      else
		let nid = new_node_for_coord !current_coord 
		and (tnid, is_used) = Stack.top function_data.continue_target_stack 
		in 
		is_used := true;
		([O.JMP (nid, Some "continue", tnid)], nid)
	    end
	      
	| I.RETURN -> 
	    let nid = new_node_for_coord !current_coord 
	    in 
	    use_lab (function_data.epi_join_node);
	    ([O.JMP_JOIN (nid, Some "return", function_data.epi_join_node)], nid)
	      
	| I.SWITCH (expr, c_stmt090) ->
	    begin
	      let switch_join_nid = new_node ()
	      in
	      let switch_join = O.JOIN (switch_join_nid, None, followup)
	      in
	      let followup = switch_join_nid
	      in
	      let _ = 
		Stack.push (followup, ref false)
		  function_data.break_target_stack
	      and _ = 
		Stack.push 
		  (ref []) function_data.switch_head_stack
	      in
	      let (stmt', start) = compile_c_stmt010 ~followup c_stmt090
	      in
	      let (_, is_used) = Stack.pop function_data.break_target_stack 
	      and target_pairs = Stack.pop function_data.switch_head_stack
	      in
	      let cond = new_node_for_coord !current_coord 
	      in
	      let patch_default = ref true
	      in
	      let target_pairs = 
		let target_pairs0 = ref []
		and default = ref (None, followup)
		in
		let _ = List.iter
		  (fun (expr_opt, t) ->
		    match expr_opt with
		      | None -> 
			  default := (None, t);
			  patch_default := false
		      | _ -> 
			  target_pairs0 := (expr_opt, t)::!target_pairs0
		  ) !target_pairs
		in
		(Safe_list.concat (List.rev !target_pairs0) [!default])
	      in
	      let _ = if !is_used or !patch_default then
		use_lab (followup)
	      in
	      ((O.TBL_JMP (cond, (expr, target_pairs)))::stmt'@[switch_join], cond)
	    end
	      
	| I.CASE (c_constant_expression, c_stmt090) ->
	    let (c_stmt090, followup) = compile_c_stmt010 ~followup c_stmt090 
	    in
	    let join_node = new_node ()
	    in
	    let target_pairs = Stack.top function_data.switch_head_stack
	    in
	    let _ = target_pairs := 
	      (Some (c_constant_expression, c_constant_expression), 
	      join_node)::!target_pairs
	    in
	    use_lab (followup);
	    use_lab (join_node);
	    ((O.JOIN (join_node, None, followup))::c_stmt090, join_node)

	| I.CASE_RANGE (e0, e1, c_stmt090) ->
	    let (c_stmt090, followup) = compile_c_stmt010 ~followup c_stmt090 
	    in
	    let join_node = new_node ()
	    in
	    let target_pairs = Stack.top function_data.switch_head_stack
	    in
	    let _ = target_pairs := (Some (e0, e1), join_node)::!target_pairs
	    in
	    use_lab (followup);
	    use_lab (join_node);
	    ((O.JOIN (join_node, None, followup))::c_stmt090, join_node)

	| I.DEFAULT stmt0 ->
	    let (stmt0, followup) = compile_c_stmt010 ~followup stmt0
	    in
	    let join_node = new_node ()
	    in
	    let target_pairs = Stack.top function_data.switch_head_stack
	    in
	    let _ = target_pairs := (None, join_node)::!target_pairs
	    in
	    use_lab (followup);
	    use_lab (join_node);
	    ((O.JOIN (join_node, None, followup))::stmt0, join_node)

	| I.LABEL (lb, stmt0) ->
	    let (stmt0, followup) = compile_c_stmt010 ~followup stmt0
	    in
	    let join_nid = new_node_for_coord !current_coord
	    in
	    let _ = 
	      Collection.StringHashtbl.add 
		function_data.label_tbl lb join_nid (*followup*)
	    in
	    let _ = use_lab (followup)
	    and _ = use_lab (join_nid)
	    in
	    ((O.JOIN (join_nid, None, followup))::stmt0, join_nid)

	| I.GOTO nam -> 
	    let nid = new_node_for_coord !current_coord
	    in
	    ([O.GOTO (nid, nam)], nid)

	| I.EPI str_opt ->
	    let nid = new_node ()
	    in
	    let join_nid = new_node ()
	    in
	    function_data.ret_variable <- str_opt;
	    function_data.epi_join_node <- join_nid;
	    ([O.JOIN (join_nid, None, nid);O.EPI (nid, str_opt, followup)],
	    join_nid)

  and compile_c_compound_stmt010: followup: Int.gnode_id -> 
  I.c_compound_stmt010 -> O.c_stmt090 list * Int.gnode_id = 
    fun ~followup expr ->
      match expr with
	| I.BLOCK (strlist, decls, stmts) ->
	    begin
	      let stmts = fusion_stmts stmts
	      in
	      let function_data = Stack.top function_scope
	      in
	      let stmt_followup = ref followup
	      and all_stmts = ref []
	      in
	      let is_toplevel_function_scope = function_data.is_toplevel_function_scope
	      in
	      let end_decl_g = 
		if is_toplevel_function_scope then
		  begin
		    function_data.is_toplevel_function_scope <- false;
		    let followup = new_node ()
		    in 
		    all_stmts :=  (O.END_FUNCTION followup)::!all_stmts;
		    stmt_followup := followup;
		    followup
		  end
		else if decls <> [] then 
		  begin
		    let pop_nid = new_node ()
		    in
		    all_stmts := (O.END_DECL (pop_nid, !stmt_followup))::!all_stmts;
		    stmt_followup := pop_nid;
		    pop_nid
		  end
		else
		  -1
	      in
	      let _ = 
		let (c_stmt090s, stmt_start_nid) = 
		  compile_c_stmt010_list ~followup:!stmt_followup stmts 
		in
		all_stmts := Safe_list.concat c_stmt090s !all_stmts;
		stmt_followup := stmt_start_nid
	      in
	      if is_toplevel_function_scope then
		begin
		  let pop_arg_list = 
		    let nid = new_node ()
		    in [O.POP_ARGS 
		      (nid, List.rev function_data.formal_params, !stmt_followup)]
		  in
		  let push_nid = new_node ()
		  in
		  all_stmts := 
		    (O.BEGIN_FUNCTION (push_nid, ref decls, !stmt_followup, end_decl_g))::
		      (Safe_list.concat pop_arg_list !all_stmts);
		  stmt_followup := push_nid;
		end
	      else if decls <> [] then 
		begin
		  let push_nid = new_node ()
		  and join_nid = new_node ()
		  in
		  all_stmts := 
		    (O.JOIN (join_nid, None, push_nid))::
		      (O.BEGIN_DECL (push_nid, decls, !stmt_followup, end_decl_g))::!all_stmts;
		  stmt_followup := join_nid;
		end;
	      (!all_stmts, !stmt_followup)
	    end
	      
  and compile_c_translation_unit: I.c_translation_unit -> 
  O.c_translation_unit = 
    fun c_translation_unit ->
      match c_translation_unit with
	| I.Translation_unit (l, eenv) ->
	    O.Translation_unit 
	      (List.map
		(fun external_declaration -> 
		  compile_c_external_declaration external_declaration
		)l, eenv)
	      
  and compile_c_external_declaration (expr:I.c_external_declaration) :
      O.c_external_declaration =
    match expr with
      | I.External_declaration_at (coord, expr) ->
	  O.External_declaration_at 
	    (coord, compile_c_external_declaration expr)
	    
      | I.External_declaration_1 c_function_definition ->
	  O.External_declaration_1 
	    (compile_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 c_declaration ->
	  O.External_declaration_2 c_declaration
	    
  and compile_c_function_definition (expr:I.c_function_definition) :O.c_function_definition =
    let coord = !current_coord
    in
    let I.Function_definition (linkage, fun_type, fun_name,
    c_compound_stmt090) = expr
    in
    let _ = begin_function ()
    in
    let function_data = Stack.top function_scope
    in
    function_data.formal_params <- 
      (List.map
	(fun (te, param) -> 
	  match param with 
	    | T.THIS_PARAM v 
	    | T.NORMAL_PARAM v
	    | T.HIDDEN_RETURN v ->
		(te, v)
	    | T.SCALAR_PARAM qmuton
	    | T.STRUCT_PARAM qmuton -> (te, qmuton.T.muton)
	) (TO.formal_params_of fun_type));
    let (c_compound_stmt090, start) = 
      compile_c_compound_stmt010 ~followup:(-1) c_compound_stmt090
    in
    let function_data = Stack.top function_scope
    in
    let _ = end_function ()
    in
    let line_tbl = 
      Array.create !(function_data.node_serno) None
    in
    CoordinateHashtbl.iter
      (fun coord i ->
	Array.set line_tbl i (Some coord);
      ) function_data.line_tbl;
    { 
      O.linkage = linkage;
      O.c_type = fun_type;
      O.name = fun_name;
      O.code_size = !(function_data.node_serno);
      O.c_stmt090_list = 
	patch_lab 
	  function_data.label_tbl
	  function_data.used_lab
	  c_compound_stmt090;
      O.line_tbl = line_tbl;
    }
  in compile_c_translation_unit c_translation_unit
