open Cabs


let normalize: definition -> definition =
  fun definition ->
    let variable_count = ref 0
    and changed = ref false
    in
    let rec get_new_variable () = 
      let var = VARIABLE ("_yc_gen_" ^ (string_of_int !variable_count))
      in
      incr variable_count;
      var

    and normalize_expression: expression -> cabsloc -> (statement * expression) option = 
      fun expression cabsloc ->
	let s = match expression with
	| PAREN _ -> assert false
	| NOTHING -> None
	| UNARY (unary_operator, sub_expr) ->
	    begin
	      let result_opt = normalize_expression sub_expr cabsloc
	      in
	      match result_opt with
	      | Some (stmt, result) ->
		  let var = get_new_variable ()
		  in
		  let new_stmt = 
		    BINARY (ASSIGN, var, (UNARY (unary_operator, result)))
		  in
		  Some (SEQUENCE (stmt, COMPUTATION (new_stmt, cabsloc), cabsloc), var)
	      | None -> None
	    end
	| LABELADDR _ -> None
	| BINARY (binary_operator, exp0, exp1) -> 
	    begin
	      let result_opt0 = normalize_expression exp0 cabsloc
	      and result_opt1 = normalize_expression exp1 cabsloc
	      in
	      match result_opt0 with
	      | Some (stmt0, result0) ->
		  begin
		    match result_opt1 with
		    | Some (stmt1, result1) ->
			let new_stmt =
			  BINARY (binary_operator, result0, result1)
			in
			Some (SEQUENCE (stmt0, stmt1, cabsloc), new_stmt)
		    | None -> 
			begin
			  let new_stmt = 
			    BINARY (binary_operator, result0, exp1)
			  in
			  Some (stmt0, new_stmt)
			end
		  end
	      | None -> 
		  begin
		    match result_opt1 with
		    | Some (stmt1, result1) ->
			begin
			  let new_stmt = 
			    BINARY (binary_operator, exp0, result1)
			  in
			  Some (stmt1, new_stmt)
			end
		    | None -> None
		  end
	    end
	| QUESTION (exp, exp1, exp2) ->
	    begin
	      changed := true;
	      let var = get_new_variable ()
	      in
	      Some (IF (exp, 
			COMPUTATION (BINARY (ASSIGN, var, exp1), cabsloc), 
			COMPUTATION (BINARY (ASSIGN, var, exp2), cabsloc), 
			cabsloc), 
		    var)
	    end
	| CAST _ 
	| CALL _ 
	| COMMA _
	| CONSTANT _
	| VARIABLE _
	| EXPR_SIZEOF _
	| TYPE_SIZEOF _
	| EXPR_ALIGNOF _
	| TYPE_ALIGNOF _
	| INDEX _
	| MEMBEROF _
	| MEMBEROFPTR _ -> None
	| GNU_BODY _ -> assert false
	| EXPR_PATTERN _ -> 
	    assert false
	in s
	  
    and normalize_statement: statement -> statement =
      fun statement ->
	let s = match statement with
	| NOP _ -> statement
	| COMPUTATION (expression, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> SEQUENCE (stmt, COMPUTATION (v, cabsloc), cabsloc)
	      | None -> statement
	    end
	| BLOCK (block, cabsloc) ->
	    begin
	      let new_block = {
		blabels = block.blabels;
		battrs = block.battrs;
		bstmts = List.map normalize_statement block.bstmts;
	      }
	      in
	      BLOCK (new_block, cabsloc)
	    end
	| SEQUENCE (statement1, statement2, cabsloc) ->
	    begin
	      SEQUENCE (normalize_statement statement1, 
			normalize_statement statement2, cabsloc)
	    end
	| IF (expression, then_statement, else_statement, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> 
		  SEQUENCE (stmt, 
			    IF (v, normalize_statement then_statement,
				normalize_statement else_statement, 
				cabsloc), 
			    cabsloc)
	      | None -> 
		  IF (expression, normalize_statement then_statement,
		      normalize_statement else_statement, cabsloc)
	    end
	| WHILE (expression, statement, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> 
		  SEQUENCE (stmt,
			    WHILE (v, normalize_statement statement, cabsloc),
			    cabsloc)
	      | None ->
		  WHILE (expression, normalize_statement statement, cabsloc)
	    end
	| DOWHILE (expression, statement, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> 
		  SEQUENCE (stmt,
			    DOWHILE (v, normalize_statement statement, cabsloc),
			    cabsloc)
	      | None ->
		  DOWHILE (expression, normalize_statement statement, cabsloc)
	    end
	| FOR (for_clause, expr0, expr1, stmt, cabsloc) ->
	    begin
	      let init_stmt = match for_clause with
	      | FC_EXP expr -> COMPUTATION (expr, cabsloc)
	      | FC_DECL def -> DEFINITION def
	      in
	      let while_block = {
		blabels = [];
		battrs = [];
		bstmts = [stmt;COMPUTATION (expr1, cabsloc)] 
	      }
	      in
	      let while_stmt = 
		match expr0 with
		| NOTHING -> 
		    WHILE (CONSTANT (CONST_INT "1"), BLOCK (while_block, cabsloc), cabsloc)
		| _ -> 
		    WHILE (expr0, BLOCK (while_block, cabsloc), cabsloc)
	      in
	      let for_block = {
		blabels = [];
		battrs = [];
		bstmts = [init_stmt;while_stmt];
	      }
	      in
	      BLOCK (for_block, cabsloc)
	    end
	| BREAK _ -> statement
	| CONTINUE _ -> statement
	| RETURN (expression, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) ->
		  SEQUENCE (stmt, RETURN (v, cabsloc), cabsloc)
	      | None -> statement
	    end
	| SWITCH (expression, statement, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> 
		  SEQUENCE (stmt,
			    SWITCH (v, normalize_statement statement, cabsloc),
			    cabsloc)
	      | None ->
		  SWITCH (expression, normalize_statement statement, cabsloc)
	    end
	| CASE (expression, statement, cabsloc) ->
	    begin
	      let s = normalize_expression expression cabsloc
	      in
	      match s with
	      | Some (stmt, v) -> assert false
	      | None ->
		  CASE (expression, normalize_statement statement, cabsloc)
	    end
	| CASERANGE (expr0, expr1, statement, cabsloc) ->
	    begin
	      CASERANGE (expr0, expr1, normalize_statement statement, cabsloc)
	    end
	| DEFAULT (statement, cabsloc) ->
	    begin
	      DEFAULT (normalize_statement statement, cabsloc)
	    end
	| LABEL (string, statement, cabsloc) ->
	    begin
	      LABEL (string, normalize_statement statement, cabsloc)
	    end
	| GOTO _ -> statement
	| COMPGOTO (expression, cabsloc) ->
	    begin
	      assert false
	    end 
	| DEFINITION _ -> statement
	| ASM _ -> assert false
	| TRY_EXCEPT _ -> assert false
	| TRY_FINALLY _ -> assert false
	in
	s
    and normalize_block: block -> block =
      fun block ->
	{ blabels = block.blabels;
	  battrs = block.battrs;
	  bstmts = List.map normalize_statement block.bstmts;
	}
    and normalize_definition: definition -> definition =  
      fun definition ->
	let s = match definition with
	| FUNDEF (single_name, block, cabsloc0, cabsloc1) ->
	    begin
	      FUNDEF (single_name, normalize_block block, cabsloc0, cabsloc1)
	    end
	| DECDEF (init_name_group, cabsloc) -> definition
	| TYPEDEF (name_group, cabsloc) -> definition
	| ONLYTYPEDEF (specifier, cabsloc) -> definition
	| GLOBASM (string, cabsloc) -> definition
	| PRAGMA (expression, cabsloc) -> definition
	| LINKAGE (string, cabsloc, definition_list) ->
	    definition
	| TRANSFORMER (definition, definition_list, cabsloc) ->
	    definition
	| EXPRTRANSFORMER (expr0, expr1, cabsloc) -> 
	    definition
	in
	s
    in
    let definition' = ref (normalize_definition definition)
    in
    while !changed do
      begin
	changed := false;
	definition' := normalize_definition !definition'
      end
    done;
    !definition'
	    
