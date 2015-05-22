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

include Cabs
open Ast_aa_gram
open C_syntax_symbol
open Ast_aa_gram_op


type declarator_type = 
  | Abs of c_abstract_declarator option
  | Con of c_declarator

let rec c99ize (definition_list:definition list):c_translation_unit = 
  let translation_unit = ref []
  in
  List.iter
    (fun def ->
      match c99ize_definition def with
	| Some v ->
	    translation_unit := v::!translation_unit
	| None -> ()
    ) definition_list;
  Translation_unit (List.rev !translation_unit)
    
and convert_cabsloc (cabsloc:cabsloc):Coordinate.t = 
  (cabsloc.filename, cabsloc.lineno, cabsloc.byteno)

and c99ize_expression0 (expr:expression):c_expression = 
  match c99ize_expression expr with 
    | Some v -> v 
    | None -> assert false
	
and c99ize_expression (expression:expression): c_expression option = 
  match expression with
    | PAREN e -> 
	begin
	  match c99ize_expression e with
	    | Some e -> Some (crt_c_expression_by_c_postfix_expression
		(Postfix_expression_1
		  (Primary_expression_4 e)))
	    | None -> None
	end
    | NOTHING -> None
    | VARIABLE v ->
	Some (crt_c_expression_by_c_postfix_expression 
	  (Postfix_expression_1
	    (Primary_expression_1
	      (Identifier v))))
	  
    | CONSTANT c ->
	begin
	  match c with
	    | CONST_INT (base, str) -> 
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_2
		      (Constant_integer 
			(Ast_aa_gram_op.extract_c_integer_constant 
			  base str)))))
		  
	    | CONST_FLOAT str -> 
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_2
		      (Constant_float str))))
		  
	    | CONST_CHAR int64_list -> 
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_2
		      (Constant_character int64_list))))
		  
	    | CONST_WCHAR int64_list -> 
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_2
		      (Constant_wcharacter int64_list))))
		  
	    | CONST_STRING str -> 
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_3
		      (String_literal str))))
		  
	    | CONST_WSTRING int64_list ->
		Some (crt_c_expression_by_c_postfix_expression
		  (Postfix_expression_1
		    (Primary_expression_3
		      (WString_literal int64_list))))
	end
    | UNARY (unary_operator, sub_expr) ->
	begin
	  let c_sub_expr_opt = c99ize_expression sub_expr
	  in
	  match c_sub_expr_opt with
	    | Some c_sub_expr ->
		begin
		  match unary_operator with
		    | MINUS 
		    | PLUS 
		    | NOT 
		    | BNOT 
		    | MEMOF 
		    | ADDROF ->
			begin
			  let op = match unary_operator with
			    | MINUS -> Unary_operator_MINUS
			    | PLUS -> Unary_operator_PLUS
			    | BNOT -> Unary_operator_TILDE
			    | NOT -> Unary_operator_EXCLAM
			    | ADDROF -> Unary_operator_AND
			    | MEMOF -> Unary_operator_STAR
			    | _ -> assert false
			  in
			  let c_cast_expr = get_c_cast_expression c_sub_expr
			  in
			  Some (crt_c_expression_by_c_unary_expression
			    (Unary_expression_4 (op, c_cast_expr)))
			end
		    | PREINCR ->
			let c_unary_expr = get_c_unary_expression c_sub_expr
			in
			Some (crt_c_expression_by_c_unary_expression
			  (Unary_expression_2_PLUS_PLUS c_unary_expr))
		    | PREDECR ->
			let c_unary_expr = get_c_unary_expression c_sub_expr
			in
			Some (crt_c_expression_by_c_unary_expression
			  (Unary_expression_3_MINUS_MINUS c_unary_expr))
		    | POSINCR ->
			let c_postfix_expr = get_c_postfix_expression c_sub_expr
			in
			Some (crt_c_expression_by_c_postfix_expression
			  (Postfix_expression_6_PLUS_PLUS c_postfix_expr))
		    | POSDECR ->
			let c_postfix_expr = get_c_postfix_expression c_sub_expr
			in
			Some 
			  (crt_c_expression_by_c_postfix_expression
			    (Postfix_expression_7_MINUS_MINUS c_postfix_expr))
		end
	    | None -> assert false
	end
    | BINARY (binary_operator, expr0, expr1) -> 
	begin
	  let expr0 = c99ize_expression0 expr0
	  and expr1 = c99ize_expression0 expr1
	  in
	  match binary_operator with
	    | MUL ->
		let expr0 = get_c_multiplicative_expression expr0
		and expr1 = get_c_cast_expression expr1
		in
		Some (crt_c_expression_by_c_multiplicative_expression
		  (Multiplicative_expression_2_STAR (expr0, expr1)))
	    | DIV ->
		let expr0 = get_c_multiplicative_expression expr0
		and expr1 = get_c_cast_expression expr1
		in
		Some (crt_c_expression_by_c_multiplicative_expression
		  (Multiplicative_expression_3_SLASH (expr0, expr1)))
	    | MOD ->
		let expr0 = get_c_multiplicative_expression expr0
		and expr1 = get_c_cast_expression expr1
		in
		Some (crt_c_expression_by_c_multiplicative_expression
		  (Multiplicative_expression_4_PERCENT (expr0, expr1)))
	    | ADD ->
		let expr0 = get_c_additive_expression expr0
		and expr1 = get_c_multiplicative_expression expr1
		in
		Some (crt_c_expression_by_c_additive_expression
		  (Additive_expression_2_PLUS (expr0, expr1)))
	    | SUB ->
		let expr0 = get_c_additive_expression expr0
		and expr1 = get_c_multiplicative_expression expr1
		in
		Some (crt_c_expression_by_c_additive_expression
		  (Additive_expression_3_MINUS (expr0, expr1)))
	    | SHL ->
		let expr0 = get_c_shift_expression expr0
		and expr1 = get_c_additive_expression expr1
		in
		Some (crt_c_expression_by_c_shift_expression
		  (Shift_expression_2_INF_INF (expr0, expr1)))
	    | SHR -> 
		let expr0 = get_c_shift_expression expr0
		and expr1 = get_c_additive_expression expr1
		in
		Some (crt_c_expression_by_c_shift_expression
		  (Shift_expression_3_SUP_SUP (expr0, expr1)))
	    | LT -> 
		let expr0 = get_c_relational_expression expr0
		and expr1 = get_c_shift_expression expr1
		in
		Some (crt_c_expression_by_c_relational_expression
		  (Relational_expression_2_INF (expr0, expr1)))
	    | GT -> 
		let expr0 = get_c_relational_expression expr0
		and expr1 = get_c_shift_expression expr1
		in
		Some (crt_c_expression_by_c_relational_expression
		  (Relational_expression_3_SUP (expr0, expr1)))
	    | LE -> 
		let expr0 = get_c_relational_expression expr0
		and expr1 = get_c_shift_expression expr1
		in
		Some (crt_c_expression_by_c_relational_expression
		  (Relational_expression_4_INF_EQ (expr0, expr1)))
		  
	    | GE -> 
		let expr0 = get_c_relational_expression expr0
		and expr1 = get_c_shift_expression expr1
		in
		Some (crt_c_expression_by_c_relational_expression
		  (Relational_expression_5_SUP_EQ (expr0, expr1)))
		  
	    | EQ ->
		let expr0 = get_c_equality_expression expr0
		and expr1 = get_c_relational_expression expr1
		in
		Some (crt_c_expression_by_c_equality_expression
		  (Equality_expression_2_EQ_EQ (expr0, expr1)))
	    | NE ->
		let expr0 = get_c_equality_expression expr0
		and expr1 = get_c_relational_expression expr1
		in
		Some (crt_c_expression_by_c_equality_expression
		  (Equality_expression_3_EXCLAM_EQ (expr0, expr1)))
		  
	    | BAND ->
		let expr0 = get_c_and_expression expr0
		and expr1 = get_c_equality_expression expr1
		in
		Some (crt_c_expression_by_c_and_expression
		  (And_expression_2_AND (expr0, expr1)))
	    | XOR ->
		let expr0 = get_c_exclusive_or_expression expr0
		and expr1 = get_c_and_expression expr1
		in
		Some (crt_c_expression_by_c_exclusive_or_expression
		  (Exclusive_or_expression_2_CIRC (expr0, expr1)))
	    | BOR ->
		let expr0 = get_c_inclusive_or_expression expr0
		and expr1 = get_c_exclusive_or_expression expr1
		in
		Some (crt_c_expression_by_c_inclusive_or_expression
		  (Inclusive_or_expression_2_PIPE (expr0, expr1)))
		  
	    | AND ->
		let expr0 = get_c_logical_and_expression expr0
		and expr1 = get_c_inclusive_or_expression expr1
		in
		Some (crt_c_expression_by_c_logical_and_expression
		  (Logical_and_expression_2_AND_AND (expr0, expr1)))
		  
	    | OR ->
		let expr0 = get_c_logical_or_expression expr0
		and expr1 = get_c_logical_and_expression expr1
		in
		Some (crt_c_expression_by_c_logical_or_expression
		  (Logical_or_expression_2_PIPE_PIPE (expr0, expr1)))
		  
	    | ASSIGN 
	    | ADD_ASSIGN 
	    | SUB_ASSIGN 
	    | MUL_ASSIGN 
	    | DIV_ASSIGN 
	    | MOD_ASSIGN
	    | BAND_ASSIGN 
	    | BOR_ASSIGN 
	    | XOR_ASSIGN 
	    | SHL_ASSIGN 
	    | SHR_ASSIGN ->
		begin
		  let binop = match binary_operator with
		    | ASSIGN -> Assignment_operator_EQ
		    | ADD_ASSIGN -> Assignment_operator_PLUS_EQ
		    | SUB_ASSIGN -> Assignment_operator_MINUS_EQ
		    | MUL_ASSIGN -> Assignment_operator_STAR_EQ
		    | DIV_ASSIGN -> Assignment_operator_SLASH_EQ
		    | MOD_ASSIGN -> Assignment_operator_PERCENT_EQ
		    | BAND_ASSIGN -> Assignment_operator_AND_EQ
		    | BOR_ASSIGN -> Assignment_operator_PIPE_EQ
		    | XOR_ASSIGN -> Assignment_operator_CIRC_EQ
		    | SHL_ASSIGN -> Assignment_operator_INF_INF_EQ
		    | SHR_ASSIGN -> Assignment_operator_SUP_SUP_EQ
		    | _ -> assert false
		  in
		  let expr0 = get_c_unary_expression expr0
		  and expr1 = get_c_assignment_expression expr1
		  in
		  Some (crt_c_expression_by_c_assignment_expression
		    (Assignment_expression_2 (expr0, binop, expr1))
		  )
		end
	end

    | QUESTION (expr0, expr1, expr2) -> 
	begin
	  match expr1 with
	    | NOTHING ->
		begin
		  let expr0 = c99ize_expression0 expr0
		  and expr2 = c99ize_expression0 expr2
		  in
		  Some (crt_c_expression_by_c_conditional_expression
		    (Conditional_expression_gnu
		      (get_c_logical_or_expression expr0, 
		      get_c_conditional_expression expr2)
		    )
		  )
		end
	    | _ ->
		begin
		  let expr0 = c99ize_expression0 expr0
		  and expr1 = c99ize_expression0 expr1
		  and expr2 = c99ize_expression0 expr2
		  in
		  Some (crt_c_expression_by_c_conditional_expression
		    (Conditional_expression_2 
		      (get_c_logical_or_expression expr0, 
		      expr1, 
		      get_c_conditional_expression expr2)
		    )
		  )
		end
	end 
    | CAST ((specifier, decl_type), expr) ->
	begin
	  let expr = match c99ize_init_expression expr with 
	    | Some v -> v | None -> assert false
	  in
	  match expr with
	    | Initializer_1 expr' -> 
		begin
		  Some 
		    (crt_c_expression_by_c_cast_expression
		      (Cast_expression_2 
			(c99ize_specifier_n_decl_type specifier decl_type, 
			get_c_cast_expression 
			  (crt_c_expression_by_c_assignment_expression 
			    expr'))
		      )
		    )
		end
	    | Initializer_2 l -> 
		begin
		  Some 
		    (crt_c_expression_by_c_postfix_expression
		      (Postfix_expression_8 
			(c99ize_specifier_n_decl_type specifier decl_type, 
			l)
		      )
		    )
		end
	end
    | CALL (expr0, exprs) ->
	begin
	  match expr0 with
	    | VARIABLE "__builtin_va_arg"
	    | VARIABLE "va_arg" ->
		begin
		  match exprs with
		    | [expr1;TYPE_SIZEOF (specifier, decl_type)] ->
			Some 
			  (crt_c_expression_by_c_primary_expression
			    (Primary_expression_macro_va_arg 
			      (c99ize_expression0 expr1, 
			      c99ize_specifier_n_decl_type 
				specifier decl_type)))
		    | _ -> assert false
		end
	    | VARIABLE "__builtin_va_start"
	    | VARIABLE "va_start" ->
		begin
		  match exprs with
		    | [expr1;expr2] ->
			Some (crt_c_expression_by_c_primary_expression
			  (Primary_expression_macro_va_start 
			    (c99ize_expression0 expr1, 
			    c99ize_expression0 expr2)))
		    | _ -> assert false
		end
	    | VARIABLE "__builtin_va_end"
	    | VARIABLE "va_end" ->
		begin
		  match exprs with
		    | [expr1] ->
			Some (crt_c_expression_by_c_primary_expression
			  (Primary_expression_macro_va_end
			    (c99ize_expression0 expr1)))
		    | _ -> assert false
		end

	    | VARIABLE "__builtin_types_compatible_p" ->
		begin
		  match exprs with
		    | [TYPE_SIZEOF (specifier0, decl_type0);
		      TYPE_SIZEOF (specifier1, decl_type1)] ->
			Some 
			  (crt_c_expression_by_c_primary_expression
			    (Primary_expression_builtin_types_compatible
			      (c99ize_specifier_n_decl_type 
				specifier0 decl_type0, 
			      c99ize_specifier_n_decl_type 
				specifier1 decl_type1)))
		    | _ -> assert false
		end
	    | VARIABLE "__builtin_constant_p" ->
		begin
		  match exprs with
		    | [expr] ->
			Some (crt_c_expression_by_c_primary_expression
			  (Primary_expression_builtin_constant_p
			    (c99ize_expression0 expr)))
		    | _ -> assert false
		end
	    | VARIABLE "__builtin_expect" ->
		begin
		  match exprs with
		    | [expr1;expr2] ->
			Some (crt_c_expression_by_c_primary_expression
			  (Primary_expression_builtin_expect
			    (c99ize_expression0 expr1, 
			    (Constant_expression 
			      (get_c_conditional_expression
				(c99ize_expression0 expr2))))))
		    | _ -> assert false
		end
	    | _ ->
		begin
		  let expr = c99ize_expression0 expr0 
		  and exprs = List.map 
		    (fun v -> match c99ize_expression v with 
		      | Some v -> get_c_assignment_expression v 
		      | None -> assert false ) 
		    exprs
		  in
		  Some 
		    (crt_c_expression_by_c_postfix_expression
		      (Postfix_expression_3 
			(get_c_postfix_expression expr,
			Some (Argument_expression_list exprs))))
		end
	end
    | COMMA l -> 
	begin
	  match l with
	    | [a] -> c99ize_expression a
	    | a::l' ->
		begin
		  let expr0 = c99ize_expression0 a 
		  and expr1 = c99ize_expression0 (COMMA l')
		  in
		  Some (Expression_2 (expr0, 
		  get_c_assignment_expression expr1))
		end
	    | _ -> assert false
	end
	  
    | EXPR_SIZEOF expr ->
	begin
	  let expr = match c99ize_expression expr with 
	    | Some v -> v | None -> assert false
	  in
	  Some 
	    (crt_c_expression_by_c_unary_expression
	      (Unary_expression_5_SIZEOF (get_c_unary_expression expr)))
	end
	  
    | TYPE_SIZEOF (specifier, decl_type) ->
	begin
	  Some 
	    (crt_c_expression_by_c_unary_expression
	      (Unary_expression_6_SIZEOF 
		(c99ize_specifier_n_decl_type specifier decl_type))
	    )
	end
    | EXPR_ALIGNOF expr ->
	begin
	  let expr = match c99ize_expression expr with 
	    | Some v -> v | None -> assert false
	  in
	  Some 
	    (crt_c_expression_by_c_unary_expression
	      (Unary_expression_7_ALIGNOF (get_c_unary_expression expr)))
	end
    | TYPE_ALIGNOF (specifier, decl_type) -> 
	begin
	  Some 
	    (crt_c_expression_by_c_unary_expression
	      (Unary_expression_8_ALIGNOF 
		(c99ize_specifier_n_decl_type specifier decl_type))
	    )
	end
	  
    | INDEX (expr0, expr1) -> 
	begin
	  let expr0 = match c99ize_expression expr0 with 
	    | Some v -> v | None -> assert false
	  and expr1 = match c99ize_expression expr1 with 
	    | Some v -> v | None -> assert false
	  in
	  Some 
	    (crt_c_expression_by_c_postfix_expression
	      (Postfix_expression_2 
		(get_c_postfix_expression expr0, expr1)))
	end
    | MEMBEROF (e, f) -> 
	begin
	  let expr = match c99ize_expression e with 
	    | Some v -> v | None -> assert false
	  in
	  Some 
	    (crt_c_expression_by_c_postfix_expression
	      (Postfix_expression_4_DOT 
		(get_c_postfix_expression expr, Identifier f)))
	end
    | MEMBEROFPTR (e, f) -> 
	begin
	  let expr = c99ize_expression0 e 
	  in
	  Some (crt_c_expression_by_c_postfix_expression
	    (Postfix_expression_5_ARROW 
	      (get_c_postfix_expression expr, Identifier f)))
	end
    | GNU_BODY block -> 
	Some (crt_c_expression_by_c_primary_expression
	  (Primary_expression_gnu_block
	    (c99ize_block block)))
	  
    | EXPR_PATTERN _ -> 
	camlp4_macro_exception 
	  "\nEXPR_PATTERN extension is used as primary expression\n"
	  
    | LABELADDR v -> 
	Some (crt_c_expression_by_c_postfix_expression 
	  (Postfix_expression_1
	    (Primary_expression_gnu_labeladdr
	      (Identifier v))))

and c99ize_statement (stmt:statement):c_block_item = 
  match c99ize_statement_opt stmt with
    | Some v -> v
    | None -> 
	Block_item_2 
	  (Statement_2 
	    (Compound_statement 
	      ([], Some [])))
	  
and c99ize_statement0 (stmt:statement): c_statement = 
  (Statement_2 
    (Compound_statement 
      ([], Some [c99ize_statement stmt])))
    
and c99ize_statement_opt (statement:statement):c_block_item option =
  let s = match statement with
    | NOP _ -> None
    | COMPUTATION (expression, cabsloc) ->
	Some (Block_item_2
	  (Statement_at 
	    (convert_cabsloc cabsloc,
	    (Statement_3 
	      (Expression_statement (c99ize_expression expression))))))
	  
    | BLOCK (block, cabsloc) -> 
	Some (Block_item_2 
	  (Statement_2 (c99ize_block block)))
	  
    | SEQUENCE (statement1, statement2, cabsloc) -> 
	Some (Block_item_2
	  (Statement_2 
	    (Compound_statement 
	      ([], Some [c99ize_statement statement1;
	      c99ize_statement statement2])))
	)
    | IF (expression, then_statement, else_statement, cabsloc) ->
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      (Statement_4
		(Selection_statement_2_if_else 
		  (expr,
		  c99ize_statement0 then_statement,
		  c99ize_statement0 else_statement
		  )))
	      )
	    )
	  )
    | WHILE (expression, statement, cabsloc) ->
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2
	    (Statement_at (convert_cabsloc cabsloc, 
	    Statement_5 
	      (Iteration_statement_1_while
		(expr, c99ize_statement0 statement)
	      )
	    )
	    )
	  )
	  
    | DOWHILE (expression, statement, cabsloc) ->
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2 
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      (Statement_5
		(Iteration_statement_2_do 
		  (c99ize_statement0 statement, expr)
		))
	      )
	    )
	  )
	  
    | FOR (for_clause, expr0, expr1, stmt, cabsloc) -> 
	begin
	  match for_clause with
	    | FC_EXP init_expr -> 
		let init_expr = c99ize_expression init_expr
		and expr0 = c99ize_expression expr0
		and expr1 = c99ize_expression expr1
		in
		Some 
		  (Block_item_2
		    (Statement_at (convert_cabsloc cabsloc, 
		    Statement_5 
		      (Iteration_statement_3_for
			(init_expr, expr0, expr1, 
			c99ize_statement0 stmt)
		      )
		    )
		    )
		  )
		  
	    | FC_DECL definition ->
		begin
		  let c_declaration = 
		    match c99ize_definition definition with
		      | Some external_declaration ->
			  begin
			    let rec f = function 
			      | External_declaration_at (coord, v) -> f v
			      | External_declaration_1 _ -> assert false
			      | External_declaration_2 c -> c
			    in
			    f external_declaration
			  end
		      | None -> assert false
		  and expr0 = c99ize_expression expr0
		  and expr1 = c99ize_expression expr1
		  in
		  Some 
		    (Block_item_2
		      (Statement_at (convert_cabsloc cabsloc, 
		      Statement_5 
			(Iteration_statement_4_for
			  (c_declaration, expr0, expr1, 
			  c99ize_statement0 stmt)
			)
		      )
		      )
		    )
		end
	end

    | BREAK cabsloc -> 
	Some 
	  (Block_item_2
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      Statement_6 Jump_statement_3_break
	      )
	    )
	  )
	  
    | CONTINUE cabsloc -> 
	Some 
	  (Block_item_2
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      Statement_6 
		Jump_statement_2_continue
	      )
	    )
	  )
	  
    | RETURN (expression, cabsloc) ->
	begin
	  match c99ize_expression expression with
	    | Some expression -> 
		Some 
		  (Block_item_2 
		    (Statement_at 
		      (convert_cabsloc cabsloc, 
		      Statement_6 
			(Jump_statement_4_return_expression
			  expression)
		      )
		    )
		  )
	    | None -> 
		Some 
		  (Block_item_2 
		    (Statement_at 
		      (convert_cabsloc cabsloc, 
		      Statement_6 Jump_statement_5_return 
		      )
		    )
		  )
	end
	  
    | SWITCH (expression, statement, cabsloc) ->
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2
	    (Statement_at (convert_cabsloc cabsloc, 
	    Statement_4 
	      (Selection_statement_3_switch 
		(expr,
		c99ize_statement0 statement)
	      )
	    )
	    )
	  )
	  
    | CASE (expression, statement, cabsloc) ->
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      Statement_1
		(Labeled_statement_2_case 
		  (Constant_expression 
		    (get_c_conditional_expression expr), 
		  c99ize_statement0 statement)
		)
	      )
	    )
	  )
	  
    | CASERANGE (expr0, expr1, statement, cabsloc) -> 
	let expr0 = match c99ize_expression expr0 with
	  | Some v -> v
	  | None -> assert false
	and expr1 = match c99ize_expression expr1 with
	  | Some v -> v
	  | None -> assert false
	in
	Some 
	  (Block_item_2
	    (Statement_at 
	      (convert_cabsloc cabsloc, 
	      Statement_1
		(Labeled_statement_gnu_case_range
		  (Constant_expression 
		    (get_c_conditional_expression expr0),
		  Constant_expression 
		    (get_c_conditional_expression expr1),
		  c99ize_statement0 statement)
		)
	      )
	    )
	  )
	  
    | DEFAULT (statement, cabsloc) -> 
	Some 
	  (Block_item_2
	    (Statement_at (convert_cabsloc cabsloc,
	    Statement_1
	      (Labeled_statement_3_default 
		(c99ize_statement0 statement)
	      )
	    )
	    )
	  )
	  
    | LABEL (string, statement, cabsloc) ->
	Some (Block_item_2
	  (Statement_at (convert_cabsloc cabsloc,
	  Statement_1
	    (Labeled_statement_1 
	      (Identifier string, c99ize_statement0 statement)))))
	  
    | GOTO (str, loc) -> 
	Some (Block_item_2
	  (Statement_at (convert_cabsloc loc,
	  Statement_6 
	    (Jump_statement_1_goto (Identifier str)))))
	  
    | COMPGOTO (expression, cabsloc) -> 
	let expr = match c99ize_expression expression with
	  | Some v -> v
	  | None -> assert false
	in
	Some (Block_item_2
	  (Statement_at (convert_cabsloc cabsloc,
	  Statement_6 
	    (Jump_statement_gnu_goto expr))))
	  
	  
    | DEFINITION definition -> 
	(Some (Block_item_1 (c99ize_internal_definition definition)))
	  
    | ASM (attr_list, str_list, asm_details_opt, cabsloc) -> 
	let asm_details_opt = match asm_details_opt with
	  | Some asm_details ->
	      let f (so, s, e) = 
		let e = match c99ize_expression e with
		  | Some v -> v
		  | None -> assert false
		in (so, s, e)
	      in
	      Some 
		({
		  asm_outputs = List.map f asm_details.aoutputs;
		  asm_inputs = List.map f asm_details.ainputs;
		  asm_clobbers = asm_details.aclobbers;
		})
	  | None -> None
	in
	let str_list = List.map (fun str -> String.escaped str) str_list
	in
	Some (Block_item_2
	  (Statement_at (convert_cabsloc cabsloc,
	  Statement_asm (str_list, asm_details_opt))))
	  
    | TRY_EXCEPT _ -> assert false
    | TRY_FINALLY _ -> assert false
  in
  s

and c99ize_block (block:block):c_compound_statement = 
  let stmt_list = ref []
  in
  List.iter 
    (fun v -> 
      match c99ize_statement_opt v with
	| Some v -> stmt_list := v::!stmt_list
	| None -> ()
    ) 
    block.bstmts;
  Compound_statement 
    (block.blabels, (Some (List.rev !stmt_list)))
    
and c99ize_storage (storage:storage):c_storage_class_specifier = 
  match storage with
    | AUTO -> Storage_class_specifier_AUTO
    | STATIC -> Storage_class_specifier_STATIC
    | EXTERN -> Storage_class_specifier_EXTERN
    | REGISTER -> Storage_class_specifier_REGISTER
    | NO_STORAGE -> Storage_class_specifier_AUTO
    | THREAD -> Storage_class_specifier_THREAD

and c99ize_cvspec (cvspec:cvspec): c_type_qualifier = 
  match cvspec with
    | CV_CONST -> Type_qualifier_CONST
    | CV_VOLATILE  -> Type_qualifier_VOLATILE
    | CV_RESTRICT -> Type_qualifier_RESTRICT
	
	
and c99ize_spec_elem (spec_elem:spec_elem) (opt:c_declaration_specifiers option):
    c_declaration_specifiers option = 
  match spec_elem with
    | SpecTypedef -> 
	Some 
	  (Declaration_specifiers_1 
	    (Storage_class_specifier_TYPEDEF, opt))
    | SpecCV cvspec -> 
	Some 
	  (Declaration_specifiers_3 
	    (c99ize_cvspec cvspec, opt))
	  
    | SpecAttr attribute -> 
	Some 
	  (Declaration_specifiers_GNU
	    (c99ize_attribute attribute, opt))

    | SpecStorage storage -> 
	Some (Declaration_specifiers_1 (c99ize_storage storage, opt))
    | SpecInline -> 
	Some (Declaration_specifiers_4 (Function_specifier_INLINE, opt))
    | SpecType typeSpecifier ->
	begin
	  let opt = Some 
	    (Declaration_specifiers_2 
	      (c99ize_typeSpecifier typeSpecifier, opt))
	  in
	  match typeSpecifier with
	    | Tstruct (_, _, attribute_list) 
	    | Tunion (_, _, attribute_list) 
	    | Tenum (_, _, attribute_list) -> 
		begin
		  let rec f = function
		    | [] -> opt
		    | a::l' ->
			let opt = f l'
			in
			Some (Declaration_specifiers_GNU 
			  (a, opt))
		  in 
		  f (List.map c99ize_attribute attribute_list)
		end
	    | _ -> 
		opt
	end
    | SpecPattern string -> 
	assert false

and c99ize_spec_elem0: spec_elem -> c_specifier_qualifier_list option -> 
    c_specifier_qualifier_list option = 
  fun spec_elem opt -> 
    match spec_elem with
      | SpecTypedef -> assert false
      | SpecCV cvspec -> 
	  Some (Specifier_qualifier_list_2 (c99ize_cvspec cvspec, opt))
      | SpecAttr attribute -> 
	  Some 
	    (Specifier_qualifier_list_GNU  
	      (c99ize_attribute attribute, opt))
      | SpecStorage storage -> assert false
      | SpecInline -> assert false
      | SpecType typeSpecifier ->
	  begin
	    let opt = Some 
	      (Specifier_qualifier_list_1 
		(c99ize_typeSpecifier typeSpecifier, opt))
	    in
	    match typeSpecifier with
	      | Tstruct (_, _, attribute_list) 
	      | Tunion (_, _, attribute_list) 
	      | Tenum (_, _, attribute_list) -> 
		  let rec f = function
		    | a::l' ->
			let opt = f l'
			in
			Some (Specifier_qualifier_list_GNU (a, opt))
		    | [] -> opt
		  in
		  f (List.map c99ize_attribute attribute_list)
	      | _ -> opt
	  end
      | SpecPattern string -> 
	  assert false
	    
and c99ize_specifier: specifier -> c_declaration_specifiers option =
  fun specifier ->
    match specifier with
      | [a] -> 
	  c99ize_spec_elem a None
      | a::specifier' ->
	  let v = c99ize_specifier specifier'
	  in
	  c99ize_spec_elem a v
      | _ -> assert false


and c99ize_specifier0: specifier -> c_specifier_qualifier_list option = 
  fun specifier ->
    match specifier with
      | [a] -> c99ize_spec_elem0 a None
      | a::specifier' ->
	  let v = c99ize_specifier0 specifier'
	  in
	  c99ize_spec_elem0 a v
      | _ -> assert false

and c99ize_typeSpecifier: typeSpecifier -> c_type_specifier =
  fun t ->
    match t with
      | Tvoid -> Type_builtin Type_specifier_VOID
      | Tchar -> Type_builtin Type_specifier_CHAR
      | Tbool -> Type_builtin Type_specifier_BOOL
      | Tcomplex -> Type_builtin Type_specifier_COMPLEX
      | Tshort -> Type_builtin Type_specifier_SHORT
      | Tint -> Type_builtin Type_specifier_INT
      | Tlong -> Type_builtin Type_specifier_LONG
      | Tint64 -> assert false
      | Tfloat -> Type_builtin Type_specifier_FLOAT
      | Tdouble -> Type_builtin Type_specifier_DOUBLE
      | Tsigned -> Type_builtin Type_specifier_SIGNED
      | Tunsigned -> Type_builtin Type_specifier_UNSIGNED
      | Tnamed name -> 
	  Type_specifier_TYPENAME 
	    (Typedef_name (Identifier_as_typ name))
      | Tstruct (name, field_group_list_opt, attribute_list) ->
	  begin
	    Type_specifier_STRUCT_OR_UNION 
	      (c99ize_struct_or_union 
		(Struct_or_union_STRUCT, name, field_group_list_opt))
	  end
	    
      | Tunion (name, field_group_list_opt, attribute_list) ->
	  begin
	    Type_specifier_STRUCT_OR_UNION 
	      (c99ize_struct_or_union
		(Struct_or_union_UNION, name, field_group_list_opt))
	  end
	    
      | Tenum (name, enum_item_list_opt, attribute_list) -> 
	  begin
	    Type_specifier_ENUM 
	      (c99ize_enum_specifier (name, enum_item_list_opt))
	  end
	    
      | TtypeofE expression -> 
	  Type_specifier_GCC_TYPEOF_E 
	    (c99ize_expression0 expression)
      | TtypeofT (specifier, decl_type) -> 
	  Type_specifier_GCC_TYPEOF_T
	    (c99ize_specifier_n_decl_type specifier decl_type)

and c99ize_struct_or_union: 
    c_struct_or_union * string * field_group list option -> 
    c_struct_or_union_specifier =
  fun (struct_or_union, str, field_group_list_opt) ->
    match field_group_list_opt with
      | Some l ->
	  begin
	    let id_opt = match str with
	      | "" -> None
	      | _ -> Some (Identifier str)
	    in
	    Struct_or_union_specifier_1 
	      (struct_or_union, id_opt,
	      List.map c99ize_field_group l)
	  end
      | None ->
	  if str <> "" then
	    Struct_or_union_specifier_2 (struct_or_union, Identifier str)
	  else
	    camlp4_macro_exception "struct/union type_name is null"

and c99ize_enum_specifier: string * enum_item list option -> 
    c_enum_specifier =
  fun (str, enum_list_opt) ->
    match enum_list_opt with
      | Some l -> 
          begin
	    let id_opt = match str with
	      | "" -> None
	      | _ -> Some (Identifier str)
	    in
	    Enum_specifier_1 
	      (id_opt, List.map c99ize_enum_item l)
          end
      | None ->
          if str <> "" then
	    Enum_specifier_2 (Identifier str)
          else
	    camlp4_macro_exception "enum has null name"

and c99ize_decl_type_as_c_declarator: decl_type -> c_direct_declarator -> 
    c_declarator =
  fun decl_type name ->
    match decl_type with
      | JUSTBASE -> 
	  Declarator (None, name)
	    
      | PARENTYPE (attr1, decl_type, attr2) ->
	  Declarator 
	    (None, Direct_declarator_2
	      (c99ize_decl_type_as_c_declarator decl_type name))

      | ARRAY (decl_type, attrs, expr) ->
	  let direct_declarator = 
	    Direct_declarator_2 
	      (c99ize_decl_type_as_c_declarator decl_type name)
	  and expr' = match c99ize_expression expr with
	    | Some v -> Some (get_c_assignment_expression v)
	    | None -> None
	  in
	  Declarator 
	    (None, Direct_declarator_3 
	      (direct_declarator, None, expr'))

      | PTR (attrs, decl_type) ->
	  let direct_declarator = 
	    Direct_declarator_2 
	      (c99ize_decl_type_as_c_declarator decl_type name)
	  in
	  Declarator (Some (Pointer_1 None), direct_declarator)
	    
      | PROTO (decl_type, single_name_list, variadic) ->
	  let direct_declarator = 
	    Direct_declarator_2 
	      (c99ize_decl_type_as_c_declarator decl_type name)
	  in
	  let parameter_list = 
	    Parameter_list 
	      (List.map 
		(fun v -> 
		  let (a, b) =  c99ize_single_name_as_formal_param v in 
		  match b with
		    | Con b ->
			Parameter_declaration_1 (a, b)
		    | Abs b ->
			Parameter_declaration_2 (a, b)
		)
		single_name_list)
	  in
	  let parameter_type_list = 
	    if variadic then
	      Parameter_type_list_VAR parameter_list
	    else
	      Parameter_type_list_FIX parameter_list
	  in
	  Declarator 
	    (None, Direct_declarator_7 
	      (direct_declarator, parameter_type_list))


and c99ize_decl_type_as_c_declarator_4_formal_param: decl_type -> 
    c_direct_declarator -> c_declarator =
  fun expr name ->
    match expr with
      | JUSTBASE -> c99ize_decl_type_as_c_declarator expr name
      | PARENTYPE (attr1, decl_type, attr2) ->
	  begin
	    match decl_type with
	      | PTR (attr, decl_type) ->
		  let name = Direct_declarator_2 
		    (Declarator (Some (Pointer_1 None), name))
		  in
		  c99ize_decl_type_as_c_declarator decl_type name
	      | _ -> 
		  c99ize_decl_type_as_c_declarator decl_type name
	  end
      | ARRAY _ -> c99ize_decl_type_as_c_declarator expr name
      | PTR _ -> c99ize_decl_type_as_c_declarator expr name
      | PROTO _ -> c99ize_decl_type_as_c_declarator expr name

and c99ize_specifier_n_decl_type: specifier -> decl_type -> c_type_name = 
  fun specifier decl_type ->
    begin
      match c99ize_specifier0 specifier with
	| Some specifier_qualifier_list -> 
	    Type_name 
	      (specifier_qualifier_list, 
	      c99ize_decl_type_as_abstract_declarator decl_type None)
	| None -> assert false
    end

and c99ize_decl_type_as_abstract_declarator: decl_type -> 
    c_direct_abstract_declarator option -> c_abstract_declarator option =
  fun decl_type abs_name_opt -> 
    match decl_type with
      | JUSTBASE -> 
	  begin
	    match abs_name_opt with
	      | Some abs_name -> 
		  Some (Abstract_declarator_2 (None, abs_name))
	      | None -> 
		  None
	  end
      | PARENTYPE (attr1, decl_type, attr2) ->
	  begin
	    let abstract_declarator_opt =
	      c99ize_decl_type_as_abstract_declarator decl_type abs_name_opt
	    in
	    match abstract_declarator_opt with
	      | Some abstract_declarator -> 
		  Some 
		    (Abstract_declarator_2 
		      (None, Direct_abstract_declarator_1 
			abstract_declarator))
	      | None ->
		  None
	  end

      | ARRAY (decl_type, attrs, expr) ->
	  begin
	    let expr' = match c99ize_expression expr with
	      | Some v -> Some (get_c_assignment_expression v)
	      | None -> None
	    in
	    let v = 
	      c99ize_decl_type_as_abstract_declarator decl_type abs_name_opt
	    in
	    match v with
	      | Some abstract_declarator -> 
		  begin
		    Some 
		      (Abstract_declarator_2 
			(None, 
			Direct_abstract_declarator_2 
			  ((Some (Direct_abstract_declarator_1 
			    abstract_declarator)), 
			  expr')
			)
		      )
		  end
	      | None -> 
		  begin
		    Some 
		      (Abstract_declarator_2 
			(None, Direct_abstract_declarator_2 (None, expr'))
		      )
		  end
	  end
      | PTR (attrs, decl_type) ->
	  begin
	    let v = 
	      c99ize_decl_type_as_abstract_declarator decl_type abs_name_opt
	    in
	    match v with 
	      | Some abstract_declarator -> 
		  begin
		    Some 
		      (Abstract_declarator_2 
			(Some (Pointer_1 None), 
			(Direct_abstract_declarator_1 abstract_declarator)
			)
		      )
		  end
	      | None -> 
		  begin
		    Some 
		      (Abstract_declarator_1 
			(Pointer_1 None)
		      )
		  end
	  end
      | PROTO (decl_type, single_name_list, variadic) ->
	  begin
	    let parameter_list = 
	      Parameter_list 
		(List.map 
		  (fun v -> 
		    let (a, b) =  c99ize_single_name_as_formal_param v in 
		    match b with
		      | Con b ->
			  Parameter_declaration_1 (a, b)
		      | Abs b ->
			  Parameter_declaration_2 (a, b)
		  )
		  single_name_list)
	    in
	    let parameter_type_list = 
	      if variadic then
		Parameter_type_list_VAR parameter_list
	      else
		Parameter_type_list_FIX parameter_list
	    in
	    let v = 
	      c99ize_decl_type_as_abstract_declarator decl_type abs_name_opt
	    in
	    match v with 
	      | Some abstract_declarator -> 
		  begin
		    Some 
		      (Abstract_declarator_2 
			(None, 
			Direct_abstract_declarator_4
			  ((Some (Direct_abstract_declarator_1 
			    abstract_declarator)), 
			  Some parameter_type_list)
			)
		      )
		  end
	      | None ->  
		  begin
		    Some 
		      (Abstract_declarator_2 
			(None, Direct_abstract_declarator_4 
			  (None, Some parameter_type_list))
		      )
		  end
	  end

and c99ize_init_expression: init_expression -> c_initializer option = 
  fun init_expression -> 
    match init_expression with
      | NO_INIT -> None
      | SINGLE_INIT expr -> 
	  begin
	    match c99ize_expression expr with
	      | Some v ->
		  Some (Initializer_1 (get_c_assignment_expression v))
	      | None -> assert false
	  end
      | COMPOUND_INIT lst ->
	  begin
	    let c_init_lst = ref []
	    in
	    List.iter
	      (fun (initwhat, init_expression) ->
		let designation_opt = 
		  match c99ize_initwhat initwhat with
		    | [] -> None
		    | _ -> 
			Some (Designation (c99ize_initwhat initwhat))
		in
		match c99ize_init_expression init_expression with
		  | Some c_initializer -> 
		      c_init_lst := 
			(designation_opt, c_initializer)::!c_init_lst
		  | None -> ()
	      ) lst;
	    Some (Initializer_2 (Initializer_list (List.rev !c_init_lst)))
	  end

and c99ize_initwhat: initwhat -> c_designator list =
  fun initwhat ->
    match initwhat with
      | NEXT_INIT -> []
      | INFIELD_INIT (s, initwhat') -> 
	  let designator_list = c99ize_initwhat initwhat'
	  in
	  ((Designator_2 (Identifier s))::designator_list)
      | ATINDEX_INIT (e, initwhat') -> 
	  let designator_list = c99ize_initwhat initwhat'
	  in
	  let expr' = match c99ize_expression e with
	    | Some v -> v
	    | None -> assert false
	  in
	  (Designator_1 
	    (Constant_expression 
	      (get_c_conditional_expression expr')))::designator_list
	    
      | ATINDEXRANGE_INIT (e0, e1) -> 
	  let e0' = match c99ize_expression e0 with
	    | Some v -> v
	    | None -> assert false
	  and e1' = match c99ize_expression e1 with
	    | Some v -> v
	    | None -> assert false
	  in
	  [Designator_gnu_range
	    (Constant_expression 
	      (get_c_conditional_expression e0'), 
	    Constant_expression 
	      (get_c_conditional_expression e1'))]
	    
and c99ize_name_group: specifier * name list -> c_declaration = 
  fun (specifier, name_list) ->
    let declaration_specifiers = match c99ize_specifier specifier with
      | Some v -> v
      | None -> assert false
    in
    Declaration 
      (declaration_specifiers, 
      Some 
	(List.map 
	  (fun v -> 
	    Init_declarator_1 (c99ize_name_as_c_declarator v)) 
	  name_list))

and c99ize_field_group: field_group -> c_struct_declaration = 
  fun field_group ->
    let (specifier, struct_declarator_list) = field_group
    in
    match c99ize_specifier0 specifier with
      | Some specifier_qualifier_list ->
	  begin
	    let l = List.map 
	      (fun (name, expr_opt) ->
		match expr_opt with
		  | Some v ->
		      begin
			let declarator_opt = 
			  c99ize_name_as_c_declarator_opt name
			in
			let expr' = match c99ize_expression v with
			  | Some v -> v
			  | None -> assert false
			in			    
			let struct_declarator = 
			  Struct_declarator_2 
			    (declarator_opt, 
			    Constant_expression 
			      (get_c_conditional_expression (expr')))
			in
			let (id, decl_type, attributes, cabsloc) = name
			in
			if (attributes = []) then
			  struct_declarator
			else
			  Struct_declarator_GNU
			    (struct_declarator, List.map c99ize_attribute attributes)
		      end
		  | None -> 
		      let declarator = c99ize_name_as_c_declarator name
		      in
		      Struct_declarator_1 declarator
	      ) struct_declarator_list
	    in
	    Struct_declaration 
	      (specifier_qualifier_list, l)
	  end
      | None -> assert false
	  
and c99ize_init_name_group: specifier * init_name list -> c_declaration = 
  fun (specifier, init_name_list) ->
    let declaration_specifiers = match c99ize_specifier specifier with
      | Some v -> v
      | None -> assert false
    in
    Declaration (declaration_specifiers, 
    Some (List.map c99ize_init_name init_name_list))

and c99ize_attribute (str, expression_list) =
  (str, 
  List.flatten 
    (List.map (fun v -> match c99ize_expression v with
      | Some v -> [v]
      | None -> [])
      expression_list))
    
and c99ize_name_as_c_declarator: name -> c_declarator =
  fun name ->
    let (id, decl_type, attributes, cabsloc) = name
    in
    let ret = match id with
      | "" -> 
	  c99ize_decl_type_as_c_declarator 
	    decl_type 
	    (Direct_declarator_1 
	      (Identifier (Random_prefix.get "___missing_field_name")))
      | "___missing_field_name" -> 
	  c99ize_decl_type_as_c_declarator 
	    decl_type 
	    (Direct_declarator_1 
	      (Identifier (Random_prefix.get "___missing_field_name")))
      | _ -> 
	  c99ize_decl_type_as_c_declarator 
	    decl_type (Direct_declarator_1 (Identifier id))
    in
    if (attributes <> []) then
      Declarator_GNU (ret, List.map c99ize_attribute attributes)
    else
      ret

and c99ize_name_as_c_declarator_opt: name -> c_declarator option =
  fun name ->
    let (id, decl_type, attributes, cabsloc) = name
    in
    match id with
      | "" -> None
      | "___missing_field_name" -> None
      | _ -> 
	  Some (c99ize_decl_type_as_c_declarator 
	    decl_type (Direct_declarator_1 (Identifier id)))
	    
and c99ize_name_as_formal_param: name -> declarator_type =
  fun name ->
    let (id, decl_type, attributes, cabsloc) = name
    in
    match id with
      | "" ->
          Abs (c99ize_decl_type_as_abstract_declarator decl_type None)
      | _ -> 
	  Con (c99ize_decl_type_as_c_declarator_4_formal_param
	    decl_type 
            ((Direct_declarator_1 (Identifier id))))
	    
and c99ize_init_name: init_name -> c_init_declarator = 
  fun init_name ->
    let (v, init_expression) = init_name
    in
    let declarator = c99ize_name_as_c_declarator v
    and initializer_opt = c99ize_init_expression init_expression
    in
    match initializer_opt with
      | Some initializer0 -> 
	  Init_declarator_2 (declarator, initializer0)
      | None ->
	  Init_declarator_1 (declarator)
	    
	    
and c99ize_single_name: single_name -> 
    (c_declaration_specifiers * c_declarator) = 
  fun single_name -> 
    let (specifier, name) = single_name
    in
    let c_declaration_specifiers_opt = c99ize_specifier specifier
    in
    match c_declaration_specifiers_opt with
      | Some c_declaration_specifiers -> 
	  (c_declaration_specifiers, c99ize_name_as_c_declarator name)
      | None -> assert false


and c99ize_single_name_as_formal_param: single_name -> 
    (c_declaration_specifiers * declarator_type) 
    = 
  fun single_name -> 
    let (specifier, name) = single_name
    in
    let c_declaration_specifiers_opt = c99ize_specifier specifier
    in
    match c_declaration_specifiers_opt with
      | Some c_declaration_specifiers -> 
	  (c_declaration_specifiers, c99ize_name_as_formal_param name)
      | None -> assert false
	  

and c99ize_enum_item: enum_item -> c_enumerator =
  fun enum_item ->
    let (id, expr, cabsloc) = enum_item
    in
    match c99ize_expression expr with
      | Some v ->
	  Enumerator_2 
	    (Enumeration_constant (Identifier id), 
	    Constant_expression
	      (get_c_conditional_expression v))
      | None -> 
	  Enumerator_1 (Enumeration_constant (Identifier id))


	    
and c99ize_definition: definition -> c_external_declaration option = 
  fun definition ->
    match definition with
      | FUNDEF (single_name, block, cabsloc0, cabsloc1) ->
	  let (a0, a1) = c99ize_single_name single_name
	  in
	  Some 
	    (External_declaration_at 
	      (convert_cabsloc cabsloc0,
	      External_declaration_1 
		(Function_definition 
		  (a0, a1, None, c99ize_block block)
		)
	      )
	    )
	    
      | DECDEF (init_name_group, cabsloc) -> 
	  Some 
	    (External_declaration_at 
	      (convert_cabsloc cabsloc,
	      External_declaration_2 
		(c99ize_init_name_group init_name_group
		))
	    )
	    
      | TYPEDEF (name_group, cabsloc) -> 
	  Some 
	    (External_declaration_at
	      (convert_cabsloc cabsloc,
	      External_declaration_2 
		(c99ize_name_group name_group
		))
	    )
	    
      | ONLYTYPEDEF (specifier, cabsloc) -> 
	  begin
	    match c99ize_specifier specifier with
	      | Some v -> 
		  Some (External_declaration_at 
		    (convert_cabsloc cabsloc,
		    External_declaration_2 
		      (Declaration (v, None))
		    )
		  )
	      | None -> assert false
	  end
      | GLOBASM (string, cabsloc) -> 
	  assert false
      | PRAGMA (expression, cabsloc) -> 
	  None
      | LINKAGE (string, cabsloc, definition_list) -> 
	  assert false
      | TRANSFORMER (definition, definition_list, cabsloc) -> 
	  assert false
      | EXPRTRANSFORMER (expr0, expr1, cabsloc) -> 
	  assert false

and c99ize_internal_definition: definition -> c_declaration = 
  fun definition ->
    match definition with
      | FUNDEF (single_name, block, cabsloc0, cabsloc1) ->
	  assert false
      | DECDEF (init_name_group, cabsloc) -> 
	  c99ize_init_name_group init_name_group
      | TYPEDEF (name_group, cabsloc) -> 
	  c99ize_name_group name_group
      | ONLYTYPEDEF (specifier, cabsloc) -> 
	  begin
	    match c99ize_specifier specifier with
	      | Some v -> Declaration (v, None)
	      | None -> assert false
	  end
      | GLOBASM (string, cabsloc) -> 
	  assert false
      | PRAGMA (expression, cabsloc) -> 
	  assert false 
      | LINKAGE (string, cabsloc, definition_list) -> 
	  assert false
      | TRANSFORMER (definition, definition_list, cabsloc) -> 
	  assert false
      | EXPRTRANSFORMER (expr0, expr1, cabsloc) -> 
	  assert false
	  
    

      
