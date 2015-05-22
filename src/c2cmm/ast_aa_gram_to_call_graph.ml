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

module I = Ast_aa_gram
module O = Call_graph
open Mapping

let user_opts = []

let scan_opt f = function
  | Some v -> f v
  | None -> []


let scan_opt: ('a -> 'b list) -> 'a option -> 'b list=
  fun f a_opt ->
    match a_opt with
    | Some a -> f a
    | None -> []

let scan: I.c_translation_unit -> (string, O.node) Hashtbl.t =
  fun c_translation_unit ->
    let hashtbl:(string, (string * int) list) Hashtbl.t = Hashtbl.create 10
    and call_argc_hashtbl = Hashtbl.create 10
    and cnt = ref 0
    in
    let get_next () = 
      let v = !cnt
      in
      let _ = incr cnt
      in
      v
    in
    let rec scan_c_identifier = function
      | I.Identifier s -> s

    and scan_c_primary_expression = function
      | I.Primary_expression_1 c_identifier ->
	  [scan_c_identifier c_identifier]
      | _ -> []
	    
    and scan_c_postfix_expression: I.c_postfix_expression -> (string * int option) list = 
      fun f ->
	match f with
	| I.Postfix_expression_1 c_primary_expression -> ([]:(string * int option)list)
	      
	| I.Postfix_expression_2 (c_postfix_expression, c_expression) ->
	    ((scan_c_postfix_expression c_postfix_expression) 
	     @ (scan_c_expression c_expression):(string * int option) list)
	      
	      (* the only function call expression *)
	| I.Postfix_expression_3 (c_postfix_expression, 
				  c_argument_expression_list_opt) 
	  ->
	    begin
	      let lst = match c_postfix_expression with
	      | I.Postfix_expression_1 c_primary_expression ->
		  List.map (fun v -> (v, None)) (scan_c_primary_expression c_primary_expression)
	      | _ -> scan_c_postfix_expression c_postfix_expression
	      in
	      let args = (scan_opt scan_c_argument_expression_list
			    c_argument_expression_list_opt) 
	      in
	      let argc = match c_argument_expression_list_opt with
	      | None -> 0
	      | Some (I.Argument_expression_list c_argument_expression_list) ->
		  List.length c_argument_expression_list
	      in
	      let fun_lst = 
		if lst = [] then (* function pointer call *)
		  [("$" ^ (string_of_int (get_next ())))]
		else
		  List.map (fun (v, opt) -> assert (opt = None); v) lst
	      in
	      (List.map 
		 (fun v -> 
		   let _ = Hashtbl.add call_argc_hashtbl v argc
		   in
		   (v, Some argc)) fun_lst
	      ) @ args
	    end
	      
	| I.Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	    (scan_c_postfix_expression c_postfix_expression)
	      
	| I.Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	    (scan_c_postfix_expression c_postfix_expression)
	      
	| I.Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	    (scan_c_postfix_expression c_postfix_expression)
	      
	| I.Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	    (scan_c_postfix_expression c_postfix_expression)

	| I.Postfix_expression_8 (c_type_name, c_initializer_list) ->
	    []
	      
    and scan_c_argument_expression_list = function
      | I.Argument_expression_list l ->
	  let lst_lst = ((List.map
			    scan_c_assignment_expression
			 ) l)
	  in
	  List.flatten lst_lst
	
    and scan_c_unary_expression = function
      | I.Unary_expression_1 c_postfix_expression ->
	  (scan_c_postfix_expression c_postfix_expression)
	    
      | I.Unary_expression_2_PLUS_PLUS c_unary_expression ->
	  (scan_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_3_MINUS_MINUS c_unary_expression ->
	  (scan_c_unary_expression c_unary_expression)
	    
      | I.Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	  (scan_c_cast_expression c_cast_expression)
	    
      | I.Unary_expression_5_SIZEOF c_unary_expression -> []
	    
      | I.Unary_expression_6_SIZEOF c_type_name -> []
	    
      | I.Unary_expression_7_ALIGNOF c_unary_expression -> []
	    
      | I.Unary_expression_8_ALIGNOF c_type_name -> []
	    
    and scan_c_cast_expression = function
      | I.Cast_expression_1 c_unary_expression ->
	  (scan_c_unary_expression c_unary_expression)
	    
      | I.Cast_expression_2 (c_type_name, c_cast_expression) ->
	  scan_c_cast_expression c_cast_expression
	    
    and scan_c_multiplicative_expression = function
      | I.Multiplicative_expression_1 c_cast_expression ->
	  (scan_c_cast_expression c_cast_expression)

      | I.Multiplicative_expression_2_STAR 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (scan_c_multiplicative_expression c_multiplicative_expression) @
	  (scan_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_3_SLASH 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (scan_c_multiplicative_expression c_multiplicative_expression) @
	  (scan_c_cast_expression c_cast_expression)
	    
      | I.Multiplicative_expression_4_PERCENT 
	  (c_multiplicative_expression, c_cast_expression) 
	->
	  (scan_c_multiplicative_expression c_multiplicative_expression) @
	  (scan_c_cast_expression c_cast_expression)
	    
    and scan_c_additive_expression = function
      | I.Additive_expression_1 c_multiplicative_expression ->
	  (scan_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_2_PLUS (c_additive_expression, 
				      c_multiplicative_expression) 
	->
	  (scan_c_additive_expression c_additive_expression) @
	  (scan_c_multiplicative_expression c_multiplicative_expression)
	    
      | I.Additive_expression_3_MINUS (c_additive_expression, 
				       c_multiplicative_expression) 
	->
	  (scan_c_additive_expression c_additive_expression) @
	  (scan_c_multiplicative_expression c_multiplicative_expression)
	    
    and scan_c_shift_expression = function
      | I.Shift_expression_1 c_additive_expression ->
	  (scan_c_additive_expression c_additive_expression)

      | I.Shift_expression_2_INF_INF (c_shift_expression, 
				      c_additive_expression) 
	->
	  (scan_c_shift_expression c_shift_expression) @
	  (scan_c_additive_expression c_additive_expression)
	    
      | I.Shift_expression_3_SUP_SUP (c_shift_expression, 
				      c_additive_expression) 
	->
	  (scan_c_shift_expression c_shift_expression) @
	  (scan_c_additive_expression c_additive_expression)
	    
    and scan_c_relational_expression = function
      | I.Relational_expression_1 c_shift_expression ->
	  (scan_c_shift_expression c_shift_expression)

      | I.Relational_expression_2_INF (c_relational_expression, 
				       c_shift_expression) 
	->
	  (scan_c_relational_expression c_relational_expression) @
	  (scan_c_shift_expression c_shift_expression)

      | I.Relational_expression_3_SUP (c_relational_expression, 
				       c_shift_expression) 
	->
	  (scan_c_relational_expression c_relational_expression)@
	  (scan_c_shift_expression c_shift_expression)

      | I.Relational_expression_4_INF_EQ (c_relational_expression, 
					  c_shift_expression) 
	->
	  (scan_c_relational_expression c_relational_expression) @
	  (scan_c_shift_expression c_shift_expression)
	    
      | I.Relational_expression_5_SUP_EQ (c_relational_expression, 
					  c_shift_expression) 
	->
	  (scan_c_relational_expression c_relational_expression) @
	  (scan_c_shift_expression c_shift_expression)

    and scan_c_equality_expression = function
      | I.Equality_expression_1 c_relational_expression ->
	  (scan_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_2_EQ_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  (scan_c_equality_expression c_equality_expression) @
	  (scan_c_relational_expression c_relational_expression)
	    
      | I.Equality_expression_3_EXCLAM_EQ 
	  (c_equality_expression, c_relational_expression) 
	->
	  (scan_c_equality_expression c_equality_expression) @
	  (scan_c_relational_expression c_relational_expression)
	    
    and scan_c_and_expression = function
      | I.And_expression_1 c_equality_expression ->
	  (scan_c_equality_expression c_equality_expression)
	    
      | I.And_expression_2_AND (c_and_expression, c_equality_expression) ->
	  (scan_c_and_expression c_and_expression) @
	  (scan_c_equality_expression c_equality_expression)
	    
    and scan_c_exclusive_or_expression = function
      | I.Exclusive_or_expression_1 c_and_expression ->
	  (scan_c_and_expression c_and_expression)

      | I.Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, 
					  c_and_expression) 
	->
	  (scan_c_exclusive_or_expression c_exclusive_or_expression) @
	  (scan_c_and_expression c_and_expression)
	    
    and scan_c_inclusive_or_expression = function
      | I.Inclusive_or_expression_1 c_exclusive_or_expression ->
	  (scan_c_exclusive_or_expression c_exclusive_or_expression)

      | I.Inclusive_or_expression_2_PIPE 
	  (c_inclusive_or_expression, c_exclusive_or_expression) 
	->
	  (scan_c_inclusive_or_expression c_inclusive_or_expression)@
	  (scan_c_exclusive_or_expression c_exclusive_or_expression)
	    
    and scan_c_logical_and_expression = function
      | I.Logical_and_expression_1 c_inclusive_or_expression ->
	  (scan_c_inclusive_or_expression c_inclusive_or_expression)
	    
      | I.Logical_and_expression_2_AND_AND 
	  (c_logical_and_expression, c_inclusive_or_expression) 
	->
	  (scan_c_logical_and_expression c_logical_and_expression) @
	  (scan_c_inclusive_or_expression c_inclusive_or_expression)
	    
    and scan_c_logical_or_expression = function
      | I.Logical_or_expression_1 c_logical_and_expression ->
	  (scan_c_logical_and_expression c_logical_and_expression)
	    
      | I.Logical_or_expression_2_PIPE_PIPE 
	  (c_logical_or_expression, c_logical_and_expression) 
	->
	  (scan_c_logical_or_expression c_logical_or_expression) @
	  (scan_c_logical_and_expression c_logical_and_expression)
	    
    and scan_c_conditional_expression = function
      | I.Conditional_expression_1 c_logical_or_expression ->
	  (scan_c_logical_or_expression c_logical_or_expression)

      | I.Conditional_expression_2 
	  (c_logical_or_expression, c_expression, c_conditional_expression) 
	->
	  (scan_c_logical_or_expression c_logical_or_expression) @
	  (scan_c_expression c_expression) @
	  (scan_c_conditional_expression c_conditional_expression)

      | I.Conditional_expression_gnu
	  (c_logical_or_expression, c_conditional_expression) 
	->
	  (scan_c_logical_or_expression c_logical_or_expression) @
	  (scan_c_conditional_expression c_conditional_expression)

    and scan_c_assignment_expression = function
      | I.Assignment_expression_1 c_conditional_expression ->
	  (scan_c_conditional_expression c_conditional_expression)
	    
      | I.Assignment_expression_2 
	  (c_unary_expression, c_assignment_operator, 
	   c_assignment_expression) 
	->
	  (scan_c_unary_expression c_unary_expression) @
	  (scan_c_assignment_expression c_assignment_expression)

    and scan_c_expression = function
      | I.Expression_1 c_assignment_expression -> 
	  scan_c_assignment_expression c_assignment_expression
	    
      | I.Expression_2 (c_expression, c_assignment_expression) ->
	  (scan_c_expression c_expression) @
	  (scan_c_assignment_expression c_assignment_expression)

(*
    and scan_c_constant_expression = function
      | I.Constant_expression c_conditional_expression ->
	  (scan_c_conditional_expression c_conditional_expression)
*)

    and scan_c_declaration = function
      | I.Declaration (c_declaration_specifiers,  
		       c_init_declarator_list_opt) 
	->
	  begin
	    match c_init_declarator_list_opt with
	    | Some lst -> 
		let lst_lst = List.map scan_c_init_declarator lst
		in
		List.flatten lst_lst
	    | None -> []
	  end
	    
    and scan_c_init_declarator = function
      | I.Init_declarator_1 c_declarator -> []
	    
      | I.Init_declarator_2 (c_declarator, c_initializer) ->
	  scan_c_initializer c_initializer
	    
    and scan_c_initializer = function
      | I.Initializer_1 c_assignment_expression ->
	  (scan_c_assignment_expression c_assignment_expression)

      | I.Initializer_2 c_initializer_list  ->
	  scan_c_initializer_list c_initializer_list
	    
    and scan_c_initializer_list = function
      | I.Initializer_list l ->
	  let lst_lst = 
	    (List.map
	       (fun (c_designation_opt, c_initializer) -> 
		 scan_c_initializer c_initializer
	       ) 
	       l)
	  in
	  List.flatten lst_lst
	    
    and scan_c_statement = function
      | I.Statement_at (coord, stmt) ->
	  scan_c_statement stmt
      | I.Statement_1 (c_labeled_statement) ->
	  scan_c_labeled_statement c_labeled_statement
      | I.Statement_2 (c_compound_statement) ->
	  scan_c_compound_statement c_compound_statement
      | I.Statement_3 (c_expression_statement)  ->
	  scan_c_expression_statement c_expression_statement
      | I.Statement_4 (c_selection_statement) -> 
	  scan_c_selection_statement c_selection_statement 
      | I.Statement_5 (c_iteration_statement) ->
	  scan_c_iteration_statement c_iteration_statement
      | I.Statement_6 (c_jump_statement) -> 
	  scan_c_jump_statement c_jump_statement
      | I.Statement_asm _ -> []

    and scan_c_labeled_statement = function
      | I.Labeled_statement_1 ((I.Identifier str), c_statement) ->
	  scan_c_statement c_statement
	    
      | I.Labeled_statement_2_case 
	  (c_constant_expression, c_statement) ->
	    scan_c_statement c_statement
	      
      | I.Labeled_statement_3_default c_statement ->
	  scan_c_statement c_statement

      | I.Labeled_statement_gnu_case_range
	  (e0, e1, c_statement) ->
	    scan_c_statement c_statement
	    
    and scan_c_compound_statement = function
      | I.Compound_statement (labels, c_block_item_list_opt) ->
	  begin
	    match c_block_item_list_opt with
	    | Some c_block_item_list -> 
		let lst_lst = List.map scan_c_block_item c_block_item_list  
		in
		List.flatten lst_lst
	    | None -> []
	  end
	    
    and scan_c_block_item = function
      | I.Block_item_1 c_declaration -> 
	  scan_c_declaration c_declaration
      | I.Block_item_2 c_statement ->
	  scan_c_statement c_statement
	    
    and scan_c_expression_statement = function
      | I.Expression_statement c_expression_opt ->
	  scan_opt scan_c_expression c_expression_opt
	    
    and scan_c_selection_statement = function
      | I.Selection_statement_1_if (c_expression, c_statement) ->
	  (scan_c_expression c_expression) @
	  (scan_c_statement c_statement)
	    
      | I.Selection_statement_2_if_else 
	  (c_expression, 
	   then_c_statement, else_c_statement) 
	->
	  (scan_c_expression c_expression) @
	  (scan_c_statement then_c_statement) @
	  (scan_c_statement else_c_statement)
	    
      | I.Selection_statement_3_switch (c_expression, c_statement) ->
	  (scan_c_expression c_expression) @
	  (scan_c_statement c_statement)
	    
    and scan_c_iteration_statement = function
      | I.Iteration_statement_1_while (c_expression, c_statement) ->
	  (scan_c_expression c_expression) @
	  (scan_c_statement c_statement)
	    
      | I.Iteration_statement_2_do (c_statement, c_expression) ->
	  (scan_c_expression c_expression) @ (scan_c_statement c_statement)
					       
      | I.Iteration_statement_3_for 
	  (c_expression_opt0, c_expression_opt1, c_expression_opt2, c_statement) 
	->
	  (scan_opt scan_c_expression c_expression_opt0) @	    
	  (scan_opt scan_c_expression c_expression_opt1) @
	  (scan_opt scan_c_expression c_expression_opt2) @
	  (scan_c_statement c_statement) 
	    
	    
      | I.Iteration_statement_4_for (c_declaration, c_expression_opt1, 
				     c_expression_opt2, c_statement) 
	->
	  (scan_opt scan_c_expression c_expression_opt1) @
	  (scan_opt scan_c_expression c_expression_opt2) @
	  (scan_c_statement c_statement) 
	    
    and scan_c_jump_statement = function
      | I.Jump_statement_1_goto (I.Identifier str) -> []
      | I.Jump_statement_2_continue -> []
      | I.Jump_statement_3_break -> []
      | I.Jump_statement_4_return_expression c_expression ->
	  scan_c_expression c_expression
      | I.Jump_statement_5_return -> []
      | I.Jump_statement_gnu_goto _ -> []
	    
    and scan_c_translation_unit = function
      | I.Translation_unit l ->
	  (List.iter
	     (fun external_declaration -> 
	       scan_c_external_declaration external_declaration
	     ) l
	  )
	    
    and scan_c_declarator = function
      | I.Declarator (c_pointer_opt, c_direct_declarator) ->
	  scan_c_direct_declarator c_direct_declarator
	    
      | I.Declarator_GNU (declarator, attributes) ->
	  scan_c_declarator declarator

    and scan_c_direct_declarator = function
      | I.Direct_declarator_1 c_identifier ->
	  (scan_c_identifier c_identifier)
	    
      | I.Direct_declarator_2 c_declarator ->
	  (scan_c_declarator c_declarator)

      | I.Direct_declarator_3 (c_direct_declarator, 
			       c_type_qualifier_list_opt, 
			       c_assignment_expression_opt) 
	->
	  (scan_c_direct_declarator c_direct_declarator)
	    
      | I.Direct_declarator_4_STATIC (c_direct_declarator, 
				      c_type_qualifier_list_opt, 
				      c_assignment_expression) 
	->
	  (scan_c_direct_declarator c_direct_declarator)

      | I.Direct_declarator_5_STATIC (c_direct_declarator, 
				      c_type_qualifier_list, 
				      c_assignment_expression) 
	->
	  (scan_c_direct_declarator c_direct_declarator)

      | I.Direct_declarator_6_STAR (c_direct_declarator, 
				    c_type_qualifier_list_opt) 
	->
	  (scan_c_direct_declarator c_direct_declarator)
	    
      | I.Direct_declarator_7 (c_direct_declarator, c_parameter_type_list) ->
	  (scan_c_direct_declarator c_direct_declarator)
	    
      | I.Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	  (scan_c_direct_declarator c_direct_declarator)
	    
    and scan_c_external_declaration = function
      | I.External_declaration_at (coord, v) ->  
	  scan_c_external_declaration v
      | I.External_declaration_1 (c_function_definition) ->
	  (scan_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 (c_declaration) -> ()

    and scan_c_function_definition = function
      | I.Function_definition 
	  (c_declaration_specifiers,
	   c_declarator,
	   c_declaration_list_opt,
	   c_compound_statement) ->
	     let fun_name = scan_c_declarator c_declarator
	     in
	     let callee_list0 = 
	       match c_declaration_list_opt with
	       | Some lst ->
		   let lst_lst =  List.map scan_c_declaration lst
		   in
		   List.flatten lst_lst
	       | None -> []
	     in
	     let callee_list1 = scan_c_compound_statement c_compound_statement
	     in
	     Hashtbl.add hashtbl fun_name 
	       (List.map (fun (v, opt) ->
		 (v, match opt with
		 | None -> assert false
		 | Some argc -> argc))
		 (callee_list0 @ callee_list1));
    in
    let _ = scan_c_translation_unit c_translation_unit
    in
    Gen_call_graph.create (hashtbl, call_argc_hashtbl)
      
      
