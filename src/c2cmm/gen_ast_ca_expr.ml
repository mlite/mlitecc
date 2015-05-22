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

module C99 = Ast_aa_gram
module I = Ast_ba_stmt
module O = Ast_ca_expr
module Os = C_semantics_symbol
open Mapping
open Safe_list

let user_opts = []

let compile_c_constant const = 
  match const with
    | C99.Constant_integer c -> 
	let (teid, cval) = C_syntax_symbol_op.cval_of_int_const c
	in O.Constant_value (teid, Const_folding.cval_ext_of_cval cval)
	     
    | C99.Constant_float v ->
	begin
	  let len = String.length v
	  in
	  match String.get v (len - 1) with
	    | 'f'
	    | 'F' ->
		begin
		  if len > 1 then
		    match String.get v 1 with
		      | 'x' 
		      | 'X' ->
			  let st = String.sub v 0 (len - 1)
			  in
			  O.Constant_value 
			    (Mach.cfloat_id, Const_folding.cval_ext_of_cval 
			      (Const_folding.cfloatx_cval_of_string st))
		      | _ ->
			  let st = String.sub v 0 (len - 1)
			  in
			  O.Constant_value 
			    (Mach.cfloat_id, Const_folding.cval_ext_of_cval 
			      (Const_folding.cfloat_cval_of_string st))
		  else
		    let st = String.sub v 0 (len - 1)
		    in O.Constant_value 
			 (Mach.cfloat_id, 
			 Const_folding.cval_ext_of_cval 
			   (Const_folding.cfloat_cval_of_string st))
		end
	    | 'l'
	    | 'L' ->
		begin
		  if len > 1 then
		    match String.get v 1 with
		      | 'x'
		      | 'X' ->
			  let st = String.sub v 0 (len - 1)
			  in
			  O.Constant_value 
			    (Mach.cldouble_id, Const_folding.cval_ext_of_cval
			      (Const_folding.cldoublex_cval_of_string st))
		      | _ -> 
			  let st = String.sub v 0 (len - 1)
			  in
			  O.Constant_value 
			    (Mach.cldouble_id, Const_folding.cval_ext_of_cval
			      (Const_folding.cldouble_cval_of_string st))
		  else
		    let st = String.sub v 0 (len - 1)
		    in
		    O.Constant_value 
		      (Mach.cldouble_id, 
		      Const_folding.cval_ext_of_cval (Const_folding.cldouble_cval_of_string st))
		end
	    | _ -> 
		O.Constant_value 
		  (Mach.cdouble_id, 
		  Const_folding.cval_ext_of_cval (Const_folding.cdouble_cval_of_string v))
	end
	  
    | C99.Constant_character v ->
	let v = Safe_list.hd v
	in 
	O.Constant_value 
	  (Mach.cchar_id, Const_folding.cval_ext_of_cval
	    (Const_folding.cchar_cval_of_string
	      (Int64.to_string v)))
	  
    | C99.Constant_wcharacter v ->
	let v = Safe_list.hd v
	in
	let cv = if Mach.cwchar_t_id = Mach.cint_id then
	  Const_folding.cint_cval_of_string (Int64.to_string v)
	else
	  assert false
	in O.Constant_value (Mach.cwchar_t_id, Const_folding.cval_ext_of_cval cv)
	     
    | C99.Constant_enumeration (C99.Enumeration_constant (C99.Identifier v)) -> 
	O.Constant_enumeration v
	  
    | C99.Constant_zero -> 
	O.Constant_value (Mach.cint_id, 
	Const_folding.cval_ext_of_cval (Const_folding.cint_cval_of_string "0"))


let rec compile (basename:string)  (c_translation_unit:I.c_translation_unit): O.c_translation_unit = 
  compile_c_translation_unit c_translation_unit
    
and compile_c_identifier = function
  | I.Identifier s -> s

and compile_c_enumeration_constant = function
  | I.Enumeration_constant c_identifier -> 
      compile_c_identifier c_identifier

and compile_gnu_attribute (str, expr_list) = 
  (str, List.map compile_c_expression expr_list)

and compile_c_primary_expression = function
  | I.Primary_expression_1 c_identifier -> 
      O.Variable (compile_c_identifier c_identifier)
  | I.Primary_expression_2 c_constant -> 
      O.Constant (compile_c_constant c_constant)
  | I.Primary_expression_3 c_string_literal -> 
      O.String c_string_literal
  | I.Primary_expression_4 c_expression -> 
      compile_c_expression c_expression
  | I.Primary_expression_macro_va_arg (c_expression, c_type_name) ->
      O.Macro_va_arg (compile_c_expression c_expression, 
      compile_c_type_name c_type_name)
  | I.Primary_expression_macro_va_start 
      (c_expression1, c_expression2) 
    ->
      O.Macro_va_start (compile_c_expression c_expression1,
      compile_c_expression c_expression2)
  | I.Primary_expression_macro_va_end c_expression -> 
      O.Macro_va_end (compile_c_expression c_expression)

  | I.Primary_expression_builtin_types_compatible (t0, t1) ->
      O.Builtin_types_compatible
	(compile_c_type_name t0, compile_c_type_name t1)

  | I.Primary_expression_builtin_constant_p (e0) ->
      O.Builtin_constant_p (compile_c_expression e0)

  | I.Primary_expression_builtin_expect (e0, e1) ->
      O.Builtin_expect (compile_c_expression e0, compile_c_constant_expression e1)

  | I.Primary_expression_gnu_block block -> 
      O.Gnu_block (compile_c_compound_stmt010 block)
	
  | I.Primary_expression_gnu_labeladdr c_identifier -> 
      O.Gnu_labeladdr (compile_c_identifier c_identifier)
	
and compile_c_postfix_expression = function
  | I.Postfix_expression_1 c_primary_expression ->
      compile_c_primary_expression c_primary_expression
	
  | I.Postfix_expression_2 (c_postfix_expression, c_expression) ->
      O.Indexof (compile_c_postfix_expression c_postfix_expression, 
      compile_c_expression c_expression)
	
  | I.Postfix_expression_3 (c_postfix_expression, 
    c_argument_expression_list_opt) 
    ->
      O.Call 
	(compile_c_postfix_expression c_postfix_expression, 
	compile_c_argument_expression_list_opt
	  c_argument_expression_list_opt)
	
  | I.Postfix_expression_4_DOT 
      (c_postfix_expression, I.Identifier id) 
    -> 
      O.Memberof (compile_c_postfix_expression c_postfix_expression, id)

  | I.Postfix_expression_5_ARROW 
      (c_postfix_expression, I.Identifier id) 
    -> 
      O.Memberof_ptr 
	(compile_c_postfix_expression c_postfix_expression, id)

  | I.Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
      O.Post_incr (compile_c_postfix_expression c_postfix_expression)
	
  | I.Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
      O.Post_decr (compile_c_postfix_expression c_postfix_expression)
	
  | I.Postfix_expression_8 (c_type_name, c_initializer_list) ->
      begin
	O.Cast_init 
	  (compile_c_type_name c_type_name, 
	  compile_c_initializer_list c_initializer_list)
      end
	
	
and compile_c_argument_expression_list_opt = function
  | Some (I.Argument_expression_list l) ->
      Safe_list.map 
	(fun c_assignment_expression ->
	  compile_c_assignment_expression c_assignment_expression
	) l
  | None -> []
      
and compile_c_unary_expression = function
  | I.Unary_expression_1 c_postfix_expression ->
      compile_c_postfix_expression c_postfix_expression
	
  | I.Unary_expression_2_PLUS_PLUS c_unary_expression ->
      O.Pre_incr (compile_c_unary_expression c_unary_expression)

  | I.Unary_expression_3_MINUS_MINUS c_unary_expression ->
      O.Pre_decr (compile_c_unary_expression c_unary_expression)

  | I.Unary_expression_4 (c_unary_operator, c_cast_expression) ->
      begin
	match c_unary_operator with
	  | C_syntax_symbol.Unary_operator_AND (** "&" **) -> 
	      O.Addrof (compile_c_cast_expression c_cast_expression)
	  | C_syntax_symbol.Unary_operator_STAR (** "*" **) ->
	      O.Memof (compile_c_cast_expression c_cast_expression)
	  | C_syntax_symbol.Unary_operator_PLUS (** "+" **) ->
	      (compile_c_cast_expression c_cast_expression)
	  | C_syntax_symbol.Unary_operator_MINUS (** "-" **) ->
	      O.Unary_arithm 
		(Os.Neg, (compile_c_cast_expression c_cast_expression))
	  | C_syntax_symbol.Unary_operator_TILDE (** "~" **) ->
	      O.Unary_arithm 
		(Os.Bnot, (compile_c_cast_expression c_cast_expression))
	  | C_syntax_symbol.Unary_operator_EXCLAM (** "!" **) ->
	      O.Logic_not (compile_c_cast_expression c_cast_expression)
      end
	
  | I.Unary_expression_5_SIZEOF c_unary_expression ->
      O.Sizeof_expr (compile_c_unary_expression c_unary_expression)

  | I.Unary_expression_6_SIZEOF c_type_name ->
      O.Sizeof_type (compile_c_type_name c_type_name)
	
  | I.Unary_expression_7_ALIGNOF c_unary_expression ->
      O.Alignof_expr (compile_c_unary_expression c_unary_expression)
	
  | I.Unary_expression_8_ALIGNOF c_type_name ->
      O.Alignof_type (compile_c_type_name c_type_name)
	

and compile_c_cast_expression = function
  | I.Cast_expression_1 c_unary_expression ->
      begin
	compile_c_unary_expression c_unary_expression
      end
  | I.Cast_expression_2 (c_type_name, c_cast_expression) ->
      O.Cast 
	(compile_c_type_name c_type_name, 
	compile_c_cast_expression c_cast_expression)

and compile_c_multiplicative_expression = function
  | I.Multiplicative_expression_1 c_cast_expression ->
      compile_c_cast_expression c_cast_expression
	
  | I.Multiplicative_expression_2_STAR 
      (c_multiplicative_expression, c_cast_expression) 
    ->
      O.Binary_arithm 
	(Os.Mul, 
	compile_c_multiplicative_expression c_multiplicative_expression, 
	compile_c_cast_expression c_cast_expression)
	
  | I.Multiplicative_expression_3_SLASH 
      (c_multiplicative_expression, c_cast_expression) ->
      O.Binary_arithm 
	(Os.Div,
	compile_c_multiplicative_expression 
	  c_multiplicative_expression,
	compile_c_cast_expression c_cast_expression)
	
  | I.Multiplicative_expression_4_PERCENT 
      (c_multiplicative_expression, c_cast_expression) 
    ->
      O.Binary_arithm 
	(Os.Mod,
	compile_c_multiplicative_expression 
	  c_multiplicative_expression,
	compile_c_cast_expression c_cast_expression)

and compile_c_additive_expression = function
  | I.Additive_expression_1 c_multiplicative_expression ->
      compile_c_multiplicative_expression c_multiplicative_expression
	
  | I.Additive_expression_2_PLUS 
      (c_additive_expression, c_multiplicative_expression) 
    ->
      O.Binary_arithm 
	(Os.Add, 
	compile_c_additive_expression c_additive_expression,
	compile_c_multiplicative_expression 
	  c_multiplicative_expression)
	
  | I.Additive_expression_3_MINUS 
      (c_additive_expression, c_multiplicative_expression) 
    ->
      O.Binary_arithm 
	(Os.Sub, 
	compile_c_additive_expression c_additive_expression,
	compile_c_multiplicative_expression c_multiplicative_expression)

and compile_c_shift_expression = function
  | I.Shift_expression_1 c_additive_expression ->
      compile_c_additive_expression c_additive_expression

  | I.Shift_expression_2_INF_INF 
      (c_shift_expression, c_additive_expression) 
    ->
      O.Binary_arithm 
	(Os.Shl, 
	compile_c_shift_expression c_shift_expression,
	compile_c_additive_expression c_additive_expression)

  | I.Shift_expression_3_SUP_SUP 
      (c_shift_expression, c_additive_expression) 
    ->
      O.Binary_arithm 
	(Os.Shr, 
	compile_c_shift_expression c_shift_expression,
	compile_c_additive_expression c_additive_expression)

and compile_c_relational_expression = function
  | I.Relational_expression_1 c_shift_expression ->
      compile_c_shift_expression c_shift_expression

  | I.Relational_expression_2_INF 
      (c_relational_expression, c_shift_expression) 
    ->
      O.Binary_predicate 
	(Os.Lt,
	compile_c_relational_expression c_relational_expression,
	compile_c_shift_expression c_shift_expression)

  | I.Relational_expression_3_SUP 
      (c_relational_expression, c_shift_expression) 
    ->
      O.Binary_predicate 
	(Os.Gt,
	compile_c_relational_expression c_relational_expression,
	compile_c_shift_expression c_shift_expression)

  | I.Relational_expression_4_INF_EQ 
      (c_relational_expression, c_shift_expression) 
    ->
      O.Binary_predicate 
	(Os.Le,
	compile_c_relational_expression c_relational_expression,
	compile_c_shift_expression c_shift_expression)

  | I.Relational_expression_5_SUP_EQ 
      (c_relational_expression, c_shift_expression) 
    ->
      O.Binary_predicate 
	(Os.Ge,
	compile_c_relational_expression c_relational_expression,
	compile_c_shift_expression c_shift_expression)

and compile_c_equality_expression = function
  | I.Equality_expression_1 c_relational_expression ->
      compile_c_relational_expression c_relational_expression
	
  | I.Equality_expression_2_EQ_EQ 
      (c_equality_expression, c_relational_expression) 
    ->
      O.Binary_predicate 
	(Os.Eq,
	compile_c_equality_expression c_equality_expression,
	compile_c_relational_expression c_relational_expression)

  | I.Equality_expression_3_EXCLAM_EQ 
      (c_equality_expression, c_relational_expression) 
    ->
      O.Binary_predicate 
	(Os.Ne,
	compile_c_equality_expression c_equality_expression,
	compile_c_relational_expression c_relational_expression)

	
and compile_c_and_expression = function
  | I.And_expression_1 c_equality_expression ->
      compile_c_equality_expression c_equality_expression

  | I.And_expression_2_AND (c_and_expression, c_equality_expression) ->
      O.Binary_arithm 
	(Os.Band,
	compile_c_and_expression c_and_expression,
	compile_c_equality_expression c_equality_expression)
	
and compile_c_exclusive_or_expression = function
  | I.Exclusive_or_expression_1 c_and_expression ->
      compile_c_and_expression c_and_expression;

  | I.Exclusive_or_expression_2_CIRC 
      (c_exclusive_or_expression, c_and_expression) 
    ->
      O.Binary_arithm 
	(Os.Bxor, 
	compile_c_exclusive_or_expression c_exclusive_or_expression,
	compile_c_and_expression c_and_expression)

and compile_c_inclusive_or_expression = function
  | I.Inclusive_or_expression_1 c_exclusive_or_expression ->
      compile_c_exclusive_or_expression c_exclusive_or_expression;
      
  | I.Inclusive_or_expression_2_PIPE 
      (c_inclusive_or_expression, c_exclusive_or_expression) 
    ->
      O.Binary_arithm 
	(Os.Bor, 
	compile_c_inclusive_or_expression c_inclusive_or_expression,
	compile_c_exclusive_or_expression c_exclusive_or_expression)
	
and compile_c_logical_and_expression = function
  | I.Logical_and_expression_1 c_inclusive_or_expression ->
      compile_c_inclusive_or_expression c_inclusive_or_expression;
      
  | I.Logical_and_expression_2_AND_AND 
      (c_logical_and_expression, c_inclusive_or_expression) 
    ->
      O.Binary_logic 
	(Os.And,
	compile_c_logical_and_expression c_logical_and_expression,
	compile_c_inclusive_or_expression c_inclusive_or_expression)
	
and compile_c_logical_or_expression = function
  | I.Logical_or_expression_1 c_logical_and_expression ->
      compile_c_logical_and_expression c_logical_and_expression;

  | I.Logical_or_expression_2_PIPE_PIPE 
      (c_logical_or_expression, c_logical_and_expression) 
    ->
      O.Binary_logic 
	(Os.Or,
	compile_c_logical_or_expression c_logical_or_expression,
	compile_c_logical_and_expression c_logical_and_expression)
	
and compile_c_conditional_expression = function
  | I.Conditional_expression_1 c_logical_or_expression ->
      compile_c_logical_or_expression c_logical_or_expression;
      
  | I.Conditional_expression_2 
      (c_logical_or_expression, c_expression, c_conditional_expression) 
    ->
      O.Question 
	(compile_c_logical_or_expression c_logical_or_expression,
	Some (compile_c_expression c_expression),
	compile_c_conditional_expression c_conditional_expression)

  | I.Conditional_expression_gnu
      (c_logical_or_expression, c_conditional_expression) 
    ->
      O.Question 
	(compile_c_logical_or_expression c_logical_or_expression,
	None,
	compile_c_conditional_expression c_conditional_expression)

and compile_c_assignment_expression = function
  | I.Assignment_expression_1 c_conditional_expression ->
      compile_c_conditional_expression c_conditional_expression
	
  | I.Assignment_expression_2 
      (c_unary_expression, c_assignment_operator, 
      c_assignment_expression) 
    ->
      begin
	match c_assignment_operator with
	  | C_syntax_symbol.Assignment_operator_EQ -> 
	      O.Assign 
		(compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_STAR_EQ ->
	      O.Assign_arithm 
		(Os.Mul, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_SLASH_EQ ->
	      O.Assign_arithm 
		(Os.Div, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_PERCENT_EQ ->
	      O.Assign_arithm 
		(Os.Mod, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_PLUS_EQ ->
	      O.Assign_arithm 
		(Os.Add, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_MINUS_EQ ->
	      O.Assign_arithm 
		(Os.Sub, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_INF_INF_EQ ->
	      O.Assign_arithm 
		(Os.Shl, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_SUP_SUP_EQ ->
	      O.Assign_arithm 
		(Os.Shr, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_AND_EQ ->
	      O.Assign_arithm 
		(Os.Band, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_CIRC_EQ ->
	      O.Assign_arithm 
		(Os.Bxor, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
	  | C_syntax_symbol.Assignment_operator_PIPE_EQ ->
	      O.Assign_arithm 
		(Os.Bor, compile_c_unary_expression c_unary_expression, 
		compile_c_assignment_expression c_assignment_expression)
      end

and compile_c_expression = function
  | I.Expression_1 c_assignment_expression -> 
      compile_c_assignment_expression c_assignment_expression;
      
  | I.Expression_2 (c_expression, c_assignment_expression) ->
      O.Comma (compile_c_expression c_expression, 
      compile_c_assignment_expression c_assignment_expression)

	
	
and compile_c_constant_expression = function
  | I.Constant_expression c_conditional_expression ->
      compile_c_conditional_expression c_conditional_expression
	
(** the following are standard copy translation **)
and compile_c_declaration = function
  | I.Declaration 
      (c_declaration_specifiers,  c_init_declarator_list_opt) 
    ->
      O.Declaration 
	(compile_c_declaration_specifiers c_declaration_specifiers,
	map_list_opt compile_c_init_declarator c_init_declarator_list_opt)
	
and compile_c_declaration_specifiers = function
  | I.Declaration_specifiers_1 
      (c_storage_class_specifier, c_declaration_specifiers_opt) 
    ->
      O.Declaration_specifiers_1 
	(compile_c_storage_class_specifier 
	  c_storage_class_specifier, 
	map_opt compile_c_declaration_specifiers
	  c_declaration_specifiers_opt)
	
  | I.Declaration_specifiers_2 
      (c_type_specifier, c_declaration_specifiers_opt) 
    ->
      O.Declaration_specifiers_2 
	(compile_c_type_specifier 
	  c_type_specifier,
	map_opt compile_c_declaration_specifiers
	  c_declaration_specifiers_opt)

  | I.Declaration_specifiers_3 
      (c_type_qualifier, c_declaration_specifiers_opt) 
    ->
      O.Declaration_specifiers_3 
	(compile_c_type_qualifier 
	  c_type_qualifier,
	map_opt compile_c_declaration_specifiers
	  c_declaration_specifiers_opt)

  | I.Declaration_specifiers_4 
      (c_function_specifier, c_declaration_specifiers_opt) 
    ->
      O.Declaration_specifiers_4 
	(compile_c_function_specifier 
	  c_function_specifier,
	map_opt compile_c_declaration_specifiers
	  c_declaration_specifiers_opt)

  | I.Declaration_specifiers_GNU (attribute,
    c_declaration_specifiers_opt) 
    ->
      O.Declaration_specifiers_GNU
	(compile_gnu_attribute attribute,
	map_opt compile_c_declaration_specifiers
	  c_declaration_specifiers_opt)
	
and compile_c_init_declarator = function
  | I.Init_declarator_1 c_declarator ->
      O.Init_declarator_1 (compile_c_declarator c_declarator)
	
  | I.Init_declarator_2 (c_declarator, c_initializer) ->
      O.Init_declarator_2 (compile_c_declarator c_declarator,
      compile_c_initializer c_initializer)

and compile_c_storage_class_specifier v = v
  
and compile_c_type_specifier = function
  | I.Type_builtin builtin_type -> O.Type_builtin builtin_type
  | I.Type_specifier_STRUCT_OR_UNION c_struct_or_union_specifier ->
      O.Type_specifier_STRUCT_OR_UNION 
	(compile_c_struct_or_union_specifier c_struct_or_union_specifier)
  | I.Type_specifier_ENUM c_enum_specifier ->
      O.Type_specifier_ENUM (compile_c_enum_specifier c_enum_specifier)
  | I.Type_specifier_TYPENAME c_typedef_name ->
      O.Type_specifier_TYPENAME (compile_c_typedef_name c_typedef_name)
  | I.Type_specifier_GCC_TYPEOF_E c_expression ->
      O.Type_specifier_GCC_TYPEOF_E (compile_c_expression c_expression)
  | I.Type_specifier_GCC_TYPEOF_T c_type_name ->
      O.Type_specifier_GCC_TYPEOF_T (compile_c_type_name c_type_name)
	
and compile_c_struct_or_union_specifier = function
  | I.Struct_or_union_specifier_1 
      (c_struct_or_union, c_identifier_opt, c_struct_declaration_list) 
    ->
      O.Struct_or_union_specifier_1 
	(compile_c_struct_or_union c_struct_or_union,
	map_opt compile_c_identifier c_identifier_opt,
	Safe_list.map compile_c_struct_declaration c_struct_declaration_list)
	
  | I.Struct_or_union_specifier_2 
      (c_struct_or_union, I.Identifier id) 
    -> 
      O.Struct_or_union_specifier_2 (
	compile_c_struct_or_union c_struct_or_union, id)
	
and compile_c_struct_or_union v = v
  
and compile_c_struct_declaration = function
  | I.Struct_declaration 
      (c_specifier_qualifier_list, c_struct_declarator_list) ->
      O.Struct_declaration 
	(compile_c_specifier_qualifier_list c_specifier_qualifier_list,
	Safe_list.map compile_c_struct_declarator c_struct_declarator_list)
	
and compile_c_specifier_qualifier_list = function
  | I.Specifier_qualifier_list_1 
      (c_type_specifier, c_specifier_qualifier_list_opt) 
    ->
      O.Specifier_qualifier_list_1 
	(compile_c_type_specifier c_type_specifier,
	map_opt compile_c_specifier_qualifier_list
	  c_specifier_qualifier_list_opt)

  | I.Specifier_qualifier_list_2 
      (c_type_qualifier, c_specifier_qualifier_list_opt) 
    ->
      O.Specifier_qualifier_list_2 
	(compile_c_type_qualifier 
	  c_type_qualifier,
	map_opt compile_c_specifier_qualifier_list
	  c_specifier_qualifier_list_opt)

  | I.Specifier_qualifier_list_GNU 
      (gnu_attribute, c_specifier_qualifier_list_opt) 
    ->
      O.Specifier_qualifier_list_GNU
	(compile_gnu_attribute gnu_attribute,
	map_opt compile_c_specifier_qualifier_list
	  c_specifier_qualifier_list_opt)

	
and compile_c_struct_declarator = function
  | I.Struct_declarator_1 c_declarator ->
      O.Struct_declarator_1 
	(compile_c_declarator c_declarator)
	
  | I.Struct_declarator_2 (c_declarator_opt, c_constant_expression) ->
      O.Struct_declarator_2 
	(map_opt compile_c_declarator c_declarator_opt,
	compile_c_constant_expression c_constant_expression)

  | I.Struct_declarator_GNU (c_struct_declarator, attributes) ->
      O.Struct_declarator_GNU
	(compile_c_struct_declarator c_struct_declarator, 
	List.map compile_gnu_attribute attributes)
	
and compile_c_enum_specifier = function
  | I.Enum_specifier_1 (c_identifier_opt, c_enumerator_list) ->
      O.Enum_specifier_1 (map_opt compile_c_identifier c_identifier_opt,
      Safe_list.map compile_c_enumerator c_enumerator_list)

  | I.Enum_specifier_2 c_identifier ->
      O.Enum_specifier_2 (compile_c_identifier c_identifier)


and compile_c_enumerator = function
  | I.Enumerator_1 c_enumeration_constant -> 
      O.Enumerator_1 
	(compile_c_enumeration_constant c_enumeration_constant)
  | I.Enumerator_2 (c_enumeration_constant, c_constant_expression) ->
      O.Enumerator_2 
	(compile_c_enumeration_constant c_enumeration_constant,
	compile_c_constant_expression c_constant_expression)

and compile_c_type_qualifier v = v
  
and compile_c_declarator = function
  | I.Declarator (c_pointer_opt, c_direct_declarator) ->
      O.Declarator (map_opt compile_c_pointer c_pointer_opt,
      compile_c_direct_declarator c_direct_declarator)
	
  | I.Declarator_GNU (declarator, attributes) ->
      O.Declarator_GNU 
	(compile_c_declarator declarator, 
	List.map compile_gnu_attribute attributes)

and compile_c_direct_declarator = function
  | I.Direct_declarator_1 c_identifier ->
      O.Direct_declarator_1 (compile_c_identifier c_identifier)
	
  | I.Direct_declarator_2 c_declarator ->
      O.Direct_declarator_2 (compile_c_declarator c_declarator)

  | I.Direct_declarator_3 (c_direct_declarator, 
    c_type_qualifier_list_opt, 
    c_assignment_expression_opt) 
    ->
      O.Direct_declarator_3 
	(compile_c_direct_declarator c_direct_declarator,
	map_list_opt compile_c_type_qualifier c_type_qualifier_list_opt,
	map_opt compile_c_assignment_expression c_assignment_expression_opt)

  | I.Direct_declarator_4_STATIC (c_direct_declarator, 
    c_type_qualifier_list_opt, 
    c_assignment_expression) 
    ->
      O.Direct_declarator_4_STATIC 
	(compile_c_direct_declarator c_direct_declarator,
	map_list_opt compile_c_type_qualifier c_type_qualifier_list_opt,
	compile_c_assignment_expression c_assignment_expression)

  | I.Direct_declarator_5_STATIC (c_direct_declarator, 
    c_type_qualifier_list, 
    c_assignment_expression) 
    ->
      O.Direct_declarator_5_STATIC 
	(compile_c_direct_declarator c_direct_declarator,
	Safe_list.map compile_c_type_qualifier c_type_qualifier_list,
	compile_c_assignment_expression c_assignment_expression)

  | I.Direct_declarator_6_STAR 
      (c_direct_declarator, c_type_qualifier_list_opt) 
    ->
      O.Direct_declarator_6_STAR 
	(compile_c_direct_declarator c_direct_declarator,
	map_list_opt compile_c_type_qualifier c_type_qualifier_list_opt)

  | I.Direct_declarator_7 
      (c_direct_declarator, c_parameter_type_list) 
    ->
      O.Direct_declarator_7 
	(compile_c_direct_declarator c_direct_declarator,
	compile_c_parameter_type_list c_parameter_type_list)

  | I.Direct_declarator_8 
      (c_direct_declarator, c_identifier_list_opt) 
    ->
      O.Direct_declarator_8 
	(compile_c_direct_declarator c_direct_declarator,
	map_list_opt compile_c_identifier c_identifier_list_opt)

and compile_c_pointer = function
  | I.Pointer_1 c_type_qualifier_list_opt ->
      O.Pointer_1 
	(map_list_opt compile_c_type_qualifier c_type_qualifier_list_opt)
	
  | I.Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
      O.Pointer_2 
	(map_list_opt compile_c_type_qualifier c_type_qualifier_list_opt,
	compile_c_pointer c_pointer)

and compile_c_function_specifier v = v

and compile_c_parameter_type_list = function
  | I.Parameter_type_list_FIX c_parameter_list ->
      O.Parameter_type_list_FIX 
	(compile_c_parameter_list c_parameter_list)

  | I.Parameter_type_list_VAR c_parameter_list ->
      O.Parameter_type_list_VAR 
	(compile_c_parameter_list c_parameter_list)

and compile_c_parameter_list = function
  | I.Parameter_list l -> 
      O.Parameter_list (Safe_list.map compile_c_parameter_declaration l)


and compile_c_parameter_declaration = function
  | I.Parameter_declaration_1 
      (c_declaration_specifiers, c_declarator) 
    ->
      O.Parameter_declaration_1 
	(compile_c_declaration_specifiers c_declaration_specifiers,
	compile_c_declarator c_declarator)

  | I.Parameter_declaration_2 
      (c_declaration_specifiers, c_abstract_declarator_opt) 
    ->
      O.Parameter_declaration_2 
	(compile_c_declaration_specifiers c_declaration_specifiers,
	map_opt compile_c_abstract_declarator c_abstract_declarator_opt)

	
and compile_c_type_name = function
  | I.Type_name 
      (c_specifier_qualifier_list, c_abstract_declarator_opt) 
    ->
      O.Type_name 
	(compile_c_specifier_qualifier_list c_specifier_qualifier_list,
	map_opt compile_c_abstract_declarator c_abstract_declarator_opt)

and compile_c_abstract_declarator = function
  | I.Abstract_declarator_1 c_pointer -> 
      O.Abstract_declarator_1 (compile_c_pointer c_pointer)
	
  | I.Abstract_declarator_2 
      (c_pointer_opt, c_direct_abstract_declarator) 
    ->
      O.Abstract_declarator_2 
	(map_opt compile_c_pointer c_pointer_opt,
	compile_c_direct_abstract_declarator 
	  c_direct_abstract_declarator)
	
and compile_c_direct_abstract_declarator = function
  | I.Direct_abstract_declarator_error -> assert false
  | I.Direct_abstract_declarator_1 c_abstract_declarator ->
      O.Direct_abstract_declarator_1 
	(compile_c_abstract_declarator c_abstract_declarator)

  | I.Direct_abstract_declarator_2 
      (c_direct_abstract_declarator_opt, c_assignment_expression_opt) 
    ->
      O.Direct_abstract_declarator_2 
	(map_opt compile_c_direct_abstract_declarator
	  c_direct_abstract_declarator_opt,
	map_opt compile_c_assignment_expression
	  c_assignment_expression_opt)

  | I.Direct_abstract_declarator_3_STAR 
      c_direct_abstract_declarator_opt  
    ->
      O.Direct_abstract_declarator_3_STAR 
	(map_opt compile_c_direct_abstract_declarator
	  c_direct_abstract_declarator_opt)
	
  | I.Direct_abstract_declarator_4 
      (c_direct_abstract_declarator_opt, c_parameter_type_list_opt) 
    ->
      O.Direct_abstract_declarator_4 
	(map_opt compile_c_direct_abstract_declarator
	  c_direct_abstract_declarator_opt,
        map_opt compile_c_parameter_type_list
	  c_parameter_type_list_opt)

and compile_c_typedef_name = function
  | I.Typedef_name (I.Identifier_as_typ id) -> O.Typedef_name id

and compile_c_initializer = function
  | I.Initializer_1 c_assignment_expression ->
      O.Initializer_1 
	(compile_c_assignment_expression c_assignment_expression)

  | I.Initializer_2 c_initializer_list  ->
      O.Initializer_2 
	(compile_c_initializer_list c_initializer_list)

and compile_c_initializer_list = function
  | I.Initializer_list l ->
      (Safe_list.map 
	(fun (c_designation_opt, c_initializer) -> 
	  (compile_c_designation_opt c_designation_opt,
	  compile_c_initializer c_initializer)
	) 
	l
      )

and compile_c_designation = function
  | I.Designation c_designator_list ->
      Safe_list.map compile_c_designator c_designator_list


and compile_c_designator = function
  | I.Designator_1 c_constant_expression ->
      O.Designator_1 (compile_c_constant_expression c_constant_expression)

  | I.Designator_2 c_identifier ->
      O.Designator_2 (compile_c_identifier c_identifier)

  | I.Designator_gnu_range (e0, e1) ->
      O.Designator_gnu_range 
	(compile_c_constant_expression e0,
	compile_c_constant_expression e1)
	
and compile_c_designation_opt = function
  | Some c_designation -> 
      compile_c_designation c_designation
  | None -> []
      
and compile_asm (str_list, asm_details_opt) = 
  let asm_details_opt = match asm_details_opt with
    | Some asm_details ->
	let f (so, s, e) = (so, s, compile_c_expression e)
	in
	Some 
	  ({
	    O.asm_outputs = Safe_list.map f asm_details.I.asm_outputs;
	    O.asm_inputs = Safe_list.map f asm_details.I.asm_inputs;
	    O.asm_clobbers = asm_details.I.asm_clobbers;
	  })
    | None -> None
  in
  O.ASM (str_list, asm_details_opt)
    
and compile_c_stmt010 = function
  | I.STMT_AT (coord, stmt) ->
      O.STMT_AT (coord, compile_c_stmt010 stmt)

  | I.NOP -> O.NOP
      
  | I.COMPUTATION (c_expression) ->
      O.COMPUTATION (compile_c_expression c_expression)

  | I.SEQUENCE (txt_opt, c_stmt010_list) ->
      O.SEQUENCE (txt_opt, Safe_list.map compile_c_stmt010 c_stmt010_list)

  | I.COMPOUND (txt_opt, c_compound_stmt010) ->
      O.COMPOUND (txt_opt, compile_c_compound_stmt010 c_compound_stmt010)
	
  | I.IF (c_expression, then_c_stmt010, else_c_stmt010) ->
      O.IF (compile_c_expression c_expression, 
      compile_c_stmt010 then_c_stmt010,
      compile_c_stmt010 else_c_stmt010)

  | I.WHILE (c_expression, c_stmt010) -> 
      O.WHILE (compile_c_expression c_expression,
      compile_c_stmt010 c_stmt010)

  | I.LOOP (c_stmt010) -> 
      O.LOOP (compile_c_stmt010 c_stmt010)

  | I.BREAK -> O.BREAK
  | I.CONTINUE -> O.CONTINUE
  | I.RETURN_VALUE (c_expression) -> 
      O.RETURN_VALUE (compile_c_expression c_expression)
	
  | I.RETURN -> O.RETURN
  | I.SWITCH (c_expression, c_stmt010) ->
      O.SWITCH (compile_c_expression c_expression, 
      compile_c_stmt010 c_stmt010)
	
  | I.CASE (c_constant_expression, c_stmt010) ->
      O.CASE (compile_c_constant_expression c_constant_expression, 
      compile_c_stmt010 c_stmt010)

  | I.CASE_RANGE (e0, e1, c_statement) ->
      O.CASE_RANGE 
	(compile_c_constant_expression e0,
	compile_c_constant_expression e1,
	compile_c_stmt010 c_statement)
	
  | I.DEFAULT (c_stmt010) ->
      O.DEFAULT (compile_c_stmt010 c_stmt010)
	
  | I.LABEL (string, c_stmt010) ->
      O.LABEL (string, compile_c_stmt010 c_stmt010)
	
  | I.GOTO (string) -> 
      O.GOTO (string)

  | I.GCC_GOTO expr ->
      O.GCC_GOTO (compile_c_expression expr)

  | I.ASM (u, v) ->
      compile_asm (u,v)
	
and compile_c_compound_stmt010 = function
  | I.BLOCK (labels, decls, stmts) ->
      O.BLOCK 
	(labels, Safe_list.map compile_c_declaration decls, 
	Safe_list.map compile_c_stmt010 stmts)
	
and compile_c_translation_unit = function
  | I.Translation_unit l ->
      O.Translation_unit 
	(Safe_list.map
	  (fun external_declaration -> 
	    compile_c_external_declaration external_declaration
	  ) l
	)
	
and compile_c_external_declaration = function
  | I.External_declaration_at (coord, expr) ->
      O.External_declaration_at (coord, compile_c_external_declaration expr)
	
  | I.External_declaration_1 (c_function_definition) ->
      O.External_declaration_1 
	(compile_c_function_definition c_function_definition)
	
  | I.External_declaration_2 (c_declaration) ->
      O.External_declaration_2 
	(compile_c_declaration c_declaration)
	
and compile_c_function_definition = function
  | I.Function_definition 
      (c_declaration_specifiers,
      c_declarator,
      c_declaration_list_opt,
      c_compound_stmt010) ->
      O.Function_definition 
	(compile_c_declaration_specifiers c_declaration_specifiers,
	compile_c_declarator c_declarator,
	map_list_opt compile_c_declaration c_declaration_list_opt,
	compile_c_compound_stmt010 c_compound_stmt010)
