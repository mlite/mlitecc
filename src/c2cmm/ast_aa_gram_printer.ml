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
open Pretty_printer
open C_syntax_symbol
open C_syntax_symbol_printer
include Ast_aa_gram


type c_file_unit = c_translation_unit
let description () = description
let suffix () = suffix

let indent = 2


let pp_print_c_identifier fm (Identifier v) = 
  print_in_box 0 pp_print_string fm v
    
let pp_print_c_identifier_opt fm id = print_opt_with_space pp_print_c_identifier id
    
let pp_print_c_identifier_as_typ fm (Identifier_as_typ v) =
  print_in_box 0 pp_print_string fm v
    
    
let pp_print_c_enumeration_constant fm (Enumeration_constant v) =
  print_in_box 0 pp_print_c_identifier fm v

let pp_print_c_integer_suffix = print_c_integer_suffix
    
let pp_print_c_integer_constant = print_c_integer_constant
    
let pp_print_c_constant: formatter -> c_constant -> unit =
  fun fm c ->
    print_in_box 0 
      (fun fm c ->
	match c with
	  | Constant_integer c_integer_constant -> 
	      pp_print_c_integer_constant fm c_integer_constant
	  | Constant_float v  ->
	      pp_print_string fm v
	  | Constant_enumeration v ->
	      pp_print_c_enumeration_constant fm v
	  | Constant_character int64_list ->
	      pp_print_string fm ("'" ^ (C_str.escape_wstring int64_list) ^ "'")
	  | Constant_wcharacter int64_list ->
	      pp_print_string fm ("L'" ^ (C_str.escape_wstring int64_list) ^ "'")
	  | Constant_zero ->
	      pp_print_string fm "0"
      ) fm c

let pp_print_c_string_literal = print_c_string_literal

let pp_print_c_unary_operator = print_c_unary_operator

let pp_print_c_assignment_operator = print_c_assignment_operator

let pp_print_c_storage_class_specifier = print_c_storage_class_specifier

let pp_print_c_struct_or_union = print_c_struct_or_union

let pp_print_c_type_qualifier = print_c_type_qualifier

let pp_print_c_function_specifier = print_c_function_specifier
    
let pp_print_c_typedef_name fm (Typedef_name v) =
  print_in_box 0 pp_print_c_identifier_as_typ fm v
    
let pp_print_l_parens fm str = 
  match str with
    | "__attribute__" -> 
	pp_print_string fm "((";
    | _ -> 
	pp_print_string fm "("


let pp_print_r_parens fm str = 
  match str with
    | "__attribute__" -> 
	pp_print_string fm "))";
    | _ -> 
	pp_print_string fm ")"
	
	

(** *********************************************************************************** **)
let rec pp_print_attribute (fm:formatter) ((str,expr_list):gnu_attribute) =
  pp_print_string fm str;
  pp_print_l_parens fm str;
  Mlite_printer.pp_print_list fm 
    (pp_print_c_expression ~need_paren:false)
    (fun fm -> pp_print_space fm ())
    expr_list;
  pp_print_r_parens fm str

and pp_print_gnu_attribute_list fm attributes =
  if attributes <> [] then
    begin 
      pp_print_space fm ();
      Mlite_printer.pp_print_list fm 
	pp_print_attribute 
	(fun fm -> pp_print_space fm ())
	attributes
    end
      
and pp_print_c_primary_expression ~(need_paren:bool) (fm:formatter) (expr:c_primary_expression) =
  print_in_box 0
    (fun fm expr ->
      match expr with
	| Primary_expression_1 c_identifier ->
	    pp_print_c_identifier fm c_identifier
	| Primary_expression_2 c_constant ->
	    pp_print_c_constant fm c_constant
	| Primary_expression_3 c_string_literal ->
	    pp_print_c_string_literal fm c_string_literal
	| Primary_expression_4 c_expression ->
	    pp_print_c_expression ~need_paren:true fm c_expression
	      
	| Primary_expression_macro_va_start (c_expression1, c_expression2) ->
	    begin
	      pp_print_string fm "__builtin_va_start";
	      pp_print_space fm ();
	      print_actual_parameters 0 
		(pp_print_c_expression ~need_paren:true) 
		fm ([c_expression1;c_expression2])
	    end
	      
	| Primary_expression_macro_va_arg (c_expression, c_type_name) ->
	    begin
	      pp_print_string fm "__builtin_va_arg";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expression ~need_paren:true fm c_expression;
	      pp_print_string fm ",";
	      pp_print_c_type_name fm c_type_name;
	      pp_print_string fm ")";
	    end
	      
	| Primary_expression_macro_va_end (c_expression) ->
	    begin
	      pp_print_string fm "__builtin_va_end";
	      pp_print_space fm ();
	      print_actual_parameters 0 
		(pp_print_c_expression ~need_paren:true) fm ([c_expression])
	    end

	| Primary_expression_builtin_types_compatible (t0, t1) ->
	    begin
	      pp_print_string fm "__builtin_types_compatible_p";
	      pp_print_space fm ();
	      print_actual_parameters 0 
		pp_print_c_type_name fm ([t0;t1])
	    end

	| Primary_expression_builtin_constant_p (e0) ->
	    begin
	      pp_print_string fm "__builtin_constant_p";
	      pp_print_space fm ();
	      print_actual_parameters 0 
		(pp_print_c_expression ~need_paren:true) fm ([e0])
	    end

	| Primary_expression_builtin_expect (e0, e1) ->
	    begin
	      pp_print_string fm "__builtin_expect";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expression ~need_paren:true fm e0;
	      pp_print_string fm ",";
	      pp_print_c_constant_expression fm e1;
	      pp_print_string fm ")"
	    end
	      
	| Primary_expression_gnu_block c_compound_statement ->
	    begin
	      pp_print_string fm "__extension__(";
	      pp_print_space fm ();
	      pp_print_c_compound_statement fm c_compound_statement;
	      pp_print_string fm ")";
	    end
	| Primary_expression_gnu_labeladdr c_identifier ->
	    pp_print_string fm "&&";
	    pp_print_c_identifier fm c_identifier
    ) fm expr

and pp_print_c_postfix_expression ~(need_paren:bool) (fm:formatter)
    (expr:c_postfix_expression) =
  pp_open_hovbox fm 0;
  let _ = match expr with
    | Postfix_expression_1 c_primary_expression ->
	pp_print_c_primary_expression ~need_paren fm c_primary_expression
    | Postfix_expression_2 (c_postfix_expression, c_expression) ->
	if need_paren then
	  pp_print_string fm "(";
	pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	pp_print_string fm "[";
	pp_print_c_expression ~need_paren:false fm c_expression;
	pp_print_string fm "]";
	if need_paren then
	  pp_print_string fm ")";
    | Postfix_expression_3 (c_postfix_expression, c_argument_expression_list) ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	  pp_print_c_space fm;
	  pp_print_string fm "(";
	  pp_print_c_argument_expression_list_opt fm c_argument_expression_list;
	  pp_print_string fm ")";
	  if need_paren then
	    pp_print_string fm ")";
	end
    | Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	  pp_print_string fm ".";
	  pp_print_c_identifier fm c_identifier;
	  if need_paren then
	    pp_print_string fm ")";
	end
    | Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	  pp_print_string fm "->";
	  pp_print_c_identifier fm c_identifier;
	  if need_paren then
	    pp_print_string fm ")";
	end
    | Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	  pp_print_string fm "++";
	  if need_paren then
	    pp_print_string fm ")";
	end
    | Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression ~need_paren:true fm c_postfix_expression;
	  pp_print_string fm "--";
	  if need_paren then
	    pp_print_string fm ")";
	end
    | Postfix_expression_8 (c_type_name, c_initializer_list) ->
	begin
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_string fm "(";
	  pp_print_c_type_name fm c_type_name;
	  pp_print_string fm ")";
	  pp_print_cut fm ();
	  pp_print_string fm "{";
	  pp_print_c_initializer_list fm c_initializer_list;
	  pp_print_string fm "}";
	  if need_paren then
	    pp_print_string fm ")";
	end
	  
  in
  pp_close_box fm ()
    
and pp_print_c_argument_expression_list: formatter -> c_argument_expression_list -> unit = 
  fun fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Argument_expression_list l ->
	  begin
	    Mlite_printer.pp_print_list fm 
	      (fun fm c_assignment_expression ->
                pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression)
	      (fun fm -> 
		pp_print_string fm ",";
		pp_print_c_space fm
	      )
	      l
	  end
    in
    pp_close_box fm ()

and pp_print_c_unary_expression: need_paren:bool -> formatter -> c_unary_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Unary_expression_1 c_postfix_expression ->
	  pp_print_c_postfix_expression ~need_paren fm c_postfix_expression
      | Unary_expression_2_PLUS_PLUS c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "++";
	    pp_print_c_unary_expression ~need_paren:true fm c_unary_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_3_MINUS_MINUS c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "--";
	    pp_print_c_unary_expression ~need_paren:true fm c_unary_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_unary_operator fm c_unary_operator;
	    pp_print_c_cast_expression ~need_paren:true fm c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_5_SIZEOF c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "sizeof";
	    pp_print_c_space fm;
	    pp_print_string fm "(";
	    pp_print_c_unary_expression ~need_paren:true fm c_unary_expression;
	    pp_print_string fm ")";
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_6_SIZEOF c_type_name ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "sizeof";
	    pp_print_c_space fm;
	    pp_print_string fm "(";
	    pp_print_c_type_name fm c_type_name;
	    pp_print_string fm ")";
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_7_ALIGNOF c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "__alignof__";
	    pp_print_c_space fm;
	    pp_print_string fm "(";
	    pp_print_c_unary_expression ~need_paren:true fm c_unary_expression;
	    pp_print_string fm ")";
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_8_ALIGNOF c_type_name ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "__alignof__";
	    pp_print_c_space fm;
	    pp_print_string fm "(";
	    pp_print_c_type_name fm c_type_name;
	    pp_print_string fm ")";
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_cast_expression: need_paren:bool -> formatter -> c_cast_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Cast_expression_1 c_unary_expression ->
	  pp_print_c_unary_expression ~need_paren fm c_unary_expression
      | Cast_expression_2 (c_type_name, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "(";
	    pp_print_c_type_name fm c_type_name;
	    pp_print_string fm ")";
	    pp_print_c_cast_expression ~need_paren:true fm c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_multiplicative_expression: need_paren:bool -> formatter -> 
      c_multiplicative_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Multiplicative_expression_1 c_cast_expression ->
	  pp_print_c_cast_expression ~need_paren fm c_cast_expression
      | Multiplicative_expression_2_STAR (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression 
	      ~need_paren:true fm c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "*";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression ~need_paren:true fm c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Multiplicative_expression_3_SLASH (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression 
	      ~need_paren:true fm c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "/";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression ~need_paren:true fm c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Multiplicative_expression_4_PERCENT (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression ~need_paren:true fm c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "%";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression ~need_paren:true fm c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_additive_expression: need_paren:bool -> formatter -> c_additive_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Additive_expression_1 c_multiplicative_expression ->
	  begin
	    pp_print_c_multiplicative_expression ~need_paren fm c_multiplicative_expression
	  end
      | Additive_expression_2_PLUS (c_additive_expression, c_multiplicative_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_additive_expression ~need_paren:true fm c_additive_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "+";
	    pp_print_c_space fm;
	    pp_print_c_multiplicative_expression ~need_paren:true fm c_multiplicative_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Additive_expression_3_MINUS (c_additive_expression, c_multiplicative_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_additive_expression ~need_paren:true fm c_additive_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "-";
	    pp_print_c_space fm;
	    pp_print_c_multiplicative_expression ~need_paren:true fm c_multiplicative_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_shift_expression: need_paren:bool -> formatter -> c_shift_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Shift_expression_1 c_additive_expression ->
	  begin
	    pp_print_c_additive_expression ~need_paren fm c_additive_expression
	  end
      | Shift_expression_2_INF_INF (c_shift_expression, c_additive_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<<";
	    pp_print_c_space fm;
	    pp_print_c_additive_expression ~need_paren:true fm c_additive_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Shift_expression_3_SUP_SUP (c_shift_expression, c_additive_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">>";
	    pp_print_c_space fm;
	    pp_print_c_additive_expression ~need_paren:true fm c_additive_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_relational_expression: need_paren:bool -> formatter -> c_relational_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Relational_expression_1 c_shift_expression ->
	  begin
	    pp_print_c_shift_expression ~need_paren fm c_shift_expression
	  end
      | Relational_expression_2_INF (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_3_SUP (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_4_INF_EQ (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<=";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_5_SUP_EQ (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">=";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression ~need_paren:true fm c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_equality_expression: need_paren:bool -> formatter -> c_equality_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Equality_expression_1 c_relational_expression ->
	  begin
	    pp_print_c_relational_expression ~need_paren fm c_relational_expression
	  end
      | Equality_expression_2_EQ_EQ (c_equality_expression, c_relational_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_equality_expression ~need_paren:true fm c_equality_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "==";
	    pp_print_c_space fm;
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Equality_expression_3_EXCLAM_EQ (c_equality_expression, c_relational_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_equality_expression ~need_paren:true fm c_equality_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "!=";
	    pp_print_c_space fm;
	    pp_print_c_relational_expression ~need_paren:true fm c_relational_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_and_expression: need_paren:bool -> formatter -> c_and_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | And_expression_1 c_equality_expression ->
	  begin
	    pp_print_c_equality_expression ~need_paren fm c_equality_expression
	  end
      | And_expression_2_AND (c_and_expression, c_equality_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_and_expression ~need_paren:true fm c_and_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "&";
	    pp_print_c_space fm;
	    pp_print_c_equality_expression ~need_paren:true fm c_equality_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_exclusive_or_expression: need_paren:bool -> formatter -> c_exclusive_or_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Exclusive_or_expression_1 c_and_expression ->
	  begin
	    pp_print_c_and_expression ~need_paren fm c_and_expression;
	  end
      | Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, c_and_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_exclusive_or_expression ~need_paren:true fm  c_exclusive_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "^";
	    pp_print_c_space fm;
	    pp_print_c_and_expression ~need_paren:true fm c_and_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_inclusive_or_expression: need_paren:bool -> formatter -> c_inclusive_or_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Inclusive_or_expression_1 c_exclusive_or_expression ->
	  begin
	    pp_print_c_exclusive_or_expression ~need_paren fm c_exclusive_or_expression;
	  end
      | Inclusive_or_expression_2_PIPE (c_inclusive_or_expression, c_exclusive_or_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_inclusive_or_expression ~need_paren:true fm c_inclusive_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "|";
	    pp_print_c_space fm;
	    pp_print_c_exclusive_or_expression ~need_paren:true fm c_exclusive_or_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()
      
and pp_print_c_logical_and_expression: need_paren:bool -> formatter -> c_logical_and_expression -> 
      unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Logical_and_expression_1 c_inclusive_or_expression ->
	  begin
	    pp_print_c_inclusive_or_expression ~need_paren fm c_inclusive_or_expression;
	  end
      | Logical_and_expression_2_AND_AND (c_logical_and_expression, c_inclusive_or_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_and_expression ~need_paren:true fm c_logical_and_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "&&";
	    pp_print_c_space fm;
	    pp_print_c_inclusive_or_expression ~need_paren:true fm c_inclusive_or_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_logical_or_expression: need_paren:bool -> formatter -> c_logical_or_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Logical_or_expression_1 c_logical_and_expression ->
	  begin
	    pp_print_c_logical_and_expression ~need_paren fm c_logical_and_expression;
	  end
      | Logical_or_expression_2_PIPE_PIPE (c_logical_or_expression, c_logical_and_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_or_expression ~need_paren:true fm c_logical_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "||";
	    pp_print_c_space fm;
	    pp_print_c_logical_and_expression ~need_paren:true fm c_logical_and_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_conditional_expression: need_paren:bool -> formatter -> c_conditional_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Conditional_expression_1 c_logical_or_expression ->
	  begin
	    pp_print_c_logical_or_expression ~need_paren fm c_logical_or_expression;
	  end
      | Conditional_expression_2 (c_logical_or_expression, c_expression, c_conditional_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_or_expression ~need_paren:true fm c_logical_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "?";
	    pp_print_c_space fm;
	    pp_print_c_expression ~need_paren:true fm c_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ":";
	    pp_print_c_space fm;
	    pp_print_c_conditional_expression ~need_paren:true fm c_conditional_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Conditional_expression_gnu (c_logical_or_expression, c_conditional_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_or_expression ~need_paren:true fm c_logical_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "?";
	    pp_print_c_space fm;
	    pp_print_string fm ":";
	    pp_print_c_space fm;
	    pp_print_c_conditional_expression ~need_paren:true fm c_conditional_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_assignment_expression: need_paren:bool -> formatter -> c_assignment_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Assignment_expression_1 c_conditional_expression ->
	  begin
	    pp_print_c_conditional_expression ~need_paren fm c_conditional_expression
	  end
      | Assignment_expression_2 (c_unary_expression, c_assignment_operator, c_assignment_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_unary_expression ~need_paren:false fm c_unary_expression;
	    pp_print_c_space fm;
	    pp_print_c_assignment_operator fm c_assignment_operator;
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression ~need_paren:false fm c_assignment_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_expression: need_paren:bool -> formatter -> c_expression -> unit = 
  fun ~need_paren fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Expression_1 c_assignment_expression -> 
	  begin
	    pp_print_c_assignment_expression ~need_paren fm c_assignment_expression;
	  end
      | Expression_2 (c_expression, c_assignment_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_expression ~need_paren:true fm c_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ",";
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_constant_expression: formatter -> c_constant_expression -> unit = 
  fun fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Constant_expression c_conditional_expression ->
	  pp_print_c_conditional_expression ~need_paren:false fm c_conditional_expression
    in
    pp_close_box fm ()

and pp_print_c_declaration: formatter -> c_declaration -> unit = 
  fun fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Declaration (c_declaration_specifiers,  c_init_declarator_list_opt) ->
	  begin
	    pp_print_c_declaration_specifiers fm c_declaration_specifiers;
	    pp_print_c_init_declarator_list_opt fm c_init_declarator_list_opt;
	    pp_print_string fm ";"
	  end
    in
    pp_close_box fm ()
      
and pp_print_c_declaration_specifiers: formatter -> c_declaration_specifiers -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Declaration_specifiers_1 (c_storage_class_specifier, c_declaration_specifiers_opt) ->
	  begin
	    pp_print_c_storage_class_specifier fm c_storage_class_specifier;
	    pp_print_c_declaration_specifiers_opt fm c_declaration_specifiers_opt
	  end
      | Declaration_specifiers_2 (c_type_specifier, c_declaration_specifiers_opt) ->
	  begin
	    pp_print_c_type_specifier fm c_type_specifier;
	    pp_print_c_declaration_specifiers_opt fm c_declaration_specifiers_opt
	  end
      | Declaration_specifiers_3 (c_type_qualifier, c_declaration_specifiers_opt) ->
	  begin
	    pp_print_c_type_qualifier fm c_type_qualifier;
	    pp_print_c_declaration_specifiers_opt fm c_declaration_specifiers_opt
	  end
      | Declaration_specifiers_4 (c_function_specifier, c_declaration_specifiers_opt) ->
	  begin
	    pp_print_c_function_specifier fm c_function_specifier;
	    pp_print_c_declaration_specifiers_opt fm c_declaration_specifiers_opt
	  end
      | Declaration_specifiers_GNU (attribute, c_declaration_specifiers_opt) ->
	  begin
	    pp_print_attribute fm attribute;
	    pp_print_c_declaration_specifiers_opt fm c_declaration_specifiers_opt
	  end
    in
    pp_close_box fm ()

and pp_print_c_init_declarator_list: formatter -> c_init_declarator list -> unit =
  fun fm c_init_declarator_list ->
    pp_open_box fm 0;
    pp_print_c_space fm;
    Mlite_printer.pp_print_list fm 
      (fun fm c_init_declarator -> 
	pp_print_c_init_declarator fm c_init_declarator
      ) 
      (fun fm -> pp_print_string fm ","; pp_print_c_space fm)
      c_init_declarator_list;
    pp_close_box fm ()
      
and pp_print_c_init_declarator: formatter -> c_init_declarator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Init_declarator_1 c_declarator ->
	  begin
	    pp_print_c_declarator fm c_declarator;
	  end
      | Init_declarator_2 (c_declarator, c_initializer) ->
	  begin
	    pp_print_c_declarator fm c_declarator;
	    pp_print_c_space fm;
	    pp_print_string fm "=";
	    pp_print_c_space fm;
	    pp_print_c_initializer fm c_initializer;
	  end
    in
    pp_close_box fm ()

and pp_print_c_type_specifier: formatter -> c_type_specifier -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Type_builtin builtin_type -> print_c_builtin_type_specifier fm builtin_type
      | Type_specifier_STRUCT_OR_UNION c_struct_or_union_specifier ->
	  pp_print_c_struct_or_union_specifier fm
	    c_struct_or_union_specifier

      | Type_specifier_ENUM c_enum_specifier ->
	  pp_print_c_enum_specifier fm c_enum_specifier

      | Type_specifier_TYPENAME c_typedef_name ->
	  pp_print_c_typedef_name fm c_typedef_name
	    
      | Type_specifier_GCC_TYPEOF_E c_expression ->
	  pp_print_string fm "__typeof__(";
	  pp_print_c_expression ~need_paren:false fm c_expression;
	  pp_print_string fm ")"
	    
      | Type_specifier_GCC_TYPEOF_T c_type_name ->
	  pp_print_string fm "__typeof__(";
	  pp_print_c_type_name fm c_type_name;
	  pp_print_string fm ")"
    in
    pp_close_box fm ()

and pp_print_c_struct_or_union_specifier: formatter -> c_struct_or_union_specifier -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      let _ = match expr with
	| Struct_or_union_specifier_1 
	    (c_struct_or_union, c_identifier_opt, c_struct_declaration_list) ->
	    begin
	      pp_open_box fm 0;
	      begin
		print_c_struct_or_union fm c_struct_or_union;
		pp_print_c_identifier_opt fm c_identifier_opt;
	      end;
	      pp_close_box fm ();
	      pp_print_cut fm ();
	      pp_print_string fm "{";
	      pp_open_vbox fm indent;
	      begin
		pp_print_cut fm ();
		pp_print_c_struct_declaration_list fm c_struct_declaration_list;
	      end;
	      pp_close_box fm ();
	      pp_print_cut fm ();
	      pp_print_string fm "}"		    
	    end
	| Struct_or_union_specifier_2 (c_struct_or_union, c_identifier) ->
	    begin
	      pp_open_box fm 0;
	      begin
		pp_print_c_struct_or_union fm c_struct_or_union;
		pp_print_c_space fm;
		pp_print_c_identifier fm c_identifier;
	      end;
	      pp_close_box fm ()
	    end
      in ()
    end;
    pp_close_box fm ()

and pp_print_c_struct_declaration_list: formatter -> c_struct_declaration list -> unit =
  fun fm l ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm pp_print_c_struct_declaration pp_print_c_newline l;
    pp_close_box fm ()

and pp_print_c_struct_declaration: formatter -> c_struct_declaration -> unit =
  fun fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Struct_declaration (c_specifier_qualifier_list, c_struct_declarator_list) ->
	  begin
	    pp_print_c_specifier_qualifier_list fm c_specifier_qualifier_list;
	    pp_print_c_space fm;
	    pp_print_c_struct_declarator_list fm c_struct_declarator_list;
	    pp_print_string fm ";"
	  end
    in
    pp_close_box fm ()

and pp_print_c_specifier_qualifier_list: formatter -> c_specifier_qualifier_list -> unit =
  fun fm expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Specifier_qualifier_list_1 (c_type_specifier, c_specifier_qualifier_list_opt) ->
	  begin
	    pp_print_c_type_specifier fm c_type_specifier;
	    pp_print_c_specifier_qualifier_list_opt fm c_specifier_qualifier_list_opt
	  end
      | Specifier_qualifier_list_2 (c_type_qualifier, c_specifier_qualifier_list_opt) ->
	  begin
	    pp_print_c_type_qualifier fm c_type_qualifier;
	    pp_print_c_specifier_qualifier_list_opt fm c_specifier_qualifier_list_opt
	  end
	    
      | Specifier_qualifier_list_GNU (gnu_attribute, c_specifier_qualifier_list_opt) ->
	  begin
	    pp_print_attribute fm gnu_attribute;
	    pp_print_space fm ();
	    pp_print_c_specifier_qualifier_list_opt fm c_specifier_qualifier_list_opt
	  end
	    
    in
    pp_close_box fm ()

and pp_print_c_struct_declarator_list: formatter -> c_struct_declarator list -> unit =
  fun fm l ->
    pp_open_box fm 0;
    Mlite_printer.pp_print_list fm 
      pp_print_c_struct_declarator
      (fun fm -> pp_print_string fm ","; pp_print_c_space fm ) l;
    pp_close_box fm ()

and pp_print_c_struct_declarator: formatter -> c_struct_declarator -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    let _ = match expr with
      | Struct_declarator_1 c_declarator ->
	  begin
	    pp_print_c_declarator fm c_declarator
	  end
      | Struct_declarator_2 (c_declarator_opt, c_constant_expression) ->
	  begin
	    pp_open_box fm 0;
	    pp_print_c_declarator_opt fm c_declarator_opt;
	    pp_print_string fm " : ";
	    pp_print_c_constant_expression fm c_constant_expression;
	    pp_close_box fm ()
	  end		
      | Struct_declarator_GNU (c_struct_declarator, attributes) ->
	  begin
	    pp_open_box fm 0;
	    pp_print_c_struct_declarator fm c_struct_declarator;
	    pp_print_space fm ();
	    Mlite_printer.pp_print_list fm 
	      pp_print_attribute 
	      (fun fm -> pp_print_space fm ())
	      attributes;
	    pp_close_box fm ()
	  end

    in
    pp_close_box fm ()

and pp_print_c_enum_specifier: formatter -> c_enum_specifier -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    let _ = match expr with
      | Enum_specifier_1 (c_identifier_opt, c_enumerator_list) ->
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "enum";
	      pp_print_c_identifier_opt fm c_identifier_opt;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    (** { **)
	    pp_open_vbox fm indent;
	    begin
	      pp_print_space fm ();
	      pp_print_c_enumerator_list fm c_enumerator_list;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    (** } **)
	    pp_print_string fm "}";
	  end
	    
      | Enum_specifier_2 c_identifier ->
	  begin
	    pp_open_box fm 0;
	    pp_print_string fm "enum";
	    pp_print_c_space fm;
	    pp_print_c_identifier fm c_identifier;
	    pp_close_box fm ();
	  end
    in
    pp_close_box fm ()

and pp_print_c_enumerator_list: formatter -> c_enumerator list -> unit =
  fun fm l ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm 
      (fun fm c_enumerator -> pp_print_c_enumerator fm c_enumerator
      )
      (fun fm -> pp_print_string fm ","; pp_print_space fm ())
      l;
    pp_close_box fm ()

and pp_print_c_enumerator: formatter -> c_enumerator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Enumerator_1 c_enumeration_constant -> 
	  pp_print_c_enumeration_constant fm c_enumeration_constant
      | Enumerator_2 (c_enumeration_constant, c_constant_expression) ->
	  begin
	    pp_print_c_enumeration_constant fm c_enumeration_constant;
	    pp_print_c_space fm;
	    pp_print_string fm "=";
	    pp_print_c_space fm;
	    pp_print_c_constant_expression fm c_constant_expression
	  end
    in
    pp_close_box fm ()

and pp_print_c_declarator: formatter -> c_declarator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Declarator (c_pointer_opt, c_direct_declarator) ->
	  begin
	    pp_print_c_pointer_opt fm c_pointer_opt;
	    pp_print_c_direct_declarator fm c_direct_declarator;
	  end
      | Declarator_GNU (declarator, attribute_list) ->
	  begin
	    pp_print_c_declarator fm declarator;
	    pp_print_gnu_attribute_list fm attribute_list
	  end
    in
    pp_close_box fm ()

and pp_print_c_direct_declarator: formatter -> c_direct_declarator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Direct_declarator_1 c_identifier ->
	  pp_print_c_identifier fm c_identifier
      | Direct_declarator_2 c_declarator ->
	  begin
	    pp_print_string fm "(";
	    pp_print_c_declarator fm c_declarator;
	    pp_print_string fm ")";
	  end
      | Direct_declarator_3 (c_direct_declarator, 
	c_type_qualifier_list_opt, 
	c_assignment_expression_opt) 
	->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "[";
	    pp_print_c_type_qualifier_list_opt fm c_type_qualifier_list_opt;
	    pp_print_c_assignment_expression_opt fm c_assignment_expression_opt;
	    pp_print_string fm "]"
	  end
      | Direct_declarator_4_STATIC (c_direct_declarator, 
	c_type_qualifier_list_opt, 
	c_assignment_expression) 
	->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "[";
	    pp_print_string fm "static";
	    pp_print_c_space fm;
	    pp_print_c_type_qualifier_list_opt fm c_type_qualifier_list_opt;
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression;
	    pp_print_string fm "]"
	  end
      | Direct_declarator_5_STATIC (c_direct_declarator, 
	c_type_qualifier_list, 
	c_assignment_expression) 
	->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "[";
	    pp_print_c_type_qualifier_list fm c_type_qualifier_list;
	    pp_print_c_space fm;
	    pp_print_string fm "static";
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression;
	    pp_print_string fm "]"
	  end
      | Direct_declarator_6_STAR (c_direct_declarator, c_type_qualifier_list_opt) ->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "[";
	    pp_print_c_type_qualifier_list_opt fm c_type_qualifier_list_opt;
	    pp_print_c_space fm;
	    pp_print_string fm "*";
	    pp_print_string fm "]"
	  end
      | Direct_declarator_7 (c_direct_declarator, c_parameter_type_list) ->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_parameter_type_list fm c_parameter_type_list;
	    pp_print_string fm ")"
	  end
      | Direct_declarator_8 (c_direct_declarator, c_identifier_list_opt) ->
	  begin
	    pp_print_c_direct_declarator fm c_direct_declarator;
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_identifier_list_opt fm c_identifier_list_opt;
	    pp_print_string fm ")"
	  end
    in
    pp_close_box fm ()

and pp_print_c_pointer: formatter -> c_pointer -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Pointer_1 c_type_qualifier_list_opt ->
	  begin
	    pp_print_string fm "*";
	    pp_print_space fm ();
	    pp_print_c_type_qualifier_list_opt fm c_type_qualifier_list_opt;
	  end
      | Pointer_2 (c_type_qualifier_list_opt, c_pointer) ->
	  begin
	    pp_print_string fm "*";
	    pp_print_space fm ();
	    pp_print_c_type_qualifier_list_opt fm c_type_qualifier_list_opt;
	    pp_print_space fm ();
	    pp_print_c_pointer fm c_pointer
	  end
    in
    pp_close_box fm ()

and pp_print_c_type_qualifier_list: formatter -> c_type_qualifier list -> unit =
  fun fm l ->
    pp_open_box fm 0;
    Mlite_printer.pp_print_list fm pp_print_c_type_qualifier pp_print_c_space l;
    pp_close_box fm ()

and pp_print_c_parameter_type_list: formatter -> c_parameter_type_list -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Parameter_type_list_FIX c_parameter_list ->
	  begin
	    pp_print_c_parameter_list fm c_parameter_list
	  end
      | Parameter_type_list_VAR c_parameter_list ->
	  begin
	    pp_print_c_parameter_list fm c_parameter_list;
	    pp_print_string fm ",";
	    pp_print_c_space fm;
	    pp_print_string fm "...";
	  end
    in
    pp_close_box fm ()


and pp_print_c_parameter_list: formatter -> c_parameter_list -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Parameter_list l -> 
	  Mlite_printer.pp_print_list fm 
	    pp_print_c_parameter_declaration
	    (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	    l
    in 
    pp_close_box fm ()

and pp_print_c_parameter_declaration: formatter -> c_parameter_declaration -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Parameter_declaration_1 (c_declaration_specifiers, c_declarator) ->
	  begin
	    pp_print_c_declaration_specifiers fm c_declaration_specifiers;
	    pp_print_c_space fm;
	    pp_print_c_declarator fm c_declarator
	  end
      | Parameter_declaration_2 (c_declaration_specifiers, c_abstract_declarator_opt) ->
	  begin
	    pp_print_c_declaration_specifiers fm c_declaration_specifiers;
	    pp_print_c_abstract_declarator_opt fm c_abstract_declarator_opt
	  end
    in
    pp_close_box fm ()

and pp_print_c_identifier_list: formatter -> c_identifier list -> unit =
  fun fm l ->
    pp_open_box fm 0;
    Mlite_printer.pp_print_list fm pp_print_c_identifier pp_print_c_comma l;
    pp_close_box fm ()

and pp_print_c_type_name: formatter -> c_type_name -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Type_name (c_specifier_qualifier_list, c_abstract_declarator_opt) ->
	  begin
	    pp_print_c_specifier_qualifier_list fm c_specifier_qualifier_list;
	    pp_print_c_abstract_declarator_opt fm c_abstract_declarator_opt
	  end
    in
    pp_close_box fm ()

and pp_print_c_abstract_declarator: formatter -> c_abstract_declarator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Abstract_declarator_1 c_pointer -> pp_print_c_pointer fm c_pointer;
      | Abstract_declarator_2 (c_pointer_opt, c_direct_abstract_declarator) ->
	  begin
	    pp_print_c_pointer_opt fm c_pointer_opt;
	    pp_print_c_space fm;
	    pp_print_c_direct_abstract_declarator fm c_direct_abstract_declarator
	  end
    in
    pp_close_box fm ()

and pp_print_c_direct_abstract_declarator: formatter -> c_direct_abstract_declarator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Direct_abstract_declarator_error -> assert false
      | Direct_abstract_declarator_1 c_abstract_declarator ->
	  begin
	    pp_print_string fm "(";
	    pp_print_c_abstract_declarator fm c_abstract_declarator;
	    pp_print_string fm ")";
	  end
      | Direct_abstract_declarator_2 (c_direct_abstract_declarator_opt, c_assignment_expression_opt) ->
	  begin
	    pp_print_c_direct_abstract_declarator_opt fm c_direct_abstract_declarator_opt;
	    pp_print_string fm "[";
	    pp_print_c_assignment_expression_opt fm c_assignment_expression_opt;
	    pp_print_string fm "]";
	  end
      | Direct_abstract_declarator_3_STAR c_direct_abstract_declarator_opt  ->
	  begin
	    pp_print_c_direct_abstract_declarator_opt fm c_direct_abstract_declarator_opt;
	    pp_print_c_space fm;
	    pp_print_string fm "[*]"
	  end
      | Direct_abstract_declarator_4 (c_direct_abstract_declarator_opt, c_parameter_type_list_opt) ->
	  begin
	    pp_print_c_direct_abstract_declarator_opt fm c_direct_abstract_declarator_opt;
	    pp_print_string fm "(";
	    pp_print_c_parameter_type_list_opt fm c_parameter_type_list_opt;
	    pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_initializer: formatter -> c_initializer -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    let _ = match expr with
      | Initializer_1 c_assignment_expression ->
	  begin
	    pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression
	  end
      | Initializer_2 c_initializer_list  ->
	  begin
	    pp_print_string fm "{";
	    pp_print_space fm ();
	    pp_open_vbox fm indent;
	    begin
	      pp_print_c_initializer_list fm c_initializer_list;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}"
	  end
    in
    pp_close_box fm ()

and pp_print_c_initializer_list: formatter -> c_initializer_list -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    begin
      let _ = match expr with
	| Initializer_list l ->
	    Mlite_printer.pp_print_list fm 
	      (fun fm (c_designation_opt, c_initializer) -> 
		pp_print_c_designation_opt fm c_designation_opt;
		pp_print_c_initializer fm c_initializer
	      ) 
	      pp_print_c_comma
	      l
      in ()
    end;
    pp_close_box fm ()


and pp_print_c_designation: formatter -> c_designation -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Designation c_designator_list ->
	  begin
	    pp_print_c_designator_list fm c_designator_list;
	    pp_print_cut fm ();
	    pp_print_string fm "="
	  end
    in
    pp_close_box fm ()


and pp_print_c_designator_list: formatter -> c_designator list -> unit =
  fun fm l ->
    pp_open_box fm 0;
    Mlite_printer.pp_print_list fm 
      pp_print_c_designator 
      (fun fm -> ())
      l;
    pp_close_box fm ()


and pp_print_c_designator: formatter -> c_designator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Designator_1 c_constant_expression ->
	  begin
	    pp_print_string fm "[";
	    pp_print_c_constant_expression fm c_constant_expression;
	    pp_print_string fm "]"
	  end
      | Designator_2 c_identifier ->
	  begin
	    pp_print_string fm ".";
	    pp_print_c_identifier fm c_identifier;
	  end
      | Designator_gnu_range (e0, e1) ->
	  begin
	    pp_print_string fm "[";
	    pp_print_c_constant_expression fm e0;
	    pp_print_string fm " ... ";
	    pp_print_c_constant_expression fm e1;
	    pp_print_string fm "]"
	  end
    in
    pp_close_box fm ()

and pp_print_c_declaration_list: formatter -> c_declaration list -> unit =
  fun fm l ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm pp_print_c_declaration pp_print_c_newline l;
    pp_close_box fm ()
      
and pp_print_c_declaration_list_opt fm expr = 
  match expr with
    | Some c_declaration_list ->
	pp_print_cut fm ();
	pp_print_c_declaration_list fm c_declaration_list 
    | None -> ()

and pp_print_c_argument_expression_list_opt fm expr =
  print_opt pp_print_c_argument_expression_list fm expr

    
and pp_print_c_init_declarator_list_opt fm expr = 
  match expr with
    | Some c_init_declarator_list -> 
	pp_print_c_space fm;
	pp_print_c_init_declarator_list fm c_init_declarator_list
    | None -> ()
	
and pp_print_c_declaration_specifiers_opt fm expr = 
  match expr with
    | Some c_declaration_specifiers ->
	pp_print_c_space fm;
	pp_print_c_declaration_specifiers fm c_declaration_specifiers
    | None -> ()

and pp_print_c_declarator_opt fm expr = 
  match expr with
    | Some c_declarator ->
	pp_print_c_space fm;
	pp_print_c_declarator fm c_declarator
    | None -> ()
	
and pp_print_c_identifier_opt fm expr =
  match expr with
    | Some c_identifier -> 
	pp_print_c_space fm;
	pp_print_c_identifier fm c_identifier
    | None -> ()
	
and pp_print_c_specifier_qualifier_list_opt fm expr = 
  match expr with
    | Some c_specifier_qualifier_list ->
	pp_print_c_space fm;
	pp_print_c_specifier_qualifier_list fm c_specifier_qualifier_list
    | None -> ()
	
and pp_print_c_assignment_expression_opt fm expr =
  match expr with 
    | Some c_assignment_expression ->
	pp_print_c_space fm;
	pp_print_c_assignment_expression ~need_paren:true fm c_assignment_expression
    | None -> ()
	
and pp_print_c_abstract_declarator_opt fm expr = 
  match expr with
    | Some c_abstract_declarator ->
	pp_print_c_space fm;
	pp_print_c_abstract_declarator fm c_abstract_declarator 
    | None -> ()
	
and pp_print_c_type_qualifier_list_opt fm expr = 
  match expr with
    | Some c_type_qualifier_list ->
	pp_print_c_space fm;
	pp_print_c_type_qualifier_list fm c_type_qualifier_list
    | None -> ()
	
and pp_print_c_identifier_list_opt fm expr = 
  match expr with
    | Some c_identifier_list -> 
	pp_print_c_space fm;
	pp_print_c_identifier_list fm c_identifier_list 
    | None -> ()
	
and pp_print_c_expression_opt:need_paren:bool -> formatter -> c_expression option -> unit = 
  fun ~need_paren fm expr_opt -> 
    match expr_opt with 
      | Some c_expression ->
	  pp_print_c_space fm;
	  pp_print_c_expression ~need_paren fm c_expression 
      | None -> ()

and pp_print_c_pointer_opt fm expr = 
  match expr with
    | Some c_pointer -> 
	pp_print_c_space fm;
	pp_print_c_pointer fm c_pointer
    | None -> ()
	
and pp_print_c_direct_abstract_declarator_opt fm expr = 
  match expr with
    | Some c_direct_abstract_declarator ->
	pp_print_c_space fm;
	pp_print_c_direct_abstract_declarator fm c_direct_abstract_declarator
    | None -> ()
	
and pp_print_c_parameter_type_list_opt fm expr = 
  match expr with
    | Some c_parameter_type_list ->
	pp_print_c_space fm;
	pp_print_c_parameter_type_list fm c_parameter_type_list 
    | None -> ()

and pp_print_c_designation_opt fm expr =
  match expr with
    | Some c_designation -> 
	pp_print_c_space fm;
	pp_print_c_designation fm c_designation 
    | None -> ()

and pp_print_asm (fm:formatter) (str_list, asm_details_opt) = 
  pp_open_vbox fm 0;
  begin
    pp_print_string fm "__asm__";
    pp_print_space fm ();
    pp_print_string fm "(";
    pp_print_space fm ();
    let _ = Mlite_printer.pp_print_list fm 
      (fun fm s -> pp_print_string fm ("\"" ^ s ^ "\""))
      pp_print_c_newline str_list
    in
    let _ = match asm_details_opt with
      | Some asm ->
	  pp_print_space fm ();
	  pp_print_string fm ":";
	  Mlite_printer.pp_print_list fm 
	    (fun fm (so, s,e) ->
	      pp_print_string fm ("\"" ^ s ^ "\"");
	      pp_print_string fm " (";
	      pp_print_c_expression ~need_paren:false fm e;
	      pp_print_string fm ")")
	    (fun fm -> pp_print_string fm ",") asm.asm_outputs;
	  pp_print_space fm ();
	  pp_print_string fm ":";
	  Mlite_printer.pp_print_list fm 
	    (fun fm (so, s,e) ->
	      pp_print_string fm ("\"" ^ s ^ "\"");
	      pp_print_string fm " (";
	      pp_print_c_expression ~need_paren:false fm e;
	      pp_print_string fm ")")
	    (fun fm -> pp_print_string fm ",") asm.asm_inputs;
	  pp_print_space fm ();
	  if (asm.asm_clobbers <> []) then
	    begin
	      pp_print_string fm ":";
	      Mlite_printer.pp_print_list fm 
		(fun fm s ->
		  pp_print_string fm ("\"" ^ s ^ "\""))
		(fun fm -> pp_print_string fm ",") asm.asm_clobbers;
	    end
      | None -> ()
    in
    pp_print_string fm ");";
  end;
  pp_close_box fm ()

and pp_print_c_statement (fm:formatter) (c_statement:c_statement) =
  pp_open_hovbox fm 0;
  let _ = match c_statement with
    | Statement_at (coord, c_statement) ->
	Coordinate.pp_print_t fm coord;
	pp_print_c_statement fm c_statement;
    | Statement_1 c_labeled_statement ->
	pp_print_c_labeled_statement fm c_labeled_statement
    | Statement_2 (c_compound_statement) ->
	pp_print_c_compound_statement fm c_compound_statement
    | Statement_3 (c_expression_statement)  ->
	pp_print_c_expression_statement fm c_expression_statement 
    | Statement_4 (c_selection_statement) -> 
	pp_print_c_selection_statement fm c_selection_statement
    | Statement_5 (c_iteration_statement) ->
	pp_print_c_iteration_statement fm c_iteration_statement
    | Statement_6 (c_jump_statement) ->
	pp_print_c_jump_statement fm c_jump_statement
    | Statement_asm (str_list, asm_details_opt) ->
	pp_print_asm fm (str_list, asm_details_opt)
  in
  pp_close_box fm ()
    
and pp_print_c_labeled_statement 
    (fm:formatter) (expr:c_labeled_statement) =
  pp_open_vbox fm 0;
  let _ = match expr with
    | Labeled_statement_1 (c_identifier, c_statement) ->
	begin
	  pp_print_c_identifier fm c_identifier;
	  pp_print_string fm ":";
	  pp_print_cut fm ();
	  pp_print_c_statement fm c_statement
	end
	  
    | Labeled_statement_2_case (c_constant_expression, c_statement) ->
	begin
	  pp_print_string fm "case ";
	  pp_print_c_constant_expression fm c_constant_expression;
	  pp_print_string fm ":";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement;
	  pp_close_box fm ()
	end

    | Labeled_statement_3_default c_statement ->
	begin
	  pp_print_string fm "default:";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement;
	  pp_close_box fm ()
	end
	  
    | Labeled_statement_gnu_case_range (e0, e1, c_statement) ->
	begin
	  pp_print_string fm "case ";
	  pp_print_c_constant_expression fm e0;
	  pp_print_string fm " ... ";
	  pp_print_c_constant_expression fm e1;
	  pp_print_string fm ":";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement;
	  pp_close_box fm ()
	end
  in
  pp_close_box fm ()

and pp_print_c_compound_statement: formatter -> c_compound_statement -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    let _ = match expr with
      | Compound_statement (labels, c_block_item_list_opt) ->
	  pp_print_string fm "{";
	  pp_open_vbox fm (indent - 1);
	  if labels <> [] then
	    begin
	      pp_print_string fm "__label__ ";
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm pp_print_string
		(fun fm -> pp_print_string fm ",") labels;
	      pp_print_string fm ";";
	    end;
	  
	  pp_print_c_block_item_list_opt fm c_block_item_list_opt;
	  pp_close_box fm ();
	  pp_print_cut fm ();
	  pp_print_string fm "}"
    in
    pp_close_box fm ()

and pp_print_c_block_item_list: formatter -> c_block_item list -> unit =
  fun fm l ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm pp_print_c_block_item 
      pp_print_c_newline l;
    pp_close_box fm ()

and pp_print_c_block_item (fm:formatter) = function
  | Block_item_1 c_declaration ->
      pp_print_c_declaration fm c_declaration
  | Block_item_2 c_statement ->
      pp_print_c_statement fm c_statement
	
and pp_print_c_expression_statement (fm:formatter) = function 
  | Expression_statement c_expression_opt ->
      Mapping.apply_opt 
	(fun c_expression -> 
	  pp_print_cut fm ();
	  pp_print_c_expression ~need_paren:false fm c_expression) c_expression_opt;
      pp_print_string fm ";"
	
and pp_print_c_selection_statement (fm:formatter) (expr:c_selection_statement) = 
  pp_open_vbox fm 0;
  let _ = match expr with
    | Selection_statement_1_if (c_expression, c_statement) ->
	begin
	  pp_print_string fm "if (";
	  pp_print_c_expression ~need_paren:false fm c_expression;
	  pp_print_string fm ")";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement;
	  pp_close_box fm ()
	end
    | Selection_statement_2_if_else (c_expression, c_statement1, c_statement2) ->
	begin
	  pp_print_string fm "if (";
	  pp_print_c_expression ~need_paren:false fm c_expression;
	  pp_print_string fm ")";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement1;
	  pp_close_box fm ();
	  pp_print_cut fm ();
	  pp_print_string fm "else";
	  pp_print_cut fm ();
	  pp_open_vbox fm indent;
	  pp_print_c_statement fm c_statement2;
	  pp_close_box fm ()
	end
    | Selection_statement_3_switch (c_expression, c_statement) ->
	begin
	  pp_print_string fm "switch (";
	  pp_print_c_expression ~need_paren:false fm c_expression;
	  pp_print_string fm ")";
	  pp_print_cut fm ();
	  pp_print_c_statement fm c_statement
	end
  in
  pp_close_box fm ()

and pp_print_c_iteration_statement: formatter -> c_iteration_statement -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    let _ = match expr with
      | Iteration_statement_1_while (c_expression, c_statement) ->
	  begin
	    pp_print_string fm "while (";
	    pp_print_c_expression ~need_paren:true fm c_expression;
	    pp_print_string fm ")";
	    pp_print_cut fm ();
	    pp_print_c_statement fm c_statement
	  end
      | Iteration_statement_2_do (c_statement, c_expression) ->
	  begin
	    pp_print_string fm "do";
	    pp_print_cut fm ();
	    pp_print_c_statement fm c_statement;
	    pp_print_string fm "while (";
	    pp_print_c_expression ~need_paren:true fm c_expression;
	    pp_print_string fm ");"
	  end
      | Iteration_statement_3_for (c_expression_opt1, c_expression_opt2, 
	c_expression_opt3, c_statement) 
	->
	  begin
	    pp_print_string fm "for (";
	    pp_print_c_expression_opt ~need_paren:false fm c_expression_opt1;
	    pp_print_string fm ";";
	    pp_print_c_expression_opt ~need_paren:true fm c_expression_opt2;
	    pp_print_string fm ";";
	    pp_print_c_expression_opt ~need_paren:false fm c_expression_opt3;
	    pp_print_string fm ")";
	    pp_print_cut fm ();
	    pp_print_c_statement fm c_statement;
	  end
      | Iteration_statement_4_for 
	  (c_declaration, c_expression_opt1, c_expression_opt2, c_statement) ->
	  begin
	    pp_print_string fm "for (";
	    pp_print_c_declaration fm c_declaration;
	    pp_print_c_expression_opt ~need_paren:true fm c_expression_opt1;
	    pp_print_string fm ";";
	    pp_print_c_expression_opt ~need_paren:false fm c_expression_opt2;
	    pp_print_string fm ")";
	    pp_print_space fm ();
	    pp_print_c_statement fm c_statement;
	  end
    in
    pp_close_box fm ()

and pp_print_c_jump_statement: formatter -> c_jump_statement -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Jump_statement_1_goto c_identifier ->
	  begin
	    pp_print_string fm "goto";
	    pp_print_c_space fm;
	    pp_print_c_identifier fm c_identifier;
	    pp_print_string fm ";"
	  end
      | Jump_statement_2_continue ->
	  pp_print_string fm "continue;"
      | Jump_statement_3_break ->
	  pp_print_string fm "break;"
      | Jump_statement_4_return_expression c_expression ->
	  begin
	    pp_print_string fm "return ";
	    pp_print_c_expression ~need_paren:true fm c_expression;
	    pp_print_string fm ";"
	  end
	    
      | Jump_statement_5_return ->
	  pp_print_string fm "return;"
	    
      | Jump_statement_gnu_goto expr ->
	  begin
	    pp_print_string fm "goto ";
	    pp_print_c_space fm;
	    pp_print_string fm "*";
	    pp_print_c_expression ~need_paren:true fm expr;
	    pp_print_string fm ";"
	  end
    in
    pp_close_box fm ()
      
and pp_print_c_block_item_list_opt fm expr = 
  match expr with
    | Some c_block_item_list -> 
	pp_print_cut fm ();
	pp_print_c_block_item_list fm c_block_item_list 
    | None -> ()
	
and pp_print_c_external_declaration (fm:formatter) (expr:c_external_declaration) = 
  pp_open_box fm 0;
  begin
    match expr with
      | External_declaration_at (coord, expr) ->
	  Coordinate.pp_print_t fm coord;
	  pp_print_c_external_declaration fm expr
      | External_declaration_1 (c_function_definition) ->
	  pp_print_c_function_definition fm c_function_definition
      | External_declaration_2 (c_declaration) ->
	  pp_print_c_declaration fm c_declaration
  end;
  pp_close_box fm ()
    
and pp_print_c_function_definition (fm:formatter) (expr:c_function_definition) =
  pp_open_vbox fm 0;
  begin
    match expr with
      | Function_definition (c_declaration_specifiers,
	c_declarator,
	c_declaration_list_opt,
	c_compound_statement) 
	->
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_c_declaration_specifiers fm c_declaration_specifiers;
	      pp_print_space fm ();
	      pp_print_c_declarator fm c_declarator;
	    end;
	    pp_close_box fm ();
	    pp_print_c_declaration_list_opt fm c_declaration_list_opt;
	    pp_print_space fm ();
	    pp_print_c_compound_statement fm c_compound_statement;
	  end
  end;
  pp_close_box fm ()

and pp_print_c_file_unit (fm:formatter) (c_file_unit:c_file_unit):unit =
  pp_open_vbox fm 0;
  begin
    match c_file_unit with
      | Translation_unit l ->
	  Mlite_printer.pp_print_list fm 
	    pp_print_c_external_declaration
	    (fun fm -> pp_print_cut fm (); pp_print_cut fm ())
	    l
  end;
  pp_close_box fm ()
