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

open Ast_ba_stmt
open C_syntax_symbol_printer
open Format
open Pretty_printer

type c_file_unit = Ast_ba_stmt.c_translation_unit
let description () = description
let suffix () = suffix

let indent = 2

let pp_print_c_space = print_c_space
let pp_print_c_comma = print_c_comma
let pp_print_c_newline = print_c_newline

let pp_print_c_identifier: formatter -> c_identifier -> unit =
  fun fm (Identifier v) ->
    pp_open_box fm 0;
    pp_print_string fm v;
    pp_close_box fm ()

let pp_print_c_identifier_opt fm id =
  match id with
  | Some c_identifier -> 
      pp_print_c_space fm;
      pp_print_c_identifier fm c_identifier
  | None -> ()


let pp_print_c_identifier_as_typ: formatter -> c_identifier_as_typ -> unit =
  fun fm (Identifier_as_typ v) ->
    pp_open_box fm 0;
    pp_print_string fm v;
    pp_close_box fm ()


let pp_print_c_enumeration_constant: formatter -> c_enumeration_constant -> unit =
  fun fm (Enumeration_constant v) ->
    pp_open_box fm 0;
    pp_print_c_identifier fm v;
    pp_close_box fm ()

let pp_print_c_constant = Ast_aa_gram_printer.pp_print_c_constant
let pp_print_c_string_literal = Ast_aa_gram_printer.pp_print_c_string_literal
let pp_print_c_unary_operator = C_syntax_symbol_printer.print_c_unary_operator
let pp_print_c_assignment_operator = C_syntax_symbol_printer.print_c_assignment_operator
let pp_print_c_storage_class_specifier = C_syntax_symbol_printer.print_c_storage_class_specifier
let pp_print_c_struct_or_union = C_syntax_symbol_printer.print_c_struct_or_union
let pp_print_c_type_qualifier = C_syntax_symbol_printer.print_c_type_qualifier
let pp_print_c_function_specifier = C_syntax_symbol_printer.print_c_function_specifier

let pp_print_c_typedef_name: formatter -> c_typedef_name -> unit =
  fun fm (Typedef_name v) ->
    pp_open_box fm 0;
    pp_print_c_identifier_as_typ fm v;
    pp_close_box fm ()


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
	


let rec pp_print_attribute (fm:formatter) ((str,expr_list):gnu_attribute) =
  pp_print_string fm str;
  pp_print_l_parens fm str;
  Mlite_printer.pp_print_list fm 
    (fun fm v -> pp_print_c_expression fm ~need_paren:false v)
    (fun fm -> pp_print_space fm ())
    expr_list;
  pp_print_r_parens fm str
      
and pp_print_c_primary_expression: formatter -> need_paren:bool -> 
      c_primary_expression -> unit =
  fun fm ~need_paren expr ->
    pp_open_box fm 0;
    let _ = match expr with
      | Primary_expression_1 c_identifier ->
	  pp_print_c_identifier fm c_identifier
      | Primary_expression_2 c_constant ->
	  pp_print_c_constant fm c_constant
      | Primary_expression_3 c_string_literal ->
	  pp_print_c_string_literal fm c_string_literal
      | Primary_expression_4 c_expression ->
	  begin
	    pp_print_c_expression fm ~need_paren:true c_expression;
	  end
      | Primary_expression_macro_va_start (c_expression1, c_expression2) ->
	  begin
	    pp_print_string fm "__builtin_va_start";
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:true c_expression1;
	    pp_print_string fm ",";
	    pp_print_c_expression fm ~need_paren:true c_expression2;
	    pp_print_string fm ")"
	  end
      | Primary_expression_macro_va_arg (c_expression, c_type_name) ->
	  begin
	    pp_print_string fm "__builtin_va_arg";
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:true c_expression;
	    pp_print_string fm ",";
	    pp_print_c_type_name fm c_type_name;
	    pp_print_string fm ")"
	  end
      | Primary_expression_macro_va_end (c_expression1) ->
	  begin
	    pp_print_string fm "__builtin_va_end";
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:true c_expression1;
	    pp_print_string fm ")"
	  end
	    
      | Primary_expression_builtin_types_compatible (t0, t1) ->
	  begin
	    pp_print_string fm "__builtin_types_compatible_p";
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_type_name fm t0;
	    pp_print_string fm ",";
	    pp_print_c_type_name fm t1;
	    pp_print_string fm ")"
	  end

      | Primary_expression_builtin_constant_p (e0) ->
	  begin
	    pp_print_string fm "__builtin_constant_p";
	    pp_print_space fm ();
	    pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:false e0;
	    pp_print_string fm ")"
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
	    pp_print_string fm "__extension__({";
	    pp_print_space fm ();
	    pp_print_c_compound_stmt010 fm c_compound_statement;
	    pp_print_string fm "})";
	  end
	    
      | Primary_expression_gnu_labeladdr c_identifier ->
	  pp_print_string fm "&&";
	  pp_print_c_identifier fm c_identifier
    in
    pp_close_box fm ()
      
and pp_print_c_postfix_expression: formatter -> need_paren:bool -> 
      c_postfix_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Postfix_expression_1 c_primary_expression ->
	  pp_print_c_primary_expression fm ~need_paren c_primary_expression
      | Postfix_expression_2 (c_postfix_expression, c_expression) ->
	  if need_paren then
	    pp_print_string fm "(";
	  pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
	  pp_print_string fm "[";
	  pp_print_c_expression fm ~need_paren:false c_expression;
	  pp_print_string fm "]";
	  if need_paren then
	    pp_print_string fm ")";
      | Postfix_expression_3 (c_postfix_expression, c_argument_expression_list_opt) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "(";
	    pp_print_c_argument_expression_list_opt fm c_argument_expression_list_opt;
	    pp_print_string fm ")";
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Postfix_expression_4_DOT (c_postfix_expression, c_identifier) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
	    pp_print_string fm ".";
	    pp_print_c_identifier fm c_identifier;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Postfix_expression_5_ARROW (c_postfix_expression, c_identifier) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
	    pp_print_string fm "->";
	    pp_print_c_identifier fm c_identifier;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Postfix_expression_6_PLUS_PLUS c_postfix_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
	    pp_print_string fm "++";
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Postfix_expression_7_MINUS_MINUS c_postfix_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_postfix_expression fm ~need_paren:true c_postfix_expression;
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
  fun fm (Argument_expression_list l) ->
    pp_open_hovbox fm 0;
    begin
      Mlite_printer.pp_print_list fm 
	(fun fm c_assignment_expression ->
	  pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression
	)
	(fun fm -> 
	  pp_print_string fm ",";
	  pp_print_c_space fm
	)
	l
    end;
    pp_close_box fm ()

and pp_print_c_unary_expression: formatter -> need_paren:bool -> 
      c_unary_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Unary_expression_1 c_postfix_expression ->
	  pp_print_c_postfix_expression fm ~need_paren c_postfix_expression
      | Unary_expression_2_PLUS_PLUS c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "++";
	    pp_print_c_unary_expression fm ~need_paren:true c_unary_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_3_MINUS_MINUS c_unary_expression ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "--";
	    pp_print_c_unary_expression fm ~need_paren:true c_unary_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Unary_expression_4 (c_unary_operator, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_unary_operator fm c_unary_operator;
	    pp_print_c_cast_expression fm ~need_paren:true c_cast_expression;
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
	    pp_print_c_unary_expression fm ~need_paren:true c_unary_expression;
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
	    pp_print_c_unary_expression fm ~need_paren:true c_unary_expression;
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

and pp_print_c_cast_expression: formatter -> need_paren:bool -> c_cast_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Cast_expression_1 c_unary_expression ->
	  pp_print_c_unary_expression fm ~need_paren c_unary_expression
      | Cast_expression_2 (c_type_name, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_string fm "(";
	    pp_print_c_type_name fm c_type_name;
	    pp_print_string fm ")";
	    pp_print_c_cast_expression fm ~need_paren:true c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_multiplicative_expression: formatter -> need_paren:bool -> 
      c_multiplicative_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Multiplicative_expression_1 c_cast_expression ->
	  pp_print_c_cast_expression fm ~need_paren c_cast_expression
      | Multiplicative_expression_2_STAR (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression fm ~need_paren:true c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "*";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression fm ~need_paren:true c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Multiplicative_expression_3_SLASH (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression fm ~need_paren:true c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "/";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression fm ~need_paren:true c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Multiplicative_expression_4_PERCENT (c_multiplicative_expression, c_cast_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_multiplicative_expression fm ~need_paren:true c_multiplicative_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "%";
	    pp_print_c_space fm;
	    pp_print_c_cast_expression fm ~need_paren:true c_cast_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_additive_expression: formatter -> need_paren:bool -> c_additive_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Additive_expression_1 c_multiplicative_expression ->
	  begin
	    pp_print_c_multiplicative_expression fm ~need_paren c_multiplicative_expression
	  end
      | Additive_expression_2_PLUS (c_additive_expression, c_multiplicative_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_additive_expression fm ~need_paren:true c_additive_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "+";
	    pp_print_c_space fm;
	    pp_print_c_multiplicative_expression fm ~need_paren:true c_multiplicative_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Additive_expression_3_MINUS (c_additive_expression, c_multiplicative_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_additive_expression fm ~need_paren:true c_additive_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "-";
	    pp_print_c_space fm;
	    pp_print_c_multiplicative_expression fm ~need_paren:true c_multiplicative_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_shift_expression: formatter -> need_paren:bool -> c_shift_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Shift_expression_1 c_additive_expression ->
	  begin
	    pp_print_c_additive_expression fm ~need_paren c_additive_expression
	  end
      | Shift_expression_2_INF_INF (c_shift_expression, c_additive_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<<";
	    pp_print_c_space fm;
	    pp_print_c_additive_expression fm ~need_paren:true c_additive_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Shift_expression_3_SUP_SUP (c_shift_expression, c_additive_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">>";
	    pp_print_c_space fm;
	    pp_print_c_additive_expression fm ~need_paren:true c_additive_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_relational_expression: formatter -> need_paren:bool -> c_relational_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Relational_expression_1 c_shift_expression ->
	  begin
	    pp_print_c_shift_expression fm ~need_paren c_shift_expression
	  end
      | Relational_expression_2_INF (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_3_SUP (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_4_INF_EQ (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "<=";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Relational_expression_5_SUP_EQ (c_relational_expression, c_shift_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ">=";
	    pp_print_c_space fm;
	    pp_print_c_shift_expression fm ~need_paren:true c_shift_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_equality_expression: formatter -> need_paren:bool -> c_equality_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Equality_expression_1 c_relational_expression ->
	  begin
	    pp_print_c_relational_expression fm ~need_paren c_relational_expression
	  end
      | Equality_expression_2_EQ_EQ (c_equality_expression, c_relational_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_equality_expression fm ~need_paren:true c_equality_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "==";
	    pp_print_c_space fm;
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Equality_expression_3_EXCLAM_EQ (c_equality_expression, c_relational_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_equality_expression fm ~need_paren:true c_equality_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "!=";
	    pp_print_c_space fm;
	    pp_print_c_relational_expression fm ~need_paren:true c_relational_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_and_expression: formatter -> need_paren:bool -> c_and_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | And_expression_1 c_equality_expression ->
	  begin
	    pp_print_c_equality_expression fm ~need_paren c_equality_expression
	  end
      | And_expression_2_AND (c_and_expression, c_equality_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_and_expression fm ~need_paren:true c_and_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "&";
	    pp_print_c_space fm;
	    pp_print_c_equality_expression fm ~need_paren:true c_equality_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_exclusive_or_expression: formatter -> need_paren:bool -> c_exclusive_or_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Exclusive_or_expression_1 c_and_expression ->
	  begin
	    pp_print_c_and_expression fm ~need_paren c_and_expression;
	  end
      | Exclusive_or_expression_2_CIRC (c_exclusive_or_expression, c_and_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_exclusive_or_expression fm ~need_paren:true  c_exclusive_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "^";
	    pp_print_c_space fm;
	    pp_print_c_and_expression fm ~need_paren:true c_and_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_inclusive_or_expression: formatter -> need_paren:bool -> c_inclusive_or_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Inclusive_or_expression_1 c_exclusive_or_expression ->
	  begin
	    pp_print_c_exclusive_or_expression fm ~need_paren c_exclusive_or_expression;
	  end
      | Inclusive_or_expression_2_PIPE (c_inclusive_or_expression, c_exclusive_or_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_inclusive_or_expression fm ~need_paren:true c_inclusive_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "|";
	    pp_print_c_space fm;
	    pp_print_c_exclusive_or_expression fm ~need_paren:true c_exclusive_or_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()
      
and pp_print_c_logical_and_expression: formatter -> need_paren:bool -> c_logical_and_expression -> 
      unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Logical_and_expression_1 c_inclusive_or_expression ->
	  begin
	    pp_print_c_inclusive_or_expression fm ~need_paren c_inclusive_or_expression;
	  end
      | Logical_and_expression_2_AND_AND (c_logical_and_expression, c_inclusive_or_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_and_expression fm ~need_paren:true c_logical_and_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "&&";
	    pp_print_c_space fm;
	    pp_print_c_inclusive_or_expression fm ~need_paren:true c_inclusive_or_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_logical_or_expression: formatter -> need_paren:bool -> c_logical_or_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Logical_or_expression_1 c_logical_and_expression ->
	  begin
	    pp_print_c_logical_and_expression fm ~need_paren c_logical_and_expression;
	  end
      | Logical_or_expression_2_PIPE_PIPE (c_logical_or_expression, c_logical_and_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_or_expression fm ~need_paren:true c_logical_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "||";
	    pp_print_c_space fm;
	    pp_print_c_logical_and_expression fm ~need_paren:true c_logical_and_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_conditional_expression: formatter -> need_paren:bool -> c_conditional_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Conditional_expression_1 c_logical_or_expression ->
	  begin
	    pp_print_c_logical_or_expression fm ~need_paren c_logical_or_expression;
	  end
      | Conditional_expression_2 (c_logical_or_expression, c_expression, c_conditional_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_logical_or_expression fm ~need_paren:true c_logical_or_expression;
	    pp_print_c_space fm;
	    pp_print_string fm "?";
	    pp_print_c_space fm;
	    pp_print_c_expression fm ~need_paren:true c_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ":";
	    pp_print_c_space fm;
	    pp_print_c_conditional_expression ~need_paren:true fm c_conditional_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
      | Conditional_expression_gnu (c_logical_or_expression, 
	c_conditional_expression) 
	->
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

and pp_print_c_assignment_expression: formatter -> need_paren:bool -> c_assignment_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Assignment_expression_1 c_conditional_expression ->
	  begin
	    pp_print_c_conditional_expression fm ~need_paren c_conditional_expression
	  end
      | Assignment_expression_2 (c_unary_expression, c_assignment_operator, c_assignment_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_unary_expression fm ~need_paren:false c_unary_expression;
	    pp_print_c_space fm;
	    pp_print_c_assignment_operator fm c_assignment_operator;
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression fm ~need_paren:false c_assignment_expression;
	    if need_paren then
	      pp_print_string fm ")";
	  end
    in
    pp_close_box fm ()

and pp_print_c_expression: formatter -> need_paren:bool -> c_expression -> unit = 
  fun fm ~need_paren expr ->
    pp_open_hovbox fm 0;
    let _ = match expr with
      | Expression_1 c_assignment_expression -> 
	  begin
	    pp_print_c_assignment_expression fm ~need_paren c_assignment_expression;
	  end
      | Expression_2 (c_expression, c_assignment_expression) ->
	  begin
	    if need_paren then
	      pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:true c_expression;
	    pp_print_c_space fm;
	    pp_print_string fm ",";
	    pp_print_c_space fm;
	    pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression;
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
	  pp_print_c_conditional_expression fm ~need_paren:false c_conditional_expression
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
  fun fm l ->
    pp_open_box fm 0;
    pp_print_c_space fm;
    Mlite_printer.pp_print_list fm 
      (fun fm c_init_declarator -> 
	pp_print_c_init_declarator fm c_init_declarator
      ) 
      (fun fm -> pp_print_string fm ","; pp_print_c_space fm)
      l;
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
	  pp_print_c_struct_or_union_specifier fm c_struct_or_union_specifier
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
		pp_print_c_struct_or_union fm c_struct_or_union;
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
	    pp_print_c_space fm;
	    pp_print_string fm ":";
	    pp_print_c_space fm;
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
	    pp_print_space fm ();
	    Mlite_printer.pp_print_list fm 
	      pp_print_attribute 
	      (fun fm -> pp_print_space fm ())
	      attribute_list;
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
	    pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression;
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
	    pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression;
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
	    pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression
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

and pp_print_c_argument_expression_list_opt fm expr =
  match expr with
    | Some c_argument_expression_list -> 
	pp_print_c_argument_expression_list fm c_argument_expression_list
    | None -> ()
	
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
	pp_print_c_assignment_expression fm ~need_paren:true c_assignment_expression
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

and pp_print_c_declaration_list_opt fm expr = 
  match expr with
    | Some c_declaration_list ->
	pp_print_cut fm ();
	pp_print_c_declaration_list fm c_declaration_list 
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
	  begin
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
	    if (asm.asm_clobbers <> []) then
	      begin
		pp_print_space fm ();
		pp_print_string fm ":";
		Mlite_printer.pp_print_list fm 
		  (fun fm s ->
		    pp_print_string fm ("\"" ^ s ^ "\""))
		  (fun fm -> pp_print_string fm ",") asm.asm_clobbers;
	      end
	  end
      | None -> ()
    in
    pp_print_string fm ");";
  end;
  pp_close_box fm ()

and pp_print_c_stmt010: formatter -> c_stmt010 -> unit = 
  fun fm c_stmt010 ->
    pp_open_vbox fm 0;
    let _ = match c_stmt010 with
      | STMT_AT (coord, stmt) -> 
	  Coordinate.pp_print_t fm coord;
	  pp_print_c_stmt010 fm stmt
	    
      | NOP -> ()
	  
      | COMPUTATION expr ->
	  begin
	    pp_print_c_expression fm ~need_paren:false expr;
	    pp_print_string fm ";"
	  end
	    
      | SEQUENCE (txt_opt, c_stmt010_list) -> 
	  begin
	    let _ = match txt_opt with
	      | Some txt -> fprintf fm "/* %s */" txt; pp_print_space fm ();
	      | None -> ()
	    in
	    Mlite_printer.pp_print_list fm 
	      (fun fm statement ->
		pp_print_c_stmt010 fm statement;
	      )
	      (fun fm -> pp_print_space fm ()) 
	      c_stmt010_list;
	  end

      | COMPOUND (txt_opt, c_compound_stmt010) ->
	  begin
	    pp_print_string fm "{";
	    pp_open_vbox fm indent;
	    begin
	      let _ = match txt_opt with
		| Some txt -> pp_print_space fm (); fprintf fm "/* %s */" txt
		| None -> ()
	      in
	      pp_print_space fm ();
	      pp_print_c_compound_stmt010 fm c_compound_stmt010;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}";
	  end
	    
      | IF (expr, then_c_stmt010, else_c_stmt010) -> 
	  begin
	    pp_print_string fm "if (";
	    pp_print_c_expression fm ~need_paren:true expr;
	    pp_print_string fm ")";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm then_c_stmt010;
	    pp_print_space fm ();
	    pp_print_string fm "else";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm else_c_stmt010
	  end

      | WHILE (expr, c_stmt0) ->
	  begin
	    pp_print_string fm "while (";
	    pp_print_c_expression fm ~need_paren:true expr;
	    pp_print_string fm ")";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm c_stmt0
	  end

      | LOOP c_stmt0 ->
	  begin
	    pp_print_string fm "for(;;)";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm c_stmt0
	  end

      | BREAK ->
	  pp_print_string fm "break;"

      | CONTINUE ->
	  pp_print_string fm "continue;"
	    
      | RETURN_VALUE expr ->
	  begin
	    pp_print_string fm "return ";
	    pp_print_c_expression fm ~need_paren:true expr;
	    pp_print_string fm ";"
	  end
	    
      | RETURN ->
	  pp_print_string fm "return;"
	    
      | SWITCH (expr, c_stmt010) ->
	  begin
	    pp_print_string fm "switch";
	    pp_print_string fm "(";
	    pp_print_c_expression fm ~need_paren:true expr;
	    pp_print_string fm ")";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm c_stmt010
	  end
	    
      | CASE (expr, c_stmt010) ->
	  begin
	    pp_print_string fm "case";
	    pp_print_space fm ();
	    pp_print_c_constant_expression fm expr;
	    pp_print_string fm ":";
	    pp_open_vbox fm indent;
	    begin
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt010;
	    end;
	    pp_print_string fm (";/*case:*/");
	    pp_close_box fm ()
	  end

      | CASE_RANGE (e0, e1, c_stmt010) ->
	  begin
	    pp_print_string fm "case";
	    pp_print_space fm ();
	    pp_print_c_constant_expression fm e0;
	    pp_print_string fm " ... ";
	    pp_print_c_constant_expression fm e1;
	    pp_print_string fm ":";
	    pp_open_vbox fm indent;
	    begin
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt010;
	    end;
	    pp_print_string fm (";/*case:*/");
	    pp_close_box fm ()
	  end
	    
      | DEFAULT (c_stmt010) ->
	  begin
	    pp_print_string fm "default:";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm c_stmt010;
	    pp_print_string fm (";/*default:*/");
	  end
	    
      | LABEL (lb, c_stmt010) ->
	  begin
	    pp_print_string fm (lb ^ ":");
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm c_stmt010;
	    pp_print_string fm (";/*label:" ^ lb ^ "*/");
	  end
	    
      | GOTO lb ->
	  pp_print_string fm ("goto " ^ lb ^ ";");

      | GCC_GOTO expr ->
	  begin
	    pp_print_string fm ("goto ");
	    pp_print_string fm "*";
	    pp_print_c_expression fm ~need_paren:true expr;
	    pp_print_string fm ";";
	  end
	    
      | ASM (u,v) ->
	  pp_print_asm fm (u,v)
    in
    pp_close_box fm ()
      
and pp_print_c_compound_stmt010: formatter -> c_compound_stmt010 -> unit = 
  fun fm (BLOCK (labels, decls, c_stmt010s)) ->
    pp_open_vbox fm 0;
    begin
      if (labels <> []) then
	begin
	  pp_print_string fm "__label__ ";
	  pp_print_space fm ();
	  Mlite_printer.pp_print_list fm pp_print_string
	    (fun fm -> pp_print_string fm ",") labels;
	  pp_print_string fm ";";
	end;
      pp_print_c_declaration_list fm decls;
      pp_print_space fm ();
      Mlite_printer.pp_print_list fm 
	(fun fm statement ->
	  pp_print_c_stmt010 fm statement;
	)
	(fun fm -> pp_print_space fm ()) 
	c_stmt010s;
    end;
    pp_close_box fm ()

      
and pp_print_c_external_declaration: formatter -> c_external_declaration -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| External_declaration_at (coord, expr) ->
	    Coordinate.pp_print_t fm coord;
	    pp_print_c_external_declaration fm expr;
	    
	| External_declaration_1 (c_function_definition) ->
	    pp_print_c_function_definition fm c_function_definition
	      
	| External_declaration_2 (c_declaration) ->
	    pp_print_c_declaration fm c_declaration
    end;
    pp_close_box fm ()
      
and pp_print_c_function_definition: formatter -> c_function_definition -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      match expr with
	| Function_definition (c_declaration_specifiers,
	  c_declarator,
	  c_declaration_list_opt,
	  c_compound_stmt010) 
	  ->
	    begin
	      pp_print_c_declaration_specifiers fm c_declaration_specifiers;
	      pp_print_cut fm ();
	      pp_print_c_declarator fm c_declarator;
	      pp_print_c_declaration_list_opt fm c_declaration_list_opt;
	      pp_print_cut fm ();
	      pp_print_string fm "{";
              pp_open_vbox fm 2;
              begin
                pp_print_space fm ();
		pp_print_c_compound_stmt010 fm c_compound_stmt010;
	      end;
              pp_close_box fm ();
              pp_print_space fm ();
              pp_print_string fm "}";
	    end
    end;
    pp_close_box fm ()
      
and pp_print_c_file_unit: formatter -> c_file_unit -> unit =
  fun fm c_file_unit ->
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
      

      
