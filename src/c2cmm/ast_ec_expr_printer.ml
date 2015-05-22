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
open Ast_ec_expr

module QNP = Qual_name_printer

type c_file_unit = Ast_ec_expr.c_translation_unit
let description () = description
let suffix () = suffix

module P = Ast_eb_expr_printer

let indent = 2
let enable_type_annotation = false
let explicit_type_coercion = false
  
let pp_print_c_space: formatter -> unit = 
  fun fm -> pp_print_break fm 1 indent

let pp_print_c_comma: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ","; 
    pp_print_c_space fm

let pp_print_c_semicolon: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ";"; 
    pp_print_space fm ()

let pp_print_c_newline: formatter -> unit = 
  fun fm ->
    pp_print_space fm ()

let pp_print_l_paren fm =
  pp_print_string fm "(";
  pp_print_cut fm ()

let pp_print_r_paren fm =
  pp_print_cut fm ();
  pp_print_string fm ")"

let pp_print_c_string_literal = Ast_aa_gram_printer.pp_print_c_string_literal

let pp_print_binary_arithmatic = C_semantics_symbol_printer.pp_print_binary_arithmatic

let pp_print_binary_predicate = C_semantics_symbol_printer.pp_print_binary_predicate

let pp_print_binary_logic_connect = C_semantics_symbol_printer.pp_print_binary_logic_connect

let pp_print_unary_arithmatic = C_semantics_symbol_printer.pp_print_unary_arithmatic

(** *********************************************************************************** **)

type c_construct =
  | EXPR of expr * bool
  | REXPR of rexpr * bool
  | CONST_EXPR of c_constant_expression
  | DECL of c_declaration
  | LOCAL_DECL of c_local_declaration
  | TYPE of (string option * c_type)
  | STMT of c_stmt010
  | COMPOUND_STMT of c_compound_stmt010
  | ASM_STMT of string list * asm_details option
      
let pp_print_c_val fm rval = 
  P.pp_print_c_construct fm (P.RVAL rval)

let pp_print_true_cond fm v = 
  P.pp_print_c_construct fm (P.TRUE_COND v)


let rec pp_print_c_declaration: formatter -> c_declaration -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.DECL expr)
    
and pp_print_c_local_declaration: formatter -> c_local_declaration -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.LOCAL_DECL expr)
    
and pp_print_c_constant_expression: formatter -> c_constant_expression -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.CONST_EXPR expr)
    
and pp_print_c_call_transfer: formatter -> call_transfer -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.CALL_EXPR expr)
    
and pp_print_c_type: formatter -> c_type -> unit = 
  fun fm expr -> 
    Tent_c_printer.pp_print_c_type_name fm expr    
      
and pp_print_c_stmt010: formatter -> c_stmt010 -> unit = 
  fun fm stmt ->
    pp_open_vbox fm 0;
    begin
      match stmt with
	| STMT_SPAN (txt, stmt) -> 
	    pp_print_string fm "/*";
	    pp_print_space fm ();
	    pp_print_string fm txt;
	    pp_print_space fm ();
	    pp_print_string fm "*/";
	    pp_print_space fm ();
	    pp_print_c_stmt010 fm stmt
	      
	| STMT_AT (coord, stmt) -> 
	    Coordinate.pp_print_t fm coord;
	    pp_print_c_stmt010 fm stmt
	      
	| NOP -> ()
	    
	| SESE exprs ->
	    begin
	      Mlite_printer.pp_print_list fm 
		(fun fm expr -> 
		  P.pp_print_c_construct fm (P.EXPR (expr, false));
		  pp_print_string fm ";")
		(fun fm -> pp_print_space fm ())
		exprs
	    end
	      
	| COMPUTATION expr -> 
	    P.pp_print_c_construct fm (P.EXPR (expr, false));
	    pp_print_string fm ";"
	      
	| SEQUENCE (txt_opt, c_stmt010_list) -> 
	    begin
	      let _ = match txt_opt with
		| Some txt -> fprintf fm "/* %s */" txt; 
		    pp_print_space fm ();
		| None -> ()
	      in
	      Mlite_printer.pp_print_list fm 
		(fun fm statement ->
		  pp_print_c_stmt010 fm statement;
		)
		(fun fm -> pp_print_space fm ()) 
		c_stmt010_list;
	    end

	| COMPOUND (txt_opt, c_stmt010_list) ->
	    begin
	      pp_print_string fm "{";
	      pp_open_vbox fm indent;
	      begin
		let _ = match txt_opt with
		  | Some txt -> pp_print_space fm (); 
		      fprintf fm "/* %s */" txt
		  | None -> ()
		in
		pp_print_space fm ();
		Mlite_printer.pp_print_list fm 
		  (fun fm statement ->
		    pp_print_c_stmt010 fm statement;
		  )
		  (fun fm -> pp_print_space fm ()) 
		  c_stmt010_list;
	      end;
	      pp_close_box fm ();
	      pp_print_space fm ();
	      pp_print_string fm "}";
	    end
	      
	| IF (expr, then_c_stmt010, else_c_stmt010) -> 
	    begin
	      pp_print_string fm "if (";
	      pp_print_true_cond fm expr;
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
	      pp_print_true_cond fm expr;
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
	      P.pp_print_c_construct fm (P.REXPR (expr, true));
	      pp_print_string fm ";"
	    end
	      
	| RETURN ->
	    pp_print_string fm "return;"

	| EPI ret_opt ->
	    begin
	      match ret_opt with
		| Some ret -> 
		    pp_open_box fm 0;
		    pp_print_string fm "return";
		    pp_print_space fm ();
		    pp_print_c_val fm ret;
		    pp_print_string fm ";";
		    pp_close_box fm ()
		| None ->
		    pp_print_string fm "return;"
	    end
	      
	| SWITCH (expr, c_stmt010) ->
	    begin
	      pp_print_string fm "switch";
	      pp_print_string fm "(";
	      pp_print_c_val fm expr;
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
	      
	| CASE_RANGE (e0, e1, c_statement) ->
	    begin
	      pp_print_string fm "case ";
	      pp_print_c_constant_expression fm e0;
	      pp_print_string fm " ... ";
	      pp_print_c_constant_expression fm e1;
	      pp_print_string fm ":";
	      pp_open_vbox fm indent;
	      begin
		pp_print_space fm ();
		pp_print_c_stmt010 fm c_statement;
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
	      pp_print_c_val fm expr;
	      pp_print_string fm ";";
	    end
	      
	| ASM (txtlst, asm_details_opt) ->
	    P.pp_print_c_construct fm (P.ASM_STMT (txtlst, asm_details_opt))

	| CALL (lval_opt, expr, expr_list) ->
	    pp_print_c_call_transfer fm (lval_opt, expr, expr_list)
    end;
    pp_close_box fm ()
      
and pp_print_c_compound_stmt010: formatter -> c_compound_stmt010 -> unit = 
  fun fm (BLOCK (labels, c_declaration_list, stmts)) ->
    pp_open_vbox fm 0;
    begin
      if (labels <> []) then
	begin
	  pp_print_string fm "__label__ ";
	  Mlite_printer.pp_print_list fm pp_print_string
	    (fun fm -> pp_print_string fm ",") labels;
	  pp_print_string fm ";";
	  pp_print_space fm ();
	end;
      pp_print_c_local_declaration_list fm c_declaration_list;
      if stmts <> [] then
	begin
	  pp_print_space fm ();
	  Mlite_printer.pp_print_list fm 
	    pp_print_c_stmt010
	    (fun fm -> pp_print_space fm ())
	    stmts;
	end
    end;
    pp_close_box fm ()

and pp_print_c_local_declaration_list: formatter -> 
  c_local_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    if expr <> [] then 
      begin
	Mlite_printer.pp_print_list fm 
	  pp_print_c_local_declaration pp_print_c_semicolon expr;
	pp_print_c_semicolon fm;
      end;
    pp_close_box fm ()

(*
in
match c_construct with
  | DECL c_declaration ->
      pp_print_c_declaration fm c_declaration
  | LOCAL_DECL c_local_declaration ->
      pp_print_c_local_declaration fm c_local_declaration
  | TYPE (string_opt, typ) -> pp_print_c_type fm typ
  | STMT c_statement ->
      pp_print_c_stmt010 fm c_statement
  | COMPOUND_STMT c_compound_stmt010 ->
      pp_print_c_compound_stmt010 fm c_compound_stmt010
  | CONST_EXPR c_constant_expression ->
      pp_print_c_constant_expression fm c_constant_expression
  | _ -> assert false
*)

(*
and pp_print_c_type: formatter -> string option -> c_type -> unit = 
  fun fm  declarator_opt expr -> 
    pp_print_c_construct fm  (TYPE (declarator_opt, expr))    
*)
      
(*
let pp_print_c_declaration fm  decl = 
  pp_print_c_construct fm  (DECL decl)
*)
    
(*
let pp_print_c_declaration_list: formatter  -> c_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm (fun fm -> pp_print_c_declaration fm ) pp_print_c_newline expr;
    pp_close_box fm ()
*)
      
and pp_print_c_declaration_list_opt fm  expr = 
  match expr with
    | Some c_declaration_list ->
	pp_print_cut fm ();
	pp_print_c_declaration_list fm  c_declaration_list 
    | None -> ()
	
(*
let pp_print_c_stmt010: formatter  -> c_stmt010 -> unit = 
  fun fm  expr ->
    pp_print_c_construct fm  (STMT expr)
*)
    
(*  
let pp_print_c_compound_stmt010: formatter  -> c_compound_stmt010 -> unit = 
  fun fm  expr ->
    pp_print_c_construct fm  (COMPOUND_STMT expr)
*)

and pp_print_c_function_definition (fm:formatter)
    (expr:c_function_definition) :unit =
  let Function_definition (linkage, c_type, fname, c_compound_stmt010) = expr
  in
  pp_open_vbox fm 0;
  begin
    P.pp_print_linkage fm linkage;
    Tent_c_printer.pp_print_id_decl fm c_type 
      (QNP.to_decl_str fname);
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
  end;
  pp_close_box fm ()

and pp_print_c_external_declaration: formatter -> c_external_declaration -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      match expr with
	| External_declaration_at (coord, expr) ->
	    Coordinate.pp_print_t fm coord;
	    pp_print_c_external_declaration fm expr;

	| External_declaration_1 (c_function_definition) ->
	    pp_print_c_function_definition fm c_function_definition
	| External_declaration_2 (c_declarations) ->
	    pp_print_c_declaration_list fm c_declarations
    end;
    pp_close_box fm ()

and pp_print_c_declaration_list: formatter -> c_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    if expr <> [] then 
      begin
	Mlite_printer.pp_print_list fm 
	  (fun fm -> pp_print_c_declaration fm ) pp_print_c_semicolon expr;
	pp_print_c_semicolon fm;
      end;
    pp_close_box fm ()

and pp_print_c_file_unit: formatter -> c_file_unit -> unit = 
  fun fm c_translation_unit ->
    pp_open_vbox fm 0;
    begin
      match c_translation_unit with
	| Translation_unit (l, eenv) ->
	    begin
	      pp_print_string fm Mlite_config.mlitecc_macro_op_h;
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm 
		pp_print_c_external_declaration
		(fun fm -> pp_print_space fm ())l;
	    end
    end;
    pp_close_box fm ()
