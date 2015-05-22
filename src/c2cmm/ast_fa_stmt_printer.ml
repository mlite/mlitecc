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
open Ast_fa_stmt

module QNP = Qual_name_printer

type c_file_unit = Ast_fa_stmt.c_translation_unit
let description () = description
let suffix () = suffix

let indent = 2
let enable_type_annotation = false

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

(** *********************************************************************************** **)

module P = Ast_eb_expr_printer

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

and pp_print_coord: Coordinate.t option array -> formatter -> int -> unit = 
  fun line_tbl fm i ->
    let coord_opt = Array.get line_tbl i
    in
    match coord_opt with
      | Some coord -> Coordinate.pp_print_t fm coord
      | None -> ()

and pp_print_c_lab_stmt090: Coordinate.t option array -> formatter -> 
  c_lab_stmt090 -> unit = 
  fun line_tbl fm (lbl_att, stmt) ->
    match stmt with
      | JOIN (g0, lb_opt, g1) ->
	  begin
	    pp_print_coord line_tbl fm g0;
	    Code_label.pp_print_label fm lbl_att g0;
	    Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;
	  end

      | GOTO (g0, lb) ->
	  begin
	    assert false
	  end
	    
      | JMP (g0, lb_opt, g1) ->
	  pp_print_coord line_tbl fm g0;
	  Code_label.pp_print_label fm lbl_att g0;
	  Code_label.pp_print_jmp_label fm g1;
	  let _ = match lb_opt with
	    | Some lb -> 
		fprintf fm "/*%s*/ " lb;
	    | None -> ()
	  in 
	  Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;

      | JMP_JOIN (g0, lb_opt, g1) ->
	  pp_print_coord line_tbl fm g0;
	  Code_label.pp_print_label fm lbl_att g0;
	  Code_label.pp_print_jmp_join_label fm g1;
	  let _ = match lb_opt with
	    | Some lb -> 
		fprintf fm "/*%s*/ " lb;
	    | None -> ()
	  in 
	  Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;

      | JMP_JOIN_BACKWARD (g0, lb_opt, g1) ->
	  pp_print_coord line_tbl fm g0;
	  Code_label.pp_print_label fm lbl_att g0;
	  Code_label.pp_print_jmp_join_label fm g1; 
	  let _ = match lb_opt with
	    | Some lb -> 
		fprintf fm "/*%s*/ " lb;
	    | None -> ()
	  in
	  pp_print_string fm "/* backward */";
	  Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;

      (*
	| CMP_ZERO_JMP (nid, (expr, t0)) ->
	begin
	pp_print_coord line_tbl fm nid;
	Code_label.pp_print_label fm lbl_att nid;
	pp_print_string fm "if";
	pp_print_string fm " (!";
	pp_print_c_val fm expr;
	pp_print_string fm ") ";
	Code_label.pp_print_jmp_label fm t0
	end
      *)

      | CMP_JMP (nid, (expr, t0)) ->
	  begin
	    pp_print_coord line_tbl fm nid;
	    Code_label.pp_print_label fm lbl_att nid;
	    pp_print_string fm "if";
	    pp_print_string fm " (";
	    pp_print_true_cond fm expr;
	    pp_print_string fm ") ";
	    Code_label.pp_print_jmp_label fm t0
	  end

      | TBL_JMP (nid, (expr, list)) ->
	  begin
	    pp_print_coord line_tbl fm nid;
	    Code_label.pp_print_label fm lbl_att nid;
	    pp_print_space fm ();
	    pp_print_string fm "switch";
	    pp_print_string fm "(";
	    pp_print_c_val fm expr;
	    pp_print_string fm ")";
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    pp_open_vbox fm indent;
	    pp_print_space fm ();
	    begin
	      Mlite_printer.pp_print_list fm
		(fun fm (case_opt, t) ->
		  match case_opt with
		    | Some (v0, v1) ->
			if (v0 == v1) then
			  begin
			    pp_print_string fm "case ";
			    pp_print_c_constant_expression fm v0;
			    pp_print_string fm ": ";
			    Code_label.pp_print_jmp_label fm t
			  end
			else
			  begin
			    pp_print_string fm "case ";
			    pp_print_c_constant_expression fm v0;
			    pp_print_string fm "...";
			    pp_print_c_constant_expression fm v1;
			    pp_print_string fm ": ";
			    Code_label.pp_print_jmp_label fm t
			  end
		    | None ->
			pp_print_string fm "default: ";
			Code_label.pp_print_jmp_label fm t
		) 
		(fun fm -> pp_print_space fm ())
		list;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}";
	  end

      | CALL (nid, code0, g0) ->
	  pp_print_coord line_tbl fm nid;
	  Code_label.pp_print_label fm lbl_att nid;
	  pp_print_c_call_transfer fm code0;
	  pp_print_string fm ";";
	  Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g0;

      | SESE_SEQUENCE (nid, txt_opt, stmts, g0) ->
	  begin
	    Code_label.pp_print_label fm lbl_att nid;
	    let _ = match txt_opt with
	      | Some txt -> 
		  C_str.pp_print_c_comment fm txt;
		  pp_print_space fm ();
	      | None -> ()
	    in
	    Mlite_printer.pp_print_list fm 
	      (fun fm expr ->
		P.pp_print_c_construct fm (P.EXPR (expr, false))
		  (*pp_print_c_sese_code fm expr*)
	      )
	      (fun fm -> 
		pp_print_string fm ";";
		pp_print_space fm ();
	      ) 
	      stmts;
	    pp_print_string fm ";";
	    Code_label.pp_print_label 
	      fm (Code_label.SEQ (ref None)) g0;
	  end

      | ASM (nid, txtlst, asm_details_opt, g0) ->
	  begin
	    Code_label.pp_print_label fm lbl_att nid;
	    P.pp_print_c_construct fm (P.ASM_STMT (txtlst, asm_details_opt));
	    Code_label.pp_print_label 
	      fm (Code_label.SEQ (ref None)) g0;
	  end

      | POP_ARGS (g0, lst, g1) ->
	  begin
	    Code_label.pp_print_label fm lbl_att g0;
	    pp_print_space fm ();
	    pp_print_string fm "/* pop_arg ";
	    Mlite_printer.pp_print_list fm
	      (fun fm (t,id) -> 
		pp_print_string fm "(";
		Tent_c_printer.pp_print_id_decl fm t
		  (QNP.to_decl_str id);
		pp_print_string fm ")")
	      (fun fm -> pp_print_string fm ",")
	      lst;
	    pp_print_string fm "*/";
	    Code_label.pp_print_label 
	      fm (Code_label.SEQ (ref None)) g1;
	  end
	    
      | BEGIN_DECL (g0, c_declaration_list, g1, end_decl_g) ->
	  begin
	    Code_label.pp_print_label fm lbl_att g0;
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    (* open box *)
	    pp_open_vbox fm indent;
	    pp_print_space fm ();
	    pp_print_c_local_declaration_list fm c_declaration_list;
	    pp_print_string fm ";";
	    Code_label.pp_print_label 
	      fm (Code_label.SEQ (ref None)) g1;
	  end

      | END_DECL (g0, g1) ->
	  begin
	    Code_label.pp_print_label fm lbl_att g0;
	    pp_print_string fm ";/*pop_decl*/";
	    pp_print_space fm ();
	    (* close_box *)
	    pp_close_box fm (); 
	    pp_print_space fm ();
	    pp_print_string fm "}";
	    Code_label.pp_print_label 
	      fm (Code_label.SEQ (ref None)) g1;
	  end
	    
      | BEGIN_FUNCTION (g0, decls, g1, end_fun_g) ->
	  Code_label.pp_print_label fm lbl_att g0;
	  fprintf fm "/* begin formal param scope - %d*/" end_fun_g;
	  Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;
	  pp_print_space fm ();
	  pp_print_c_local_declaration_list fm !decls;
	  pp_print_string fm ";";

      | EPI (g0, str_opt, g1) ->
	  begin
	    Code_label.pp_print_label fm lbl_att g0;
	    let _ = match str_opt with
	      | Some ret ->
		  pp_open_box fm 0;
		  pp_print_string fm "return";
		  pp_print_space fm ();
		  pp_print_c_val fm ret;
		  pp_print_string fm ";";
		  pp_print_string fm "/*epi*/";
		  pp_close_box fm ()
	      | None -> 
		  fprintf fm "return;/*epi*/"
	    in
	    Code_label.pp_print_label fm (Code_label.SEQ (ref None)) g1;
	  end

      | END_FUNCTION g0 ->
	  Code_label.pp_print_label fm lbl_att g0;
	  pp_print_string fm "/* end formal param scope */"

and pp_print_c_lab_stmt090_list: Coordinate.t option array -> formatter -> c_lab_stmt090 list -> unit = 
  fun line_tbl fm stmts -> 
    pp_open_vbox fm 0;
    begin
      if stmts <> [] then
	begin
	  Mlite_printer.pp_print_list fm 
	    (pp_print_c_lab_stmt090 line_tbl)
	    (fun fm -> pp_print_space fm ())
	    stmts;
	end
    end;
    pp_close_box fm ()

and pp_print_c_function_definition: formatter -> c_function_definition -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      pp_print_space fm ();
      P.pp_print_linkage fm expr.linkage;
      pp_print_space fm ();
      Tent_c_printer.pp_print_id_decl fm expr.c_type 
	(QNP.to_decl_str expr.name);
      pp_print_space fm ();
      fprintf fm "/* code_size:%d */" expr.code_size;
      pp_print_space fm ();
      pp_print_string fm "{";
      pp_open_vbox fm 2;
      begin
	pp_print_space fm ();
	pp_print_c_lab_stmt090_list expr.line_tbl fm expr.c_stmt090_list;
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
	| External_declaration_1 c_function_definition ->
	    pp_print_c_function_definition fm c_function_definition
	| External_declaration_2 c_declarations ->
	    pp_print_c_declaration_list fm c_declarations
    end;
    pp_close_box fm ()

and pp_print_c_local_declaration_list: formatter -> c_local_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      let l = expr 
      in 
      if l <> [] then 
	begin
	  Mlite_printer.pp_print_list fm pp_print_c_local_declaration pp_print_c_semicolon l;
	  pp_print_c_semicolon fm;
	end
    end;
    pp_close_box fm ()
      
and pp_print_c_declaration_list: formatter -> c_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      let l = expr 
      in 
      if l <> [] then 
	begin
	  Mlite_printer.pp_print_list fm pp_print_c_declaration pp_print_c_semicolon l;
	  pp_print_c_semicolon fm;
	end
    end;
    pp_close_box fm ()

and pp_print_c_file_unit: formatter -> c_file_unit -> unit = 
  fun fm c_translation_unit ->
    let _  = Tent_c_printer.disable_type_qualifier_print ()
    in
    pp_open_vbox fm 0;
    begin
      match c_translation_unit with
	| Translation_unit (l,eenv) ->
	    begin
	      pp_print_string fm Mlite_config.mlitecc_macro_op_h;
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm 
		pp_print_c_external_declaration
		(fun fm -> pp_print_space fm ()) l
	    end
    end;
    pp_close_box fm ()

