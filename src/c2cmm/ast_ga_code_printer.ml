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
open Ast_ga_code

module QNP = Qual_name_printer
type c_file_unit = Ast_ga_code.c_translation_unit
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

module P = Ast_eb_expr_printer

let pp_print_c_val fm rval = 
  P.pp_print_c_construct fm (P.RVAL rval)

let pp_print_true_cond fm rval = 
  P.pp_print_c_construct fm (P.TRUE_COND rval)

let pp_print_c_declaration: formatter -> c_declaration -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.DECL expr)
    
let pp_print_c_local_declaration: formatter -> c_local_declaration -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.LOCAL_DECL expr)
    
let pp_print_c_constant_expression: formatter -> c_constant_expression -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.CONST_EXPR expr)
    
let pp_print_c_call_transfer: formatter -> call_transfer -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.CALL_EXPR expr)


let pp_print_c_identifier: formatter -> c_identifier -> unit =
  fun fm expr -> P.pp_print_c_identifier fm expr

let pp_print_c_local_declaration_list: formatter -> c_local_declaration list -> unit =
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
      
let pp_print_c_declaration_list: formatter -> c_declaration list -> unit =
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

let pp_print_sese_code fm = function
  | Computing stmts ->
      begin
	Mlite_printer.pp_print_list fm 
	  (fun fm expr ->
	    P.pp_print_c_construct fm (P.EXPR (expr, false))
	  )
	  (fun fm -> 
	    pp_print_string fm ";";
	    pp_print_space fm ();
	  ) 
	  stmts;
	pp_print_string fm ";";
      end

  | Asm (txtlst, asm_details_opt) ->
      P.pp_print_c_construct fm (P.ASM_STMT (txtlst, asm_details_opt));
      
  | Pop_args lst ->
      let c_comment = camlp4_macro_str_pp_print
	(fun fm -> 
	  pp_print_string fm "pop_arg ";
	  Mlite_printer.pp_print_list fm 
	    (fun fm (t,id) ->
	      pp_print_string fm "(";
	      Tent_c_printer.pp_print_id_decl fm t (QNP.to_decl_str id);
	      pp_print_string fm ")")
	    (fun fm -> pp_print_string fm ",")
	    lst)
      in
      C_str.pp_print_c_comment fm c_comment


let pp_print_flow_ctrl fm jmp_tbl = function
  | Jmp i ->
      Code_label.pp_print_jmp_label fm i
	
  | Jmp_join i ->
      Code_label.pp_print_jmp_join_label fm i
	
  | Jmp_join_backward i ->
      Code_label.pp_print_jmp_join_label fm i
	
(*
  | Cmp_zero_jmp (c_val, t0) ->
      pp_print_string fm "if";
      pp_print_string fm "(!";
      pp_print_c_val fm c_val;
      pp_print_string fm ") ";
      Code_label.pp_print_jmp_label fm t0
*)

  | Cmp_jmp (c_val, t0) ->
      pp_print_string fm "if";
      pp_print_string fm "(";
      pp_print_true_cond fm c_val;
      pp_print_string fm ") ";
      Code_label.pp_print_jmp_label fm t0
	
  | Tbl_jmp (expr, i) -> 
      begin
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
	  let list = Array.get jmp_tbl i
	  in
	  Mlite_printer.pp_print_list fm
	    (fun fm (case_opt, t) ->
	      match case_opt with
		| Some (v0, v1) ->
		    if v0 == v1 then
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
	
let pp_print_scope_ctrl: formatter -> int -> scope_ctrl -> unit =
  fun fm i expr ->
    match expr with
      | Begin_fun (decls, j) ->
	  fprintf fm "/* begin function scope [%d-%d]*/" i j;
	  pp_print_space fm ();
	  pp_print_c_local_declaration_list fm decls

      | End_fun ->
	  pp_print_string fm "/* end function scope */"
	    
      | Begin_decl (c_declaration_list, j) ->
	  pp_print_space fm ();
	  pp_print_string fm "{";
	  (** open box **)
	  pp_open_vbox fm indent;
	  pp_print_space fm ();
	  pp_print_c_local_declaration_list fm c_declaration_list

      | End_decl ->
	  pp_print_string fm ";/*pop_decl*/";
	  pp_print_space fm ();
	  (** close_box **)
	  pp_close_box fm (); 
	  pp_print_space fm ();
	  pp_print_string fm "}"
	  

let pp_print_c_code100: formatter -> jmp_table -> 
  int ->  c_code100 -> unit =
  fun fm jmp_tbl i code100 ->
    let pp_print_c_code100: formatter -> jmp_table -> 
      int ->  c_code100 -> unit =
      fun fm jmp_tbl i expr ->
	match expr with
	  | Scope expr -> pp_print_scope_ctrl fm i expr
	  | Call expr -> pp_print_c_call_transfer fm expr
	  | Flow expr -> pp_print_flow_ctrl fm jmp_tbl expr
	  | Sese expr -> 
	      pp_open_vbox fm 0;
	      begin
		Mlite_printer.pp_print_list fm
		  (fun fm c -> pp_print_sese_code fm c; pp_print_string fm ";")
		  (fun fm -> pp_print_space fm ())
		  expr
	      end;
	      pp_close_box fm ()
	  | Join -> pp_print_string fm ";"
	  | Epi ret_opt ->
	      begin
		match ret_opt with
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
	      end
	  | Nop -> pp_print_string fm ";"
    in pp_print_c_code100 fm jmp_tbl i code100
    


let rec pp_print_coord: Coordinate.t option array -> formatter -> int -> unit = 
  fun line_tbl fm i ->
    let coord_opt = Array.get line_tbl i
    in
    match coord_opt with
      | Some coord -> Coordinate.pp_print_t fm coord
      | None -> ()
	  
and pp_print_c_function_definition: formatter -> 
    c_function_definition -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      P.pp_print_linkage fm expr.linkage;
      pp_print_space fm ();
      Tent_c_printer.pp_print_id_decl fm expr.c_type
	(QNP.to_decl_str expr.name);
      pp_print_space fm ();
      pp_print_string fm "{";
      pp_open_vbox fm indent;
      begin
	pp_print_space fm ();
	Mlite_printer.pp_print_array fm 
	  (fun fm i (is_target, v, coord_opt) ->
	    let _ = match coord_opt with
	      | Some coord -> Coordinate.pp_print_t fm coord
	      | None -> ()
	    in
	    Code_label.pp_print_label fm is_target i;
	    pp_print_c_code100 fm expr.jmp_tbl i v
	  )
	  (fun fm -> pp_print_space fm ())
	  expr.code_array;
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
		(fun fm -> pp_print_space fm ())
		l;
	    end
    end;
    pp_close_box fm ()

