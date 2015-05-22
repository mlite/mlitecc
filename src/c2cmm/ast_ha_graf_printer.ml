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
open Ast_ha_graf

module QNP = Qual_name_printer
type c_file_unit = Ast_ha_graf.c_translation_unit
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


let rec pp_print_c_declaration: formatter -> c_declaration -> unit = 
  fun fm expr -> P.pp_print_c_construct fm (P.DECL expr)

and pp_print_coord: Coordinate.t option array -> formatter -> int -> unit = 
  fun line_tbl fm i ->
    let coord_opt = Array.get line_tbl i
    in
    match coord_opt with
      | Some coord -> Coordinate.pp_print_t fm coord
      | None -> ()
	  
and pp_print_c_function_definition: formatter -> c_function_definition -> unit =
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
	  (fun fm i node -> 
	    let _ = match node.coord_opt with
	      | Some coord -> Coordinate.pp_print_t fm coord
	      | None -> ()
	    in
	    Code_label.pp_print_label fm node.label_att i;
	    Ast_ga_code_printer.pp_print_c_code100 fm expr.jmp_tbl i node.code
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
	| Translation_unit (l, eenv) ->
	    begin
	      pp_print_string fm Mlite_config.mlitecc_macro_op_h;
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm 
		pp_print_c_external_declaration
		(fun fm -> pp_print_space fm ()) l;
	    end
    end;
    pp_close_box fm ()

