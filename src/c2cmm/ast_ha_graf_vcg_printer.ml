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
open Vcg_printer
open Ast_ha_graf_printer

module Code = Ast_ga_code
module QNP = Qual_name_printer

let enable_vcg_print = ref false
let enable_complete_label = ref true
    
let user_opts =
  [ 
    ("--ast-ha-graf-vcg-out",
    Arg.Set enable_vcg_print,
    "print out as vcgs");
    ("--ast-ha-graf-vcg-complete-label",
    Arg.Set enable_complete_label,
    "print out as vcgs") 
  ]
    
let print_c_file_unit: string * c_translation_unit -> unit = 
  fun (basename, c_translation_unit) ->
    let rec pp_print_c_function_definition: c_function_definition -> unit =
      fun expr ->
	let bg_color = ref Vcg.default_vcg_node_att.Vcg.bg_color
	in
	let succs n = n.succs
	and get_idom_opt n = n.idom
	and get_vcg_node_att_opt idx n =
	  let txt = camlp4_macro_str_pp_print
	    (fun fm -> 
	      pp_open_vbox fm 0;
	      begin
		let _ = match n.code with
		  | Code.Scope expr ->
		      begin
			match expr with
			  | Code.Begin_fun _ -> 
			      pp_print_string fm "begin_fun";
			      bg_color := "lightred"
			  | Code.End_fun  -> 
			      pp_print_string fm "end_fun";
			      bg_color := "lightred"
			  | Code.Begin_decl _ -> 
			      pp_print_string fm "begin_decl";
			      bg_color := "red"
			  | Code.End_decl  -> 
			      pp_print_string fm "end_decl";
			      bg_color := "red"
		      end
		  | _ -> 
		      bg_color := Vcg.default_vcg_node_att.Vcg.bg_color
		in
		if !enable_complete_label then
		  Ast_ga_code_printer.pp_print_c_code100 
		    fm expr.jmp_tbl idx n.code;
		
		Mlite_printer.pp_print_list fm 
		  (fun fm i -> pp_print_int fm i)
		  (fun fm -> pp_print_space fm ())
		  n.domfrontiers;
	      end;
	      pp_close_box fm ()
	    )
	  in
	  { 
	    Vcg.label = txt;
	    Vcg.shape = Vcg.default_vcg_node_att.Vcg.shape;
	    Vcg.bg_color = !bg_color;
	    Vcg.fg_color = Vcg.default_vcg_node_att.Vcg.fg_color;
	  }
	in
	to_idom_vcg (basename ^ "." ^ (QNP.to_decl_str expr.name) ^ ".vcg") 
	  expr.code_array
	  ~succs 
	  ~rank_opt:None 
	  ~get_vcg_node_att_opt:(Some get_vcg_node_att_opt)
	  ~get_idom_opt
	  
    and pp_print_c_external_declaration: c_external_declaration -> unit =
      fun expr ->
	match expr with
	  | External_declaration_at (coord, expr) ->
              pp_print_c_external_declaration expr;
	  | External_declaration_1 c_function_definition ->
	      pp_print_c_function_definition c_function_definition
	  | External_declaration_2 c_declarations -> ()
    in
    if !enable_vcg_print then
      match c_translation_unit with
	| Translation_unit (l,eenv) ->
	    List.iter pp_print_c_external_declaration l
	      
