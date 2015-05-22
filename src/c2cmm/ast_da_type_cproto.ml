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
open Ast_da_type

let pp_print: formatter -> c_translation_unit -> unit = 
  fun fm c_translation_unit ->
    let rec pp_print_c_function_definition: formatter -> c_function_definition -> unit =
      fun fm expr ->
	pp_open_vbox fm 0;
	begin
	  match expr with
	  | Function_definition (linkage, 
				 c_type,
				 fname, 
				 c_compound_stmt010) 
	    ->
	      begin
		pp_open_box fm 0;
		begin
		  match linkage with
		  | Static -> ()
		  | _ ->
		      begin
			pp_print_string fm "extern ";
			Ast_da_type_printer.pp_print_linkage fm linkage;
			Ast_da_type_printer.pp_print_c_type fm (Some fname, []) c_type;
			pp_print_string fm ";";
		      end
		end;
		pp_close_box fm ();
		pp_print_space fm ();
	      end
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
	    | External_declaration_2 (c_declarations) -> ()
	    | External_declaration_string _ -> ()
	end;
	pp_close_box fm ()
    in
    pp_open_vbox fm 0;
    begin
      match c_translation_unit with
      | Translation_unit l ->
	  begin
	    Mlite_printer.pp_print_list fm 
	      pp_print_c_external_declaration
	      (fun fm -> ())
	      l;
	  end
    end;
    pp_close_box fm ();
    pp_print_flush fm ()
