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

module type C_Printer_Type =
  sig
    type c_file_unit

    val description: unit -> string list
    val pp_print_c_file_unit: formatter -> c_file_unit -> unit
    val suffix: unit -> string
  end

      
module Make (CP:C_Printer_Type) =
  struct 
    let write_c_formatter: formatter -> CP.c_file_unit -> unit = 
      fun fm c_file_unit ->
	pp_open_vbox fm 0;
	begin
	  pp_print_string fm ("/*" ^ (CP.suffix ()) ^ "*/");
	  pp_print_space fm ();
	end;
	pp_close_box fm ();
	pp_print_flush fm ();
	CP.pp_print_c_file_unit fm c_file_unit;
	pp_print_newline fm ();
	pp_print_string fm "/* EOF */";
	pp_print_newline fm ();
	pp_print_flush fm ()


    let write_c_file: string * CP.c_file_unit -> unit = 
      fun (filename, c_file_unit) ->
	let out_chan = open_out (filename ^ CP.suffix ())
	in
	let fm = formatter_of_out_channel out_chan
	in
	write_c_formatter fm c_file_unit;
	close_out out_chan
  end

module Ast_aa_gram = Make (Ast_aa_gram_printer)
module Call_graph = Make (Call_graph_printer)
module Ast_ba_stmt = Make (Ast_ba_stmt_printer)
module Ast_ca_expr = Make (Ast_ca_expr_printer)
module Ast_da_type = Make (Ast_da_type_printer)
module Ast_ea_expr = Make (Ast_ea_expr_printer)
module Ast_eb_expr = Make (Ast_eb_expr_printer)
module Ast_ec_expr = Make (Ast_ec_expr_printer)
module Ast_fa_stmt = Make (Ast_fa_stmt_printer)
module Ast_ga_code = Make (Ast_ga_code_printer)
module Ast_ha_graf = Make (Ast_ha_graf_printer)
module Ast_ia_init = Make (Ast_ia_init_printer)
module Ast_cmm = Make (Cmm_ast_printer)
