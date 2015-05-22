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

type unmap = Int.gnode_id -> t option
and map = t -> Int.gnode_id list
and t = (string * int * int)

let default i = None

let to_str: t -> string =
  fun (file, row, col) ->
    "(\"" ^ file ^ "\"," ^ (string_of_int row) ^ "," ^ (string_of_int col) ^ ")"


let pp_print_t: formatter -> t -> unit = 
  fun fm (file, row, col) ->
    if !Mlite_config.enable_c_line_directive then
      begin
	pp_open_vbox fm 0;
	begin
	  pp_open_box fm 0;
	  pp_print_string fm "# line ";
	  pp_print_int fm row;
	  pp_print_string fm (" \"" ^ file ^ "\" ");
	  pp_close_box fm ();
	  pp_print_space fm ();
	end;
	pp_close_box fm ();
      end


let pp_print_complete_lexing_position: formatter -> Lexing.position -> unit =
  fun fm pos ->
    if !Mlite_config.enable_lexing_position then
      begin
	pp_open_box fm 0;
	fprintf fm "/* %s:%d[%d-%d] */" 
	  pos.Lexing.pos_fname 
	  pos.Lexing.pos_lnum 
	  pos.Lexing.pos_bol
	  pos.Lexing.pos_cnum;
	pp_close_box fm ()
      end


let pp_print_lexing_position: formatter -> Lexing.position -> unit =
  fun fm pos ->
    if !Mlite_config.enable_lexing_position then
      begin
	pp_open_box fm 0;
	fprintf fm "/* %s:%d */" 
	  pos.Lexing.pos_fname 
	  pos.Lexing.pos_lnum;
	pp_close_box fm ()
      end
