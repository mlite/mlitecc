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
open Cabs
open Trans_phase

let user_opts = Frontc.args

let compile_file: filename:string -> trans_phase -> out_channel -> 
  string * Ast_aa_gram.c_translation_unit =
  fun ~filename trans_phase out_chan ->
    let out_fm = formatter_of_out_channel out_chan
    in
    let (filename, cabs) = Frontc.parse filename
    in
    let basename = Mlitecc_files.check_mlitecc_in filename
    in
    if !(Mlite_config.enable_ast_debug) then
      begin
	let o = open_out (basename ^ ".cabs")
	in
	let _ = Whitetrack.setOutput o
	in
	Cprint.printFile o (basename, cabs);
	close_out o
      end;
    if trans_phase = Phase_cabs then
      begin
	let _ = Whitetrack.setOutput out_chan
	in
	Cprint.printFile out_chan (basename, cabs);
	raise Phase_trans_done
      end
    else (** ast-c99 **)
      begin
	let c99_asts = C99ize_cabs.c99ize cabs
	in
	if !(Mlite_config.enable_ast_debug) then
	  C_file_basic.Ast_aa_gram.write_c_file (basename, c99_asts);
	if trans_phase = Phase_cabs then
	  begin
	    C_file_basic.Ast_aa_gram.write_c_formatter out_fm c99_asts;
	    raise Phase_trans_done;
	  end
	else 
	  (basename, c99_asts)
      end
