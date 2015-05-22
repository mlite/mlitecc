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

let banner =
  "SYNTAX:\tmlitecc1 [options] -o <preprocessed-c-file>\n" ^
    "[options] --\n"

let out_file = ref "a.mxx.c"
let merge = ref false
let emit_code = ref false
  
let user_opts =
  [
    ("-o", Arg.Set_string out_file, "output to file");
    ("--out", Arg.Set_string out_file, "output to file");
    ("--merge", Arg.Set merge, "merge ast_ha_graf translation units");
    ("--emit-code", Arg.Set emit_code, "emit C code");
    ("--stop-at-gteid", Arg.Set_int Uni_typ_db.debug_global_teid, "stop at global_teid");
    ("--stop-at-iteid", Arg.Set_int Uni_typ_db.debug_import_teid, "stop at import_teid");
    ("--stop-at-nteid", Arg.Set_int Uni_typ_db.debug_new_teid, "stop at new teid");
    ("--stop-at-gceid", Arg.Set_int Uni_ce_tbl.debug_global_ceid, "stop at global_ceid");
    ("--stop-at-iceid", Arg.Set_int Uni_ce_tbl.debug_import_ceid, "stop at import_ceid");
    ("--stop-at-nceid", Arg.Set_int Uni_ce_tbl.debug_new_ceid, "stop at new ceid");
    ("--field-name", Arg.String (fun str -> Uni_typ_db.debug_field_name := Some str),
    "check field name");
  ] 
    
    
let options = 
  user_opts @ 
    Mlite_config.mlitecc_cmd_opts @ 
    Ast_translation.trans_phase_opts @
    Ast_translation.user_opts
    
    
let is_c_file filename = 
  Filename.check_suffix filename ".i" || Filename.check_suffix filename ".c"


let is_trans_unit_file filename = 
  Filename.check_suffix filename ".tnu"
    
let is_obj_file filename = 
  Filename.check_suffix filename ".o"
    
let _ = 
  let files = ref []
  in
  Arg.parse options
    (fun file -> files := !files @ [file]) banner;
  if (!merge) then
    begin
      if !out_file = "" then
	Arg.usage options banner
      else
	Ast_translation.merge !files ~output:!out_file
	  ~merged_basename:!out_file (*"merged"*)
    end  
  else 
    begin
      if List.length !files <> 1 || !out_file = "" then
	Arg.usage options banner
      else
	begin
	  let filename = List.hd !files
	  in
	  if !emit_code then
	    begin
	      if !out_file = "" then
		Arg.usage options banner
	      else
		begin
		  if is_c_file filename then
		    Ast_translation.emit_code (Ast_translation.C_FILE filename) !out_file
		  else
		    begin
		      let in_chan = open_in filename
		      in
		      let v = Marshal.from_channel in_chan
		      in
		      let _ = close_in in_chan
		      in Ast_translation.emit_code v !out_file 
		    end
		end
	    end
	  else
	    begin
	      let trans_unit = 
		if is_c_file filename then
		  Ast_translation.translate 
		    (Ast_translation.C_FILE filename) !(Ast_translation.trans_phase)
		else if is_trans_unit_file filename then
		  let in_chan = open_in filename
		  in
		  let v = Marshal.from_channel in_chan
		  in
		  let _ = close_in in_chan
		  in
		  Ast_translation.translate v !(Ast_translation.trans_phase)		    
		else
		  assert false
	      in
	      if is_trans_unit_file !out_file then
		let out_chan = open_out !out_file
		in
		let _ = Marshal.to_channel out_chan trans_unit []
		in close_out out_chan
	      else 
		Ast_translation.emit_code trans_unit !out_file
	    end
	end
    end
