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

let out_file = ref ""
let out_stdout = ref false
type file_type = 
  | S_FILE
  | I_FILE
  | O_FILE
  | C_FILE
  | CPP_FILE
  | EXE_FILE
  | AR_FILE
    
let user_opts =
  [("-o", Arg.Set_string out_file,
    "output to file");
   ("--out", Arg.Set_string out_file,
    "output to file");
   ("--stdout", Arg.Set out_stdout,
    "output to stdout");
 ] 


let check_file_type: string -> file_type = 
  fun filename ->
    if Filename.check_suffix filename ".c" then
      C_FILE
    else if Filename.check_suffix filename ".cc" then
      CPP_FILE
    else if Filename.check_suffix filename ".s" then
      S_FILE
    else if Filename.check_suffix filename ".i" then
      I_FILE
    else if Filename.check_suffix filename ".o" then
      O_FILE
    else if Filename.check_suffix filename ".a" then
      AR_FILE
    else 
      EXE_FILE

  
let check_mlitecc_in: string -> string =
  fun filename ->
    let v = 
      if Filename.check_suffix filename ".c" then
	Filename.chop_suffix filename ".c"
      else if Filename.check_suffix filename ".i" then
	Filename.chop_suffix filename ".i"
      else 
	camlp4_macro_exception "unexpected file type '%s'\n" filename
    in
    Filename.basename v

let open_mlitecc_out: basename:string -> out_channel * string * bool =
  fun ~basename ->
    if !out_stdout then
      (stdout, basename, false)
    else
      begin
	let (out_file, basename) = 
	  if !out_file <> "" then
	    if Filename.check_suffix !out_file ".mxx.c" then
	      (!out_file, Filename.chop_suffix !out_file ".mxx.c")
	    else if Filename.check_suffix !out_file ".cil.c" then
	      (!out_file, Filename.chop_suffix !out_file ".cil.c")
	    else
	      camlp4_macro_exception "unexpected file type '%s'\n" !out_file
	  else
	    (basename ^ ".mxx.c", basename)
	in
	(open_out out_file, basename, true)
      end
	
(*
let open_phase_III_out: basename:string -> out_channel * bool =
  fun ~basename ->
    if !out_stdout then
      (stdout, false)
    else
      begin
	let out_file = 
	  if !out_file <> "" then
	    !out_file
	  else
	    basename ^ ".exe.c"
	in
	(open_out out_file, true)
      end
	
let check_phase_III_in: string -> string =
  fun filename ->
    let v = 
      if Filename.check_suffix filename ".exe.mrsh" then
	(Filename.chop_suffix filename ".exe.mrsh") 
      else 
	camlp4_macro_exception "unexpected file type '%s'\n" filename
    in
    Filename.basename v
*)
