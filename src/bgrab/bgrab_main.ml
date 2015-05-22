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

let banner =
  "SYNTAX:\tbgrab [mode] ...\n" ^
    "\t-cc1    : intercept the input/output of cc1\n" ^
    "\t-cc1plus: intercept the input/output of cc1plus\n" ^
    "\t-as     : intercept the input/output of as\n" ^
    "\t-ar     : intercept the input/output of ar\n" ^
    "\t-ld     : intercept the input/output of ld\n" ^
    "\tbgrab -cleanup \n" ^
    "\t\tclean up the grabbed files\n" ^
    "\tbgrab -script-gen <file>\n" ^
    "\t\tgenerate a build script from a grabbed file\n"

  
let _ = 
  let (lst, mode) = match (Array.to_list Sys.argv) with
    | e::m::l -> (e::l, m)
    | e -> 
	let _ = prerr_string banner
	in exit 1
  in
  let argv = Array.of_list lst
  in  
  let code = match mode with
    | "-cc1" -> 
	if not Bgrab_common.bgrab_capture then
	  Bgrab_common.pass_to "cc1" argv
	else
	  Bgrab_ccx.doit "cc1" argv
    | "-cc1plus" -> 
	if not Bgrab_common.bgrab_capture then
	  Bgrab_common.pass_to "cc1plus" argv
	else
	  Bgrab_ccx.doit "cc1plus" argv
    | "-as" -> 
	if not Bgrab_common.bgrab_capture then
	  Bgrab_common.pass_to "as" argv
	else
	  Bgrab_as.doit argv
    | "-ar" -> 
	if not Bgrab_common.bgrab_capture then
	  Bgrab_common.pass_to "ar" argv
	else
	  Bgrab_ar.doit argv
    | "-ld" -> 
	if not Bgrab_common.bgrab_capture then
	  Bgrab_common.pass_to "ld" argv
	else
	  Bgrab_ld.doit argv
    | "-cleanup" ->
	let _ = Bgrab_common.clean_up_absolute_build_dir ()
	in 0 
    | "-script-gen" ->
	let _ = Bgrab_script.merge_compile ~output:(Array.get argv 1)
	in 0
    | _ ->
	let _ = prerr_string ("unknown mode " ^ (Bgrab_common.make_cmd lst))
	in prerr_newline ();
	let _ = prerr_string banner
	in 1
  in exit code
	
