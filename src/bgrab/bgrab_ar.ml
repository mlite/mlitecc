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

module M = Bgrab_common

let act_and_save: string -> M.argument_type list -> output:string -> 
  input:M.argument_type list -> int =
  fun oexec_cmd argv ~output ~input ->
    let exec_cmd = M.make_argument_type_cmd 
      ((M.Exec M.gnu_ar_real)::argv @ [M.Switch [output]] @ input)
    in 
    M.print_msg M.mlite_bgrab_ar exec_cmd;
    let exit_code = Sys.command exec_cmd
    in
    if (exit_code = 0) then
      let _ = M.create_ar_output_zip ~oexec_cmd ~exec_cmd ~argv ~output ~input
      in 0
    else
      exit_code
	
	
type ar_args = 
    {
      mutable version: bool;
      mutable help: bool;
      mutable input: M.argument_type list;
      mutable output: string;
      mutable compiler_args: M.argument_type list;
    }

let process_ar_args argv = 
    let cmds = M.make_cmd (Array.to_list argv)
    in
    M.print_msg M.mlite_bgrab_ar cmds;
    let args = 
      {
	version = false;
	help = false;
	input = [];
	output = "";
	compiler_args = [];
      }
    in
    let max_idx = (Array.length argv) - 1
    and idx = ref 1
    in
    let start_with idx c = 
      if idx > max_idx then
	raise M.Error;
      let param = Array.get argv idx
      in
      if M.start_with_char c param then
	true
      else 
	false
    in
    let _ = args.compiler_args <- [M.Switch [Array.get argv 1]]
    and _ = args.output <- Array.get argv 2
    and idx = ref 3
    in
    while !idx <= max_idx 
    do
      let arg = Array.get argv !idx
      in
      begin
	if (M.end_with_substr [".o"] arg) then
	  args.input <- args.input @ [M.Objects (ref [arg])]
	else if (M.end_with_substr [".a"] arg) then
	  args.input <- args.input @ [M.Library arg]
	else
	  assert false
      end;
      incr idx
    done;
    args    
      
let doit sys_argv =
  let oexec_cmd = M.make_cmd (Array.to_list sys_argv)
  in
  let args = process_ar_args sys_argv
  in
  if args.help || args.version then
    M.exec_argv M.gnu_ar_real sys_argv
  else if (args.input = []) then 
    begin
      print_endline "No input is specified";
      exit 1
    end
  else
    let code = act_and_save oexec_cmd args.compiler_args 
      ~output:args.output ~input:args.input
    in exit code
      
