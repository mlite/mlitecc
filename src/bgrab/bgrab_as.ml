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

let act_and_save: input:string -> output:string -> int =
  fun ~input ~output ->
    let (args, real_input) = M.unzip_as_input_zip input
    in
    let cmd = M.make_argument_type_cmd 
      ((M.Exec M.gnu_as_real)::args @ [M.Switch ["-o";output]; M.SFile real_input])
    in 
    M.print_msg M.mlite_bgrab_as cmd;
    let exit_code = Sys.command cmd
    in
    if exit_code = 0 then
      let _ = M.create_as_output_zip ~input_zip:input ~output
      in 0
    else
      exit_code

	
type as_args = 
    {
      mutable version: bool;
      mutable help: bool;
      mutable target_help: bool;
      mutable input: string;
      mutable output: string;
      mutable compiler_args: M.argument_type list;
      mutable save_temps: bool;
      mutable all_but_output: string list;
    }

let as_complex_arg_prefix = 
  [
    "-a";
    "--hash-size=";
    "-march=";
    "-mtune=";
    "-Q";
  ]
      
let process_as_args sys_argv = 
  let cmds = M.make_cmd (Array.to_list sys_argv)
  in
  M.print_msg M.mlite_bgrab_as cmds;
  let args = 
    {
      version = false;
      help = false;
      target_help = false;
      input = "";
      output = "a.out";
      compiler_args = [];      
      save_temps = false;
      all_but_output = [];
    }
  in
  let max_idx = (Array.length sys_argv) 
  and idx = ref 1
  in
  let get_next_value () = 
    incr idx;
    if !idx > max_idx then
      raise M.Error;
    let param = Array.get sys_argv !idx
    in
    if M.is_switcher param then
      raise M.Error;
    param
  in
  while !idx < max_idx 
  do
    let arg = Array.get sys_argv !idx
    in
    let _ = match arg with
      | "--version" ->
	  args.version <- true
      | "--help" ->
	  args.help <- true
      | "--target-help" ->
	  args.target_help <- true
      | "-V"
      | "--alternate" 
      | "-D"
      | "--execstack"
      | "--noexecstack"
      | "-f"
      | "-g"
      | "--gen-debug"
      | "--gstabs"
      | "--gstabs+"
      | "--gdwarf-2"
      | "-J"
      | "-K"
      | "-L"
      | "--keep-locals"
      | "-M"
      | "--mri"
      | "-nocpp"
      | "-R"
      | "--reduce-memory-overheads"
      | "--statistics"
      | "--strip-local-absolute"
      | "--traditional-format"
      | "-W"
      | "--no-warn"
      | "--warn"
      | "--fatal-warnings"
      | "-w"
      | "-X"
      | "-Z"
      | "--listing-lhs-width"
      | "--listing-lhs-width2"
      | "--listing-rhs-width"
      | "--listing-cont-lines"
      | "-k"
      | "-n"
      | "-q"
      | "-s"
      | "--32"
      | "--64"
      | "--divide" ->
	  begin
	    args.compiler_args <- args.compiler_args @ [M.Switch [arg]]
	  end
      | "--debug-prefix-map"
      | "--defsym"
      | "-I"
      | "--MD"
      | "--itbl" ->
	  begin
	    args.compiler_args <- args.compiler_args @ [M.Switch [arg;get_next_value()]]
	  end
      | "-o" ->
	  begin
	    args.output <- get_next_value ()
	  end
      | _ -> 
	  begin
	    if M.is_switcher arg then
	      let (prefix, _) = M.split_complex_arg
		as_complex_arg_prefix arg
              in
	      args.compiler_args <- args.compiler_args @ [M.Switch [arg]];
	    else
	      args.input <- arg
	  end
    in
    let _ = match arg with
      | "-o" -> ()
      | _ -> 
	  begin
	    args.all_but_output <- args.all_but_output @ [arg]
	  end
    in
    incr idx
  done;
  args    
      
let doit sys_argv =    
  let args = process_as_args sys_argv
  in
  if args.help || args.target_help || args.version then
    M.exec_argv M.gnu_as_real sys_argv
  else if (args.input = "") then 
    begin
      print_endline "No input is specified";
      exit 1
    end
  else
    let input = M.create_as_input_zip 
      ~oexec_cmd:(M.make_cmd (Array.to_list sys_argv)) ~argv:args.compiler_args ~input_file:args.input
    in exit (act_and_save ~input ~output:args.output)

