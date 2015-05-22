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

let act_and_save: string -> M.argument_type list -> output:string -> int =
  fun oexec_cmd argv ~output ->
    let exec_cmd = M.make_argument_type_cmd 
      ((M.Exec M.gnu_ld_real)::argv @ [M.Switch ["-o";output;]])
    in 
    M.print_msg M.mlite_bgrab_ld exec_cmd;
    let exit_code = Sys.command exec_cmd
    in
    if (exit_code = 0) then
      let _ = M.create_ld_output_zip ~oexec_cmd ~exec_cmd ~argv ~output
      in 0
    else
      exit_code
      
	
type ld_args = 
    {
	mutable version: bool;
	mutable help: bool;
	mutable target_help: bool;
	mutable input: string list;
	mutable output: string;
	mutable compiler_args: M.argument_type list;
	mutable save_temps: bool;
	mutable all_but_output: string list;
	mutable search_paths: string list;
      }

let ld_complex_arg_prefix = 
  [
    "--hash-size=";
    "--hash-style=";
    "-march=";
    "-mtune=";
    "--sysroot=";
    "--unresolved-symbols";
    "--build-id";
    "-L";
    "-l";
  ]
    
let crt_objs = 
  [
    "crt1.o";
    "crti.o";
    "crtn.o";      
    "gcrt1.o";
    "Mcrt1.o";
    "Scrt1.o";
    "crtbegin.o";
    "crtbeginS.o";
    "crtbeginT.o";
    "crtend.o";
    "crtendS.o";
    "crtfastmath.o";
  ]

let skip_objs = 
  try
    let str = Sys.getenv "MLITE_SKIP_OBJS"
    in M.split_str str ':'
  with
    | Not_found -> []


let process_ld_args argv = 
    let cmds = M.make_cmd (Array.to_list argv)
    in
    M.print_msg M.mlite_bgrab_ld cmds;
    let args = 
      {
	version = false;
	help = false;
	target_help = false;
	input = [];
	output = "a.out";
	compiler_args = [];      
        save_temps = false;
	all_but_output = [];
	search_paths = [];
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
    let get_next_value () = 
      incr idx;
      if !idx > max_idx then
	raise M.Error;
      let param = Array.get argv !idx
      in
      if M.is_switcher param then
	raise M.Error;
      param
    in
    let obj_list_is_added = ref false
    and obj_list = ref []
    in
    while !idx <= max_idx 
    do
      let arg = Array.get argv !idx
      in
      let _ = match arg with
	| "-v"
	| "--version" ->
	    args.version <- true
	| "--help" ->
	    args.help <- true
	| "--target-help" ->
	    args.target_help <-true
	| "-d"
	| "-dc"
	| "-dp"
	| "-E"
	| "--export-dynamic"
	| "-EB"
	| "-EL"
	| "-g"
	| "-M"
	| "--print-map"
	| "-n"
	| "--nmagic"
	| "-N"
	| "--omagic"
	| "--no-omagic"
	| "-O"
	| "-Qy"
	| "-q"
	| "--emit-relocs"
	| "-r"
	| "-i"
	| "--relocatable"
	| "-s"
	| "--strip-all"
	| "-S"
	| "--strip-debug"
	| "--strip-discarded"
	| "--no-strip-discarded"
	| "-t"
	| "--trace"
	| "-dT"
	| "-Ur"
	| "-x"
	| "--discard-all"
	| "-X"
	| "--discard-locals"
	| "--discard-none"
	| "-("
	| "--start-group"
	| "-)"
	| "--end-group"
	| "--accept-unknown-input-arch"
	| "--no-accept-unknown-input-arch"
	| "--add-needed"
	| "--no-add-needed"
	| "--as-needed"
	| "--no-as-needed"
	| "-Bdynamic"
	| "-dy"
	| "-call_shared"
	| "-Bstatic"
	| "-dn"
	| "-non_shared"
	| "-static"
	| "-Bsymbolic"
	| "-Bsymbolic-functions"
	| "--check-sections"
	| "--no-check-sections"
	| "--cref"
	| "--embedded-relocs"
	| "--fatal-warnings"
	| "--force-exe-suffix"
	| "--gc-sections"
	| "--no-gc-sections"
	| "--print-gc-sections"
	| "--no-print-gc-sections"
	| "--no-define-common"
	| "--no-demangle"
	| "--no-keep-memory"
	| "--no-undefined"
	| "--allow-shlib-undefined"
	| "--no-allow-shlib-undefined"
	| "--alow-multiple-definition"
	| "--no-undefined-version"
	| "--default-symver"
	| "--default-imported-symver"
	| "--no-warn-mismatch"
	| "--no-warn-search-mismatch"
	| "--no-whole-archive"
	| "--noinhibit-exec"
	| "-nostdlib"
	| "-qmagic"
	| "--reduce-memory-overheads"
	| "--relax"
	| "-shared"
	| "-Bshareable"
	| "-pie"
	| "--pic-executable"
	| "--sort-common"
	| "--stats"
	| "--traditional-format"
	| "--verbose"
	| "--dynamic-list-data"
	| "--dynamic-list-cpp-new"
	| "--dynamic-list-cpp-typeinfo"
	| "--warn-common"
	| "--warn-constructors"
	| "--warn-multiple-gp"
	| "--warn-once"
	| "--warn-section-align"
	| "--warn-shared-textrel"
	| "--warn-unresolved-symbols"
	| "--error-unresolved-symbols"
	| "--whole-archive"
	| "-Bgroup"
	| "--disable-new-dtags"
	| "--enable-new-dtags"
	| "--eh-frame-hdr" ->
	    begin
	      args.compiler_args <- 
		args.compiler_args @ [M.Switch [arg]]
	    end
	| "-a"
	| "-A"
	| "--architecture"
	| "-b"
	| "--format"
	| "-c"
	| "--mri-script"
	| "-e"
	| "--entry"
	| "-f"
	| "--auxiliary"
	| "-F"
	| "--filter"
	| "-G"
	| "--gpsize"
	| "-h"
	| "-soname"
	| "-I"
	| "-dynamic-linker"
	| "-l"
	| "--library"
	| "-L"
	| "--library-path"
	| "-m" 
	| "-R"
	| "--just-symbols"
	| "-T"
	| "--script"
	| "--default-script"
	| "-u"
	| "--undefined"
	| "-y"
	| "--trace-symbol"
	| "-Y"
	| "-assert"
	| "--defsysm"
	| "-fini"
	| "-init"
	| "-Map"
	| "--oformat"
	| "--retain-symbols-file"
	| "-rpath"
	| "-rpath-link"
	| "--spare-dynamic-tags"
	| "--task-link"
	| "--section-start"
	| "-Tbss"
	| "-Tdata"
	| "-Ttext"
	| "--dynamic-list"
	| "--wrap"
	| "-z"
	  ->
	    begin
	      args.compiler_args <- 
		args.compiler_args @ [M.Switch [arg;get_next_value()]]
	    end
	| "--output"
	| "-o" ->
	    begin
	      args.output <- get_next_value ()
	    end
	      
	| "--unique" 
	| "--demangle"
	| "--split-by-file"
	| "--split-by-reloc" ->
	    begin
	      if start_with (!idx + 1) '=' then
		args.compiler_args <- 
		  args.compiler_args @ 
		  [M.Switch [arg;get_next_value()]]
	      else
		args.compiler_args <- 
		  args.compiler_args @ [M.Switch [arg]]
	    end
	| _ -> 
	    begin
	      if M.is_switcher arg then
		let (prefix, v) = M.split_complex_arg
		  ld_complex_arg_prefix arg
                in
		let _ = match prefix with
		  | "-L" -> args.search_paths <- args.search_paths @ [v]
		  | _ -> ()
		in args.compiler_args <- args.compiler_args @ [M.Switch [arg]];
	      else
		begin
		  if (M.end_with_substr crt_objs arg) then
		    args.compiler_args <- args.compiler_args @ 
		      [M.CrtObj arg]
		  else if (M.end_with_substr skip_objs arg) then
		    args.compiler_args <- args.compiler_args @
		      [M.CrtObj arg]
		  else if (M.end_with_substr [".a"] arg) then
		    args.compiler_args <- args.compiler_args @
		      [M.Library arg]
		  else
		    begin
		      args.input <- args.input @ [arg];
		      obj_list := !obj_list @ [arg];
		      if not !obj_list_is_added then
			begin
			  let _ = obj_list_is_added := true
			  in
			  args.compiler_args <- args.compiler_args @ 
			    [M.Objects obj_list];
			end
		    end
		end
	    end
      in
      incr idx
    done;
    args    
      
let doit sys_argv =
  let oexec_cmd = M.make_cmd (Array.to_list sys_argv)
  in
  let args = process_ld_args sys_argv
  in
  if args.help || args.target_help || args.version then
    M.exec_argv M.gnu_ld_real sys_argv
  else if (args.input = []) then 
    begin
      print_endline "No input is specified";
      exit 1
    end
  else
    exit (act_and_save oexec_cmd args.compiler_args ~output:args.output)
      
