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

let ccx_real ccx = 
  match ccx with
    | "cc1" -> M.gnu_cc1_real
    | "cc1plus" -> M.gnu_cc1plus_real
    | _ -> assert false
    

let act_and_save: ccx:string -> input:string -> output:string -> int =
  fun ~ccx ~input ~output ->
    let (args, real_input) = M.unzip_ccx_input_zip ccx input
    in
    let cmd = M.make_argument_type_cmd ((M.Exec (ccx_real ccx))::args @ 
      [M.Switch["-o";output];M.IFile real_input])
    in 
    M.print_msg (M.mlite_bgrab_ccx ^ ccx) cmd;
    let exit_code = Sys.command cmd
    in
    if (exit_code = 0) then
      let _  = M.create_ccx_output_zip ~ccx ~input_zip:input ~output
      in 0
    else
      exit_code
	

type ccx_args = 
    {
      mutable help: bool;
      mutable target_help: bool;
      mutable version: bool;
      mutable preprocessed: bool;
      mutable input: string;
      mutable input_basename: string;
      mutable output: string;
      mutable pch_preprocess: bool;
      mutable e_preprocess: bool;
      mutable compiler_args: M.argument_type list;
      mutable save_temps: bool;
      mutable all_but_output: M.argument_type list;
    }
      
let cpp_complex_arg_prefix = 
  [
    "-D"; 
    "-U";
    "-I";
    "-d";
    "-W";
  ]
    
let ccx_complex_arg_prefix = 
  [
    "-G";
    "-O";
    "-W";
    "-fabi-version=";
    "-falign-functions";
    "-falign-jumps";
    "-falign-labels";
    "-falign-loops";
    "-fcall-saved"; (* -fcall-saved<register> *)
    "-fcall-used";  (* -fcall-used<register> *)
    "-fdiagnostics-show-location="; (* [once|every-line] *)
    "-fdump-"; (* <type> *)
    "-ffixed-"; (* <register> *)
    "-finline-limit-"; (* This switch lacks documentation *)
    
    "-finline-limit="; (* <number> Limit the size of inlined functions to
			  <number> *)
    
    "-fmessage-length="; (* <number> Limit diagnostics to <number> characters per
                            line.  0 suppresses line-wrapping *)

    "-fpack-struct="; (* <number> Set initial maximum structure member
			 alignment *)

    "-frandom-seed";  (*  This switch lacks documentation 
			  -frandom-seed=<string>      
			  Make compile reproducible using <string> *)

    "-fsched-stalled-insns-dep"; (* Set dependence distance checking in premature
				    scheduling of queued insns 
				    -fsched-stalled-insns-dep=<number> Set
				    dependence distance checking in premature
				    scheduling of queued insns
				 *)

    "-fsched-stalled-insns="; (* <number> Set number of queued insns that can be
				 prematurely scheduled *)
    
    "-fsched-verbose="; (* <number>    Set the verbosity level of the
			   scheduler *)

    "-fstack-limit-register="; (* <register> Trap if the stack goes past
				  <register> *)
    "-fstack-limit-symbol="; (* <name> Trap if the stack goes past symbol
				<name> *)

    "-ftls-model="; (* [global-dynamic|local-dynamic|initial-exec|local-exec] Set the
                       default thread-local storage code generation model *)

    "-ftree-vectorizer-verbose="; (* <number> Set the verbosity level of the
				     vectorizer *)
  ]

    
let process_ccx_args ccx argv = 
  let cmds = M.make_cmd (Array.to_list argv)
  in
  M.print_msg (M.mlite_bgrab_ccx ^ ccx) cmds;
  let args = 
    {
      help = false;
      target_help = false;
      version = false;
      preprocessed = false;
      input = "";
      input_basename = "";
      output = "a.out";
      pch_preprocess = false;
      e_preprocess = false;
      compiler_args = [];      
      save_temps = false;
      all_but_output = [];
    }
  in
  let max_idx = (Array.length argv) - 1
  and idx = ref 1
  in
  let get_next_value ~ignore = 
    incr idx;
    if !idx > max_idx then
      raise M.Error;
    let param = Array.get argv !idx
    in
    if M.is_switcher param then
      begin
	(* switch follows another switch *)
	decr idx;
	if not ignore then
	  begin
	    prerr_string param;
	    prerr_newline ();
	    raise M.Error;
	  end
      end;
    param
  in
  while !idx <= max_idx 
  do
    let arg = Array.get argv !idx
    in
    let _ = match arg with
      | "--help" ->
	  args.help <- true
	    
      | "--target-help" ->
	  args.target_help <- true
	    
      | "--version" -> 
	  args.version <- true
	    
      | "-fpreprocessed" -> 
	  begin
	    args.preprocessed <- true;
	    args.compiler_args <- args.compiler_args @ [M.Switch [arg]]
	  end
	    
      | "-fpch-preprocess" ->
	  begin
	    args.pch_preprocess <- true;
	  end
	    
      | "-E" ->
	  begin
	    args.e_preprocess <- true;
	  end

      | "-M" 
      | "-MM"
      | "-MG"
      | "-MP"
      | "-MD"
      | "-MMD"
      | "-I-"
      | "-nostdinc"
      | "-P"
      | "-C"
      | "-CC"
      | "-traditional-cpp"
      | "-trigraphs"
	-> ()
      | "-MF"
      | "-MT" 
      | "-MQ"
      | "-U"
      | "-D" 
      | "-I" 
      | "-include" 
      | "-imacros" 
      | "-idirafter"
      | "-iprefix"
      | "-iwithprefix"
      | "-iwithprefixbefore"
      | "-isystem"
      | "-quote"
      | "-isysroot" 
	-> 
	  ignore(get_next_value ~ignore:true)
	    
      | "-fargument-noalias"
      | "-fargument-noalias-global"
      | "-fasynchronous-unwind-tables"
      | "-fbounds-check"
      | "-fbranch-count-reg"
      | "-fbranch-probabilities"
      | "-fbranch-target-load-optimize"
      | "-fbtr-bb-exclusive"
      | "-fcaller-saves"
      | "-fcommon"
      | "-fcprop-registers"
      | "-fcrossjumping"
      | "-fcse-follow-jumps"
      | "-fcse-skip-blocks"
      | "-fcx-limited-range"
      | "-fdata-sections"
      | "-fdefer-pop"
      | "-fdelayed-branch"
      | "-fdelete-null-pointer-checks"
      | "-fdiagnostics-show-option"
      | "-fdump-unnumbered"
      | "-fearly-inlining"
      | "-feliminate-dwarf2-dups"
      | "-feliminate-unused-debug-symbols"
      | "-feliminate-unused-debug-types"
      | "-fexceptions"
      | "-fexpensive-optimizations"
      | "-ffast-math"
      | "-ffinite-math-only"
      | "-ffloat-store"
      | "-fforce-addr"
      | "-fforce-mem"
      | "-ffunction-cse" 
	  (* Allow function addresses to be held 
	     in registers *)
	  
      | "-ffunction-sections" 
	  (* Place each function into its own 
	     section *)
	  
      | "-fgcse" 
	  (* Perform global common subexpression elimination *)
	  
      | "-fgcse-after-reload" 
	  (* Perform global common subexpression 
	     elimination after register allocation *)
	  
      | "-fgcse-las" 
	  (* Perform redundant load after store elimination in
             global common subexpression *)
	  
      | "-fgcse-lm" 
	  (* Perform enhanced load motion during global common
             subexpression elimination *)
	  
      | "-fgcse-sm" 
	  (* Perform store motion after global common
             subexpression elimination *)
	  
      | "-fguess-branch-probability" 
	  (* Enable guessing of branch
	     probabilities *)
	  
      | "-fident" 
	  (* Process #ident directives *)
	  
      | "-fif-conversion" 
	  (* Perform conversion of conditional jumps to
             branchless equivalents *)
	  
      | "-fif-conversion2" 
	  (* Perform conversion of conditional jumps to
	     conditional execution *)
	  
      | " -finhibit-size-directive" 
	  (* Do not generate .size directives *)
	  
      | "-finline"  
	  (* Pay attention to the "inline" keyword *)
	  
      | "-finline-functions" 
	  (* This switch lacks documentation *)
	  
      | "-finline-functions-called-once" 
	  (* Integrate functions called once 
	     into their callers *)

      | "-finstrument-functions"
	  (* Instrument function entry and exit with profiling
             calls *)

      | "-fipa-cp"
          (* Perform Interprocedural constant propagation *)
	  
      | "-fipa-pure-const"
          (* Discover pure and const functions *)
	  
      | "-fipa-reference"
          (* Discover readonly and non addressable static 
             variables *)
	  
      | "-fipa-type-escape"
          (* Type based escape and alias analysis *)
	  
      | "-fivopts"
          (* Optimize induction variables on trees *)
	  
      | "-fjump-tables"
          (* Use jump tables for sufficiently large switch
             statements *)
	  
      | "-fkeep-inline-functions"
	  (* Generate code for functions even if they are
             fully inlined *)
	  
      | "-fkeep-static-consts"
          (* Emit static const variables even if they are not
             used *)
	  
      | "-fleading-underscore"
          (* Give external symbols a leading underscore *)
	  
      | "-floop-optimize"
          (* Perform loop optimizations *)
	  
      | "-floop-optimize2"
          (* Perform loop optimizations using the new loop
             optimizer *)
	  
      | "-fmath-errno"
          (* Set errno after built-in math functions *)
	  
      | "-fmem-report"
          (* Report on permanent memory allocation *)
	  
      | "-fmerge-all-constants"
	  (* Attempt to merge identical constants and constant
             variables *)
	  
      | "-fmerge-constants"
          (* Attempt to merge identical constants across
             compilation units *)

      | "-fmodulo-sched"
          (* Perform SMS based modulo scheduling before the
             first scheduling pass *)
	  
      | "-fmove-loop-invariants"
	  (* Move loop invariant computations out of loops *)
	  
      | "-fmudflap"
          (* Add mudflap bounds-checking instrumentation for
             single-threaded program *)
	  
      | "-fmudflapir"
          (* Ignore read operations when inserting mudflap
             instrumentation *)
	  
      | "-fmudflapth"
          (* Add mudflap bounds-checking instrumentation for
             multi-threaded program *)
	  
      | "-fnon-call-exceptions"
	  (* Support synchronous non-call exceptions *)
	  
      | "-fomit-frame-pointer"
          (* When possible do not generate stack frames *)
	  
      | "-foptimize-register-move"
	  (* Do the full register move optimization pass *)
	  
      | "-foptimize-sibling-calls"
	  (* Optimize sibling and tail recursive calls *)
      | "-fpack-struct"
          (* Pack structure members together without holes *)
	  
      | "-fpcc-struct-return"
          (* Return small aggregates in memory, not registers *)

      | "-fpeel-loops"
          (* Perform loop peeling *)
	  
      | "-fpeephole"
          (* Enable machine specific peephole optimizations *)
	  
      | "-fpeephole2"
          (* Enable an RTL peephole pass before sched2 *)
	  
      | "-fpic"
          (* Generate position-independent code if possible 
             (small mode) *)
	  
      | "-fpie"
          (* Generate position-independent code for
             executables if possible (small mode) *)
	  
      | "-fprefetch-loop-arrays"
	  (* Generate prefetch instructions, if available, for
             arrays in loops *)
	  
      | "-fprofile"
          (* Enable basic program profiling code *)
	  
      | "-fprofile-arcs"
          (* Insert arc-based program profiling code *)
	  
      | "-fprofile-generate"
          (* Enable common options for generating profile info
             for profile feedback directed optimizations *)
	  
      | "-fprofile-use"
          (* Enable common options for performing profile
             feedback directed optimizations *)
	  
      | "-fprofile-values"
          (* Insert code to profile values of expressions *)
	  
      | "-freg-struct-return"
          (* Return small aggregates in registers *)
	  
      | "-fregmove"
          (* Enables a register move optimization *)
	  
      | "-frename-registers"
          (* Perform a register renaming optimization pass *)
	  
      | "-freorder-blocks"
          (* Reorder basic blocks to improve code placement *)
	  
      | "-freorder-blocks-and-partition"
	  (* Reorder basic blocks and partition into hot
             and cold sections *)
	  
      | "-freorder-functions"
          (* Reorder functions to improve code placement *)
	  
      | "-frerun-cse-after-loop"
	  (* Add a common subexpression elimination pass after
             loop optimizations *)
	  
      | "-frerun-loop-opt" 
          (* Run the loop optimizer twice *)
	  
      | "-freschedule-modulo-scheduled-loops"
	  (* Enable/Disable the traditional scheduling
             in loops that already passed modulo scheduling *)
	  
      | "-frounding-math"
          (* Disable optimizations that assume default FP
             rounding behavior *)
	  
      | "-fsched-interblock"
          (* Enable scheduling across basic blocks *)
	  
      | "-fsched-spec"
          (* Allow speculative motion of non-loads *)
	  
      | "-fsched-spec-load"
          (* Allow speculative motion of some loads *)
	  
      | "-fsched-spec-load-dangerous"
	  (* Allow speculative motion of more loads *)
	  
      | "-fsched-stalled-insns"
	  (* Allow premature scheduling of queued insns *)
	  

      | "-fsched2-use-superblocks"
	  (* If scheduling post reload, do superblock
             scheduling *)
	  
      | "-fsched2-use-traces"
          (* If scheduling post reload, do trace scheduling *)
	  
      | "-fschedule-insns"
          (* Reschedule instructions before register allocation *)
	  
      | "-fschedule-insns2"
          (* Reschedule instructions after register allocation *)
	  
      | "-fshared-data"
          (* Mark data as shared rather than private *)
	  
      | "-fshow-column"
          (* Show column numbers in diagnostics, when
             available.  Default on *)
	  
      | "-fsignaling-nans"
          (* Disable optimizations observable by IEEE
             signaling NaNs *)
	  
      | "-fsingle-precision-constant"
	  (* Convert floating point constants to single
             precision constants *)
	  
      | "-fsplit-ivs-in-unroller"
	  (* Split lifetimes of induction variables when loops
             are unrolled *)
	  
      | "-fstack-check"
          (* Insert stack checking code into the program *)
	  
      | "-fstack-limit"
	  (* This switch lacks documentation *)
	  
      | "-fstack-protector"
          (* Use propolice as a stack protection method *)
	  
      | "-fstack-protector-all"
	  (* Use a stack protection method for every function *)
	  
      | "-fstrength-reduce"
          (* Perform strength reduction optimizations *)
	  
      | "-fstrict-aliasing"
          (* Assume strict aliasing rules apply *)
	  
      | "-fsyntax-only"
          (* Check for syntax errors, then stop *)
	  
      | "-ftest-coverage"
          (* Create data files needed by "gcov" *)
	  
      | "-fthread-jumps"
          (* Perform jump threading optimizations *)
	  
      | "-ftime-report"
          (* Report the time taken by each compiler pass *)
      | "-ftracer"
          (* Perform superblock formation via tail duplication *)
	  
      | "-ftrapping-math"
          (* Assume floating-point operations can trap *)
	  
      | "-ftrapv"
          (* Trap for signed overflow in addition, subtraction 
             and multiplication *)
	  
      | "-ftree-ccp"
          (* Enable SSA-CCP optimization on trees *)
	  
      | "-ftree-ch"
          (* Enable loop header copying on trees *)
	  
      | "-ftree-combine-temps"
          (* Coalesce memory temporaries in the SSA->normal
             pass *)

      | "-ftree-copy-prop"
          (* Enable copy propagation on trees *)
	  
      | "-ftree-copyrename"
          (* Replace SSA temporaries with better names in
             copies *)
	  
      | "-ftree-dce"
          (* Enable SSA dead code elimination optimization on
             trees *)
	  
      | "-ftree-dominator-opts"
	  (* Enable dominator optimizations *)
	  
      | "-ftree-dse"
          (* Enable dead store elimination *)
	  
      | "-ftree-fre"
          (* Enable Full Redundancy Elimination (FRE) on trees *)
	  
      | "-ftree-loop-im"
          (* Enable loop invariant motion on trees *)
	  
      | "-ftree-loop-ivcanon"
          (* Create canonical induction variables in loops *)
	  
      | "-ftree-loop-linear"
          (* Enable linear loop transforms on trees *)
	  
      | "-ftree-loop-optimize"
          (* Enable loop optimizations on tree level *)
	  
      | "-ftree-lrs"
          (* Perform live range splitting during the SSA->normal pass *)
	  
      | "-ftree-pre"
          (* Enable SSA-PRE optimization on trees *)

      | "-ftree-salias"
          (* Perform structural alias analysis *)
	  
      | "-ftree-sink"
          (* Enable SSA code sinking on trees *)
	  
      | "-ftree-sra"
          (* Perform scalar replacement of aggregates *)
	  
      | "-ftree-store-ccp"
          (* Enable SSA-CCP optimization for stores and loads *)
	  
      | "-ftree-store-copy-prop"
	  (* Enable copy propagation for stores and loads *)

      | "-ftree-ter"
          (* Replace temporary expressions in the SSA->normal
             pass *)
	  
      | "-ftree-vect-loop-version"
	  (* Enable loop versioning when doing loop
             vectorization on trees *)
	  
      | "-ftree-vectorize"
          (* Enable loop vectorization on trees *)
	  
      | "-ftree-vrp"
          (* Perform Value Range Propagation on trees *)
	  
      | "-funit-at-a-time"
          (* Compile whole compilation unit at a time *)
	  
      | "-funroll-all-loops"
          (* Perform loop unrolling for all loops *)
	  
      | "-funroll-loops"
          (* Perform loop unrolling when iteration count is
             known *)
	  
      | "-funsafe-loop-optimizations"
	  (* Allow loop optimizations to assume that the loops
             behave in normal way *)
	  
      | "-funsafe-math-optimizations"
	  (* Allow math optimizations that may violate IEEE or
             ISO standards *)
	  
      | "-funswitch-loops"
          (* Perform loop unswitching *)
	  
      | "-funwind-tables"
          (* Just generate unwind tables for exception handling *)
	  
      | "-fvar-tracking"
          (* Perform variable tracking *)
	  
      | "-fvariable-expansion-in-unroller"
	  (* Apply variable expansion when loops are
             unrolled *)

      | " -fverbose-asm"
          (* Add extra commentary to assembler output *)
	  
      | "-fvpt"
          (* Use expression value profiles in optimizations *)
	  
      | "-fweb"
          (* Construct webs and split unrelated uses of single
             variable *)
	  
      | "-fwhole-program"
          (* Perform whole program optimizations *)
	  
      | "-fwrapv"
          (* Assume signed arithmetic overflow wraps around *)
	  
      | "-fzero-initialized-in-bss"
	  (* Put zero initialized data in the bss section *)
	  
      | "-g"
          (* Generate debug information in default format *)
	  
      | "-gcoff"
          (* Generate debug information in COFF format *)
	  
      | "-gdwarf-2"
          (* Generate debug information in DWARF v2 format *)
	  
      | "-ggdb"
          (* Generate debug information in default extended
             format *)
	  
      | "-gstabs"
          (* Generate debug information in STABS format *)
	  
      | "-gstabs+"
          (* Generate debug information in extended STABS
             format *)
	  
      | "-gvms"
          (* Generate debug information in VMS format *)
	  
      | "-gxcoff"
          (* Generate debug information in XCOFF format *)
	  
      | "-gxcoff+"
          (* Generate debug information in extended XCOFF
             format *)
	  
      | "-p"
          (* Enable function profiling *)
	  
      | "-pedantic"
          (* Issue warnings needed for strict compliance to 
             the standard *)
	  
      | "-pedantic-errors"
          (* Like -pedantic but issue them as errors *)
	  
      | "-quiet"
          (* Do not display functions compiled or elapsed time *)
	  
      | "-w"
          (* Suppress warnings *)

      | "-std=iso9899:1990"
      | "-fno-show-column"
      | "-fno-builtin"
      | "-ansi"
      | "-Wall"
      | "-mtune=generic" ->
	  begin
	    args.compiler_args <- args.compiler_args @ [M.Switch [arg]];
	  end
      | "-o" ->
	  begin
	    args.output <- get_next_value ~ignore:false
	  end
      | "-auxbase" ->
	  begin
	    args.input_basename <- get_next_value ~ignore:false;
	    args.compiler_args <- args.compiler_args
	    @ [M.Switch [arg;args.input_basename]]
	  end
	    
      | "-imultilib"
      | "-auxbase-strip" 
      | "-dumpbase" ->
	  begin
	    args.compiler_args <- 
	      args.compiler_args @ [M.Switch [arg;get_next_value ~ignore:false]];
	  end
	    
      | "--param" ->
	  begin
	    args.compiler_args <- 
	      args.compiler_args @ [M.Switch [arg;get_next_value ~ignore:false]];
	  end
	    
      | _ -> 
	  begin
	    if M.is_switcher arg then
	      try
		let (prefix, _) = M.split_complex_arg
		  cpp_complex_arg_prefix arg
                in
		args.compiler_args <-args.compiler_args @ [M.Switch [arg]];
	      with
		  M.File str -> 
		    args.compiler_args <- args.compiler_args @ [M.Switch [arg]]
	    else
	      args.input <- arg
	  end
    in
    incr idx
  done;
  let _ = idx := 1
  in
  while !idx <= max_idx 
  do
    let arg = Array.get argv !idx
    in
    let _ = match arg with
      | "-o" -> ignore(get_next_value ~ignore:true)
      | _ -> 
	  begin
	    args.all_but_output <- args.all_but_output @ [M.Switch [arg]]
	  end
    in incr idx;
  done;
  if (args.input = "") then 
    begin
      print_endline "No input is specified";
      exit 0
    end;
  args    
      
let doit ccx sys_argv =
  let oexec_cmd = M.make_cmd (Array.to_list sys_argv)
  in
  let args = process_ccx_args ccx sys_argv
  in
  if args.help || args.target_help || args.version then
    M.exec_argv (ccx_real ccx) sys_argv
  else if args.pch_preprocess then
    let arg_list = 
      match (Array.to_list sys_argv) with
	| a::l -> l
	| _ -> []
    in
    let cmd = M.make_cmd ((ccx_real ccx)::arg_list)
    in Sys.command cmd
  else if args.e_preprocess then
    let arg_list = 
      match (Array.to_list sys_argv) with
	| a::l -> l
	| _ -> []
    in
    let cmd = M.make_cmd ((ccx_real ccx)::arg_list)
    in Sys.command cmd
  else
    begin
      let (argv, input, output) = 
	if args.preprocessed then
	  (args.compiler_args @[M.Switch ["-fpreprocessed"]], args.input, args.output)
	else
	  begin
	    let tmp_file = 
	      if M.end_with_substr [".c"] args.input then
		M.get_temp_filename 
		  args.save_temps 
		  (M.get_basename args.input ".c") ".i"
	      else if M.end_with_substr [".cc"] args.input then
		M.get_temp_filename 
		  args.save_temps 
		  (M.get_basename args.input ".cc") ".i"
	      else if M.end_with_substr [".cxx"] args.input then
		M.get_temp_filename 
		  args.save_temps 
		  (M.get_basename args.input ".cxx") ".i"
	      else
		begin
		  print_string ("unknown file type " ^ args.input);
		  assert false
		end
	    in
	    let cpp_exec = (ccx_real ccx) ^ " -E"
	    in
	    let cmd = 
	      M.make_argument_type_cmd ((M.Exec cpp_exec)::args.all_but_output
	      @ [M.Switch ["-quiet";"-fpch-preprocess";"-o";tmp_file]])
	    in 
	    M.print_msg (M.mlite_bgrab_ccx ^ ccx) cmd;
	    let exit_code = Sys.command cmd 
	    in
            if (exit_code = 0) then
	      (args.compiler_args @ [M.Switch ["-fpreprocessed"]], tmp_file, args.output)
	    else
	      exit exit_code
	  end
      in
      let input = M.create_ccx_input_zip ccx oexec_cmd argv input
      in exit (act_and_save ~ccx ~input ~output)
    end
