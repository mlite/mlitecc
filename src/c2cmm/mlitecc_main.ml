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
open Dynlink

type gcc_cmd = 
    {
      gcc_exec: string;
      mutable gcc_argv: string list;
    }
and mlitecc_cmd = 
    {
      mlitecc_exec: string;
      mutable mlitecc_disable: bool;
      mutable mlitecc_argv: string list;
    }
and argv_wrapper = 
    {
      mutable arguments: string list;
    }
      
let cpp_combination_arg_prefix = 
  [
    ("-D", String.length "-D"); 
    ("-U", String.length "-U");
    ("-I", String.length "-I");
    ("-d", String.length "-d")
  ]
    
let gcc_combination_arg_prefix = 
  [
    ("-l", String.length "-l");
    ("-L", String.length "-L");
    ("-B", String.length "-B")
  ]
    
exception Error
exception True of (string * int)
exception File of string
    
let save_temps = ref false
let test_only = ref false
let silence = ref false
let invoke_native = ref false
let preprocessed = ref false
let c99 = ref false
let use_gcc = ref false
let obj_only = ref false
let preprocess_only = ref false
  
let make_mlitecc_cmd argv in_file out_file =
  if argv.mlitecc_disable then
    "mv " ^ in_file ^ " " ^ out_file
  else
    let cmd = 
      List.fold_left
	(fun cmd arg -> cmd ^ " " ^ arg) argv.mlitecc_exec 
	argv.mlitecc_argv
    in cmd ^ " " ^ in_file ^ " -o " ^ out_file
      
      
let make_cmd argv in_file out_file =
  let cmd = 
    if out_file <> "" then
      argv.gcc_exec ^ " " ^ (Printf.sprintf "%s -o %s" in_file out_file)
    else
      argv.gcc_exec ^ " " ^ (Printf.sprintf "%s" in_file)
  in
  let cmd = List.fold_left
    (fun cmd arg -> cmd ^ " " ^ arg) cmd argv.gcc_argv
  in (String.escaped cmd)
    

let get_temp_filename orig prefix suffix =
  if !save_temps then
    orig ^ suffix
  else
    Filename.temp_file prefix suffix

let remove_file filename = 
  if !save_temps then
    ()
  else
    Sys.remove filename

let is_switcher str =
  (String.get str 0) = '-' 
  
let split_combination_arg lst str = 
  try
    List.iter
      (fun (prefix, prefix_len) -> 
	if String.length str > prefix_len then
	  let sub = String.sub str 0 prefix_len
	  in
	  if String.compare sub prefix = 0 then
	    raise (True (prefix, prefix_len))
      ) lst;
    raise (File str)
  with
  | True (prefix, prefix_len) ->
      (prefix, String.sub str prefix_len (String.length str - prefix_len))
	

let exec_cmd cmd =
  if not !test_only then
    Sys.command cmd
  else
    0

let print_cmd cmd = 
  if !silence then
    ()
  else
    begin
      print_string cmd;
      print_newline ()
    end

let preprocess ~(cpp_cmd:gcc_cmd) infile outfile = 
  let cpp_cmd = make_cmd cpp_cmd infile outfile
  in
  print_cmd cpp_cmd;
  let _ = exec_cmd cpp_cmd
  in ()
  
    
let compile ~(cpp_cmd:gcc_cmd) (mlitecc_cmd) ~(gcc_cmd:gcc_cmd) ~(infile:string)
    ~(outfile_opt:string option) (*obj_file*) =
  let basename = match outfile_opt with
    | Some obj_file ->
	if Filename.check_suffix obj_file ".o" then
	  Filename.chop_suffix obj_file ".o"
	else
	  obj_file
    | None ->
	if Filename.check_suffix infile ".c" then
	  Filename.chop_suffix infile ".c"
	else if Filename.check_suffix infile ".i" then
	  Filename.chop_suffix infile ".i"
	else assert false
  in
  let infile_type = Mlitecc_files.check_file_type infile
  in
  match infile_type with
    | Mlitecc_files.C_FILE 
    | Mlitecc_files.I_FILE ->
	begin
	  let preprocessed_infile = 
	    if infile_type = Mlitecc_files.C_FILE then
	      let cpp_out_file = get_temp_filename basename "mlitecc_cpp_" ".i"
	      in
	      let _ = preprocess ~cpp_cmd infile cpp_out_file
	      in cpp_out_file
	    else
	      infile
	  in
	  let mlitecc_out_file = get_temp_filename basename "mlitecc_cc1_" (if !use_gcc then ".c" else ".cmm")
	  in
	  let cmd = make_mlitecc_cmd mlitecc_cmd preprocessed_infile mlitecc_out_file;
	  in
	  print_cmd cmd;
	  let _ = exec_cmd cmd
	  in 
	  let outfile = match outfile_opt with
	    | Some s -> s
	    | None -> basename ^ ".o"
	  in
	  let gcc_cmd = make_cmd gcc_cmd mlitecc_out_file outfile
	  in
	  print_cmd gcc_cmd;
	  let _ = exec_cmd gcc_cmd
	  in outfile
	end
    | Mlitecc_files.O_FILE ->
	begin
	  let outfile = match outfile_opt with
	    | Some s -> s
	    | None -> basename ^ ".o"
	  in
	  let gcc_cmd = make_cmd gcc_cmd infile outfile
	  in
	  print_cmd gcc_cmd;
	  let _ = exec_cmd gcc_cmd
	  in outfile
	end
    | _ ->
	camlp4_macro_exception "unexpected file format '%s'\n" infile
	  


let parseExtraFile (s: string) =
  let filenames = ref []
  in
  let recordFile fname =
    filenames := fname :: (!filenames)
  in
  let _ = try
    let sfile = open_in s in
    while true do
      let line = try input_line sfile with e -> (close_in sfile; raise e) in
      let linelen = String.length line in
      let rec scan (pos: int) (* next char to look at *)
          (start: int) : unit (* start of the word,
                                 or -1 if none *) =
        if pos >= linelen then
          if start >= 0 then
            recordFile (String.sub line start (pos - start))
          else
            () (* Just move on to the next line *)
        else
          let c = String.get line pos in
          match c with
            ' ' | '\n' | '\r' | '\t' ->
              (* whitespace *)
              if start >= 0 then begin
                recordFile (String.sub line start (pos - start));
              end;
              scan (pos + 1) (-1)

          | _ -> (* non-whitespace *)
              if start >= 0 then
                scan (pos + 1) start
              else
                scan (pos + 1) pos
      in
        scan 0 (-1)
    done
  with 
  | Sys_error _ -> camlp4_macro_exception "Cannot find extra file: %s\n" s
  | End_of_file -> ()
  in
  !filenames
      
let _ = 
  let mlitecc_cmd = 
    { 
      mlitecc_exec = "mlitecc1"; 
      mlitecc_disable = false; 
      mlitecc_argv = []
    }
  and qcmm_globals = ref true
  and qcmm_argv = { arguments = [] }
  and mlitecc_argv = { arguments = [] }
  and cpp_argv = { arguments = [] }
  and host_c_argv = { arguments = [] }
  and files = ref []
  and final_out_put = ref ""
  and produce_exec = ref true
  in
  let current_argv = ref mlitecc_argv
  and (_, orig_argv) = 
    Array.fold_left 
      (fun (idx, l) argv -> 
	if idx = 0 then (idx + 1, l)
	else
	  let l = match argv with 
	    | "---host-c-argv" -> []
	    | _ -> l @ [argv] 
	  in (idx + 1, l)
      ) (0, []) Sys.argv
  in
  let max_idx = (Array.length Sys.argv) 
  and idx = ref 1
  in
  while !idx <> max_idx do
    let arg = Array.get Sys.argv !idx
    in
    let _ = match arg with
      | "---gcc" -> use_gcc := true
      | "---preprocessed" -> preprocessed := true
      | "---clone" -> mlitecc_cmd.mlitecc_disable <- true
      | "---silence" -> silence := true
      | "---native" -> invoke_native := true
      | "---test-only" -> test_only := true
      | "---save-temps" -> save_temps := true
      | "---host-c-argv" -> current_argv := host_c_argv
      | "-std=c99" -> c99 := true
      | "-E" -> preprocess_only := true
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
	-> cpp_argv.arguments <- cpp_argv.arguments@[arg]
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
	-> 
	  begin
	    incr idx;
	    if !idx > max_idx then
	      raise Error;
	    let param = Array.get Sys.argv !idx
	    in
	    if is_switcher param then
	      raise Error;
	    cpp_argv.arguments <- cpp_argv.arguments @ [arg;param]
	  end
      | "-S" 
      | "-c" -> 
	  begin
	    qcmm_globals := false;
	    obj_only := true;
	    qcmm_argv.arguments <- qcmm_argv.arguments @ [arg];
	    (*mlitecc_argv.arguments <- mlitecc_argv.arguments @ [arg];*)
	    host_c_argv.arguments <- host_c_argv.arguments @ [arg];
	    produce_exec := false
	  end
      | "-pedantic-errors" -> () (* skip *)
      | "-Wall" ->
	  begin
	    cpp_argv.arguments <- cpp_argv.arguments @ [arg];
	    host_c_argv.arguments <-  host_c_argv.arguments @ [arg];
	  end
      | "-o" ->
	  begin
	    incr idx;
	    if !idx > max_idx then
	      raise Error;
	    let param = Array.get Sys.argv !idx
	    in
	    if is_switcher param then
	      raise Error;
	    final_out_put := param
	  end	 
      | _ -> 
	  begin
	    if is_switcher arg then
	      try
		let args = split_combination_arg 
		  (cpp_combination_arg_prefix @ gcc_combination_arg_prefix)
		  arg
		in
		match args with
		  | ("-I", _)
		  | ("-U", _)
		  | ("-d", _)
		  | ("-D", _) -> 
		      cpp_argv.arguments <- cpp_argv.arguments @ [arg]
		  | ("-L", _) 
		  | ("-l", _) ->
		      host_c_argv.arguments <- host_c_argv.arguments @ [arg]
		  | _ -> assert false
	      with
		  File str ->
		    (!current_argv).arguments <- (!current_argv).arguments @ [str]
	    else
	      files := !files @ [arg]
	  end
    in incr idx
  done;
  let cpp_cmd = 
    { 
      gcc_exec = "cpp"; (* cpp *)
      gcc_argv = (""::cpp_argv.arguments) 
	@ (if !c99 then  ["-std=c99"] else [])
    }
  and gcc_cmd = 
    { 
      gcc_exec = if !use_gcc then "gcc" else "qc--"; 
      gcc_argv = 
        if !use_gcc then host_c_argv.arguments
        else if !qcmm_globals then ["-globals"]
	else qcmm_argv.arguments @ ["-globals"];
      (*["-c"];*)
      (*host_c_argv.arguments 
	@ (if !preprocessed then ["-fpreprocessed"] else [])
	@ (if !c99 then  ["-std=c99"] else [])*)
    }
  and _ = mlitecc_cmd.mlitecc_argv <- mlitecc_argv.arguments
  in
  let obj_files = 
    if !preprocess_only then
      let cmd = 
	List.fold_left 
	  (fun s e -> s ^ " " ^ e) "gcc" (orig_argv)
      in
      print_cmd cmd;
      let _ = exec_cmd cmd
      in 
      if !final_out_put <> "" then
	[!final_out_put]
      else
	[]
    else
      begin
	match !files with
	  | [] -> []
	  | [in_file] -> 
	      [compile cpp_cmd mlitecc_cmd gcc_cmd in_file (Some !final_out_put)]
	  | _ ->
	      let _ = 
		if not !obj_only then
		  gcc_cmd.gcc_argv <- gcc_cmd.gcc_argv @ ["-c"]
	      in
	      List.map
		(fun infile -> 
		  let ft = Mlitecc_files.check_file_type infile 
		  in match ft with
		    | Mlitecc_files.O_FILE -> infile
		    | Mlitecc_files.AR_FILE -> infile
		    | Mlitecc_files.S_FILE -> assert false
		    | _ ->
			
			compile cpp_cmd mlitecc_cmd gcc_cmd infile None
		) !files
      end
  in
  match obj_files with
    | [in_file] -> ()
    | _ ->
	begin
	  let all_files = 
	    List.flatten 
	      (List.map 
		(fun f ->
		  if Filename.check_suffix f ".o" then
		    [f]
		  else if Filename.check_suffix f ".a" then
		    begin
		      let tmp_file = Filename.temp_file "mlitecc_ar" ".a.lst"
		      and dirname = Filename.dirname f
		      in
		      let lst_cmd = "ar t " ^ f ^ " > " ^ tmp_file
		      in
		      print_cmd lst_cmd;
		      let _ = Sys.command lst_cmd
		      in
		      let objs = List.map (fun v -> Filename.concat dirname v) 
			(parseExtraFile tmp_file)
		      in
		      remove_file tmp_file;
		      objs
		    end
		  else assert false
		) obj_files
	      )
	  in	
	  let gcc_cmd = { gcc_exec = if !use_gcc then "gcc" else "qc--"; gcc_argv = host_c_argv.arguments }
	  in
	  let in_files = 
	    List.fold_left 
	      (fun in_files obj ->
		in_files ^ " " ^ obj
	      ) "" all_files
	  in
	  let gcc_cmd = make_cmd gcc_cmd in_files !final_out_put;
	  in
	  print_cmd gcc_cmd;
	  let _ = exec_cmd gcc_cmd
	  in 
	  ()
	end
