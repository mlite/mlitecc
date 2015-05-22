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

exception Error
exception True of (string * int)
exception File of string

let mlite_bgrab_ccx = "mlite.bgrab."

let mlite_bgrab_as = "mlite.bgrab.as"
let mlite_bgrab_ar = "mlite.bgrab.ar"
let mlite_bgrab_ld = "mlite.bgrab.ld"


type argument_type =
  | Exec of string
  | Switch of string list
  | Library of string (* -lm file *)
  | CrtObj of string (* C runtime object *)
  | Objects of (string list ref) (* Object *)
  | CFile of string (* C file *)
  | CPPFile of string (* C++ file *)
  | IFile of string (* Preprocessed I file *)
  | SFile of string (* Assembly file *)

and file_section = 
  | Root
  | SubDir of string
  | NormalFile of string

let is_switcher str =
  (String.get str 0) = '-' 

let start_with_char c str = 
  (String.get str 0) = c

let translation_serno = ref 0

let get_serno () =
  let _ = incr translation_serno
  in !translation_serno


let split_complex_arg lst str = 
  try
    List.iter
      (fun prefix ->
	let prefix_len = String.length prefix
	in
	if String.length str >= prefix_len then
	  let sub = String.sub str 0 prefix_len
	  in
	  if String.compare sub prefix = 0 then
	    raise (True (prefix, prefix_len))
      ) lst;
    raise (File str)
  with
    | True (prefix, prefix_len) ->
	(prefix, String.sub str 
	  prefix_len (String.length str - prefix_len))


let end_with_substr lst str = 
  try
    List.iter
      (fun substr ->
	let substr_len = String.length substr
	and str_len = String.length str
	in
	if str_len >= substr_len then
	  let sub = String.sub str (str_len - substr_len) substr_len
	  in
	  if String.compare sub substr = 0 then
	    raise (True (substr, substr_len))
      ) lst;
    false
  with
    | True _ -> 
	true




let load_env env =
  try
    Sys.getenv env
  with
    | Not_found ->
	Printf.eprintf "env %s is not defined" env;
	prerr_newline ();
	exit 1

let check_env env = 
  try 
    let s = Sys.getenv env
    in
    if s = "true" then
      true
    else
      false
  with
    | Not_found -> false


let bgrab_capture = check_env "BGRAB_CAPTURE"
let gnu_as_real = load_env "GNU_AS_REAL"
let gnu_cc1_real = load_env "GNU_CC1_REAL"
let gnu_cc1plus_real = load_env "GNU_CC1PLUS_REAL"
let gnu_ld_real = load_env "GNU_LD_REAL"
let gnu_ar_real = load_env "GNU_AR_REAL"

let make_cmd argv =
  let cmd = List.fold_left (fun a b -> a ^ b ^ " ") "" argv
  in (String.escaped cmd)

let make_argument_type_cmd argv =
  let cmd = List.fold_left 
    (fun a b -> 
      let b_str = match b with
	| Exec s -> s
	| Switch l -> List.fold_left (fun a b -> a ^ b ^ " ") "" l
	| Library s -> s
	| CrtObj s -> s
	| Objects l -> List.fold_left (fun a b -> a ^ b ^ " ") "" !l
	| CPPFile s -> s
	| CFile s -> s
	| IFile s -> s
	| SFile s -> s
      in a ^ b_str ^ " ") "" argv
  in 
  (String.escaped cmd)


let exec_argv exec argv = 
  let arg_list = 
    match (Array.to_list argv) with
      | a::l -> l
      | _ -> []
  in
  let cmd = make_cmd (exec::arg_list)
  in exit (Sys.command cmd)


let get_temp_filename save_temps orig suffix =
  if save_temps then
    orig ^ suffix
  else
    Filename.temp_file "mlite_" suffix


let get_basename name suffix =
  if name <> "" then
    if Filename.check_suffix name suffix then
      Filename.chop_suffix name suffix
    else
      assert false
  else
    assert false


let test_only = ref false

let exec_cmd cmd =
  if not !test_only then
    Sys.command cmd
  else
    0


let rec split_dir file = 
  match file with
    | "." -> []
    | _ ->
	begin
	  if String.get file 0 = '/' then
	    Root::(split_dir (String.sub file 1 (String.length file - 1)))
	  else
	    try 
	      let left_idx = String.index file '/' 
	      in
	      let subdir = String.sub file 0 left_idx
	      in
	      (SubDir subdir)::
		(split_dir 
		  (String.sub file (left_idx + 1) 
		    (String.length file - left_idx - 1)))
	    with
	      | Not_found -> [SubDir file]
	end

let get_mlite_output_home () = 
  try
    Sys.getenv ("MLITE_OUTPUT_HOME")
  with
    | Not_found ->
	let home_dir = Sys.getenv ("HOME")
	in  Filename.concat home_dir ".mlite"


let get_absolute_build_dir_section () =   
  let absolute_workspace_dir = 
    Filename.concat (get_mlite_output_home ()) "build"
  in
  let build_id = 
    try
      Sys.getenv ("MLITE_BUILD_ID")
    with
      | Not_found ->
	  begin
	    try
	      Sys.getenv ("USER")
	    with
	      | Not_found -> 
		  Printf.eprintf ("neither env MLITE_BUILD_ID nor USER is defined");
		  exit 1
	  end
  in
  let str = Filename.concat absolute_workspace_dir build_id
  in split_dir str

let get_absolute_log_dir_section () = 
  let absolute_workspace_dir = 
    Filename.concat (get_mlite_output_home ()) "log"
  in 
  let build_id = 
    try
      Sys.getenv ("MLITE_BUILD_ID")
    with
      | Not_found ->
	  begin
	    try
	      Sys.getenv ("USER")
	    with
	      | Not_found -> 
		  Printf.eprintf ("neither env MLITE_BUILD_ID nor USER is defined");
		  exit 1
	  end
  in
  let str = Filename.concat absolute_workspace_dir build_id
  in split_dir str

let create_dir full_file_name = 
  let rec crt_dir str file_sections =
    match file_sections with
      | Root::l -> crt_dir "/" l
      | (SubDir s)::l -> 
	  begin
	    let subdir = Filename.concat str s
	    in
	    if not (Sys.file_exists subdir) then
	      begin
		let exit_code = Sys.command ("mkdir " ^ subdir)
		in
		if exit_code <> 0 then
		  begin
		    Printf.eprintf "fail to create dir %s\n" subdir;
		    exit exit_code
		  end
	      end;
	    crt_dir subdir l
	  end
      | (NormalFile s)::l -> 
	  assert (l = [])
      | [] -> ()
  in crt_dir "" full_file_name

let file_sections_to_string file = 
  List.fold_left
    (fun a b -> 
      match b with
	| Root -> "/"
	| SubDir s -> 
	    Filename.concat a s
	| NormalFile s -> 
	    Filename.concat a s
    ) "" file

let log_channel = 
  let file_sections = (get_absolute_log_dir_section ()) @ [NormalFile "mlite.bgrab.log"]
  in
  let _ = create_dir file_sections
  and filename = file_sections_to_string file_sections
  in
  let out_channel = open_out_gen
  [Open_creat;Open_text;Open_wronly;Open_nonblock;Open_append] 
  0o600 filename
  in
  let _ = at_exit (fun _ -> close_out out_channel)
  in out_channel
        
let print_msg tag cmd = 
  if check_env "MLITE_BGRAB_DEBUG" then
    output_string log_channel ("[" ^ tag ^ "] " ^ cmd ^ "\n")
      
      
(*
let start enable_flag real_cmo doit argv = 
  let _ = Dynlink.init ()
  in
  let _ = 
    try
      let str = Sys.getenv enable_flag
      in
      if str = "true" then
	begin
          let file = 
	    try
	      Sys.getenv real_cmo
	    with
	      | Not_found ->
		  Printf.eprintf "env %s is not defined" real_cmo;
		  prerr_newline ();
		  exit 1
	  in
	  try 
	    Dynlink.loadfile file
	  with Dynlink.Error(e) ->
	    prerr_string (Dynlink.error_message e ^ "\n");
	    prerr_newline ();
	    exit 1
	end
    with Not_found -> ()
  in doit argv
*)



let clean_up_absolute_build_dir () =
  let file = get_absolute_build_dir_section ()
  in
  let str = file_sections_to_string file
  in
  let exit_code = Sys.command ("rm -rf " ^ str)
  in
  if (exit_code <> 0) then
    begin
      Printf.eprintf "fail to delete %s\n" str;
      exit exit_code
    end


let is_zip_file file =
  let tmp = Filename.temp_file "" ""
  in
  let cmd = "file " ^ file ^ ">" ^ tmp
  in
  let exit_code = Sys.command cmd
  in
  if (exit_code <> 0) then
    let _ = Printf.eprintf "fail to exec '%s'\n" cmd;
    in exit exit_code
  else
    begin
      let in_chan = open_in tmp
      in
      try
	let str = input_line in_chan
	in
	let _ = close_in in_chan
	in
	let b = Str.string_match (Str.regexp ".+Zip") str 0
	in b
      with
	| End_of_file ->
	    false
    end

let get_zip_file_name file suffix = 
  let build_dir = (get_absolute_build_dir_section ())
  in
  let basename = Filename.basename file
  and dirname = Filename.dirname file
  in
  let file_section_list = split_dir dirname
  in
  let full_file_dir = 
    match file_section_list with
      | Root::l -> build_dir @ l 
      | _ ->
	  begin
	    let current_working_dir = Sys.getcwd ()
	    in
	    let current_working_dir = 
	      match split_dir current_working_dir with
		| Root::l -> l
		| _ -> assert false
	    in
	    build_dir @ current_working_dir @ file_section_list
	  end
  in
  let _ = create_dir full_file_dir
  in
  file_sections_to_string (full_file_dir  @ [NormalFile (basename ^ suffix)])
  

let append_zip: filter:(Zip.entry -> (bool * string * string * string)) -> src:string -> 
  dest:Zip.out_file -> unit = 
  fun ~filter ~src ~dest:write_out ->
    let read_src = Zip.open_in src
    in
    let entries = Zip.entries read_src
    in
    let _ = 
      List.iter
	(fun entry ->
	  let (copy_over, new_entry_filename, new_entry_extra, new_entry_comment) = filter entry
	  in
	  if (copy_over) then
	    Zip.add_entry (Zip.read_entry read_src entry) write_out
	      new_entry_filename ~extra:new_entry_extra ~comment:new_entry_comment
	) entries
    in
    Zip.close_in read_src
      
let create_ccx_input_zip ccx oexec_cmd argv input_file =
  let tag = mlite_bgrab_ccx ^ ccx
  in
  let input_zip = get_zip_file_name input_file ("." ^ ccx ^ "_in")
  in
  let zip_file = Zip.open_out ~comment: (ccx ^ "_in") input_zip
  in
  let _ = print_msg tag ("cp " ^ input_file ^ " to " ^ input_zip)
  and _ = Zip.copy_file_to_entry input_file zip_file (ccx ^ "_input_file.i")
  in 
  let _ = Zip.add_entry oexec_cmd zip_file (ccx ^ ".cmd.txt")
  and _ = Zip.add_entry (Marshal.to_string argv []) zip_file (ccx ^ ".arg.bin")
  in
  Zip.close_out zip_file;
  input_zip



let unzip_ccx_input_zip: ccx:string -> input_file:string -> (argument_type list * string) =
  fun ~ccx ~input_file ->
    let input_entry_name = ccx ^ "_input_file.i"
    in
    let zip_file = Zip.open_in input_file
    in
    let comment = Zip.comment zip_file
    in
    assert (comment = (ccx ^ "_in"));
    let entry = Zip.find_entry zip_file input_entry_name
    and arg_entry = Zip.find_entry zip_file (ccx ^ ".arg.bin")
    in
    let _ = Zip.copy_entry_to_file zip_file entry input_entry_name
    and bstring = Zip.read_entry zip_file arg_entry
    in (Marshal.from_string bstring 0, input_entry_name)

    

let create_ccx_output_zip: ccx:string -> input_zip:string -> output:string -> string =
  fun ~ccx ~input_zip ~output:output_file ->
    let tag = mlite_bgrab_ccx ^ ccx
    in
    let output_zip = get_zip_file_name output_file ("." ^ ccx ^ "_out")
    in
    let zip_file = Zip.open_out ~comment:(ccx ^ "_out") output_zip
    in
    let filter = 
      fun s -> let _ = 
	print_msg tag ("cp " ^ s.Zip.filename ^ " from " ^ input_zip ^ " to " ^ output_zip)
      in (true, s.Zip.filename, s.Zip.extra, s.Zip.comment)
    in
    let _ = append_zip ~filter ~src:input_zip ~dest:zip_file 
    in 
    let _ = Zip.copy_file_to_entry output_file zip_file (ccx ^ "_output_file.s") ~comment:output_file
    in
    Zip.close_out zip_file;
    output_zip
    
      
let create_as_input_zip: oexec_cmd:string -> argv:argument_type list -> input_file:string -> string =
  fun ~oexec_cmd ~argv ~input_file ->
    let tag = mlite_bgrab_as
    in
    let input_zip = get_zip_file_name input_file ".as_in"
    in
    let zip_file = Zip.open_out ~comment:"as_in" input_zip
    in
    let cc1_output_zip = get_zip_file_name input_file ".cc1_out"
    and cc1plus_output_zip = get_zip_file_name input_file ".cc1plus_out"
    in
    let filter = 
      (fun entry -> 
	match entry.Zip.filename with
	  | "cc1_output_file.s" -> 
	      let _ = print_msg tag ("cp cc1_output_file.s in " ^
		cc1_output_zip ^ " to as_input_file.s in " ^ input_zip)
	      in (true, "as_input_file.s", entry.Zip.extra, entry.Zip.comment)
	  | "cc1plus_output_file.s" -> 
	      let _ = print_msg tag ("cp cc1plus_output_file.s in " ^
		cc1_output_zip ^ " to as_input_file.s in " ^ input_zip)
	      in (true, "as_input_file.s", entry.Zip.extra, entry.Zip.comment)
	  | _ -> 
	      let _ = print_msg tag ("cp " ^ entry.Zip.filename ^ " in " ^
		cc1_output_zip ^ " to " ^ entry.Zip.filename ^ " in " ^ input_zip)
	      in (true, entry.Zip.filename, entry.Zip.extra, entry.Zip.comment))
    in
    let _ = 
      if (Sys.file_exists cc1_output_zip) then
	append_zip ~filter ~src:cc1_output_zip ~dest:zip_file
      else if (Sys.file_exists cc1plus_output_zip) then
	append_zip ~filter ~src:cc1plus_output_zip ~dest:zip_file
      else
	let _ = Zip.copy_file_to_entry input_file zip_file "as_input_file.s"
	in ()
    and _ = Zip.add_entry oexec_cmd zip_file "as.cmd.txt"
    and _ = Zip.add_entry (Marshal.to_string argv []) zip_file "as.arg.bin"
    in
    Zip.close_out zip_file;
    input_zip
    

let unzip_as_input_zip: string -> (argument_type list * string) =
  fun input_file ->
    assert (is_zip_file input_file);
    let zip_file = Zip.open_in input_file
    in
    let comment = Zip.comment zip_file
    in
    assert (comment = "as_in");
    let entry = Zip.find_entry zip_file "as_input_file.s"
    and arg_entry = Zip.find_entry zip_file "as.arg.bin"
    in
    let _ = Zip.copy_entry_to_file zip_file entry "as_input_file.s"
    and bstring = Zip.read_entry zip_file arg_entry
    in (Marshal.from_string bstring 0, "as_input_file.s")
	 
	 
let create_as_output_zip: input_zip:string -> output:string -> string =
  fun ~input_zip ~output:output_file ->
    let output_zip = get_zip_file_name output_file ".as_out"
    in
    let zip_file = Zip.open_out ~comment:"as_out" output_zip
    in
    let filter = 
      fun s -> 
	let _ = print_msg mlite_bgrab_as 
	  ("cp " ^ s.Zip.filename ^ " from " ^ input_zip ^ " to " ^ output_zip)
	in (true, s.Zip.filename, s.Zip.extra, s.Zip.comment)
    in
    let _ = append_zip ~filter ~src:input_zip ~dest:zip_file 
    in 
    let _ = Zip.copy_file_to_entry output_file 
      zip_file "as_output_file.o" ~comment:output_file
    in
    Zip.close_out zip_file;
    output_zip


let chop_ld_output_suffix output = 
  if (Filename.check_suffix output ".o") then
    Filename.chop_suffix output ".o"
  else if (Filename.check_suffix output ".out") then
    Filename.chop_suffix output ".out"
  else if (Filename.check_suffix output ".a") then
    Filename.chop_suffix output ".a"
  else
    output


let copy_obj_to_dest_zip tag dest_zip_name dest_zip obj = 
  let obj_zip = get_zip_file_name obj ".as_out"
  and basename = Filename.basename obj
  in
  let base = chop_ld_output_suffix basename
  in
  let _ = 
    if (Sys.file_exists obj_zip) then
      append_zip 
	~filter:(fun entry -> 
	  let fname = entry.Zip.filename
	  in
	  let msg = ("cp " ^ fname ^ " in " ^ obj_zip ^ " to ")
	  in
	  match fname with
	    | "cc1_input_file.i" -> 
		let _ = print_msg tag (msg ^ base ^ ".cc1.i in " 
		^ dest_zip_name)
		in (true, base ^ ".cc1.i", entry.Zip.extra, entry.Zip.comment)
		     
	    | "cc1plus_input_file.i" -> 
		let _ = print_msg tag (msg ^ base ^ ".cc1plus.i in " 
		^ dest_zip_name)
		in (true, base ^ ".cc1plus.i", entry.Zip.extra, entry.Zip.comment)
		     
	    | "as_input_file.s" -> 
		let _ = print_msg tag (msg ^ base ^ ".s in "
		^ dest_zip_name)
		in (true,  base ^ ".s", entry.Zip.extra,
		entry.Zip.comment)

	    | "cc1.cmd.txt" ->
		let _ = print_msg tag (msg ^ base ^ ".cc1.cmd.txt in " ^ dest_zip_name)
		in (true, base ^ ".cc1.cmd.txt",
		entry.Zip.extra, entry.Zip.comment)

	    | "cc1.arg.bin" ->
		let _ = print_msg tag (msg ^ base ^ ".cc1.arg.bin in " ^ dest_zip_name)
		in (true, base ^ ".cc1.arg.bin",
		entry.Zip.extra, entry.Zip.comment)
		     
	    | "cc1plus.cmd.txt" ->
		let _ = print_msg tag (msg ^ base ^ ".cc1plus.cmd.txt in " ^ dest_zip_name)
		in (true, base ^ ".cc1plus.cmd.txt", entry.Zip.extra, entry.Zip.comment)
		     
	    | "cc1plus.arg.bin" ->
		let _ = print_msg tag (msg ^ base ^ ".cc1plus.arg.bin in " ^ dest_zip_name)
		in (true, base ^ ".cc1plus.arg.bin", entry.Zip.extra, entry.Zip.comment)
		     
	    | "as.cmd.txt" ->
		let _ = print_msg tag (msg ^ base ^
		  ".as.cmd.txt in " ^ dest_zip_name)
		in (true, base ^ ".as.cmd.txt", entry.Zip.extra, entry.Zip.comment)
		     
	    | "as.arg.bin" ->
		let _ = print_msg tag (msg ^ base ^ ".as.arg.bin in " ^ dest_zip_name)
		in (true, base ^ ".as.arg.bin", entry.Zip.extra, entry.Zip.comment)
		     
	    | "as_output_file.o" -> 
		let _ = print_msg tag (msg ^ basename ^ " in " ^ dest_zip_name)
		in (true, basename, "", "")

	    | _ -> 
		let _ = print_msg tag (msg ^ fname ^ " in " ^ dest_zip_name)
		in (true, entry.Zip.filename, entry.Zip.extra, entry.Zip.comment)
	) ~src:obj_zip ~dest:dest_zip
    else
      begin
	Printf.eprintf "object zip %s does not exist.\n" obj_zip;
	exit 1;
      end
  in basename
    
  

let create_ar_output_zip: oexec_cmd:string -> exec_cmd:string -> argv:argument_type list -> output:string -> input:argument_type list
  -> string =
  fun ~oexec_cmd ~exec_cmd ~argv ~output ~input ->
    let tag = mlite_bgrab_ar
    in
    let output_zip = get_zip_file_name output ".ar_out"
    in
    let zip_file = Zip.open_out ~comment:"ar_out" output_zip
    in
    let new_input = List.map
      (fun arg ->
	match arg with
	  | Objects lst -> 
	      let new_list = 
		List.map (copy_obj_to_dest_zip tag output_zip zip_file) !lst
	      in Objects (ref new_list)
	  | _ -> arg
      ) input
    in
    let _ = Zip.add_entry exec_cmd zip_file (output ^ ".ar_args")
    and _ = Zip.add_entry oexec_cmd zip_file (output ^ ".ar.cmd.txt")
    and _ = Zip.add_entry (Marshal.to_string (argv, output, new_input) []) 
      zip_file (output ^ ".ar.args.bin")
    in
    let _ = Zip.close_out zip_file
    in output_zip


let create_ld_output_zip: oexec_cmd:string -> exec_cmd:string -> argv:argument_type list -> output:string -> string =
  fun ~oexec_cmd ~exec_cmd ~argv ~output ->
    let tag = mlite_bgrab_ld
    in
    let output_zip = get_zip_file_name output ".ld_out"
    in
    let zip_file = Zip.open_out ~comment:"ld_out" output_zip
    in
    let new_argv = List.map
      (fun arg ->
	match arg with
	  | Objects lst -> 
	      let new_list = List.map (copy_obj_to_dest_zip tag output_zip zip_file) !lst
	      in Objects (ref new_list)
		   
	  | Library obj ->
	      begin
		let obj_zip = get_zip_file_name obj ".ar_out"
		and basename = Filename.basename obj
		in
		let _ = 
		  if (Sys.file_exists obj_zip) then
		    append_zip 
		      ~filter:(fun entry -> 
			let _ = print_msg tag ("cp " ^ entry.Zip.filename ^ " from " ^
			  obj_zip ^ " to " ^ output_zip)
			in (true, entry.Zip.filename, entry.Zip.extra, entry.Zip.comment)
		      ) ~src:obj_zip ~dest:zip_file
		  else
		    begin
		      Printf.eprintf "object zip %s does not exist.\n" obj_zip;
		      exit 1;
		    end
		in Library basename
	      end
	  | CrtObj obj ->
	      let basename = Filename.basename obj
	      in
	      let _ = Zip.copy_file_to_entry obj zip_file basename ~comment:obj
	      in CrtObj basename
	  | _ -> arg
      ) argv
    in
    let _ = Zip.add_entry exec_cmd  zip_file "ld_input_args.txt"
      ~extra:oexec_cmd ~comment:(Marshal.to_string (new_argv, output) [])
    and _ = Zip.add_entry oexec_cmd zip_file "ld.cmd.txt"
    and _ = Zip.add_entry exec_cmd zip_file "ld_real.cmd.txt"
    and _ = Zip.add_entry (Marshal.to_string (new_argv, output) []) zip_file "ld_real.cmd.bin"
    in
    let _ = Zip.close_out zip_file
    in output_zip





let split_str str c =
  let rec inner_split_str str =
    try
      let i = String.index str c
      and len = String.length str
      in
      (String.sub str 0 i)::(inner_split_str (String.sub str (i+1) (len - i - 1)))
    with
      | Not_found -> [str]
  in inner_split_str str


let pass_to cmd argv = 
  let real_cmd = match cmd with
    | "cc1" -> gnu_cc1_real
    | "cc1plus" -> gnu_cc1plus_real
    | "as" -> gnu_as_real
    | "ar" -> gnu_ar_real
    | "ld" -> gnu_ld_real
    | _ -> assert false
  in
  let cmd = Array.fold_left (fun cmd arg -> cmd ^ " arg ") real_cmd argv
  in 
  print_msg "mlite.bgrab.pass_to" cmd;
  Sys.command cmd
