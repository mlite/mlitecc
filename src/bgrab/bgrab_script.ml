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


let user_opts = []


let banner =
  "(mlite)_(c)c_(m)erge_(a)s_(l)d\n" ^
  "SYNTAX:\tmlite_cmal [options] file1 file2 ...\n" ^
  "\tmlite_cmal [options] --\n"

let merge ls =  ls



let merge_compile: output:string -> unit = 
  fun ~output -> 
    let out_channel = open_out_gen [Open_creat;Open_text;Open_wronly;Open_trunc] 
      0o600 (output ^ ".sh")
    in
    let print_cmd str =  output_string out_channel (str ^ "\n")
    in
    let zip_in = Zip.open_in output
    in
    let entries = Zip.entries zip_in
    in
    let (obj_files, ar_args_list) = 
      List.fold_left
	(fun (obj_lst, ar_arg_list) entry ->
	  if (M.end_with_substr [".o"] entry.Zip.filename) then
	    (obj_lst @ [entry.Zip.filename], ar_arg_list)
	  else if (M.end_with_substr [".ar_args"] entry.Zip.filename) then
	    (obj_lst, ar_arg_list @ [entry.Zip.filename])
	  else
	    (obj_lst, ar_arg_list)
	) ([], []) entries 
    in
    let _ = 
      List.iter
	(fun obj -> 
	  if Filename.check_suffix obj ".o" then
	    begin
	      let obj_basename = Filename.chop_suffix obj ".o"
	      in
	      let as_file = (obj_basename ^ ".s")
	      in
	      let _ = print_string ("looking for " ^ as_file ^ "\n")
	      in
	      try
		let as_entry = Zip.find_entry zip_in as_file
		and as_arg_entry = Zip.find_entry zip_in (obj_basename ^ ".as.arg.bin")
		in
		let as_args = Marshal.from_string (Zip.read_entry zip_in as_arg_entry) 0
		in
		let _ = 
		  try
		    let filename = obj_basename ^ ".cc1.i"
		    in
		    let obj_entry = Zip.find_entry zip_in filename
		    and cmd_entry = Zip.find_entry zip_in (obj_basename ^ ".cc1.cmd.txt")
		    and arg_entry = Zip.find_entry zip_in (obj_basename ^ ".cc1.arg.bin")
		    in
		    let _ = print_cmd ("#" ^ (Zip.read_entry zip_in cmd_entry))
		    in
		    let _ = Zip.copy_entry_to_file zip_in obj_entry filename
		    in
		    let args = Marshal.from_string (Zip.read_entry zip_in arg_entry) 0
		    in
		    let cmd = M.make_argument_type_cmd
		      ((M.Exec "CC1")::args @ [M.Switch ["-o";as_file];M.IFile filename])
		    in print_cmd cmd;
		  with
		    | Not_found ->
			begin
			  try
			    let filename = obj_basename ^ ".cc1plus.i"
			    in
			    let obj_entry = Zip.find_entry zip_in filename
			    and cmd_entry = Zip.find_entry zip_in 
			      (obj_basename ^ ".cc1plus.cmd.txt")
			    and arg_entry = Zip.find_entry zip_in 
			      (obj_basename ^ ".cc1plus.arg.bin")
			    in
			    let _ = print_cmd ("#" ^ (Zip.read_entry zip_in cmd_entry))
			    in
			    let _ = Zip.copy_entry_to_file zip_in obj_entry filename
			    in
			    let args = Marshal.from_string 
			      (Zip.read_entry zip_in arg_entry) 0
			    in
			    let cmd = M.make_argument_type_cmd
			      ((M.Exec "CC1PLUS")::args @ [M.Switch ["-o";as_file];M.IFile filename])
			    in print_cmd cmd;
			  with
			    | Not_found -> 
				Zip.copy_entry_to_file zip_in as_entry as_file
			end
		in 
		let cmd = M.make_argument_type_cmd
		  ((M.Exec "AS")::as_args @ [M.Switch ["-o";obj];M.IFile as_file])
		in print_cmd cmd
	      with
		| Not_found ->
		    let obj_entry = Zip.find_entry zip_in obj
		    in
		    let _ = print_cmd ("#" ^ obj_entry.Zip.comment)
		    in Zip.copy_entry_to_file zip_in obj_entry obj
	    end
	  else
	    assert false
	) obj_files
    in
    let _ = 
      List.iter
	(fun ar_args ->
	  let ar_args_entry = Zip.find_entry zip_in ar_args
	  in
	  let original_ar_cmd = Zip.read_entry zip_in ar_args_entry
	  in
	  let _ = print_cmd ("#" ^ original_ar_cmd)
	  in
	  let (ar_args, o, i) = Marshal.from_string (ar_args_entry.Zip.comment) 0
	  in 
	  let cmd = M.make_argument_type_cmd 
	    ((M.Exec "AR")::ar_args::[M.Switch [o];M.Objects i])
	  in print_cmd cmd
	) ar_args_list
    in
    let ld_args_entry = Zip.find_entry zip_in "ld_input_args.txt"
    in
    let original_exec_cmd = Zip.read_entry zip_in ld_args_entry
    in
    let _ = print_cmd ("#" ^ original_exec_cmd)
    in
    let (ld_args, o) = Marshal.from_string (ld_args_entry.Zip.comment) 0
    in 
    let cmd = M.make_argument_type_cmd ((M.Exec "LD")::ld_args)
    in
    let _ = print_cmd cmd
    in
    let _ = Zip.close_in zip_in
    and _ = close_out out_channel
    in ()
    


(*
let _ = 
  let filenames = ref ""
  in
  Arg.parse user_opts
    (fun str -> filenames := str) 
    banner;
  if !filenames = "" then
    ()
  else
    merge_compile !filenames
    
*)
