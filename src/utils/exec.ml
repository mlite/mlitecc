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

module type UserOptType =
  sig
    val banner: string
    val user_opts: unit -> (string * Arg.spec * string) list
  end

module type UserProcessorType =
    functor (SD:Parser_driver.Driver) ->
  sig
    type t = SD.t
    val process: t -> unit
  end

module type Driver = 
  sig
    type t
    val exec:  (Parser_handle.arg list -> out_channel -> t -> unit) -> unit
    val execN: (Parser_handle.arg list -> out_channel -> string -> t list -> unit) -> unit  
    val exec_to_file:  (Parser_handle.arg list -> string -> t -> unit) -> unit
    (* this is used by linker which need to load and parse multiple files *)
  end

      
module Make (UO:UserOptType) (SD:Parser_driver.Driver) :Driver
with type t = SD.t 
      = 
  struct
    type t = SD.t
    let files = ref []
    let log_file = ref ""
    let debug = ref false
    let out_file = ref ""
	
    let default_banner = 
      "Copyright (c) 2005, 2006, 2007 Ning Wang <wangn@.uci.edu>\n\n" ^
      "Copyright (c) 2008, 2009, 2010 Ning Wang <email@ningwang.org>\n\n"

	
    let set_log_file s =
      log_file := s
    
    let set_out_file s = 
      out_file := s
	  
    let common_opts = 
      [
       ("-l", Arg.String set_log_file,
	"Output log info to the given file.");
       ("-d", Arg.Set debug, 
	"Enable debug information.");
       ("-o", Arg.String set_out_file,
        "Output to the given file.");
     ]

    let _ =
      begin
	try 
	  let _ = Mlite_config.mlite_log_rc := (Sys.getenv "MLITE_LOG_RC")
	  in ()
	with 
	  Not_found -> ()
      end;
      try
	let _ = Mlite_config.mlite_home := (Sys.getenv "MLITE_HOME")
	in ()
      with
	Not_found ->
	let _ = Mlite_config.mlite_home := (Sys.getenv "HOME") ^ "/.mlite"
	in ()

    let exec process = 
      let opts = List.append common_opts (UO.user_opts ())
      and banner = default_banner ^ UO.banner
      in 
      Arg.parse opts (fun file -> files := file :: !files) banner;
      let _ = Log.open_log false !log_file 
      and (close, out_chan) = 
        if !out_file <> "" then (true, open_out !out_file) else (false, stdout)
      in
      let log_basename = 
	if !log_file <> "" 
	then !log_file 
	else if !out_file <> "" 
	then !out_file
	else "a_log"
      in
      let args = ref [] in 
      let _ = if !debug then args := (Parser_handle.DEBUG (true))::!args in
      let _ = match Log.get_log_chan () with
      | Some chan -> args := Parser_handle.LOG_CHAN (chan)::!args
      | None -> ()
      in
      let _ = Logrc.load (log_basename, (!(Mlite_config.mlite_home) ^ "/" ^ !(Mlite_config.mlite_log_rc))) !args in
      let _ =
	match !files with 
	| [file] -> 
	    let result = SD.load (Parser_handle.INPUT_FILE(file)::!args) in 
	    let _ = match result with
	    |Some r -> process !args out_chan r
	    |None -> ()
	    in ()
	| _ -> Arg.usage opts banner
      in
      if close then close_out out_chan;
      Log.close_log ();
      Logrc.close ()


    let exec_to_file process = 
      let opts = List.append common_opts (UO.user_opts  ())
      and banner = default_banner ^ UO.banner
      in 
      Arg.parse opts (fun file -> files := file :: !files) banner;
      let _ = Log.open_log false !log_file 
      in
      let log_basename = 
	if !log_file <> "" 
	then !log_file 
	else if !out_file <> "" 
	then !out_file
	else "a_log"
      in
      let args = ref [] in 
      let _ = if !debug then args := (Parser_handle.DEBUG (true))::!args in
      let _ = match Log.get_log_chan () with
      | Some chan -> args := Parser_handle.LOG_CHAN (chan)::!args
      | None -> ()
      in
      let _ = Logrc.load (log_basename, (!(Mlite_config.mlite_home) ^ "/" ^ !(Mlite_config.mlite_log_rc))) !args in
      let _ =
	match !files with 
	| [file] -> 
	    let result = SD.load (Parser_handle.INPUT_FILE(file)::!args) in 
	    let _ = match result with
	    |Some r -> process !args !out_file r
	    |None -> ()
	    in ()
	| _ -> Arg.usage opts banner
      in
      Log.close_log ();
      Logrc.close ()
      

    let execN process = 
      let opts = List.append common_opts (UO.user_opts ())
      and banner = default_banner ^ UO.banner
      in 
      Arg.parse opts (fun file -> files := file :: !files) banner;
      let _ = Log.open_log false !log_file 
      and (close, out_chan) = 
        if !out_file <> "" then (true, open_out !out_file) else (false, stdout)
      in
      let log_basename = 
	if !log_file <> "" 
	then !log_file 
	else if !out_file <> "" 
	then !out_file
	else "a_log"
      in
      let args = ref [] in 
      let _ = 
	if !debug then 
	  args := (Parser_handle.DEBUG (true))::!args 
      in
      let _ = match Log.get_log_chan () with
      | Some chan -> args := Parser_handle.LOG_CHAN (chan)::!args
      | None -> ()
      in
      let _ = Logrc.load (log_basename, (!(Mlite_config.mlite_home) ^ "/" ^ !(Mlite_config.mlite_log_rc))) !args in
      let results = 
	List.map
	  (fun file ->
	    Log.printf "**Parsing %s**\n" file;
	    let result = SD.load (Parser_handle.INPUT_FILE(file)::!args) in 
	    match result with
	    |Some r -> r
	    |None -> 
		Printf.fprintf stderr "error:parser didn't return value\n";
		assert false
	  )
	  !files
      in
      let dot_idx = 
         try 
            String.rindex !out_file '.'
         with
            Not_found -> String.length !out_file
      in
      let basename = String.sub !out_file 0 dot_idx
      in 
      process !args out_chan basename results;
      if close then close_out out_chan;
      Log.close_log ();
      Logrc.close ()

    let exec_str str process =
      let opts = List.append common_opts (UO.user_opts ())
      and banner = default_banner ^ UO.banner
      in
      Arg.parse opts (fun file -> files := file :: !files) banner;
      let _ = Log.open_log false !log_file
      and (close, out_chan) =
        if !out_file <> "" 
	then (true, open_out !out_file) 
	else (false, stdout)
      in
      let args = ref [] in
      let _ = if !debug then args := (Parser_handle.DEBUG (true))::!args in
      let _ = match Log.get_log_chan () with
      | Some chan -> args := Parser_handle.LOG_CHAN (chan)::!args
      | None -> ()
      in
      let _ =
        match !files with
        | [file] ->
            Log.printf "**Parsing %s**\n" file;
            let result = SD.load (Parser_handle.INPUT_FILE(file)::!args) in
            let _ = match result with
            |Some r -> process !args out_chan r
            |None -> ()
            in ()
        | _ -> Arg.usage opts banner
      in
      if close then close_out out_chan;
      Log.close_log ()
  end
