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

exception Syntax_Error

type arg =
  | DEBUG of bool
  | INPUT_FILE of string 
  | BASENAME of string
  | LOG_CHAN of out_channel
  | ERR_CHAN of out_channel (** Use the given channel for outputting errors. *)
	
and t = {
    debug: bool;
    in_chan: in_channel;
    close: bool;
    log_chan: out_channel;
    err_chan: out_channel;
    basename: string;
    filename: string;
  }
      
let scan_args arglist =
  let debug     = ref false 
  and in_chan   = ref stdin
  and close     = ref false
  and log_chan  = ref stdout
  and err_chan  = ref stderr
  and basename  = ref ""
  and file_name = ref ""
  in
  (* Scan the arguments *)
  let rec scan args =
    match args with
      [] -> ()
    | (INPUT_FILE path) :: tl ->
	let _ = 
	  if !file_name <> "" then 
	    begin
	      Printf.fprintf stderr "more than one input file names\n";
	      assert false
	    end
	  else
	    let _ = file_name := path 
	    and _ = in_chan := open_in path
	    and _ = close := true
	    in ()
	in
	scan tl
    | (BASENAME bname) :: tl ->
	basename := bname; scan tl
    | (LOG_CHAN chan) :: tl ->
	log_chan := chan; scan tl
    | (DEBUG opt) :: tl ->
        debug := opt; scan tl
    | (ERR_CHAN chan) :: tl ->
        err_chan := chan; scan tl
  in
  scan arglist;
  {debug = !debug;
   in_chan = !in_chan;
   close = !close;
   log_chan = !log_chan;
   err_chan = !err_chan;
   basename = !basename;
   filename = !file_name;}
   
let default_opt = 
  {debug = true;
   in_chan = stdin;
   close = false;
   log_chan = stdout;
   err_chan = stderr;
   basename = "__stdin__";
   filename = "__stdin__";
 }
