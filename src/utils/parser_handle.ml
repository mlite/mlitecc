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

type arg =
  | DEBUG of bool
  | INPUT_FILE of string 
  | BASENAME of string
  | LOG_CHAN of out_channel
  | ERR_CHAN of out_channel (** Use the given channel for outputting errors. *)
	
and t = {
    debug: bool ref;
    file_name: string;
    base_name: string;
    err_chan: out_channel;
    log_chan: out_channel;
    in_chan:  in_channel;
    close_in: bool;
    
    line_buffer: string ref;
    buffer: string ref;
    
    file: string ref;
    row: int ref;
    col: int ref
  }

      

let scan_args: arg list -> t =
  fun arglist ->
    let debug = ref false 
    and in_chan = ref stdin
    and close_in = ref false
    and log_chan = ref stdout
    and err_chan = ref stderr
    and base_name = ref ""
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
	      and _ = close_in := true
	      in ()
	  in
	  scan tl
      | (BASENAME bname) :: tl ->
	  base_name := bname; scan tl
      | (LOG_CHAN chan) :: tl ->
	  log_chan := chan; 
	  scan tl
      | (DEBUG opt) :: tl ->
          debug := opt; scan tl
      | (ERR_CHAN chan) :: tl ->
          err_chan := chan; 
	  scan tl
    in
    let _ = scan arglist
    in
    { 
      debug = debug;
      in_chan = !in_chan;
      close_in = !close_in;
      log_chan = !log_chan;
      err_chan = !err_chan;
      base_name = !base_name;
      file_name = !file_name; 
      file = file_name;
      row = ref 0;
      col = ref 0;
      line_buffer = ref "";
      buffer = ref "";
    }
      

let enable_debug: t -> bool -> unit = 
  fun t flag ->
    t.debug := flag


let shift: t -> msg:string -> unit =
  fun t ~msg ->
    if !(t.debug) then
      begin
	Printf.fprintf t.log_chan "SHIFT %s\n" msg;
	flush t.log_chan
    end
	
let reduce: t -> msg:string -> unit = 
  fun t ~msg -> 
    if !(t.debug) then 
      begin
	Printf.fprintf t.log_chan "REDUCE %s\n" msg;
	flush t.log_chan
      end

let get_lineno: t -> int = 
  fun t -> !(t.row)

let set_row: t -> int -> unit = 
  fun t row -> t.row := row

let set_file: t -> string -> unit = 
  fun t file -> t.file := file

let get_filename: t -> string = 
  fun t -> t.file_name

let get_coordinate: t -> string * int * int = 
  fun t -> (!(t.file), !(t.row), !(t.col))

let underline_error:  string -> int -> int -> string =
  fun str_buffer start stop ->
    let len = String.length str_buffer in
    let start' = max 0 start in
    let stop'  = max 1 stop in
    (
     (
      if start' > 0 
      then (String.sub str_buffer 0 start') 
      else "  "
     ) 
     ^ "\027[4m" ^ 
	 (
	  if (stop' - start') <> 0 
	  then (String.sub str_buffer start' (stop' - start'))
	  else ""
	 )
     ^ "\027[0m" ^ 
     (
      if stop' < len 
      then (String.sub str_buffer stop' (len - stop')) 
      else ""
     )
    )

	 
let display_error: t -> msg:string -> token_start:int  -> token_end:int -> unit =
  fun t ~msg ~token_start ~token_end ->
    Printf.fprintf t.err_chan "DEBUG line[%d]: %s token_start %d token_end %d\n" 
      !(t.row) !(t.line_buffer) token_start token_end;
    
    let _ = output_string t.err_chan 
	(
	 ((t.file_name) ^ "[" ^ (string_of_int !(t.row)) ^ "] ")
	 ^ msg ^ ": \n"
	 ^ (underline_error !(t.line_buffer) (token_start - !(t.col))  (token_end - !(t.col)))
	) 
    in flush (t.err_chan)

let syntax_error: t -> msg:string -> unit = 
  fun t ~msg -> 
    let _ = display_error t msg (Parsing.symbol_start ()) (Parsing.symbol_end ())
    and _ = raise Parsing.Parse_error 
    in ()

let lexeme_error t lexbuf msg = 
  display_error t msg (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)
      

let get_buffer: t -> string -> int -> int = 
  fun t dst len ->
    try
      let bufferp =
	if !(t.buffer) <> "" then 
	  !(t.buffer)
	else 
	  let buffer0 = (input_line t.in_chan) ^ "\n" in
	  let _ = t.line_buffer := buffer0 
	  and _ = incr t.row
	  and _ = t.col := !(t.col) + (String.length !(t.line_buffer))
	  in
	  buffer0
      in
      let bufl = String.length bufferp in
      let lenp = min len bufl in
      let _ = String.blit bufferp 0 dst 0 lenp 
      and _ = 
	if bufl = lenp then 
	  t.buffer := ""
	else 
	  t.buffer := String.sub bufferp lenp (bufl - lenp) 
      in
      lenp
    with End_of_file -> 
      0
	
	
	
let default_handle () = 
  scan_args []
