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

class handle opt =
  object (self)
    val debug       = ref opt.Cmd_opt.debug
    val file_name   = opt.Cmd_opt.filename
    val basename    = opt.Cmd_opt.basename
    val err_chan    = opt.Cmd_opt.err_chan
    val log_chan    = opt.Cmd_opt.log_chan
    val in_chan     = opt.Cmd_opt.in_chan
    val lineno      = ref 0
    val line_buffer = ref ""
    val buffer      = ref ""
    val pos         = ref 0


    method enable_debug f = 
      debug := f
	
    method get_filename () = 
      file_name

    method get_lineno () =
      !lineno

    method shift s =
      if !debug then 
	begin
	  Printf.fprintf log_chan "SHIFT %s\n" s;
	  flush log_chan
	end 
      else ()
	  
    method reduce s =
      if !debug then 
	begin
	  Printf.fprintf log_chan "REDUCE %s\n" s;
	  flush log_chan
	end
      else ()
	  
    method syntax_error msg  =
      let _ = self#display_error msg (Parsing.symbol_start ()) (Parsing.symbol_end ())
      and _ = raise Parsing.Parse_error 
      in ()

    method lexeme_error lexbuf msg = 
      self#display_error msg (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf);
      
    method get_buffer: string -> int -> int = 
      fun dst len ->
	try
	  let bufferp =
	    if !buffer <> "" then 
	      !buffer
	    else 
	      let buffer0 = (input_line in_chan) ^ "\n" in
	      let _ = line_buffer := buffer0 
	      and _ = incr lineno
	      and _ = pos := !pos + (String.length !line_buffer)
	      in
	      buffer0
          in
	  let bufl = String.length bufferp in
	  let lenp = min len bufl in
	  let _ = String.blit bufferp 0 dst 0 lenp 
	  and _ = 
	    if bufl = lenp
	    then buffer := ""
	    else buffer := String.sub bufferp lenp (bufl - lenp) 
	  in
          lenp
	with End_of_file -> 0
	    
    method private underline_error: string -> int -> int -> string =
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
	  
    method display_error msg token_start token_end =
      Printf.fprintf err_chan "DEBUG line[%d]: %s token_start %d token_end %d\n" 
	!lineno !line_buffer token_start token_end;
      
      let _ = output_string err_chan 
	  (
	   ((file_name) ^ "[" ^ (string_of_int (!lineno)) ^ "] ")
	   ^ msg ^ ": \n"
	   ^ (self#underline_error !line_buffer (token_start - !pos)  (token_end - !pos))
	  ) 
      in flush (err_chan)
  end

let default_handle () = 
  new handle Cmd_opt.default_opt 


module type ArgHandleType =
  sig
    val init: handle -> unit
    val handle: handle ref
  end

module type SyntaxAnalyzerType =
  sig
    type token
    type t
    val lexer_init: unit -> unit
    val tokenize: Lexing.lexbuf -> token
    val parse: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> t
  end


module type Driver = 
  sig
    type t
    val load : Cmd_opt.arg list -> t option
  end 


module Make (SA:SyntaxAnalyzerType) (HD:ArgHandleType) :Driver 
with type t = SA.t =
  struct
    type t = SA.t
	  
    let load args = 
      let opt = Cmd_opt.scan_args args in
      let handle = new handle opt in
      let result =
	try
	  HD.init handle;
	  SA.lexer_init ();
	  let result' = SA.parse SA.tokenize (Lexing.from_function (!HD.handle#get_buffer)) in
	  Parsing.clear_parser ();
	  Some (result')
	with Parsing.Parse_error -> None
      in
      if opt.Cmd_opt.close then close_in opt.Cmd_opt.in_chan;
      result
  end
