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

module type ArgHandleType =
  sig
    val init: Parser_handle.t -> unit
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
    val load : Parser_handle.arg list -> t option
  end 


module Make (SA:SyntaxAnalyzerType) (HD:ArgHandleType) :Driver 
with type t = SA.t =
  struct
    type t = SA.t
	  
    let load args = 
      let handle = Parser_handle.scan_args args in
      let result =
	try
	  HD.init handle;
	  SA.lexer_init ();
	  let result' = 
	    SA.parse SA.tokenize 
	      (Lexing.from_function (Parser_handle.get_buffer handle)) 
	  in
	  Parsing.clear_parser ();
	  Some (result')
	with Parsing.Parse_error -> None
      in
      if handle.Parser_handle.close_in 
      then 
	close_in handle.Parser_handle.in_chan;
      result
  end
