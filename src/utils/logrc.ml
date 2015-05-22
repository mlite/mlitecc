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

open Log
module Logrc_syntax =
  struct
    type token = Logrc_parser.token
    type t = Log.log_unit list 
    let  lexer_init = Logrc_lexer.init
    let  tokenize = Logrc_lexer.token
    let  parse = Logrc_parser.file
  end
    
module Logrc_Syntax_Analyzer = Parser_driver.Make(Logrc_syntax) 
    (Logrc_handle)
    
let load: string * string -> Parser_handle.arg list -> unit = 
  fun (basename, logrc_file) args ->
    let log_units = 
      match Logrc_Syntax_Analyzer.load 
	  (Parser_handle.INPUT_FILE(logrc_file)::args)  with
      | Some r -> r
      | None -> 
	  Printf.fprintf stderr "error:parsing \"%s\" didn't return value\n" logrc_file;
	  assert false
    in
    Log.init (basename, log_units)


let close: unit -> unit =
    fun () ->
      Log.close ()
