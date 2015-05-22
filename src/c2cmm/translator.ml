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

module type Translator_Type =
sig
  val description: unit -> string list
  val suffix: unit -> string
  val translate: basename:string -> args:string list -> 
    input:in_channel -> output:out_channel -> unit 
  val emit_c_code: input:in_channel -> output:formatter -> unit
end
    
      
module Make (TT:Translator_Type) =
  struct 
    let translate: input:string -> output:string -> unit =
      fun ~input ~output -> 
	let in_chan = open_in input
	and out_chan = open_out output
	in
	let _ = TT.translate ~basename:"" ~args:[] 
	  ~input:in_chan ~output:out_chan
	in
	let _ = close_out out_chan
	and _ = close_in in_chan
	in ()
	     
    let write_c_file: input:string -> output:string -> unit =
      fun ~input ~output -> 
	let in_chan = open_in input
	and out_chan = open_out output
	in
	let fm = formatter_of_out_channel out_chan
	in
	TT.emit_c_code ~input:in_chan ~output:fm;
	let _ = close_out out_chan
	and _ = close_in in_chan
	in ()
  end

module Ast_aa_gram = Make (Ast_aa_gram_generator)
