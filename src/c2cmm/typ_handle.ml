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

(* $Id: nchandle.ml,v 1.2 2005/07/21 00:49:03 wangn Exp $ *)
let handle = ref (Parser_handle.scan_args [])

let init hdl = 
  handle := hdl;
  if !Mlite_config.enable_cfrag_parsing_debug then
    Parser_handle.enable_debug !handle true
  else
    Parser_handle.enable_debug !handle false

let d s = Parser_handle.shift !handle s
let reduce s = Parser_handle.reduce !handle s
let get_lineno () = Parser_handle.get_lineno !handle 
let get_filename () = Parser_handle.get_filename !handle 
let syntax_error s = Parser_handle.syntax_error !handle s
let incr_lineno () =  ()



