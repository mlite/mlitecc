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

let description () = Ast_ga_code.description
let suffix () = Ast_ga_code.suffix

let translate: basename:string -> args:string list -> 
  input:in_channel -> output:out_channel -> unit = 
  fun ~basename ~args ~input:in_chan ~output:out_chan ->
    let (basename, ast, typ_db, ident_env) = 
      Marshal.from_channel in_chan
    in
    let ast = Gen_ast_ga_code.compile basename ast
    in Marshal.to_channel out_chan (basename, ast, typ_db, ident_env) []

let emit_c_code: input:in_channel -> output:Format.formatter -> unit =
  fun ~input:in_chan ~output:fm ->
    let (basename, ast, typ_db, ident_env) = Marshal.from_channel in_chan
    in C_file_basic.Ast_ga_code.write_c_formatter fm ast
