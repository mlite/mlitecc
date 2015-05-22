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
open C_semantics_symbol

let pp_print_binary_arithmatic: formatter -> binary_arithmatic -> unit =
  fun fm op ->
    let str = match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Band -> "&"
    | Bor -> "|"
    | Bxor -> "^"
    | Shl -> "<<"
    | Shr -> ">>"
    in
    pp_print_string fm str


let pp_print_binary_predicate: formatter -> binary_predicate -> unit =
  fun fm op ->
    let str = match op with
    | Eq -> "=="
    | Ne -> "!="
    | Lt -> "<"
    | Gt -> ">"
    | Le -> "<="
    | Ge -> ">="
    in
    pp_print_string fm str


let pp_print_binary_logic_connect: formatter -> binary_logic_connect -> unit =
  fun fm op ->
    let str = match op with
    | And -> "&&"
    | Or -> "||"
    in
    pp_print_string fm str


let pp_print_unary_arithmatic: formatter -> unary_arithmatic -> unit =
  fun fm op ->
    let str = match op with
    | Neg -> "-"
    | Bnot -> "~"
    in
    pp_print_string fm str


let pp_print_linkage: formatter -> linkage -> unit = 
  fun fm op ->
    let str = match op with
    | Default_extern -> ""
    | Default -> ""
    | Extern -> "extern"
    | Extern_Inline -> "extern __inline"
    | Static -> "static"
    | Auto -> "auto"
    | Register -> "register"
    | Inline -> "__inline"
    | Type_alias -> "typedef"
    in
    if str <> "" then
      begin
	pp_print_string fm str;
	pp_print_space fm ();
      end


let pp_print_c_type_qualifier: formatter -> type_qualifier -> unit = 
  fun fm op ->
    let str = match op with
    | Const -> "const"
    | Restrict -> "restrict"
    | Volatile -> "volatile"
    in
    pp_print_string fm str
