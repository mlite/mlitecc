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

let c_terminate_char = Char.chr 0
    
let to_chars: c_string:string -> char list = 
  fun ~c_string:c_string_literal -> 
    if c_string_literal = "" then
      [c_terminate_char]
    else
      begin
	let chars = ref []
	in
	String.iter
	  (fun c -> 
	    chars := c::!chars
	  ) c_string_literal;
	List.rev (c_terminate_char::!chars)
      end
      
let to_char_codes: string -> int list =
  fun c_string_literal -> 
    let ints = ref []
    in
    String.iter
      (fun c -> 
	ints := (Char.code c)::!ints
      ) (c_string_literal ^ (String.make 1 (Char.chr 0)));
    List.rev !ints
    

let c_comment_string: string -> string =
  fun str ->
    let comment = ref ""
    in
    String.iter
      (fun c ->
        if c = '/' then
          comment := !comment ^ " "
        else
          comment := !comment ^ (Char.escaped c)
      ) str;
    !comment


let pp_print_c_comment: formatter -> string -> unit = 
  fun fm str ->
    pp_print_string fm "/*";
    String.iter
      (fun c ->
	if c = ' ' or c = '\n' then
	  () (* save space *)
	else if c = '/' then
	  pp_print_char fm ' '
	else
	  pp_print_string fm (Char.escaped c)
      ) str;
    pp_print_string fm "*/"


let c_identifierize: string -> string =
  fun str ->
    let c_id = ref ""
    in
    String.iter
      (fun c ->
	match c with
	| ':'
	| ','
	| '.'
	| ' '
	| '-' -> 
	    c_id := !c_id ^ "_"
	| _ ->
	    c_id := !c_id ^ (Char.escaped c)
      )
      str;
    !c_id
    
(*
 *
 * Copyright (c) 2003, 
 *  Ben Liblit          <liblit@cs.berkeley.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)


(** OCaml types used to represent wide characters and strings *)

type wchar = int64
type wstring = wchar list


let escape_char = function
  | '\007' -> "\\a"
  | '\b' -> "\\b"
  | '\t' -> "\\t"
  | '\n' -> "\\n"
  | '\011' -> "\\v"
  | '\012' -> "\\f"
  | '\r' -> "\\r"
  | '"' -> "\\\""
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | ' ' .. '~' as printable -> String.make 1 printable
  | unprintable -> Printf.sprintf "\\%03o" (Char.code unprintable)

let escape_string str =
  let length = String.length str in
  let buffer = Buffer.create length in
  for index = 0 to length - 1 do
    Buffer.add_string buffer (escape_char (String.get str index))
  done;
  Buffer.contents buffer

(* a wide char represented as an int64 *)
let escape_wchar =
  (* limit checks whether upper > probe *)
  let limit upper probe = (Int64.to_float (Int64.sub upper probe)) > 0.5 in
  let fits_byte = limit (Int64.of_int 0x100) in
  let fits_octal_escape = limit (Int64.of_int 0o1000) in
  let fits_universal_4 = limit (Int64.of_int 0x10000) in
  let fits_universal_8 = limit (Int64.of_string "0x100000000") in
  fun charcode ->
    if fits_byte charcode then
      escape_char (Char.chr (Int64.to_int charcode))
    else if fits_octal_escape charcode then
      Printf.sprintf "\\%03Lo" charcode
    else if fits_universal_4 charcode then
      Printf.sprintf "\\u%04Lx" charcode
    else if fits_universal_8 charcode then
      Printf.sprintf "\\u%04Lx" charcode
    else
      invalid_arg "Cprint.escape_string_intlist"

(* a wide string represented as a list of int64s *)
let escape_wstring (str : int64 list) =
  let length = List.length str in
  let buffer = Buffer.create length in
  let append charcode =
    let addition = escape_wchar charcode in
    Buffer.add_string buffer addition
  in
  List.iter append str;
  Buffer.contents buffer
