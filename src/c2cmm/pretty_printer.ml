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

let indent = 2
let order_as_comment = true
let debug = false

type ord = string * int

let print_c_space: formatter -> unit = 
  fun fm -> pp_print_break fm 1 indent

let pp_print_c_space = print_c_space
      
let print_c_comma: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ","; 
    print_c_space fm

let pp_print_c_comma = print_c_comma
      
let print_c_newline: formatter -> unit = 
  fun fm ->
    pp_print_cut fm ()

let pp_print_c_newline = print_c_newline

let print_order: (formatter -> 'a -> unit) -> formatter -> (ord * 'a) -> unit =
  fun f fm ((str, id), a) ->
    if debug then
      begin
	if order_as_comment then
	  pp_print_string fm ("/*" ^ (string_of_int id) ^ "*/")
	else
	  pp_print_string fm ("#" ^ (string_of_int id) ^ "#");
      end;
    f fm a

let print_opt: (formatter -> 'a -> unit) -> formatter -> 'a option -> unit = 
  fun f fm a_opt ->
    match a_opt with
    | Some a -> f fm a
    | None -> ()

let print_opt_with_space: (formatter -> 'a -> unit) -> formatter -> 'a option -> unit = 
  fun f fm a_opt ->
    match a_opt with
    | Some a -> print_c_space fm; f fm a
    | None -> ()
   
let print_in_box: int -> (formatter -> 'a -> unit) -> formatter -> 'a -> unit =
  fun n f fm v ->
    pp_open_box fm n;
    f fm v;
    pp_close_box fm ()
      
let print_in_vbox: int -> (formatter -> 'a ->unit) -> formatter -> 'a -> unit = 
  fun n f fm v ->
    pp_open_vbox fm n;
    f fm v;
    pp_close_box fm ()

let print_in_hovbox: int -> (formatter -> 'a ->unit) -> formatter -> 'a -> unit = 
  fun n f fm v ->
    pp_open_hovbox  fm n;
    f fm v;
    pp_close_box fm ()

let print_list_with_line_break: (formatter -> 'a ->unit) -> formatter -> 'a list -> unit = 
  fun f fm lst ->
    Mlite_printer.pp_print_list fm  f (fun fm -> pp_print_space fm ()) lst

let print_list_with_newline_delimiter: (formatter -> 'a -> unit) -> formatter -> 'a list -> unit = 
  fun f fm lst ->
    Mlite_printer.pp_print_list fm f print_c_newline lst
    

let print_actual_parameters: int -> (formatter -> 'a -> unit) -> formatter -> 'a list -> unit = 
  fun n f fm lst ->
    pp_open_hovbox fm n;
    pp_print_string fm "(";
    Mlite_printer.pp_print_list fm f (fun fm -> pp_print_string fm ","; pp_print_space fm ()) lst;
    pp_print_string fm ")";
    pp_close_box fm ()
      

