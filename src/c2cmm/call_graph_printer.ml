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
open Call_graph
open Pretty_printer

type c_file_unit = (string, Call_graph.node) Hashtbl.t 
let description () = description
let suffix () = suffix

let string_of_actual_argc i = ("(" ^ (string_of_int i) ^ ")")
    
let rec pp_print_t: formatter -> node -> unit = 
  fun fm f ->
    let pp_print_t' fm (node, i) =
      pp_print_string fm (string_of_actual_argc i);
      pp_print_string fm ":";
      pp_print_t fm node
    in
    match f with
    | Recursion str -> 
	pp_print_string fm (str ^ ":{rec}");
    | Leaf str -> 
	pp_print_string fm str
    | Unresolved str -> 
	pp_print_string fm (str ^ ":{unresolved}");
    | Function_Ptr i -> 
	pp_print_string fm ("$" ^ (string_of_int i) ^ ":{fptr}");
    | Subtree (str, callee_nodes) ->
	pp_print_string fm str;
	print_in_vbox 2
	  (fun fm callee_nodes ->
	    pp_print_space fm ();
	    print_list_with_line_break 
	      pp_print_t' fm callee_nodes
	  )
	  fm callee_nodes	
	  
	  
let pp_print_c_file_unit fm  hashtbl=
  print_in_vbox 2
    (fun fm hashtbl ->
      Hashtbl.iter 
	(fun key node ->
	  pp_print_space fm ();
	  pp_print_string fm "+";
	  pp_print_t fm node;
	  pp_print_space fm ();
	) hashtbl
    )
    fm hashtbl
