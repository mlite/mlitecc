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

module I = Ast_aa_gram
module O = Call_graph
open Mapping
open Call_graph

let user_opts = []

let scan_opt f = function
  | Some v -> f v
  | None -> []


type visiting_flag =
    { is_onstack: bool ref;
      is_finished: bool ref;
      is_rec: bool ref;
    }

module StrElmt =
  struct
    type t = string
    let compare  = String.compare
  end
    
module StrSet = Set.Make(StrElmt)

    
let create: (string, (string * int) list) Hashtbl.t * (string, int) Hashtbl.t 
  -> (string, node) Hashtbl.t = 
    fun (hashtbl, fptr_argc_hashtbl) ->
      let visiting_hashtbl = Hashtbl.create 10
      in
      let rec dfs tree_node_hash_tbl caller = 
	try
	  let visiting_flag = Hashtbl.find visiting_hashtbl caller
	  in
	  if !(visiting_flag.is_finished) then
	    Hashtbl.find tree_node_hash_tbl caller
	  else if !(visiting_flag.is_onstack) then
	    begin
	      let _ = visiting_flag.is_rec := true
	      in
	      Recursion caller
	    end
	  else
	    begin
	      let _ = visiting_flag.is_onstack := true
	      in
	      let callee_list = Hashtbl.find hashtbl caller
	      in
              (*
		 print_string (" " ^ caller ^ " ");
		 List.iter (fun str -> print_string (" " ^ str ^ " "); 
		 print_newline ()) callee_list;
	       *)
	      let callee_nodes = 
		List.map (fun (f_name, argc) -> (dfs tree_node_hash_tbl f_name, argc)) 
		  callee_list
	      in
	      let _ = visiting_flag.is_onstack := false
	      and _ = visiting_flag.is_finished := true
	      in
	      let caller_node = 
		match callee_nodes with
		| [] -> 
		    Leaf caller
		| _ -> 
		    Subtree (caller, callee_nodes)
	      in
	      Hashtbl.add tree_node_hash_tbl caller (caller_node);
	      caller_node
	    end
	with
	  Not_found ->
	    if String.contains caller '$' then
	      Function_Ptr 
		(int_of_string (String.sub caller 1 ((String.length caller) - 1)))
	    else
	      Unresolved caller
      in
      Hashtbl.iter
	(fun fun_name _ ->
	  Hashtbl.add visiting_hashtbl fun_name { is_onstack = ref false;
						  is_finished = ref false;
						  is_rec = ref false;
						}
	) hashtbl;
      let tree_node_hash_tbl = Hashtbl.create (Hashtbl.length hashtbl)
      in
      Hashtbl.iter 
	(fun caller _ -> let _ = dfs tree_node_hash_tbl caller in ()) hashtbl;
      tree_node_hash_tbl
