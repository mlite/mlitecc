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

module C99 = Ast_aa_gram
module I = Ast_fa_stmt
module O = Ast_ga_code
  
open Int
open Collection

let assert_no_return_inst = ref true

let user_opts = 
  [
    ("--ast-ga-code-assert-no-return-inst",
    Arg.Set assert_no_return_inst,
    "assert all return instruction should be normalized as jmp")
  ]

exception Error 

module HashCoordinate = 
  struct 
    type t = Coordinate.t
    let equal (s1:t) (s2:t) = 
      let (file1, row1, col1) = s1
      and (file2, row2, col2) = s2
      in 
      file1 = file2 & row1 = row2 & col1 = col2
	
    let hash (s:t) =
      let (file, row, col) = s 
      in 
      Hashtbl.hash (file ^ (string_of_int row) ^ (string_of_int col))
  end
    
module CoordinateHashtbl = Hashtbl.Make(HashCoordinate)   

let current_coord = ref ("", 0, 0)


type function_data = 
    { 
      node_serno: int ref;
      code_size: int;
      label_tbl: int IntHashtbl.t;
      tbl_jmp: 
	((O.c_constant_expression * O.c_constant_expression) option * 
	  Int.gnode_id) list 
	IntHashtbl.t;
      tbl_jmp_index: int ref;
    }
      
let compile: string -> I.c_translation_unit -> O.c_translation_unit = 
  fun basename c_translation_unit ->
    let function_scope = Stack.create ()
    in
    let begin_function size = 
      let function_data = 
	{ node_serno = ref 0;
	  code_size = size;
	  label_tbl = IntHashtbl.create size;
	  tbl_jmp = IntHashtbl.create (size/2);
	  tbl_jmp_index = ref 0;
	}
      in
      Stack.push function_data function_scope
	
    and end_function () =
      ignore (Stack.pop function_scope)
    in
    let rec relab i new_i =
      let function_data = Stack.top function_scope
      in
      try 
	let _ = IntHashtbl.find function_data.label_tbl i
	in
	assert false
      with
	Not_found ->
	  begin
	    let v = new_i
	    in
	    IntHashtbl.add function_data.label_tbl i v
	  end

    and get_newlab i = 
      let function_data = Stack.top function_scope
      in
      IntHashtbl.find function_data.label_tbl i
	
    and add_tbl_jmp targets = 
      let function_data = Stack.top function_scope
      in
      let tbl_jmp_index = !(function_data.tbl_jmp_index)
      in
      IntHashtbl.add function_data.tbl_jmp tbl_jmp_index targets;
      incr function_data.tbl_jmp_index;
      tbl_jmp_index
    in
    (** the following are standard copy translation **)
    let rec create_jmp_tbl: unit -> 
      ((O.c_constant_expression * O.c_constant_expression) option * int) 
	list array = 
      fun () ->
	let function_data = Stack.top function_scope
	in
	let size = IntHashtbl.length function_data.tbl_jmp
	in
	let jmp_tbl = Array.create size []
	in
	IntHashtbl.iter
	  (fun i targets ->
	    let new_targets = 
	      List.map 
		(fun (e, t) -> (e, get_newlab t))
		targets
	    in
	    Array.set jmp_tbl i new_targets
	  ) function_data.tbl_jmp;
	jmp_tbl

    and patch_code100: (Code_label.t * O.c_code100 * Coordinate.t option) 
	array -> unit = 
      fun code_array ->
	Array.iteri
	  (fun i (is_target, code, coord_opt) ->
	    match code with
	    | O.Scope expr ->
		begin
		  match expr with
		  | O.Begin_fun (d, j) -> 
		      Array.set code_array i 
			(is_target, 
			O.Scope (O.Begin_fun (d, get_newlab j)), coord_opt)
		  | O.End_fun -> ()
		  | O.Begin_decl (d, j) ->
		      Array.set code_array i 
			(is_target, O.Scope (O.Begin_decl (d, get_newlab j)), coord_opt)
		  | O.End_decl -> ()
		end
	    | O.Call expr -> ()
	    | O.Flow expr ->
		begin
		  match expr with
		  | O.Jmp t ->
		      Array.set code_array i 
			(is_target, O.Flow (O.Jmp (get_newlab t)), coord_opt)
		  | O.Jmp_join t ->
		      Array.set code_array i 
			(is_target, O.Flow (O.Jmp_join (get_newlab t)), coord_opt)
		  | O.Jmp_join_backward t ->
		      Array.set code_array i 
			(is_target, O.Flow (O.Jmp_join_backward (get_newlab t)), coord_opt)
		  (*| O.Cmp_zero_jmp (c_val, t0) ->
		      Array.set code_array i 
		    (is_target, O.Flow (O.Cmp_zero_jmp (c_val, get_newlab t0)),
		    coord_opt)
		  *)
		  | O.Cmp_jmp (c_val, t0) ->
		      Array.set code_array i 
			(is_target, O.Flow (O.Cmp_jmp (c_val, get_newlab t0)), coord_opt)
		  | O.Tbl_jmp _ -> ()
		end
	    | O.Sese expr -> ()
	    | O.Join -> ()
	    | O.Epi _ -> ()
	    | O.Nop -> ()
	  ) code_array

    and compile_c_lab_stmt090: Coordinate.t option array -> 
    (Code_label.t * O.c_code100 * Coordinate.t option) array -> int -> 
    I.c_lab_stmt090 -> unit = 
      fun line_tbl code_array index (is_target, expr) -> 
	match expr with
	  | I.BEGIN_FUNCTION (g0, decls, g1, end_decl_g) ->
	      let _ = relab g0 index
	      in
	      let decls = !decls
	      in
	      Array.set code_array index 
		(is_target, O.Scope (O.Begin_fun (decls, end_decl_g)), Array.get line_tbl g0)
		
	  | I.END_FUNCTION g0 ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index (is_target, O.Scope O.End_fun, Array.get line_tbl g0)
		
	  | I.BEGIN_DECL (g0, c_decls, g1, end_decl_g) ->
	      let _ = relab g0 index
	      in
	      let decls = c_decls
	      in
	      Array.set code_array index 
		(is_target, O.Scope (O.Begin_decl (decls, end_decl_g)), Array.get line_tbl g0)
		
	  | I.END_DECL (g0, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index 
		(is_target, O.Scope O.End_decl, Array.get line_tbl g0)
	
	  | I.GOTO (g0, lb) -> assert false
		
	  | I.SESE_SEQUENCE (g0, _, sese_code_list, g1) ->
	      let _ = relab g0 index
	      in 
	      Array.set code_array index 
		(is_target, O.Sese [O.Computing sese_code_list], 
		Array.get line_tbl g0)

	  | I.ASM (g0, strlst, asm_details_opt, g1) ->
	      let _ = relab g0 index
	      in 
	      Array.set code_array index 
		(is_target, O.Sese [O.Asm (strlst, asm_details_opt)], 
		Array.get line_tbl g0)
		
          | I.CALL (g0, code, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index 
                (is_target, O.Call code, Array.get line_tbl g0)
		
(*
	  | I.CMP_ZERO_JMP (g0, (cval, g1)) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index 
		(is_target, O.Flow 
		  (O.Cmp_zero_jmp (cval, g1)), 
		Array.get line_tbl g0)
*)

	  | I.CMP_JMP (g0, (cval, g1)) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index 
		(is_target, O.Flow 
		  (O.Cmp_jmp (cval, g1)),
		Array.get line_tbl g0)
		
	  | I.TBL_JMP (g0, (cval, target_pairs)) -> 
	      let _ = relab g0 index
	      in
	      let target_pairs = 
		List.map 
		  (fun (e_opt, t) -> 
		    match e_opt with
		      | Some (e0, e1) -> 
			  (Some (e0, e1), t)
		      | None ->
			  (None, t)
		  ) target_pairs
	      in
	      let tbl_jmp_index = add_tbl_jmp target_pairs
	      in
	      Array.set code_array index 
		(is_target, O.Flow 
		  (O.Tbl_jmp (cval, tbl_jmp_index)), Array.get line_tbl g0)
		
	  | I.JMP (g0, lb, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array 
		index (is_target, O.Flow (O.Jmp (g1)), Array.get line_tbl g0)
		
	  | I.JMP_JOIN (g0, lb, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index (is_target, O.Flow (O.Jmp_join (g1)), Array.get line_tbl g0)
		
	  | I.JMP_JOIN_BACKWARD (g0, lb, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index (is_target, O.Flow (O.Jmp_join_backward (g1)), Array.get line_tbl g0)

	  | I.POP_ARGS (g0, decl, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index 
		(is_target, O.Sese ([O.Pop_args (decl)]), 
		Array.get line_tbl g0)

	  | I.JOIN (g0, lb, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index (is_target, O.Join, Array.get line_tbl g0)

	  | I.EPI (g0, str_opt, g1) ->
	      let _ = relab g0 index
	      in
	      Array.set code_array index (is_target, O.Epi str_opt, Array.get line_tbl g0)
		
    and compile_c_translation_unit = function
      | I.Translation_unit (l, eenv) ->
	  O.Translation_unit 
	    (List.map
	       (fun external_declaration -> 
		 compile_c_external_declaration external_declaration
	       ) l, eenv)

    and compile_c_external_declaration = function
      | I.External_declaration_at (coord, expr) ->
	  let _ = current_coord := coord
	  in O.External_declaration_at 
	       (coord, compile_c_external_declaration expr)
	    
      | I.External_declaration_1 c_function_definition ->
	  O.External_declaration_1 
	    (compile_c_function_definition c_function_definition)
	    
      | I.External_declaration_2 c_declaration ->
	  O.External_declaration_2 c_declaration
	    
    and compile_c_function_definition: I.c_function_definition -> O.c_function_definition =
      fun expr ->
	let coord = !current_coord
	in
	let _ = begin_function expr.I.code_size
	in
	let code_array = Array.create 
	  (expr.I.code_size) (Code_label.SEQ (ref None), O.Nop, None)
	and index = ref (expr.I.code_size - 1)
	in
	let linkage = expr.I.linkage
	and _ = 
	  Safe_list.iter 
	    (fun stmt -> 
	      compile_c_lab_stmt090 expr.I.line_tbl code_array !index stmt;
	      decr index;
	    )
	    (List.rev expr.I.c_stmt090_list)
	in
	let _ = patch_code100 code_array
	and jmp_tbl = create_jmp_tbl ()
	in
	let _ = end_function ()
	in
	{ 
	  O.linkage = linkage;
	  O.c_type = expr.I.c_type;
	  O.name = expr.I.name;
	  O.code_array = code_array;
	  O.jmp_tbl = jmp_tbl;
	}
    in compile_c_translation_unit c_translation_unit
