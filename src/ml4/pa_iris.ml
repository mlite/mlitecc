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

open Pcaml

let enable_eval loc cond e1 e2 =
  match e2 with
  | [a] -> <:expr< 
      let (enable,_) = $cond$ in 
      if enable then  $e1$ $a$ else ()
	>>
  | a::l ->
      let ex = List.fold_left 
	  (fun e1' e2 -> <:expr< $e1'$ $e2$ >>)  
	  <:expr< $e1$>> 
	  e2
      in
      <:expr< let (enable, _) = $cond$ in if enable then $ex$ else () >>

  | [] -> <:expr< 
      let (enable, _) = $cond$ in 
      if enable then  $e1$ else ()
	>>

let build_log loc cond tag fm e2 =
  match e2 with
  | [a] -> <:expr< 
      let (enable, chan) = $cond$ in 
      if enable then  
	let _ = Printf.fprintf chan "--<%s>--\n" $str:tag$
	in
	Printf.fprintf chan $fm$ $a$ 
      else ()
	>>
  | a::l ->
      let ex = List.fold_left 
	  (fun e1' e2 -> <:expr< $e1'$ $e2$ >>)  
	  <:expr< Printf.fprintf chan $fm$ >> 
	  e2
      in
      <:expr< let (enable, chan) = $cond$ in 
      if enable 
      then 
	let _ = Printf.fprintf chan "--<%s>--\n" $str:tag$ 
	in
	$ex$ 
      else () 
	  >>

  | [] -> <:expr< 
      let (enable, chan) = $cond$ in 
      if enable then  Printf.fprintf chan $fm$ else ()
	>>

let exception_log loc cond s e2 =
  let e1 = 
    <:expr< let (enable, chan) = $cond$ in 
    if enable then 
      Printf.fprintf chan $s$ 
    else
      Printf.fprintf stderr $s$ 
      >>
  in
  match e2 with
  | [a] ->
      <:expr< let _ = $e1$ $a$ in 
      if Debug.exception_log.Debug.throw_exception then 
	assert False 
      else () >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let _ = $ex$ in 
      if Debug.exception_log.Debug.throw_exception
      then assert False else ()>>
  | [] ->
      <:expr< let _ = $e1$ in 
      if Debug.exception_log.Debug.throw_exception
      then assert False else ()>>
      

let throw_exception loc s e2 =
  let e1 = <:expr< Printf.fprintf stderr $s$ >>
  in
  match e2 with
  | [a] ->
      <:expr< let _ = $e1$ $a$ in assert False >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let _ = $ex$ in assert False >>
  | [] ->
      <:expr< let _ = $e1$ in assert False >>


let warning loc s e2 =
  let e1 = <:expr< Printf.fprintf stderr $s$ >>
  in
  match e2 with
  | [a] ->
      <:expr< let _ = $e1$ $a$ in () >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let _ = $ex$ in () >>
  | [] ->
      <:expr< let _ = $e1$ in () >>



let sqlite3_update loc s e2 =
  let e1 = <:expr< Printf.fprintf stderr $s$ >>
  in
  match e2 with
  | [a] ->
      <:expr< let _ = $e1$ $a$ in () >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let _ = $ex$ in () >>
  | [] ->
      <:expr< let _ = $e1$ in () >>


let str_pp_print loc e1 e2 =
  let e1 = <:expr< $e1$ sfm.Str_util.formatter >>
  in
  match e2 with
  | [a] ->
      <:expr< let sfm = Str_util.create_sfm 100 in 
              let _ = $e1$ sfm.Str_util.formatter in
              (Str_util.flush_sfm sfm)
      >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let sfm = Str_util.create_sfm 100 in
              let _ = $ex$ in 
              (Str_util.flush_sfm sfm)
      >>
  | [] ->
      <:expr< let sfm = Str_util.create_sfm 100 in
              let _ = $e1$ in 
              (Str_util.flush_sfm sfm)
      >>


let str_opt_pp_print loc e1 e2 =
  let e1 = <:expr< $e1$ sfm.Str_util.formatter >>
  in
  let (start_pos, end_pos) = loc
  in
  let fname_expr = <:expr< $str:!input_file$>>
    (** MLast.ExStr (loc, !input_file) **)
  and lnum_expr = <:expr<$int:string_of_int (start_pos.Lexing.pos_lnum)$>>
    (** MLast.ExInt (loc, string_of_int (start_pos.Lexing.pos_lnum)) **)
  in
  let print_loc = 
    <:expr<Str_util.pp_print_str_pp_print_pos 
      sfm.Str_util.formatter $fname_expr$ $lnum_expr$>>
  in
  match e2 with
  | [a] ->
      <:expr< let v = Mlite_config.is_enable_ast_debug () in
              if v then 
                let sfm = Str_util.create_sfm 100 in 
		let _ = $print_loc$
		in
                let _ = $e1$ sfm.Str_util.formatter in
                Some (Str_util.flush_sfm sfm)
              else 
                None
      >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let v = Mlite_config.is_enable_ast_debug () in 
              if v then
                let sfm = Str_util.create_sfm 100 in
		let _ = $print_loc$
		in
                let _ = $ex$ in 
                Some (Str_util.flush_sfm sfm)
              else
		None
      >>
  | [] ->
      <:expr< let v = Mlite_config.is_enable_ast_debug () in
              if v then 
                let sfm = Str_util.create_sfm 100 in
		let _ = $print_loc$
		in
                let _ = $e1$ in 
                Some (Str_util.flush_sfm sfm)
              else
		None
      >>



let hash_key_pp_print loc e1 e2 =
  let e1 = <:expr< $e1$ sfm.Str_util.formatter >>
  in
  match e2 with
  | [a] ->
      <:expr< let sfm = Str_util.create_sfm 100 in 
              let _ = $e1$ sfm.Str_util.formatter in
              let row_txt = (Str_util.flush_sfm sfm)
	      in
	      Str_util.delete_spaces row_txt
     >>
  | a::l ->
      let ex = List.fold_left
	  (fun e1' elmt -> <:expr< $e1'$ $elmt$ >>)  
	  <:expr< $e1$>> 
	e2
      in
      <:expr< let sfm = Str_util.create_sfm 100 in
              let _ = $ex$ in 
              let row_txt = (Str_util.flush_sfm sfm)
	      in
              Str_util.delete_spaces row_txt
      >>
  | [] ->
      <:expr< let sfm = Str_util.create_sfm 100 in
              let _ = $e1$ in 
              let row_txt = (Str_util.flush_sfm sfm)
	      in
	      Str_util.delete_spaces row_txt
      >>


let get_lexing_pos_str loc =
  let (start_pos, end_pos) = loc
  in
  let fname_expr =  
    MLast.ExStr (loc, !input_file)
  and lnum_expr = 
    MLast.ExStr (loc, string_of_int (start_pos.Lexing.pos_lnum))
  and link = MLast.ExStr (loc, ":")
  in
  <:expr<($fname_expr$ ^ $link$ ^ $lnum_expr$)>>


let get_lexing_position loc =
  let (start_pos, end_pos) = loc
  in
  let fields = 
    [(<:patt<Lexing.pos_fname>>, <:expr<$str:!input_file$>>);
     (<:patt<Lexing.pos_lnum>>, 
       <:expr<$int:string_of_int (start_pos.Lexing.pos_lnum)$>>);
     (<:patt<Lexing.pos_bol>>, 
       <:expr<$int:string_of_int (start_pos.Lexing.pos_bol)$>>);
     (<:patt<Lexing.pos_cnum>>, 
       <:expr<$int:string_of_int (start_pos.Lexing.pos_cnum)$>>);
   ]
  in
  <:expr<{ $list:fields$ }>>
  
let patch_pta_fun_impl loc s =
  let fun_name = Printf.sprintf "pta_fun_%s" s
  in
  <:expr< let i = find_fun $str:s$
          in 
          Array.set function_array i ($str:s$, $lid:fun_name$)
  >>

let patch_pta_var_impl loc s =
  let var_name = Printf.sprintf "pta_var_%s" s
  in
  <:expr< let i = find_fun $str:s$
          in 
          Array.set variable_array i ($str:s$, $lid:var_name$)
  >>

EXTEND
    expr: LEVEL "expr1"
      [ [ "camlp4_macro_eval"; s = STRING; "("; e1 = expr LEVEL "."; 
	  e2 = LIST1 expr LEVEL "." ; ")" -> 
	    let cond = <:expr<Log.get $str:s$>>
	    in
	    enable_eval loc cond e1 e2
	]

      | [ "camlp4_macro_log"; s0 = STRING; s1 = STRING; e = LIST0 expr LEVEL "." ->
	  let cond = <:expr<Log.get $str:s0$>>
	  and fm = <:expr<$str:s1$>>
	  in
	  build_log loc cond s0 fm e
	]

      | [ "camlp4_macro_exception_log"; s0 = STRING; s1 = STRING; e = LIST0 expr LEVEL "." ->
	  let cond = <:expr<Log.get $str:s0$>>
	  and fm = <:expr< $str:s1$>>
	  in
	  exception_log loc cond fm e
	]
        
      | [ "camlp4_macro_log"; cond = expr LEVEL "simple"; s1 = STRING; e = LIST0 expr LEVEL "." ->
	  (*"("; e1 = expr LEVEL "."; e2 = LIST0 expr LEVEL "."; ")" -> *)
	  let fm = <:expr< $str:s1$>>
	  in
	  build_log loc cond "expr" fm e
	] 

      | [ "camlp4_macro_exception"; "("; ")" ->
	  <:expr< assert False >>
	]

      | [ "camlp4_macro_exception"; s = STRING; e = LIST0 expr LEVEL "." ->
	  let fmt = <:expr< $str:s$>>
	  in
	  throw_exception loc fmt e
	]  
      | [ "camlp4_macro_warning"; s = STRING; e = LIST0 expr LEVEL "." ->
	  let fmt = <:expr< $str:s$>>
	  in
	  warning loc fmt e
	]    
      | [ "camlp4_macro_sqlite3_update"; s = STRING; e = LIST0 expr LEVEL "." ->
	  let fmt = <:expr< $str:s$>>
	  in
	  sqlite3_update loc fmt e
	] 
      | [ "camlp4_macro_str_pp_print"; e1 = expr LEVEL "."; e = LIST0 expr LEVEL "." ->
	  str_pp_print loc e1 e
	] 
      | [ "camlp4_macro_hash_key_pp_print"; e1 = expr LEVEL "."; e = LIST0 expr LEVEL "." ->
	  hash_key_pp_print loc e1 e
	] 
      | [ "camlp4_macro_str_opt_pp_print"; e1 = expr LEVEL "."; e = LIST0 expr LEVEL "." ->
	  str_opt_pp_print loc e1 e
	]  
      | [ "camlp4_macro_lexing_pos_str"; "("; ")" ->
	  get_lexing_pos_str loc 
	]  
      | [ "camlp4_macro_lexing_position"; "("; ")" ->
	  get_lexing_position loc 
	]  
      | [ "camlp4_macro_patch_pta_fun_impl"; s = STRING ->
	  patch_pta_fun_impl loc s
	] 
      | [ "camlp4_macro_patch_pta_var_impl"; s = STRING ->
	  patch_pta_var_impl loc s
	]  
      ]; 
END
