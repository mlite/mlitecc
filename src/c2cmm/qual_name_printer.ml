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
open Qual_name

let make_sname str : string =
  let prefix = ref "_"
  in
  String.iter
    (fun c ->
      match c with
	| '-' -> prefix := !prefix ^ "_"
	| '.' -> prefix := !prefix ^ "_"
	| _ -> prefix := !prefix ^ (String.make 1 c)
    ) str;
  !prefix ^ "_"
    
let pp_print_qn_scope (fm:formatter) (qn_span:qn_span) (qn_scope:qn_scope) :unit =
  match qn_scope with
    | QN_SCOPE_FILE str -> 
	begin
	  match qn_span with
	    | QN_AUTO -> ()
	    (*| QN_EXTERN -> ()*)
	    | QN_STATIC ->
		pp_print_string fm (make_sname str)
	end
    | QN_SCOPE_FUN str -> pp_print_string fm str
    | QN_SCOPE_BLOCK i -> pp_print_int fm i
    | QN_SCOPE_UTYPE str -> ()


let pp_print_qn_init (fm:formatter) (qn_init:qn_init) ~(to_decl_str) : bool =
  match qn_init with
    | QN_INIT str -> 
	begin
	  pp_print_string fm str;
	  if not to_decl_str then 
	    pp_print_string fm ".";
	  true
	end

    | QN_INT i -> pp_print_int fm i; true
    | QN_NULL -> false
	  
let pp_print_qn (fm:formatter) (qname:t) ~(to_decl_str:bool) : unit = 
  let print_full_path () = 
    Mlite_printer.pp_print_list fm
      (fun  fm v -> pp_print_qn_scope fm qname.qn_span v)
      (fun fm -> pp_print_string fm "_")
      qname.qn_scopes;
    let has_init = pp_print_qn_init fm qname.qn_init ~to_decl_str
    in
    if not (has_init & to_decl_str) then
      pp_print_string fm qname.qn_sname
  in
  match qname.qn_namespace with
    | QN_CLANG 
    | QN_CSTD
    | QN_BUILTIN ->
	pp_print_string fm qname.qn_sname

    | QN_DEFAULT ->
	begin
	  match qname.qn_span, qname.qn_class with
	    | QN_AUTO, QN_CODE_ADDR ->
		pp_print_string fm qname.qn_sname
	    | QN_STATIC, QN_CODE_ADDR -> print_full_path ()
	    | _, QN_TYPE_NAME 
	    | _, (QN_ENUM_CNST _) ->
		pp_print_string fm qname.qn_sname
		  
	    | _, QN_PARAM_NAME i ->
		pp_print_string fm (qname.qn_sname ^ "_T_" ^ (string_of_int i))
		  
	    | _, QN_STRN_CNST ->
		begin
		  Mlite_printer.pp_print_list fm
		    (fun fm v -> pp_print_qn_scope fm qname.qn_span v)
		    (fun fm -> pp_print_string fm "_")
		    qname.qn_scopes;
		  pp_print_string fm qname.qn_sname
		end
		  
	    | _, _ ->
		begin
		  match qname.qn_scopes with
		    | []
		    | [QN_SCOPE_FILE _] ->
			begin
			  let has_init = 
			    pp_print_qn_init fm qname.qn_init ~to_decl_str
			  in
			  if not (has_init & to_decl_str) then
			    pp_print_string fm qname.qn_sname
			end
		    | _ ->
			print_full_path ()
		end
	end
    | QN_TMP ->
	pp_print_string fm qname.qn_sname
    | QN_CUSTOM str -> assert false

let to_ref_str: t -> string = 
  fun qual_name -> 
    let sfm = Str_util.create_sfm 10
    in
    pp_print_qn sfm.Str_util.formatter qual_name ~to_decl_str:false;
    Str_util.flush_sfm sfm

let to_decl_str: t -> string = 
  fun qual_name -> 
    let sfm = Str_util.create_sfm 10
    in
    pp_print_qn sfm.Str_util.formatter qual_name ~to_decl_str:true;
    Str_util.flush_sfm sfm

let pp_print_qn_decl_str (fm:formatter) (qname:t) : unit = 
  pp_print_qn fm qname ~to_decl_str:true


let pp_print_qn_ref_str (fm:formatter) (qname:t) : unit = 
    pp_print_qn fm qname ~to_decl_str:false
