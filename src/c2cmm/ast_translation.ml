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

module T = Tent
module TO = Tent_op
module E = Senv
module CEO = Cent_op
module CE = Cent

type trans_unit =
  | C_FILE of string
  | CABS of (string * Cabs.definition list)
  | AST_AA_GRAM of (string * Ast_aa_gram.c_translation_unit)
  | AST_BA_STMT of (string * Ast_ba_stmt.c_translation_unit)
  | AST_CA_EXPR of (string * Ast_ca_expr.c_translation_unit)
  | AST_DA_TYPE of (string * Ast_da_type.c_translation_unit)
  | AST_EA_EXPR of (string * Ast_ea_expr.c_translation_unit * E.env)
  | AST_EB_EXPR of (string * Ast_eb_expr.c_translation_unit * E.env)
  | AST_EC_EXPR of (string * Ast_ec_expr.c_translation_unit * E.env)
  | AST_FA_STMT of (string * Ast_fa_stmt.c_translation_unit * E.env)
  | AST_GA_CODE of (string * Ast_ga_code.c_translation_unit * E.env)
  | AST_HA_GRAF of (string * Ast_ha_graf.c_translation_unit * E.env)
  | AST_IA_INIT of (string * Ast_ia_init.c_translation_unit * E.env)
  | CMM_AST of (string * Cmm_ast.program)
  | SCMM_AST of (string * Scmm_ast.program)

and trans_phase = 
  | Phase_cabs
  | Phase_ast_aa_gram
  | Phase_ast_ba_stmt
  | Phase_ast_ca_expr
  | Phase_ast_da_type
  | Phase_ast_ea_expr
  | Phase_ast_eb_expr
  | Phase_ast_ec_expr
  | Phase_ast_fa_stmt
  | Phase_ast_ga_code
  | Phase_ast_ha_graf
  | Phase_ast_ia_init
  | Phase_ast_cmm
  | Phase_ast_scmm
  | Phase_end

exception Phase_trans_done
  
let trans_phase = ref Phase_end
    
let cc_basic_phase_opts = 
  [
    ("--ast-cabs",
    Arg.Unit 
      (fun () -> trans_phase := Phase_cabs),
    "output frontc cabs C code");
    ("--ast-aa-gram", 
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_aa_gram),
    "output C99 gram C code");
    ("--ast-ba-stmt",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_ba_stmt),
    "output ba-stmt C code");
    ("--ast-ca-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ca_expr),
    "output ca-expr C code");
    ("--ast-da-type",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_da_type),
    "output da-type C code");
    ("--ast-ea-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ea_expr),
    "output ea-expr C code");
    ("--ast-eb-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_eb_expr),
    "output eb-expr C code");
    ("--ast-ec-expr",
    Arg.Unit 
      (fun () -> trans_phase := Phase_ast_ec_expr),
    "output ec-expr C code");
    ("--ast-fa-stmt",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_fa_stmt),
    "output fa-stmt C code");
    ("--ast-ga-code",
    Arg.Unit
      (fun () -> trans_phase := Phase_ast_ga_code),
    "output ga-code C code");    
    ("--ast-ha-graf",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_ha_graf)),
    "output ha-graf C code");
    ("--ast-ia-init",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_ia_init)),
    "output ia-init C code");
    ("--ast-cmm",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_cmm)),
    "output cmm code");
    ("--ast-scmm",
    Arg.Unit 
      (fun () -> trans_phase := (Phase_ast_scmm)),
    "output scmm code");
  ]


let enable_cproto = ref false

let user_opts =
  [
    ("--cproto", Arg.Set enable_cproto,
    "output C prototype in *.cproto, this works like cproto");
  ] @ 
    Norm_ast_ba_stmt.user_opts @ 
    Gen_ast_ca_expr.user_opts @
    Gen_ast_da_type.user_opts @
    Norm_ast_da_type.user_opts @
    Gen_ast_ea_expr.user_opts @
    Norm_ast_ea_expr.user_opts @
    Gen_ast_fa_stmt.user_opts @
    Gen_ast_ga_code.user_opts @
    Gen_ast_ha_graf.user_opts @
    Ast_ha_graf_vcg_printer.user_opts @
    Gen_cmm_ast.user_opts 
    

let cc_intermediate_phase_opts = []
    
let cc_link_phase_opts = []
  
let trans_phase_opts = 
  cc_basic_phase_opts 



let get_debug_filename: trans_unit -> string = 
  fun trans_unit ->
    let (basename, suffix) = match trans_unit with
      | C_FILE filename -> 
	  (filename, ".00.i")
	    
      | CABS (basename, ast) -> 
	  (basename, ".cabs.i")
	  
      | AST_AA_GRAM (basename, c_translation_unit) -> 
	  (basename, Ast_aa_gram.suffix)
	    
      | AST_BA_STMT (basename, c_translation_unit) -> 
	  (basename, Ast_ba_stmt.suffix)
	    
      | AST_CA_EXPR (basename, c_translation_unit) -> 
	  (basename, Ast_ca_expr.suffix)
	    
      | AST_DA_TYPE (basename, c_translation_unit) -> 
	  (basename, Ast_da_type.suffix)
	    
      | AST_EA_EXPR (basename, c_translation_unit, env) ->
	  (basename, Ast_ea_expr.suffix)
	    
      | AST_EB_EXPR (basename, c_translation_unit, env) ->
	  (basename, Ast_eb_expr.suffix)

      | AST_EC_EXPR (basename, c_translation_unit, env) ->
	  (basename, Ast_ec_expr.suffix)
	    
      | AST_FA_STMT (basename, c_translation_unit, env) ->
	  (basename, Ast_fa_stmt.suffix)
	    
      | AST_GA_CODE (basename, c_translation_unit, env) ->
	  (basename, Ast_ga_code.suffix)
	    
      | AST_HA_GRAF (basename, c_translation_unit, env) ->
	  (basename, Ast_ha_graf.suffix)
	    
      | AST_IA_INIT (basename, c_translation_unit, env) ->
	  (basename, Ast_ia_init.suffix)
	    
      | CMM_AST (basename, c_translation_unit) ->
	  (basename, ".c--")

      | SCMM_AST (basename, c_translation_unit) ->
	  (basename, ".sc--")
    in
    basename ^ suffix


let emit_code: trans_unit -> string -> unit =
  fun trans_unit filename ->
    let out_chan = open_out filename
    in
    let out_fm = Format.formatter_of_out_channel out_chan
    in
    let _ = match trans_unit with
      | C_FILE filename ->
	  begin
	    let in_chan = open_in filename
	    in
	    try
	      while true do
		let str = input_line in_chan
		in output_string out_chan str
	      done
	    with
	      | End_of_file ->
		  close_in in_chan;
		  close_out out_chan
	  end
	    
      | CABS (basename, ast) ->
	  begin
	    let _ = Whitetrack.setOutput out_chan
	    in Cprint.printFile out_chan (basename, ast);
	  end
	    
      | AST_AA_GRAM (basename, c_translation_unit) ->
	  C_file_basic.Ast_aa_gram.write_c_formatter out_fm c_translation_unit
	    
      | AST_BA_STMT (basename, c_translation_unit) ->
	  C_file_basic.Ast_ba_stmt.write_c_formatter out_fm c_translation_unit

      | AST_CA_EXPR (basename, c_translation_unit) ->
	  C_file_basic.Ast_ca_expr.write_c_formatter out_fm c_translation_unit
	    
      | AST_DA_TYPE (basename, c_translation_unit) ->
	  C_file_basic.Ast_da_type.write_c_formatter out_fm c_translation_unit
	    
      | AST_EA_EXPR (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_ea_expr.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_EB_EXPR (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_eb_expr.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_EC_EXPR (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_ec_expr.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_FA_STMT (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_fa_stmt.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_GA_CODE (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_ga_code.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_HA_GRAF (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_ha_graf.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | AST_IA_INIT (basename, c_translation_unit, env) ->
	  C_file_basic.Ast_ia_init.write_c_formatter out_fm c_translation_unit;
	  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl basename
	    
      | CMM_AST (basename, c_translation_unit) ->
	  Cmm_ast_printer.pp_print_t out_fm c_translation_unit
	    
      | SCMM_AST (basename, c_translation_unit) ->
	  Scmm_ast_printer.pp_print_t out_fm c_translation_unit
    in
    Format.pp_print_flush out_fm ();
    close_out out_chan
	    

let dump_ast ast filename = 
  let out_chan = open_out filename
  in 
  let _ = Marshal.to_channel out_chan ast []
  in close_out out_chan



let print_cproto: string -> Ast_da_type.c_translation_unit -> unit = 
  fun filename cunit ->
    let out_chan = open_out (filename ^ ".cproto")
    in
    let out_fm = Format.formatter_of_out_channel out_chan
    in
    Format.pp_set_margin out_fm 99999; (* make sure no line overflows *)
    Ast_da_type_cproto.pp_print out_fm cunit;
    Format.pp_print_flush out_fm ();
    close_out out_chan

let translate: trans_unit -> trans_phase -> trans_unit =
  fun input stop ->
    let ast = ref input
    and is_done = ref false
    in
    let _ = 
      while not !is_done do
	begin
	  if !(Mlite_config.enable_ast_debug) then
	    emit_code !ast (get_debug_filename !ast);
	  
	  match !ast with
	    | C_FILE filename ->
		begin
		  trans_phase := Phase_cabs;
		  let (filename, cabs) = Frontc.parse filename
		  in
		  let basename = Mlitecc_files.check_mlitecc_in filename
		  in ast := CABS (basename, cabs)
		end
		  
	    | CABS (basename, c_translation_unit) ->
		begin
		  trans_phase := Phase_ast_aa_gram;
		  let c_translation_unit = C99ize_cabs.c99ize c_translation_unit
		  in ast := AST_AA_GRAM (basename, c_translation_unit)
		end
		  
	    | AST_AA_GRAM (basename, c_translation_unit) ->
		begin
		  trans_phase := Phase_ast_ba_stmt;
		  let c_translation_unit = Gen_ast_ba_stmt.compile basename c_translation_unit
		  in
		  let c_translation_unit = Norm_ast_ba_stmt.normalize c_translation_unit
		  in ast := AST_BA_STMT (basename, c_translation_unit)
		end
		  
	    | AST_BA_STMT (basename, c_translation_unit) ->
		begin
		  trans_phase := Phase_ast_ca_expr;
		  let c_translation_unit = Gen_ast_ca_expr.compile basename c_translation_unit
		  in ast := AST_CA_EXPR (basename, c_translation_unit)
		end

	    | AST_CA_EXPR (basename, c_translation_unit) ->
		begin
		  trans_phase := Phase_ast_da_type;
		  let c_translation_unit = Gen_ast_da_type.compile basename c_translation_unit
		  in
		  let c_translation_unit = Norm_ast_da_type.normalize c_translation_unit
		  in ast := AST_DA_TYPE (basename, c_translation_unit)
		end
		  
	    | AST_DA_TYPE (basename, c_translation_unit) ->
		begin
		  trans_phase := Phase_ast_ea_expr;
		  if !enable_cproto then
		    print_cproto basename c_translation_unit;

		  let (c_translation_unit, env) = 
		    Gen_ast_ea_expr.compile 
		      Config_builtin.function_list
		      Gnu_builtin.function_list
		      basename c_translation_unit
		  in
		  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl (basename ^ ".pre_typ_norm");
		  let c_translation_unit = Norm_ast_ea_expr.normalize env c_translation_unit
		  in ast := AST_EA_EXPR (basename, c_translation_unit, env);
		  Typ_db_log.doit env.E.te_tbl env.E.ce_tbl (basename ^ ".post_typ_norm");
		  TO.assert_uniqueness env.E.te_tbl;
		end
		  
	    | AST_EA_EXPR (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_ast_eb_expr;
		  let c_translation_unit = 
		    Gen_ast_eb_expr.compile 
		      basename c_translation_unit env
		  in ast := AST_EB_EXPR (basename, c_translation_unit, env);
		  TO.assert_uniqueness env.E.te_tbl;
		end

	    | AST_EB_EXPR (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_ast_ec_expr;
		  let c_translation_unit = 
		    Gen_ast_ec_expr.compile
		      basename c_translation_unit env 
		  in ast := AST_EC_EXPR (basename, c_translation_unit, env);
		  TO.assert_uniqueness env.E.te_tbl;
		end
		  
	    | AST_EC_EXPR (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_ast_fa_stmt;
		  let c_translation_unit = Gen_ast_fa_stmt.compile basename c_translation_unit
		  in ast := AST_FA_STMT (basename, c_translation_unit, env)
		end
		  
	    | AST_FA_STMT (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_ast_ga_code;
		  let c_translation_unit = Gen_ast_ga_code.compile basename c_translation_unit
		  in ast := AST_GA_CODE (basename, c_translation_unit, env)
		end
		  
	    | AST_GA_CODE (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_ast_ha_graf;
		  let c_translation_unit = Gen_ast_ha_graf.compile basename c_translation_unit
		  in ast := AST_HA_GRAF (basename, c_translation_unit, env)
		end
		  
	    | AST_HA_GRAF (basename, c_translation_unit, env) ->
		begin
		  let _ = 
		    Ast_ha_graf_vcg_printer.print_c_file_unit
		      (basename, c_translation_unit)
		  in
		  trans_phase := Phase_ast_ia_init;
		  let c_translation_unit = Gen_ast_ia_init.compile
		    basename c_translation_unit env
		  in ast := AST_IA_INIT (basename, c_translation_unit, env)
		end
		  
	    | AST_IA_INIT (basename, c_translation_unit, env) ->
		begin
		  trans_phase := Phase_end;
		  TO.assert_uniqueness env.E.te_tbl;
		  let ast0 = Gen_cmm_ast.compile (env, c_translation_unit)
		  in ast := CMM_AST (basename, ast0)
		end
	    | CMM_AST _ -> assert false
	    | SCMM_AST _ -> assert false
	end;
	is_done := (!trans_phase = stop) || (!trans_phase = Phase_end)
      done	
    in !ast





open Collection

let merge (files:string list) ~(output:string) ~(merged_basename:string) :unit =
  let id_hash_tbl = StringHashtbl.create 2127
  and typ_hash_tbl = Int.Type_idHashtbl.create 2127
  and fun_hash_tbl = StringHashtbl.create 2127
  in
  let global_env_opt = ref None
  and global_eenv_opt = ref None
  in
  let id_is_first_occurance: string -> bool = 
    fun key ->
      try
	let _ = StringHashtbl.find id_hash_tbl key
	in
	false
      with
	  Not_found ->
	    StringHashtbl.add id_hash_tbl key None;
	    true
	      
  and id_has_occured: string -> unit = 
    fun key ->
      StringHashtbl.add id_hash_tbl key None

  and typ_is_first_occurance: Ast_ha_graf.c_type -> Ast_ha_graf.c_declaration -> bool = 
    fun key c_decl ->
      try
	if (snd key) = !Uni_typ_db.debug_global_teid then
	  print_string "";
	let c_decl0 = Int.Type_idHashtbl.find typ_hash_tbl (snd key)
	in
	match c_decl0 with
	  | Ast_eb_expr.Type_decl _ -> false
	  | Ast_eb_expr.Type_only _ -> 
	      begin
		match c_decl with
		  | Ast_eb_expr.Type_decl _ -> 
		      Int.Type_idHashtbl.add typ_hash_tbl (snd key) c_decl;
		      true
		  | Ast_eb_expr.Type_only _ -> false
		  | _ -> assert false
	      end	    
	  | _ -> assert false
      with
	  Not_found ->
	    Int.Type_idHashtbl.add typ_hash_tbl (snd key) c_decl;
	    true
	      
  and fun_is_first_occurance: string -> bool = 
    fun key ->
      try
	let _ = StringHashtbl.find fun_hash_tbl key
	in
	false
      with
	  Not_found ->
	    StringHashtbl.add fun_hash_tbl key None;
	    true
	      
  and fun_has_occured: string -> unit = 
    fun key ->
      StringHashtbl.add fun_hash_tbl key None
	
  in
  let inner_merge ~(basename:string)
      (c_translation_unit:Ast_ha_graf.c_translation_unit) 
      (env:E.env): Ast_ha_graf.c_external_declaration list = 
    let (env, Ast_ha_graf.Translation_unit (lst, eenv)) = 
      match !global_env_opt with
	| Some (global_env) -> 
	    (global_env, 
	    Lnk_ast_ha_graf.link 
	      ~basename ~global:global_env ~local:env c_translation_unit)
	| None ->
	    global_env_opt := Some (env);
	    (env, Lnk_ast_ha_graf.link
	      ~basename ~global:env ~local:env c_translation_unit)
    and new_lst = ref []
    in
    let _ = global_eenv_opt := Some (eenv)
    in
    let rec f v = match v with
      | Ast_ha_graf.External_declaration_at (coord, v) -> f v
      | Ast_ha_graf.External_declaration_1 (func) -> 
	  begin
	    if func.Ast_ha_graf.name.Qual_name.qn_sname = "xlmath_badfop" then
	      print_string func.Ast_ha_graf.name.Qual_name.qn_sname
	    else ();
	    let _ = match func.Ast_ha_graf.linkage with
	      | Ast_eb_expr.Extern 
	      | Ast_eb_expr.Extern_Inline 
	      | Ast_eb_expr.Static 
	      | _ -> ()
	    in
	    if fun_is_first_occurance func.Ast_ha_graf.name.Qual_name.qn_sname then
	      new_lst := v::!new_lst;
	    fun_has_occured func.Ast_ha_graf.name.Qual_name.qn_sname
	  end
	    
      | Ast_ha_graf.External_declaration_2 (c_decl_list) ->
	  begin
	    let c_decl_list = 
	      List.filter 
		(fun v ->
		  match v with
		    | Ast_eb_expr.Obj_decl (linkage, ce) ->
			begin
			  let c_type = CEO.te_of ce
			  and string = CEO.qname_of ce
			  in
			  match linkage with
			    | Ast_eb_expr.Extern
			    | Ast_eb_expr.Static ->
				id_is_first_occurance string.Qual_name.qn_sname
			    | _ -> 
				id_has_occured string.Qual_name.qn_sname;
				true
			end

		    | Ast_eb_expr.Obj_decl_init (linkage, ce, init) -> 
			begin
			  let c_type = CEO.te_of ce
			  and string = CEO.qname_of ce
			  in
			  match linkage with
			    | Ast_eb_expr.Extern
			    | Ast_eb_expr.Static ->
				id_is_first_occurance string.Qual_name.qn_sname
			    | _ -> 
				id_has_occured string.Qual_name.qn_sname;
				true
			end
			  
		    | Ast_eb_expr.Str_decl_init (linkage, ce, str_literal) ->
			begin
			  let string = CEO.qname_of ce
			  in
			  match linkage with
			    | Ast_eb_expr.Extern
			    | Ast_eb_expr.Static ->
				id_is_first_occurance string.Qual_name.qn_sname
			    | _ -> 
				id_has_occured string.Qual_name.qn_sname;
				true
			end
			  
		    | Ast_eb_expr.Type_def c_type ->
			false (** all typedefs are skipped **)
			  
		    | Ast_eb_expr.Type_decl c_type ->
			typ_is_first_occurance c_type v
			  
		    | Ast_eb_expr.Type_only c_type ->
			typ_is_first_occurance c_type v			      			
		) c_decl_list
	    in
	    if c_decl_list <> [] then
	      new_lst := 
		(Ast_ha_graf.External_declaration_2 (c_decl_list))::!new_lst
	  end
    in
    List.iter f lst;
    (List.rev !new_lst)
  in
  let new_definition_list = ref []
  in
  List.iter
    (fun filename ->
      Printf.printf "processing %s\n" filename;
      let in_chan = open_in filename
      in
      let trans_unit = Marshal.from_channel in_chan
      in
      close_in in_chan;
      let (basename, c_translation_unit, env) = 
	match trans_unit with
	  | AST_HA_GRAF (basename, c_translation_unit, env) ->
	      Tent_op.assert_uniqueness env.E.te_tbl;
	      (basename, c_translation_unit, env)
	  | _ -> assert false
      in
      let l = inner_merge ~basename c_translation_unit env
      in new_definition_list := Safe_list.concat !new_definition_list l
    ) files;
  match !global_env_opt, !global_eenv_opt with
    | Some (global_env), Some (global_eenv) -> 
	begin
	  let v = 
	    AST_HA_GRAF (merged_basename, Ast_ha_graf.Translation_unit
	      (!new_definition_list, global_eenv), global_env)
	  in
	  let out_chan = open_out output (*filename*)
	  in
	  let _ = Marshal.to_channel out_chan v []
	  in close_out out_chan
	end
    | _, _ -> assert false
	  
	
