open Scmm_ast
open Format
open Mapping

let enable_region = ref false

type c_file_unit = Cmm_ast.program
let description () = []
let suffix () = ".c--"


let rec pp_print_name fm s =
  pp_print_string fm s
    
and pp_print_conv fm s =
  begin
    pp_open_hbox fm ();
    pp_print_string fm "foreign";
    pp_print_space fm ();
    pp_print_string fm "\"";
    pp_print_string fm s;
    pp_print_string fm "\"";
    pp_close_box fm ();
  end
    
and pp_print_hint fm s =
  pp_print_string fm ("\"" ^ s ^ "\"")
    
and pp_print_reg fm s = 
  pp_print_string fm ("\"" ^ s ^ "\"")
    
and pp_print_target fm s = 
  pp_print_string fm s
    
and pp_print_alias_set fm s = 
  pp_print_string fm s
    
and pp_print_size fm s = 
  pp_print_int fm s
    
and pp_print_align fm s = 
  pp_print_int fm s
    
and pp_print_aligned fm s = 
  pp_print_string fm "aligned";
  pp_print_space fm ();
  pp_print_int fm s
    
and pp_print_in_alias fm s = 
  pp_print_string fm s
    
and pp_print_op fm s = 
  pp_print_string fm s
    
and pp_print_region fm (i, j) = 
  begin
    if !enable_region then
      begin
	pp_open_box fm 0;
	pp_print_string fm "(";
	pp_print_int fm i;
	pp_print_string fm ",";
	pp_print_int fm j;
	pp_print_string fm ")";
	pp_close_box fm ();
	pp_print_space fm ();
      end
  end

and pp_print_ty fm = function
  | TyAt (ty, region) ->
      pp_print_region fm region;
      pp_print_ty fm ty;
      
  | BitsTy size ->
      pp_print_string fm "bits";
      pp_print_size fm size
	
  | TypeSynonym name ->
      pp_print_string fm name

and pp_print_name_or_memloc fm = function
  | Reg name -> pp_print_name fm name
  | Memloc memloc -> pp_print_memloc fm memloc

and pp_print_memloc fm = function
  | MemAt (memloc, region) ->
      begin
	pp_print_region fm region;
	pp_print_memloc fm memloc
      end
	
  | Mem (ty, expr, aligned_opt, in_alias_list) ->
      begin
	pp_print_ty fm ty;
	pp_print_string fm "[";
	pp_print_aexpr fm expr;
	apply_opt (fun v ->
	  pp_print_space fm ();
	  pp_print_aligned fm v)
	  aligned_opt;
	pp_print_string fm "]";
	if in_alias_list <> [] then
	  begin
	    pp_print_space fm ();
	    Mlite_printer.pp_print_list fm
	      pp_print_in_alias 
	      (fun fm -> pp_print_cut fm ())
	      in_alias_list;
	  end
      end
	
and pp_print_actual fm (hint_opt, expr, aligned_opt) = 
  begin
    apply_opt (fun v -> pp_print_hint fm v; pp_print_space fm ()) hint_opt;
    pp_print_aexpr fm expr;
    apply_opt (pp_print_aligned fm) aligned_opt
  end
    
and pp_print_actual_list fm actual_list = 
  pp_print_string fm "(";
  Mlite_printer.pp_print_list fm
    pp_print_actual
    (fun fm -> pp_print_string fm ","; pp_print_space fm ())
    actual_list;
  pp_print_string fm ")"


and pp_print_cactual fm (hint_opt, expr, aligned_opt) = 
  begin
    apply_opt (fun v -> pp_print_hint fm v; pp_print_space fm ()) hint_opt;
    pp_print_cexpr fm expr;
    apply_opt (pp_print_aligned fm) aligned_opt
  end
    
and pp_print_cactual_list fm actual_list = 
  pp_print_string fm "(";
  Mlite_printer.pp_print_list fm
    pp_print_cactual
    (fun fm -> pp_print_string fm ","; pp_print_space fm ())
    actual_list;
  pp_print_string fm ")"
    

and pp_print_target_list fm target_list = 
  if target_list <> [] then
    begin
      pp_print_space fm ();
      pp_print_string fm "targets";
      pp_print_space fm ();
      Mlite_printer.pp_print_list fm
	pp_print_target
	(fun fm -> pp_print_string fm ","; pp_print_space fm ())
	target_list;
    end

and pp_print_val_ty fm v =
  pp_print_string fm "::";
  pp_print_ty fm v
    
and pp_print_cexpr fm = function
  | CexprAt (expr, region) -> 
      pp_print_region fm region;
      pp_print_cexpr fm expr;
      
  | Sint (string, ty_opt) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm string;
	apply_opt (pp_print_val_ty fm) ty_opt;
	pp_close_box fm ()
      end
	
  | Uint (string, ty_opt) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm string;
	apply_opt (pp_print_val_ty fm) ty_opt;
	pp_close_box fm ()
      end
	
  | Float (string, ty_opt) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm string;
	apply_opt (pp_print_val_ty fm) ty_opt;
	pp_close_box fm ();
      end
	
  | Char (i, ty_opt) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "'";
	pp_print_string fm (Char.escaped (Char.chr i));
	pp_print_string fm "'";
	apply_opt (pp_print_val_ty fm) ty_opt;
	pp_close_box fm ()
      end
	
  | SymAddr name ->
      pp_print_name fm name
	
  | CbinOp (e1, op, e2) ->
      begin
	pp_open_box fm 0;
	pp_print_cexpr fm e1;
	pp_print_space fm ();
	pp_print_op fm op;
	pp_print_space fm ();
	pp_print_cexpr fm e2;
	pp_close_box fm ()
      end
	
  | CunOp (op, expr) ->
      begin
	pp_open_box fm 0;
	pp_print_op fm op;
	pp_print_cexpr fm expr;
	pp_close_box fm ()
      end
	
  | CprimOp (name, actual_list) ->
      begin
	pp_open_box fm 0;
	pp_print_name fm ("%" ^ name);
	pp_print_cactual_list fm actual_list;
	pp_close_box fm ()
      end

and pp_print_aexpr fm = function
  | Cexpr cexpr ->
      pp_print_cexpr fm cexpr
  | FetchReg name -> 
      pp_print_string fm "fetchr ";
      pp_print_name fm name

and pp_print_expr fm = function
  | ExprAt (expr, region) -> 
      pp_print_region fm region;
      pp_print_expr fm expr;

  | Aexpr expr ->
      pp_print_aexpr fm expr
      
  | FetchMem memloc ->
      pp_print_string fm "fetchm ";
      pp_print_memloc fm memloc
	
  | BinOp (e1, op, e2) ->
      begin
	pp_open_box fm 0;
	pp_print_aexpr fm e1;
	pp_print_space fm ();
	pp_print_op fm op;
	pp_print_space fm ();
	pp_print_aexpr fm e2;
	pp_close_box fm ()
      end
	
  | UnOp (op, expr) ->
      begin
	pp_open_box fm 0;
	pp_print_op fm op;
	pp_print_aexpr fm expr;
	pp_close_box fm ()
      end
	
  | PrimOp (name, actual_list) ->
      begin
	pp_open_box fm 0;
	pp_print_name fm ("%" ^ name);
	pp_print_actual_list fm actual_list;
	pp_close_box fm ()
      end

and pp_print_import fm (str_opt, name) =
  apply_opt 
    (fun v -> 
      pp_print_string fm ("\"" ^ v ^ "\"");
      pp_print_space fm ();
      pp_print_string fm "as";
      pp_print_space fm ()
    ) str_opt;
  pp_print_name fm name
    
and pp_print_export fm (name, str_opt) =
  pp_print_name fm name;
  apply_opt 
    (fun v -> 
      pp_print_space fm ();
      pp_print_string fm "as";
      pp_print_space fm ();
      pp_print_string fm ("\"" ^ v ^ "\"")
    ) str_opt

and pp_print_variance fm = function
  | Invariant -> 
      pp_print_string fm "invariant";
      pp_print_space fm ()
	
  | Invisible -> assert false
  | Variant -> () (*pp_print_string fm "variant"*)
      
and pp_print_register fm (variance, hint_opt, ty, name, reg_opt) =
  begin
    pp_open_hbox fm ();
    pp_print_variance fm variance;
    apply_opt (fun v -> pp_print_hint fm v; pp_print_space fm ()) hint_opt;
    pp_print_ty fm ty;
    pp_print_space fm ();
    pp_print_string fm name;
    apply_opt(fun v -> pp_print_space fm ();
      pp_print_string fm "="; 
      pp_print_space fm (); pp_print_reg fm v) reg_opt;
    pp_print_string fm ";";
    pp_close_box fm ()
  end

and pp_print_arch fm v = 
  let _ = pp_open_hbox fm ()
  in
  let _ = match v with
    | Memsize i -> 
	pp_print_string fm "memsize";
	pp_print_space fm ();
	pp_print_int fm i
	  
    | ByteorderBig -> 
	pp_print_string fm "byteorder";
	pp_print_space fm ();
	pp_print_string fm "big"
	  
    | ByteorderLittle ->
	pp_print_string fm "byteorder";
	pp_print_space fm ();
	pp_print_string fm "little"
	  
    | FloatRepr str ->
	pp_print_string fm "float";
	pp_print_space fm ();
	pp_print_string fm ("\"" ^ str ^ "\"")
	  
    | Charset str ->
	pp_print_string fm str
	  
    | WordSize i ->
	pp_print_string fm "wordsize";
	pp_print_space fm ();
	pp_print_int fm i
	  
    | PointerSize i ->
	pp_print_string fm "pointersize";
	pp_print_space fm ();
	pp_print_int fm i
  in
  pp_close_box fm ()
    
and pp_print_decl fm = function
  | DeclAt (decl, region) ->
      begin
	pp_print_region fm region;
	pp_print_decl fm decl
      end
	
  | Import (ty_opt, import_list) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "import";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_import
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  import_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | Export (typ_opt, export_list) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "export";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_export
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  export_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | Const (ty_opt, name, expr) ->
      begin
	pp_open_box fm 0;
	pp_print_string fm "const";
	pp_print_space fm ();
	apply_opt (fun v -> pp_print_ty fm v; pp_print_space fm ()) ty_opt;
	pp_print_name fm name;
	pp_print_space fm ();
	pp_print_string fm "=";
	pp_print_space fm ();
	pp_print_cexpr fm expr;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | Typedef (ty, name_list) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "typedef";
	pp_print_space fm ();
	pp_print_ty fm ty;
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm 
	  pp_print_name
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  name_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | Registers register_list ->
      begin
	Mlite_printer.pp_print_list fm
	  pp_print_register
	  (fun fm -> pp_print_space fm ())
	  register_list
      end
	
  | Pragma -> assert false
  | Target arch_list ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "target";
	pp_open_vbox fm 0;
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm 
	  pp_print_arch
	  (fun fm -> pp_print_space fm ())
	  arch_list;
	pp_print_string fm ";";
	pp_close_box fm ();
	pp_close_box fm ();
      end
	

and pp_print_bare_formal fm 
    (hint_opt, variance, ty, name, aligned_opt) =
  apply_opt (fun v -> pp_print_hint fm v; pp_print_space fm ()) hint_opt;
  pp_print_variance fm variance;
  pp_print_ty fm ty;
  pp_print_space fm ();
  pp_print_name fm name;
  apply_opt (pp_print_aligned fm) aligned_opt;
  

and pp_print_formal fm (region, bare_formal) = 
  pp_print_region fm region;
  pp_print_bare_formal fm bare_formal
    
and pp_print_memsize fm = function
  | NoSize -> () (* not array type *)
  | DynSize -> pp_print_string fm "[]"
  | FixSize expr -> 
      begin
	pp_print_string fm "[";
	pp_print_cexpr fm expr;
	pp_print_string fm "]"
      end

and pp_print_init fm = function
  | InitAt (init, region) -> 
      begin
	pp_print_region fm region;
	pp_print_init fm init
      end
	
  | InitExprs (expr_list) ->
      begin
	pp_open_box fm 0;
	pp_print_string fm "{";
	Mlite_printer.pp_print_list fm 
	  pp_print_cexpr
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  expr_list;
	pp_print_string fm "}";
	pp_close_box fm ()
      end
	
  | InitStr str ->
      begin
	pp_print_string fm "\"";
	pp_print_string fm str; (*(String.escaped str);*)
	pp_print_string fm "\""
      end
	
  | InitUStr str ->
      begin
	pp_print_string fm "\"";
	pp_print_string fm (String.escaped str);
	pp_print_string fm "\""
      end
	
and pp_print_datum fm = function
  | DatumAt (datum, region) ->
      begin
	pp_print_region fm region;
	pp_print_datum fm datum;
      end
	
  | Label name ->
      pp_print_name fm (name ^ ":")
	
  | Align align ->
      pp_print_string fm "align";
      pp_print_space fm ();
      pp_print_align fm align;
      pp_print_string fm ";"
	
  | MemDecl (ty, memsize, init_opt) ->
      begin
	pp_open_hbox fm ();
	pp_print_ty fm ty;
	pp_print_space fm ();
	pp_print_memsize fm memsize;
	apply_opt 
	  (fun v -> pp_print_space fm ();
	    pp_print_init fm v) init_opt;
	pp_print_string fm ";";
	pp_close_box fm ();
      end
	
and pp_print_cformal fm 
    (region, hint_opt, name, aligned_opt) =
  apply_opt (pp_print_hint fm) hint_opt;
  pp_print_name fm name;
  apply_opt (pp_print_aligned fm) aligned_opt
    
and pp_print_flow_list fm l =
  Mlite_printer.pp_print_list fm 
    pp_print_flow
    (fun fm -> pp_print_space fm ())
    l
    
and pp_print_flow fm = function
  | FlowAt (flow, region) ->
      pp_print_region fm region;
      pp_print_flow fm flow
	
  | CutsTo targets -> 
      begin
	pp_print_string fm "also cuts to";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_name 
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  targets;
      end
	
  | UnwindsTo targets -> 
      begin
	pp_print_string fm "also unwinds to";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_name 
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  targets;
      end
	
  | ReturnsTo targets -> 
      begin
	pp_print_string fm "also returns to";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_name 
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  targets;
      end
	
  | NeverReturns -> 
      begin
	pp_print_string fm "never";
	pp_print_space fm ();
	pp_print_string fm "returns"
      end
	
  | Aborts -> 
      pp_print_string fm "also aborts"

and pp_print_mem fm = function
  | AliasAt (mem, region) ->
      pp_print_region fm region;
      pp_print_mem fm mem
	
  | Reads (name_list) ->
      Mlite_printer.pp_print_list fm
	pp_print_name
	(fun fm -> pp_print_cut fm ())
	name_list
	
  | Writes name_list ->
      Mlite_printer.pp_print_list fm
	pp_print_name
	(fun fm -> pp_print_cut fm ())
	name_list
	
and pp_print_procann fm = function
  | Flow flow ->
      pp_print_flow fm flow
  | Alias mem ->
      pp_print_string fm "reads";
      pp_print_space fm ();
      pp_print_mem fm mem

and pp_print_altconnt fm (e1, e2) =
  begin
    pp_open_box fm 0;
    pp_print_string fm "<";
    pp_print_expr fm e1;
    pp_print_string fm "/";
    pp_print_expr fm e2;
    pp_print_string fm ">";
    pp_close_box fm ();
  end
    
and pp_print_range fm = function
  | Point expr ->
      pp_print_cexpr fm expr
  | Range (e1, e2) ->
      pp_print_cexpr fm e1;
      pp_print_space fm ();
      pp_print_string fm "..";
      pp_print_space fm ();
      pp_print_cexpr fm e2
	
and pp_print_guarded fm (expr_opt, expr) =
  apply_opt (pp_print_expr fm) expr_opt;
  pp_print_expr fm expr

and pp_print_arm fm = function
  | ArmAt (arm, region) ->
      pp_print_region fm region;
      pp_print_arm fm arm
	
  | Case (range_list, body_list) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "case";
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm
	  pp_print_range 
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  range_list;
	pp_print_string fm ":";
	pp_print_space fm ();
	pp_print_body_list fm body_list;
	pp_close_box fm ()
      end
and pp_print_asm (fm:formatter) (str_list, asm_details_opt) = 
  pp_open_vbox fm 0;
  begin
    pp_print_string fm "__asm__";
    pp_print_space fm ();
    pp_print_string fm "(";
    pp_print_space fm ();
    let _ = Mlite_printer.pp_print_list fm 
      (fun fm s -> pp_print_string fm ("\"" ^ s ^ "\""))
      (fun fm -> pp_print_space fm ()) str_list
    in
    let _ = match asm_details_opt with
      | Some asm ->
	  begin
	    pp_print_space fm ();
	    pp_print_string fm ":";
	    Mlite_printer.pp_print_list fm 
	      (fun fm (so, s,e) ->
		pp_print_string fm ("\"" ^ s ^ "\"");
		pp_print_string fm " (";
		pp_print_name fm e;
		pp_print_string fm ")")
	      (fun fm -> pp_print_string fm ",") asm.asm_outputs;
	    pp_print_space fm ();
	    pp_print_string fm ":";
	    Mlite_printer.pp_print_list fm 
	      (fun fm (so, s,e) ->
		pp_print_string fm ("\"" ^ s ^ "\"");
		pp_print_string fm " (";
		pp_print_expr fm e;
		pp_print_string fm ")")
	      (fun fm -> pp_print_string fm ",") asm.asm_inputs;
	    if (asm.asm_clobbers <> []) then
	      begin
		pp_print_space fm ();
		pp_print_string fm ":";
		Mlite_printer.pp_print_list fm 
		  (fun fm s ->
		    pp_print_string fm ("\"" ^ s ^ "\""))
		  (fun fm -> pp_print_string fm ",") asm.asm_clobbers;
	      end
	  end
      | None -> ()
    in
    pp_print_string fm ");";
  end;
  pp_close_box fm ()

and pp_print_stmt fm = function
  | StmtAt (stmt, region) ->
      pp_print_region fm region;
      pp_print_stmt fm stmt
	
  | IfStmt (expr, body_list1, body_list2) ->
      begin
	pp_open_box fm 2;
	pp_print_string fm "if (";
	pp_print_expr fm expr;
	pp_print_string fm ")";
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_body_list fm body_list1;
	if body_list2 <> [] then
	  begin
	    pp_print_space fm ();
	    pp_print_string fm "else";
	    pp_print_space fm ();
	    pp_print_body_list fm body_list2
	  end
      end
	
  | SwitchStmt (range_opt, expr, arm_list) ->
      begin
	pp_open_box fm 2;
	pp_print_string fm "switch";
	pp_print_space fm ();
	apply_opt
	  (fun v -> 
	    pp_print_string fm "["; 
	    pp_print_range fm v; 
	    pp_print_string fm "]";
	    pp_print_space fm ()) 
	  range_opt;
	pp_print_aexpr fm expr;
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_string fm "{";
	pp_open_vbox fm 2;
	pp_print_space fm ();
	Mlite_printer.pp_print_list fm 
	  pp_print_arm
	  (fun fm -> pp_print_space fm ())
	  arm_list;
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_string fm "}";
      end

  | LabelStmt name ->
      pp_print_name fm (name ^ ":")
	
  | ContStmt (name, cformal_list) ->
      begin
	pp_open_hbox fm ();
	pp_print_string fm "continuation";
	pp_print_space fm ();
	pp_print_name fm name;
	pp_print_space fm ();
	pp_print_string fm "(";
	Mlite_printer.pp_print_list fm
	  pp_print_cformal
	  (fun fm -> pp_print_string fm ","; pp_print_space fm ())
	  cformal_list;
	pp_print_string fm "):";
	pp_close_box fm ()
      end
	
  | SpanStmt (e1, e2, body_list) ->
      begin
	pp_open_box fm 0;
	pp_print_string fm "span";
	pp_print_space fm ();
	pp_print_cexpr fm e1;
	pp_print_space fm ();
	pp_print_cexpr fm e2;
	pp_print_space fm ();
	pp_print_body_list fm body_list;
	pp_close_box fm ()
      end

  | SaveStmt (name, expr) ->
      begin
	pp_open_box fm 0;
	pp_print_name fm name;
	pp_print_string fm " = ";
	pp_print_expr fm expr;
	pp_print_string fm ";";
	pp_close_box fm ()
      end

  | StoreStmt (memloc, guarded) ->
      begin
	pp_open_box fm 0;
	pp_print_memloc fm memloc;
	pp_print_string fm " = ";
	pp_print_guarded fm guarded;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | LoadStmt (name, memloc) ->
      begin
	pp_open_box fm 0;
	pp_print_name fm name;
	pp_print_string fm " = ";
	pp_print_memloc fm memloc;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | CallStmt (name_opt, conv_opt, expr, 
    actual_list, target_list, procann_list) -> 
      begin
	pp_open_box fm 0;
	apply_opt
	  (fun v -> 
	    pp_print_name_or_memloc fm v; 
	    pp_print_space fm ();
	    pp_print_string fm "=";
	    pp_print_space fm ()) name_opt;
	apply_opt 
	  (fun v -> pp_print_conv fm v; 
	    pp_print_space fm ()) conv_opt;
	pp_print_aexpr fm expr;
	pp_print_space fm ();
	pp_print_actual_list fm actual_list;
	pp_print_target_list fm target_list;
	if procann_list <> [] then
	  begin
	    pp_print_space fm ();
	    Mlite_printer.pp_print_list fm
	      pp_print_procann
	      (fun fm -> pp_print_space fm ())
	      procann_list;
	  end;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | PrimStmt (name_opt, conv_opt, 
    name, actual_list, flow_list) -> 
      begin
	pp_open_box fm 0;
	apply_opt
	  (fun v -> 
	    pp_print_name_or_memloc fm v; 
	    pp_print_space fm ();
	    pp_print_string fm "=";
	    pp_print_space fm ()) name_opt;
	apply_opt 
	  (fun v -> pp_print_conv fm v; 
	    pp_print_space fm ()) conv_opt;
	pp_print_string fm "%%";
	pp_print_name fm name;
	pp_print_space fm ();
	pp_print_actual_list fm actual_list;
	if flow_list <> [] then
	  begin
	    pp_print_space fm ();
	    pp_print_flow_list fm flow_list;
	  end;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | GotoStmt (e, target_list) -> 
      begin
	pp_open_box fm 0;
	pp_print_string fm "goto";
	pp_print_space fm ();
	pp_print_expr fm e;
	pp_print_target_list fm target_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | JumpStmt (conv_opt, expr, actual_list, target_list) -> 
      begin
	pp_open_box fm 0;
	apply_opt 
	  (fun v -> pp_print_conv fm v; pp_print_space fm ())
	  conv_opt;
	pp_print_string fm "jump";
	pp_print_space fm ();
	pp_print_expr fm expr;
	pp_print_space fm ();
	pp_print_actual_list fm actual_list;
	pp_print_target_list fm target_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | CutStmt (expr, actual_list, flow_list) -> 
      begin
	pp_open_box fm 0;
	pp_print_string fm "cut";
	pp_print_space fm ();
	pp_print_string fm "to";
	pp_print_space fm ();
	pp_print_expr fm expr;
	pp_print_space fm ();
	pp_print_actual_list fm actual_list;
	if flow_list <> [] then
	  pp_print_space fm ();
	pp_print_flow_list fm flow_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | ReturnStmt (conv_opt, altcont_opt, actual_list) -> 
      begin
	pp_open_hbox fm ();
	apply_opt 
	  (fun v -> pp_print_conv fm v; pp_print_space fm ()) conv_opt;
	pp_print_string fm "return";
	pp_print_space fm ();
	apply_opt 
	  (fun v -> pp_print_altconnt fm v; pp_print_space fm ())
	  altcont_opt;
	pp_print_actual_list fm actual_list;
	pp_print_string fm ";";
	pp_close_box fm ()
      end
	
  | EmptyStmt -> pp_print_string fm ";"
  | CommentStmt str -> assert false
  | LimitcheckStmt (expr, expr_opt) -> assert false
  | AsmStmt (string_list, asm_opt) ->
      pp_print_asm fm (string_list, asm_opt)

and pp_print_body_list fm body_list = 
  begin
    pp_open_vbox fm 0;
    pp_print_string fm "{";
    pp_open_vbox fm 2;
    pp_print_space fm ();
    Mlite_printer.pp_print_list fm
      pp_print_body
      (fun fm -> pp_print_space fm ())
      body_list;
    pp_close_box fm ();
    pp_print_space fm ();
    pp_print_string fm "}";
    pp_close_box fm ()
  end
    
and pp_print_body fm = function
  | BodyAt (body, region) ->
      pp_print_region fm region;
      pp_print_body fm body
	
  | DeclBody decl ->
      pp_print_decl fm decl
	
  | StmtBody stmt ->
      pp_print_stmt fm stmt
	
  | DataBody datum_list ->
      begin
	pp_open_box fm 0;
	pp_print_string fm "stackdata";
	pp_print_space fm ();
	pp_print_string fm "{";
	Mlite_printer.pp_print_list fm
	  pp_print_datum 
	  (fun fm -> pp_print_space fm ())
	  datum_list;
	pp_print_string fm "}";
	pp_close_box fm ()
      end
	
and pp_print_proc fm (conv_opt, name, formal_list, body_list, region) =
  begin
    pp_open_hbox fm ();
    apply_opt 
      (fun v -> pp_print_conv fm v; pp_print_space fm ()) conv_opt;
    pp_print_name fm name;
    pp_print_space fm ();
    pp_print_string fm "(";
    Mlite_printer.pp_print_list fm
      pp_print_formal
      (fun fm -> pp_print_string fm ","; pp_print_space fm ())
      formal_list;
    pp_print_string fm ")";
    pp_close_box fm ();
    pp_print_space fm ();
    pp_print_body_list fm body_list
  end
    
and pp_print_section_list fm section_list = 
  pp_print_string fm "{";
  pp_open_vbox fm 2;
  pp_print_space fm ();
  Mlite_printer.pp_print_list fm
    pp_print_section
    (fun fm -> pp_print_space fm ())
    section_list;
  pp_close_box fm ();
  pp_print_space fm ();
  pp_print_string fm "}"
    
and pp_print_section fm = function
  | SectionAt (section, region) ->
      pp_print_region fm region;
      pp_print_section fm section
	
  | Decl decl ->
      pp_print_decl fm decl
	
  | Procedure proc ->
      pp_print_proc fm proc
	
  | Datum datum ->
      pp_print_datum fm datum
	
  | SSpan (e1, e2, section_list) ->
      begin
	pp_open_vbox fm 0;
	pp_open_hbox fm ();
	pp_print_string fm "span";
	pp_print_space fm ();
	pp_print_cexpr fm e1;
	pp_print_space fm ();
	pp_print_cexpr fm e2;
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_section_list fm section_list;
	pp_close_box fm ()
      end

and pp_print_toplevel fm = function
  | ToplevelAt (toplevel, region) ->
      pp_print_region fm region;
      pp_print_toplevel fm toplevel
	
  | Section (name, section_list) ->
      begin
	pp_open_vbox fm 0;
	pp_open_hbox fm ();
	pp_print_string fm "section";
	pp_print_space fm ();
	pp_print_name fm ("\"" ^ name ^ "\"");
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_section_list fm section_list;
	pp_close_box fm ()
      end
	
  | TopDecl decl ->
      pp_print_decl fm decl
	
  | TopProcedure proc ->
      pp_print_proc fm proc

and pp_print_t (fm:formatter) (p:program) = 
  pp_open_vbox fm 0;
  Mlite_printer.pp_print_list fm
    pp_print_toplevel
    (fun fm -> pp_print_space fm ())
    p;
  pp_close_box fm ();
  pp_print_flush fm ()


let pp_print_c_file_unit = pp_print_t
