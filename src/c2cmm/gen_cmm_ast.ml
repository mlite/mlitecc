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

open Int
open Collection
open Safe_list

module C99 = Ast_aa_gram
module I = Ast_ia_init
module O = Cmm_ast
module E = Ast_eb_expr
module EO = Ast_eb_expr_op
module G = Ast_ga_code
module TO = Tent_op
module T = Tent
module QN = Qual_name
module QNP = Qual_name_printer
module CEO = Cent_op
module CE = Cent
module CA = Const_folding
module AEE = Gen_ast_eb_expr

let user_opts = []

let lab_prefix = ref ""

let round_down = (None, O.PrimOp ("round_down", []), None)
let round_up = (None, O.PrimOp ("round_up", []), None)
let round_nearest = (None, O.PrimOp ("round_nearest", []), None)
let round_zero = (None, O.PrimOp ("round_zero", []), None)

let importedString = StringHashtbl.create 117


let string_of_fbits = function
  | E.F32 -> "32"
  | E.F64 -> "64"
  | E.F80 -> "80"

let string_of_ibits = function
  | E.I8 -> "8"
  | E.I16 -> "16"
  | E.I32 -> "32"
  | E.I64 -> "64"

let string_of_ubits = function
  | E.I8 -> "8"
  | E.I16 -> "16"
  | E.I32 -> "32"
  | E.I64 -> "64"

let mk_label_expr i = 
  let l = Code_label.get_num_string i
  in O.Fetch (O.Name (None, !lab_prefix ^ l, None))

let mk_type_span c_type = 
  let tid = snd c_type
  in (O.Uint ("TYPE", None), O.Uint (string_of_int tid, None))
       
let mk_typedecl_span c_type = 
  let tid = snd c_type
  in (O.Uint ("TYPEDECL", None), O.Uint (string_of_int tid, None))
       
let mk_typedef_span c_type = 
  let tid = snd c_type
  in (O.Uint ("TYPEDEF", None), O.Uint (string_of_int tid, None))

let mk_typeonly_span c_type = 
  let tid = snd c_type
  in (O.Uint ("TYPEONLY", None), O.Uint (string_of_int tid, None))

let mk_hint_opt c_type = 
  if TO.is_ptr_typ c_type then
    Some "address"
  else if TO.is_unsigned_integer_typ c_type then
    Some "unsigned"
  else
    None

let mk_span_consts () = 
  [
    O.TopDecl (O.Const (None, "TYPE", O.Uint ("1", None)));
    O.TopDecl (O.Const (None, "TYPEDECL", O.Uint ("2", None)));
    O.TopDecl (O.Const (None, "TYPEDEF", O.Uint ("2", None)));
    O.TopDecl (O.Const (None, "TYPEONLY", O.Uint ("2", None)));
  ]


let bits_type typ = 
  (*if TO.is_bool_typ typ then
    O.BitsTy 1
    else *)
    let csize = 
      if TO.is_ptr_typ typ then
	Mach.sizeof_pointer
      else
	let typ = Tent_op.norm_typ_size typ (* temperariy hacking *)
	in TO.static_sizeof typ
    in
    let size_int = Csize.int_of_csize csize
    in O.BitsTy (size_int * 8)


let global_str_of_qname qname = 
  QNP.to_decl_str qname

let str_of_qname qname = 
  "@" ^ (QNP.to_decl_str qname)

    
let compile_c_identifier_as_kinded_name t id = 
  if (Qual_name_op.is_register id) or (Qual_name_op.is_fun id) then
    O.Name (mk_hint_opt t, str_of_qname id, None)
  else
    O.Mem (bits_type t, O.Fetch (O.Name (mk_hint_opt t, str_of_qname id, None)), None, [])


let compile_c_identifier id = 
  let ce_info = CEO.ce_info_of id
  in
  if (CEO.is_register id) or (CEO.is_fun id) then
    O.Name (None, str_of_qname ce_info.CE.ce_qname, None)
  else 
    O.Mem (bits_type ce_info.CE.ce_te, 
    O.Fetch (O.Name (None, str_of_qname ce_info.CE.ce_qname, None)), None, [])
    

let emit_type_section typ_db =
  let (*typ_array = Typ_to_typ_ast.convert_typ_db typ_db
	and *) data = ref []
  in
  (*
    let _ = 
    Array.iteri
    (fun i v -> 
    let sfm = Str_util.create_sfm 256
    in
    Typ_ast_printer.pp_print_t sfm.Str_util.formatter v;
    let str = Str_util.flush_sfm sfm
    in
    data := !data @ 
    [O.Datum (O.Label ("T" ^ string_of_int i));
    O.Datum (O.MemDecl (O.BitsTy 8, O.DynSize, Some (O.InitStr str)))]
    )
    typ_array
    in *)
  O.Section ("data", !data)


let need_to_export linkage =
  match linkage with
    | E.Auto 
    | E.Default_extern 
    | E.Default_storage -> true
    | E.Extern
    | E.Extern_Inline
    | E.Static
    | E.Static_Inline
    | E.Register
    | E.Inline
    | E.Type_alias -> false
    | E.Thread 
    | E.Extern_Thread 
    | E.Static_Thread ->
	camlp4_macro_exception "'__thread' storage is not supported yet"

type storage_code =
  | Import
  | Ignore
  | Decl
      
let get_storage_code linkage ce_info =
  if ce_info.CE.ce_is_real_ent then
    (* this compilation unit contains the definition *)
    if TO.is_crt_function_typ ce_info.CE.ce_te then
      (* it's defined somewhere, dont import the symbol *)
      Ignore
    else if TO.is_abs_function_typ ce_info.CE.ce_te then
      (* this can happen for function which void formal parameter and void
	 return *)
      Ignore
    else 
      match linkage with
	| E.Extern -> Ignore
	| E.Default_extern 
	| E.Default_storage 
	| E.Auto 
	| E.Extern_Inline
	| E.Static
	| E.Static_Inline
	| E.Register
	| E.Inline
	| E.Type_alias -> Decl
	| E.Thread 
	| E.Extern_Thread 
	| E.Static_Thread ->
	    camlp4_macro_exception "'__thread' storage is not supported yet"
  else
    let str = (Qual_name_printer.to_decl_str ce_info.CE.ce_qname)
    in
    try
      let _ = StringHashtbl.find importedString str
      in Ignore
    with
      | Not_found -> 
	  let _ = StringHashtbl.add importedString str true
	  in Import
    

type cmpstate =
  {
    env:Senv.env;
    eenv:Ast_eb_expr.expr_env;
  }


let bits8 = O.BitsTy 8
let intbits env = (Int64.to_int (TO.static_sizeof env.Senv.int_typ)) * 8


let mk_bitsty c_type = bits_type c_type
  
and mk_memsize c_type = 
  let csize = TO.static_sizeof c_type
  in O.FixSize (O.Uint ((Csize.string_of_csize csize), None))
       
and mk_int_memsize i = 
  O.FixSize (O.Uint (string_of_int i, None))


let rec mk_init_datum cmpstate init_storage = 
  let (str, mem_list) = init_storage.I.init
  in
  let (mem_decl_list, last_addr) = 
    List.fold_left
      (fun (l, curr_addr) mem_cell ->
	let (init_exprs, one_size_opt) = 
	  Array.fold_left 
	    (fun (l, one_size_opt) v -> 
	      let (v', bits) = match v with
		| I.I8Const string -> (O.Uint (string, Some (O.BitsTy 8)), O.BitsTy 8)
		| I.I8Space -> (O.Uint ("0", Some (O.BitsTy 8)), O.BitsTy 8) (*assert false*)
		| I.IXCexpr cexpr -> 
		    (compile_cexpr cmpstate cexpr, mk_bitsty (AEE.get_cexpr cmpstate.env
		      cmpstate.eenv cexpr(*.E.ce_typ*)))
	      in 
	      let one_size_opt = match one_size_opt with
		| Some size ->
		    if (size = bits) then Some size
		    else None
		| None -> Some bits
	      in
	      ((v', bits)::l, one_size_opt)
	    ) ([], None) mem_cell.I.cell
	in
	let init_exprs = List.rev init_exprs
	in
	let mem_init = 
	  match one_size_opt with
	    | Some bits ->
		begin
		  match bits with
		    | O.BitsTy n ->
			let elmt_size = (mem_cell.I.size * 8) / n
			and (init_exprs, _) = List.split init_exprs
			in 
			[O.MemDecl (bits, mk_int_memsize elmt_size, Some
			  (O.InitExprs  init_exprs))]
		    | _ -> assert false
		end
	    | None ->
		Safe_list.map 
		  (fun (v, bits) ->
		    O.MemDecl (bits, mk_int_memsize 1, Some (O.InitExprs [v]))
		  ) init_exprs
	in
	if (curr_addr = mem_cell.I.addr) then
	  (l @$ mem_init, curr_addr + mem_cell.I.size)
	else if curr_addr < mem_cell.I.addr then
	  (l @$ [O.MemDecl (bits8, mk_int_memsize (mem_cell.I.addr -
	    curr_addr), None)] @$ mem_init,
	  mem_cell.I.addr + mem_cell.I.size)
	else
	  assert false
      ) ([], 0) mem_list
  in mem_decl_list
       
and mk_memdecl c_type = 
  O.MemDecl (bits8, mk_memsize c_type, None)
    
and compile_c_local_declaration cmpstate (expr:I.c_local_declaration) : O.stmt = 
  match expr with
    | I.Local_obj_decl (linkage, ce) ->
	begin
	  let c_type = CEO.te_of ce
	  and string = CEO.qname_of ce
	  and ce_info = CEO.ce_info_of ce
	  in
	  let (e0, e1) = mk_type_span c_type
	  in
	  match get_storage_code linkage ce_info with
	    | Ignore -> 
		(* is defined somewhere after this *)
		O.SpanStmt (e0, e1, [])
	    | Import -> 
		O.SpanStmt
		  (e0, e1,
		  [O.DeclBody 
		    (O.Import (None, [(Some (global_str_of_qname string), 
		    str_of_qname string)]))])
	    | Decl ->
		O.SpanStmt
		  (e0, e1,
		  [O.DataBody 
		    [O.Label (str_of_qname string);mk_memdecl c_type]])
	end

    | I.Local_register ce ->
	let c_type = CEO.te_of ce
	and string = CEO.qname_of ce
	in
	let (e0, e1) = mk_type_span c_type
	in
	O.SpanStmt
	  (e0, e1,
	  [O.DeclBody (O.Registers 
	    [(O.Variant, None, mk_bitsty c_type, str_of_qname string,
	    None)])])
	  
    | I.Local_type_def c_type ->
	let (e0, e1) = mk_typedef_span c_type
	in O.SpanStmt (e0, e1, [])
	     
    | I.Local_type_decl c_type ->
	let (e0, e1) = mk_typedecl_span c_type
	in O.SpanStmt (e0, e1, [])
	     
    | I.Local_type_only c_type ->
	let (e0, e1) = mk_typeonly_span c_type
	in O.SpanStmt (e0, e1, [])
	     
    | I.Local_dat_decl_init init_storage ->
	let (e0, e1) = 
	  mk_type_span init_storage.I.orig_type
	in
	O.SpanStmt
	  (e0, e1,
	  [O.DataBody 
	    ((O.Label (str_of_qname init_storage.I.orig_id))::(mk_init_datum
	      cmpstate init_storage))])

and compile_c_declaration cmpstate (expr:I.c_declaration) :O.section =
  match expr with
    | I.Str_decl_init (linkage, ce, str_literal) ->
	(* We need to add the null termination character for cmm. *)
	let c_type = CEO.te_of ce
	and str = CEO.qname_of ce
	in
	let (e0, e1) = mk_type_span c_type
	in
	let null_terminated_string str = 
	  let c = Char.chr 0
	  in str ^ (String.escaped (String.make 1 c))
	in
	let init_opt = 
	  (*let str = 
	    camlp4_macro_str_pp_print
	    (fun fm -> 
	    print_c_string_literal fm str_literal)
	    in Some (O.InitStr (null_terminated_string str)) *)
	  match str_literal with
	    | C_syntax_symbol.String_literal s ->
		let str = (C_str.escape_string s)
		in Some (O.InitStr (null_terminated_string str))
	    | C_syntax_symbol.WString_literal int64_list ->
		assert false
	in
	O.SSpan 
	  (e0, e1, 
	  [
	    O.Datum (O.Label (str_of_qname str));
	    O.Datum (O.MemDecl (bits8, mk_memsize c_type, init_opt))
	  ])
	  
    | I.Obj_decl (linkage, ce) ->
	begin
	  let c_type = CEO.te_of ce
	  and string = CEO.qname_of ce
	  and ce_info = CEO.ce_info_of ce
	  in
	  let (e0, e1) = mk_type_span c_type
	  in
	  match get_storage_code linkage ce_info with
	    | Ignore -> 
		O.SSpan (e0, e1, [])
	    | Import -> 
		O.Decl 
		  (O.Import (None, [(Some (global_str_of_qname string), str_of_qname string)]))
	    | Decl ->
		O.SSpan 
		  (e0, e1, 
		  [O.Datum (O.Label (str_of_qname string));
		  O.Datum (mk_memdecl c_type)])
	end
	  
    | I.Type_def c_type ->
	let (e0, e1) = mk_typedef_span c_type
	in O.SSpan (e0, e1, [])
	     
    | I.Type_decl c_type ->
	let (e0, e1) = mk_typedecl_span c_type
	in O.SSpan (e0, e1, [])
	     
    | I.Type_only c_type ->
	let (e0, e1) = mk_typeonly_span c_type
	in O.SSpan (e0, e1, [])
	     
    | I.Dat_decl_init init_storage ->
	let (e0, e1) = mk_type_span init_storage.I.orig_type
	in 
	let init_memdecl = 
	  List.map (fun v -> O.Datum v) (mk_init_datum cmpstate init_storage)
	in
	O.SSpan 
	  (e0, e1, 
	  (O.Datum ((O.Label (str_of_qname init_storage.I.orig_id)))::init_memdecl))
	  (*
	    O.Datum (mk_memdecl init_storage.I.orig_type 
	    (Some init_storage.I.init))])*)
	  
and compile_binary_arithmatic op = 
  "bin_op"
    
and compile_binary_predicate op = 
  "bin_pred"

and compile_binary_logic_connect op = 
  "bin_logic"

and compile_unary_arithmatic op = 
  "una_op"

and to_unsigned e =
  let (cv, str, suffix_opt) = e
  in
  let cv' = 
    match cv with
      | CA.CCHAR _ -> CA.cval_CCHAR_to_CUCHAR cv
      | CA.CSCHAR _ -> CA.cval_CSCHAR_to_CUCHAR cv
      | CA.CUCHAR _ -> cv
      | CA.CSHORT _ -> CA.cval_CSHORT_to_CUSHORT cv
      | CA.CUSHORT _ -> cv
      | CA.CINT v -> CA.cval_CINT_to_CUINT cv
      | CA.CUINT _ -> cv
      | CA.CLONG _ -> CA.cval_CLONG_to_CULONG cv
      | CA.CULONG _ -> cv
      | CA.CLLONG _ -> CA.cval_CLLONG_to_CULLONG cv
      | CA.CULLONG _ -> cv
      | CA.CBOOL _ -> cv
      | _ -> assert false
  in CA.cval_ext_of_cval cv'

and compile_const_int (expr:E.cexpr) :int64 = 
  match expr with
    | E.Cconst c_const040 ->
	let (c_type, cval_ext) = c_const040
	in
	let (cv, s, suffix_opt) = to_unsigned cval_ext
	in
	let s = Str_util.trim_llu_suffix s
	in Int64.of_string s
    | _ -> assert false

and compile_cexpr cmpstate (expr:E.cexpr) :O.expr = 
  let t = AEE.get_cexpr cmpstate.env cmpstate.eenv expr (*.E.ce_typ*)
  in
  match expr with
    | E.Cconst c_const040 ->
	compile_c_const040 c_const040
	  
    | E.Csizeof (c_type, c_const040) ->
	compile_c_const040 c_const040
	  
    | E.Calignof (c_type, c_const040) ->
	compile_c_const040 c_const040
	  
    | E.Cvct ce -> 
	let qname = CEO.qname_of ce
	in O.Fetch (O.Name (None, str_of_qname qname, None))
	     
    | E.Cvar_lbl ce -> 
	let qname = CEO.qname_of ce
	in O.Fetch (O.Name (None, str_of_qname qname, None))
	     
    | E.Ccode_lbl str ->
	O.Fetch (O.Name (None, str, None))
	  
    | E.Cfun_lbl ce ->
	let qname = CEO.qname_of ce
	in O.Fetch (O.Name (None, str_of_qname qname, None))
	     
    | E.CCast (c_type, expr) -> 
	begin
	  let dest_size = TO.static_sizeof c_type
	  and src_size = TO.static_sizeof 
	    (AEE.get_cexpr cmpstate.env cmpstate.eenv expr) (*.E.ce_typ*)
	  in
	  if (dest_size = src_size) then
	    compile_cexpr cmpstate expr
	  else
	    assert false
	end

    | E.CQuestion (cond, expr0, expr1) -> assert false
	
    | _ -> 
	let rexpr_ = EO.cexpr__to_rexpr_ expr (*.E.ce_val*)
	in compile_rexpr cmpstate  rexpr_ (*{ E.re_typ = expr.E.ce_typ; E.re_val = rexpr_; }*)

and compile_expr cmpstate (expr:E.expr) :O.stmt = 
  match expr with
    | E.Rexpr expr -> assert false
	(* compile_rexpr expr *)
	
    | E.Macro_va_start (expr0, expr1) ->
	O.CallStmt 
	  ([], Some "C", 
	  O.Fetch (O.Name (None, "__builtin_va_start", None)), 
	  [(None, compile_rval cmpstate expr0,None);
	  (None, compile_rval cmpstate expr1, None)], [], [])
	  
    | E.Macro_va_end expr ->
	O.CallStmt 
	  ([], Some "C", 
	  O.Fetch (O.Name (None,
	  "__builtin_va_end", None)), 
	  [(None, compile_rval cmpstate expr,None);], [], [])
	  
    | E.Macro_va_arg (lval, expr, c_type) ->
	O.CallStmt
	  ([compile_lval cmpstate lval], Some "C",
	  O.Fetch (O.Name (None, "__builtin_va_arg", None)), 
	  [(None, compile_rval cmpstate expr,None);], [], [])
	  (* compile_c_type fm c_type *)
	  
    | E.Assign (lval, expr1) ->
	O.AssignStmt 
	  ([compile_lval cmpstate lval], [(None, compile_rexpr cmpstate expr1)])
	  
    | E.Memcpy (expr0, expr1, csize) ->
	O.CallStmt
	  ([], Some "C",
	  O.Fetch (O.Name (None, "@memcpy", None)),
	  [(None, compile_rval cmpstate expr0, None);
	  (None, compile_rval cmpstate expr1, None);
	  (None, compile_cexpr cmpstate csize, None)],
	  [], [])

    | E.Alloca (lval, te, rexpr) -> 
	assert false
	  
and compile_fbin_op fbin_op = 
  match fbin_op with
    | E.F_ADD -> "fadd"
    | E.F_SUB -> "fsub"
    | E.F_DIV -> "fdiv"
    | E.F_MUL -> "fmul"

and compile_logic_op op = 
  match op with
    | E.AND -> "conjoin"
    | E.OR -> "disjoin"

and string_of_frel = function
  | E.F_EQ -> "feq"
  | E.F_NE -> "fne"
  | E.F_GT -> "fgt"
  | E.F_GE -> "fge"
  | E.F_LT -> "flt"
  | E.F_LE -> "fle"
      
and string_of_bin_op = function
  | E.ADD -> "add"
  | E.SUB -> "sub"
  | E.MUL -> "mul"
  | E.MULU -> "mulu"
  | E.DIV -> "div"
  | E.DIVU  -> "divu"
  | E.MOD -> "mod"
  | E.MODU -> "modu"
  | E.BAND -> "and"
  | E.BOR -> "or"
  | E.BXOR -> "xor"
  | E.SHL -> "shl"
  | E.SHR -> "shra"
  | E.SHRU -> "shrl"
      
and string_of_rel = function
  | E.EQ -> "eq"
  | E.NE -> "ne"
  | E.GT -> "gt"
  | E.GE -> "ge"
  | E.LT -> "lt"
  | E.LE -> "le"
  | E.GTU -> "gtu"
  | E.GEU -> "geu"
  | E.LTU -> "ltu"
  | E.LEU -> "leu"
      
and compile_rexpr cmpstate (rexpr:E.rexpr) :O.expr = 
  let t = AEE.get_rexpr cmpstate.env cmpstate.eenv rexpr (*.E.re_typ*)
  in
  match rexpr (*.E.re_val*) with
    | E.Rval_cast (t, rval) -> compile_rval cmpstate rval

    | E.Rval rval -> compile_rval cmpstate rval
	
    | E.Logic (bop, expr0, expr1) ->
	O.PrimOp
	  (compile_logic_op bop, 
	  [(None, compile_rval cmpstate expr0, None);
	  (None, compile_rval cmpstate expr1, None)])
	  
    | E.Logic_not expr ->
	let typ = AEE.get_rval cmpstate.env cmpstate.eenv expr (*.E.rv_typ*)
	in
	let csize = 
	  if TO.is_ptr_typ typ then
	    Mach.sizeof_pointer
	  else
	    TO.static_sizeof typ
	in
	let size_int = Csize.int_of_csize csize
	in
	let (opstr, zero) =
	  if TO.is_integer_typ typ then
	    let ibits = 
	      match size_int with
		| 1 -> E.I8
		| 4 -> E.I32
		| 8 -> E.I64
		| _ ->
		    assert false
	    in (string_of_rel E.EQ, O.Sint ("0", Some (mk_bitsty typ)))
	  else
	    let fbits = 
	      match size_int with
		| 4 -> E.F32
		| 8 -> E.F64
		| _ ->
		    assert false
	    in (string_of_frel E.F_EQ, O.Float ("0.0", Some (mk_bitsty typ)))
	in
	O.PrimOp
	  ("zx32",
	  [(None, 
	  O.PrimOp
	    ("bit",
	    [(None, 
	    O.PrimOp
	      (opstr,
	      [(None, compile_rval cmpstate expr, None);(None, zero, None)]), None)]), None)])
	  (*
	    O.PrimOp ("not", [(None, compile_rval expr, None)])
	  *)
	  
    | E.Fneg (fbits, rexpr) -> 
	O.PrimOp ("fneg", [(None, compile_rval cmpstate rexpr, None)])
	  
    | E.Fbin (fbin_op, fbits, e0, e1) -> 
	O.PrimOp (compile_fbin_op fbin_op, 
	[(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate e1, None);
	round_down])
	  
    (* integer *)
    | E.Ineg (ibits, rexpr) -> 
	O.PrimOp("neg", [(None, compile_rval cmpstate rexpr, None)])

    | E.Ibnot (ibits, rexpr) -> 
	O.PrimOp("com",
	[(None, compile_rval cmpstate rexpr, None)])
	  
    | E.Ibin (ibin_op, ibits, e0, e1) ->
	let opstr = string_of_bin_op ibin_op
	in
	O.PrimOp
	  (opstr,
	  [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate e1, None)])
	  
    (* floating point relation expressions *)
    | E.Frel (rel, fbits, e0, e1) ->
	let opstr = string_of_frel rel
	in
	let rel = 
	  O.PrimOp
	    (opstr,
	    [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate
	      e1, None)])
	in 
	let bits1 = O.PrimOp ("bit", [(None, rel, None)])
	in O.PrimOp ("zx" ^ (string_of_int (intbits cmpstate.env)), [(None, bits1, None)])
	     
    | E.Irel (rel, ibits, e0, e1) ->
	let opstr = string_of_rel rel
	in
	let rel = 
	  O.PrimOp
	    (opstr, 
	    [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate
	      e1, None)])
	in 
	let bits1 = O.PrimOp ("bit", [(None, rel, None)]) 
	in O.PrimOp ("zx" ^ (string_of_int (intbits cmpstate.env)), [(None, bits1, None)])
	     
    | E.PTR_TO_PTR (c_type, expr) ->
	compile_rval cmpstate expr
	  
    | E.PTR_TO_UI (expr) ->
	compile_rval cmpstate expr
	  
    (* floating point types to integer types *)
    | E.F_TO_SI (rexpr, f, i) -> 
	let istr = string_of_ibits i
	in
	O.PrimOp
	  (("f2i" ^ istr),
	  [(None, compile_rval cmpstate rexpr, None);round_down])
	  
    | E.F_TO_UI (rexpr, f, i) -> 
	let istr = string_of_ubits i
	in
	O.PrimOp
	  (("f2i" ^ istr),
	  [(None, compile_rval cmpstate rexpr, None);round_down])

    | E.B_TO_SI (rexpr, f, i) -> 
	let istr = string_of_ubits i
	in
	let bit = O.PrimOp
	  ("bit",[(None, compile_rval cmpstate rexpr, None)])
	in
	O.PrimOp (("sx" ^ istr), [(None, compile_rval cmpstate rexpr, None)])
	  
    | E.B_TO_UI (rexpr, f, i) -> 
	let istr = string_of_ubits i
	in
	let bit = O.PrimOp
	  ("bit",[(None, compile_rval cmpstate rexpr, None)])
	in O.PrimOp (("zx" ^ istr), [(None, compile_rval cmpstate rexpr, None)])
	     
    (* integer types to floating point types *)
    | E.SI_TO_F (rexpr, i, f) -> 
	let fstr = string_of_fbits f
	in
	O.PrimOp
	  (("i2f" ^ fstr),
	  [(None, compile_rval cmpstate rexpr, None);round_down])
	  
    | E.UI_TO_F (rexpr, i, f) -> 
	let fstr = string_of_fbits f
	in
	O.PrimOp
	  (("i2f" ^ fstr),
	  [(None, compile_rval cmpstate rexpr, None);round_down])
	  
    (* floating point types to floating point types *)
    | E.F_TO_F (rexpr, f0, f1) -> 
	let fstr1 = string_of_fbits f1
	in
	O.PrimOp
	  (("f2f" ^ fstr1),
	  [(None, compile_rval cmpstate rexpr, None);round_down])
	  
    (* sign extension *)
    | E.SXI (rexpr, srcbits, destbits) -> 
	let fstr0 = string_of_ibits srcbits
	and fstr1 = string_of_ibits destbits
	in
	O.PrimOp
	  (("sx" ^ fstr1), [(None, compile_rval cmpstate rexpr, None)])
	  
    | E.ZXI (rexpr, srcbits, destbits) -> 
	let fstr0 = string_of_ibits srcbits
	and fstr1 = string_of_ibits destbits
	in
	O.PrimOp
	  (("zx" ^ fstr1), [(None, compile_rval cmpstate rexpr, None)])
	  
    | E.LOBITS (rexpr, srcbits, destbits) -> 
	let fstr0 = string_of_ibits srcbits
	and fstr1 = string_of_ibits destbits
	in
	O.PrimOp
	  (("lobits" ^ fstr1),
	  [(None, compile_rval cmpstate rexpr, None)])

and compile_int i = 
  O.Uint (string_of_int i, None)
    
and compile_c_const040: E.c_const040 -> O.expr =
  fun (c_type, (c,s, suffix_opt)) ->
    let s = Str_util.trim_ll_suffix s
    in
    let s = Str_util.trim_plus_sign s
    in
    if TO.is_floating_typ c_type then
      O.Float (s, Some (mk_bitsty c_type))
    else if TO.is_unsigned_integer_typ c_type then
      O.Uint (s, Some (mk_bitsty c_type))
    else 
      O.Sint (s, Some (mk_bitsty c_type))

	
and compile_lval cmpstate (lval:E.lval) : O.name_or_mem = 
  let t = AEE.get_lval cmpstate.env cmpstate.eenv lval (*.E.lv_typ*)
  in
  match lval (*.E.lv_val*) with
      (*
    | E.Lvct str -> assert false
	
    | E.Ldir str -> 
	compile_c_identifier str
	  
    | E.Lindir str -> 
	O.Mem (bits_type t, O.Fetch (compile_c_identifier str), None, [])
      *)
    | E.Lreg str -> assert false
    | E.Lladdr (E.Nlbl ce) -> compile_c_identifier ce
    | E.Lladdr (E.Nctn ce) -> 
	O.Mem (bits_type t, O.Fetch (compile_c_identifier ce), None, [])
	  
and compile_rval cmpstate (rval:E.rval) :O.expr = 
  let t = AEE.get_rval cmpstate.env cmpstate.eenv rval (*.E.rv_typ*)
  in
  match rval with
    | E.Rsizeof (typ, c_const040) ->
	compile_c_const040 c_const040
	  
    | E.Ralignof (typ, c_const040) ->
	compile_c_const040 c_const040
	  
    | E.Rconst c_const040 -> 
	compile_c_const040 c_const040
	  
    | E.Rreg str -> 
	O.Fetch (compile_c_identifier str)
	  
    | E.Rindir str ->
	O.Fetch (O.Mem (bits_type t, O.Fetch (compile_c_identifier str), None, []))
	  
    | E.Rfun str -> 
	let str = CEO.qname_of str
	in O.Fetch (O.Name (None, str_of_qname str, None))
	     
    | E.Rladdr (E.Nlbl str) ->
	let str = CEO.qname_of str
	in O.Fetch (O.Name (None, str_of_qname str, None))

    | E.Rladdr (E.Nctn str) -> 
	O.Fetch (compile_c_identifier str)

    | E.Rvct ce -> 
	let qname = CEO.qname_of ce
	in O.Fetch (O.Name (None, str_of_qname qname, None))
	     
    | E.Rcode_label str ->
	O.Fetch (O.Name (None, str, None))
	  
    | E.Rvoid -> 
	assert false

    | E.Rbyte_value str ->
	O.Char (int_of_string str, None)

    | E.Rcexpr expr ->
	compile_cexpr cmpstate expr

and compile_asm cmpstate asm = 
  {
    O.asm_outputs = 
      List.map (fun (s_opt, s, lval) -> (s_opt, s,
      compile_lval cmpstate lval)) asm.E.asm_outputs;
    O.asm_inputs = 
      List.map (fun (s_opt, s, expr) -> (s_opt, s, 
      compile_rval cmpstate expr)) asm.E.asm_inputs;
    asm_clobbers = asm.E.asm_clobbers;
  }
    
and compile_true_cond cmpstate cond = 
  match cond with
    | E.EQ_ZERO c_val 
    | E.NEQ_ZERO c_val ->
	let typ = AEE.get_rval cmpstate.env cmpstate.eenv c_val (*.E.rv_typ*)
	in
	let iop, fop = match cond with
	  | E.EQ_ZERO _ -> E.EQ, E.F_EQ
	  | E.NEQ_ZERO _ -> E.NE, E.F_NE
	  | _ -> assert false
	in
	let csize = 
	  if TO.is_ptr_typ typ then
	    Mach.sizeof_pointer
	  else
	    TO.static_sizeof typ
	in
	let size_int = Csize.int_of_csize csize
	in
	let (opstr, zero) =
	  if TO.is_integer_typ typ then
	    let ibits = 
	      match size_int with
		| 1 -> E.I8
		| 4 -> E.I32
		| 8 -> E.I64
		| _ ->
		    assert false
	    in (string_of_rel iop, O.Sint ("0", Some (mk_bitsty typ)))
	  else
	    let fbits = 
	      match size_int with
		| 4 -> E.F32
		| 8 -> E.F64
		| _ ->
		    assert false
	    in (string_of_frel fop, O.Float ("0.0", Some (mk_bitsty typ)))
	in
	O.PrimOp
	  (opstr,
	  [(None, compile_rval cmpstate c_val, None);(None, zero, None)])
	  
    | E.FPRED (rel, fbits, e0, e1) ->
	let opstr = string_of_frel rel
	in
	O.PrimOp
	  (opstr,
	  [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate e1, None)])
	  
    | E.FNOTPRED (rel, fbits, e0, e1) ->
	let opstr = string_of_frel rel
	in
	let rel = 
	  O.PrimOp
	    (opstr,
	    [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate
	      e1, None)])
	in O.PrimOp ("not", [(None, rel, None)])
	     
    | E.IPRED (rel, ibits, e0, e1) ->
	let opstr = string_of_rel rel
	in
	O.PrimOp
	  (opstr, 
	  [(None, compile_rval cmpstate e0, None);(None, compile_rval cmpstate e1, None)])
	  
and compile_c_code100 cmpstate (jmp_tbl:I.jmp_table) (i:int)
    (code100:I.c_code100) :O.stmt list = 
  let compile_flow_ctrl cmpstate jmp_tbl expr = 
    match expr with
      | G.Jmp i ->
	  O.GotoStmt (mk_label_expr i, [])
	    
      | G.Jmp_join i ->
	  O.GotoStmt (mk_label_expr i, [])
	    
      | G.Jmp_join_backward i ->
	  O.GotoStmt (mk_label_expr i, [])
	    
      | G.Cmp_jmp (c_val, t0) ->
	  O.IfStmt 
	    (compile_true_cond cmpstate c_val,
	    [O.StmtBody(O.GotoStmt (mk_label_expr t0, []))], [])
	    
      | G.Tbl_jmp (expr, i) -> 
	  begin
	    (* In C--, switch ranges are always unsigned, see Section 6.5 *)
	    let expr_E_rv_typ = AEE.get_rval cmpstate.env cmpstate.eenv expr
	    in
	    let list = Array.get jmp_tbl i
	    and range_list = ref []
	    and (_,_,_,_,_,max,min) = 
	      Mach.get_type_matrix (snd (TO.flip_to_unsigned_te expr_E_rv_typ))
	    in
	    let int_const v = 
	      let c_type = expr_E_rv_typ
	      in O.Uint ((Int64.to_string v) ^ "U", Some (mk_bitsty c_type))
	    in
	    let l = List.map
	      (fun (case_opt, t) ->
		match case_opt with
		  | Some (v0, v1) ->
		      let (i_start, i_end) = 
			(compile_const_int v0, compile_const_int v1)
		      in			
		      let _ = range_list := (i_start, i_end)::!range_list
		      in
		      let flip = i_start > i_end
		      in
		      (*if (i_start <0L) & (0L < i_end) then
			camlp4_macro_exception 
			"the range %s..%s cross zero, you have to fix the compiler now\n"
			(Int64.to_string i_start) (Int64.to_string i_end);*)
		      if v0 == v1 then
			O.Case 
			  ([O.Point 
			    (compile_cexpr cmpstate v0)], 
			  [O.StmtBody(O.GotoStmt (mk_label_expr t, []))])
		      else if not flip then
			O.Case
			  ([O.Range
			    (compile_cexpr cmpstate v0, compile_cexpr cmpstate v1)],
			  [O.StmtBody(O.GotoStmt (mk_label_expr t, []))])
		      else
			O.Case
			  ([O.Range
			    (compile_cexpr cmpstate v1, compile_cexpr cmpstate v0)],
			  [O.StmtBody(O.GotoStmt (mk_label_expr t, []))])
		  | None ->
		      let unused_list = Sort_ranges.unused_range_list 
			!range_list ~min ~max
		      in			
		      let ranges = 
			List.map 
			  (fun (v0, v1) -> 
			    if v0 == v1 then O.Point (int_const v0)
			    else O.Range (int_const v0, int_const v1)
			  ) unused_list
		      in
		      O.Case
			(ranges,
			[O.StmtBody(O.GotoStmt (mk_label_expr t, []))])
	      ) list
	    in
	    O.SwitchStmt (Some (O.Range (int_const min, int_const max)),
	    compile_rval cmpstate expr, l)
	  end
	    
  and compile_sese_code cmpstate stmt = 
    match stmt with
      | G.Computing stmts ->
	  List.map (compile_expr cmpstate) stmts
	    
      | G.Asm (txtlst, asm_details_opt) ->
	  [O.AsmStmt (txtlst, 
	  Mapping.map_opt (compile_asm cmpstate) asm_details_opt)]
	    
      | G.Pop_args lst -> []

  and compile_scope_ctrl cmpstate (i:int) (expr:I.scope_ctrl) :O.stmt list = 
    match expr with
      | I.Begin_fun (decls, j) ->
	  List.map (compile_c_local_declaration cmpstate) decls
	    
      | I.End_fun -> []
	  
      | I.Begin_decl (c_declaration_list, j) ->
	  List.map (compile_c_local_declaration cmpstate) c_declaration_list
	    
      | I.End_decl -> []

  and compile_call_transfer cmpstate ((lval_opt, expr, expr_list):(E.lval option * E.rval * E.rval list)):
      O.stmt list = 
    let ls = match lval_opt with
      | Some lval ->
	  [compile_lval cmpstate lval]
      | None -> 
	  []
    in
    let actual_list = 
      List.map (fun v -> (None, v, None))
	(List.map (compile_rval cmpstate) expr_list)
    in
    [O.CallStmt (ls, Some "C", compile_rval cmpstate expr, actual_list, [], [])]
  in
  let compile_c_code100 cmpstate (jmp_tbl:I.jmp_table) (i:int)
      (expr:I.c_code100) :O.stmt list = 
    match expr with
      | I.Scope expr -> compile_scope_ctrl cmpstate i expr
      | I.Call expr -> compile_call_transfer cmpstate expr
      | I.Flow expr -> [compile_flow_ctrl cmpstate jmp_tbl expr]
      | I.Sese expr -> 
	  let l = List.map
	    (fun c -> compile_sese_code cmpstate c)
	    expr
	  in List.flatten l
	       
      | I.Join -> [O.EmptyStmt]
      | I.Epi str_opt ->
	  begin
	    match str_opt with
	      | Some str ->
		  let mem = compile_rval cmpstate str
		  in
		  [O.ReturnStmt (Some "C", None, [(None, compile_rval cmpstate str, None)])]
	      | None ->
		  [O.ReturnStmt (Some "C", None, [])]
	  end
      | I.Nop -> []
  in compile_c_code100 cmpstate jmp_tbl i code100
       
and compile_label (t:Code_label.t) (nid:int) :O.stmt list = 
  let num = Code_label.get_num_string nid
  in
  match t with
    | Code_label.TARGET -> [O.LabelStmt (!lab_prefix ^ num)]
    | Code_label.JOIN n -> [O.LabelStmt (!lab_prefix ^ num)]
    | Code_label.SEQ n -> []
	
and extract_formal_list t = 
  let formal_params = TO.formal_params_of t
  in
  List.map (fun (ty, qname) -> 
    let qname = match qname with
      | T.THIS_PARAM p 
      | T.NORMAL_PARAM p
      | T.HIDDEN_RETURN p -> p
      | T.SCALAR_PARAM qmuton 
      | T.STRUCT_PARAM qmuton -> qmuton.T.muton
    in
    ((0,0), (mk_hint_opt ty, O.Variant, bits_type ty, 
    str_of_qname qname, None)))
    formal_params
    
and compile_c_function_definition cmpstate (expr:I.c_function_definition) 
    :O.section list = 
  let export_decl = if need_to_export expr.I.linkage then
    [O.Decl (O.Export (None, [(str_of_qname expr.I.name, 
    Some (global_str_of_qname expr.I.name))]))]
  else
    []
  and _ = lab_prefix := str_of_qname expr.I.name
  in
  (*
    let static_decls = 
    List.map compile_c_declaration expr.I.static_decls
    in*)
  let stmts = ref []
  in
  let _ = Array.iteri
    (fun i c_node ->
      let _ = stmts := !stmts @ (compile_label c_node.I.label_att i)
      in 
      stmts := !stmts @ 
	(compile_c_code100 cmpstate expr.I.jmp_tbl i c_node.I.code)
    ) expr.I.code_array
  in
  let (e0, e1) = mk_type_span expr.I.c_type
  in
  let formal_list = extract_formal_list expr.I.c_type
  in
  let body_list = List.map (fun v -> O.StmtBody v) !stmts
  in
  export_decl @ 
    [O.SSpan (e0, e1, 
    [O.Procedure (Some "C", str_of_qname expr.I.name, formal_list,
    body_list, (0,0))])]
    
and compile_coord (str, x, y) = (x, y)

and mk_target () =
  let l = ref 
    [O.FloatRepr "ieee754";
    O.Memsize Mach.char_bit;
    O.WordSize (Mach.char_bit * 4); 
    O.PointerSize (Mach.char_bit * (Int64.to_int Mach.sizeof_pointer))]
  in
  let _ = if Mach.little_endian then
    l := O.ByteorderLittle::!l
  else
    l := O.ByteorderBig::!l
  in
  [
    O.TopDecl (O.Target !l);
    O.TopDecl 
      (O.Registers 
	([(O.Variant, None, (O.BitsTy 2), 
	"System.rounding_mode", 
	Some ("IEEE 754 rounding mode"))]))
  ]
    
and compile_c_external_declaration cmpstate
    (expr:I.c_external_declaration) :O.toplevel =
  match expr with
    | I.External_declaration_at (coord, expr) ->
	O.ToplevelAt 
	  (compile_c_external_declaration cmpstate expr, compile_coord coord)
    | I.External_declaration_1 c_function_definition ->
	O.Section ("text", compile_c_function_definition cmpstate c_function_definition)
    | I.External_declaration_2 c_declarations ->
	O.Section ("data", List.map (compile_c_declaration cmpstate) c_declarations)

and compile ((env, c_translation_unit) : (Senv.env * I.c_translation_unit))
    :O.program = 
  let l = 
    match c_translation_unit with
      | I.Translation_unit (l, eenv) ->
	  let cmpstate = 
	    {
	      eenv = eenv;
	      env = env;
	    }
	  in
	  List.map (compile_c_external_declaration cmpstate) l
  in
  let targets = mk_target ()
  and span_consts = mk_span_consts ()
  and ts = emit_type_section env
  in targets @ span_consts @ (ts::l)
	 
