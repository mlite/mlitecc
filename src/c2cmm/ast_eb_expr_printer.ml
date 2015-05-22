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
open Ast_eb_expr

module QNP = Qual_name_printer

type c_file_unit = Ast_eb_expr.c_translation_unit
let description () = description
let suffix () = suffix

let indent = 2
let enable_type_annotation = false
let explicit_type_coercion = false
  

let pp_print_c_comma: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ","; 
    pp_print_space fm ()

let pp_print_c_semicolon: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ";"; 
    pp_print_space fm ()
      
let pp_print_c_newline: formatter -> unit = 
  fun fm ->
    pp_print_space fm ()
      

let pp_print_c_identifier (fm:formatter) (qual_name:c_identifier):unit =
  pp_open_box fm 0;
  Cent_printer.pp_print_ref_ce fm qual_name;
  pp_close_box fm ()

let pp_print_c_identifier_opt fm id =
  match id with
  | Some c_identifier -> 
      pp_print_space fm ();
      pp_print_c_identifier fm c_identifier
  | None -> ()

let pp_print_l_paren fm =
  pp_print_string fm "(";
  pp_print_cut fm ()

let pp_print_r_paren fm =
  pp_print_cut fm ();
  pp_print_string fm ")"



let pp_print_c_string_literal = Ast_aa_gram_printer.pp_print_c_string_literal

let pp_print_binary_arithmatic = C_semantics_symbol_printer.pp_print_binary_arithmatic

let pp_print_binary_predicate = C_semantics_symbol_printer.pp_print_binary_predicate

let pp_print_binary_logic_connect = C_semantics_symbol_printer.pp_print_binary_logic_connect

let pp_print_unary_arithmatic = C_semantics_symbol_printer.pp_print_unary_arithmatic

let pp_print_linkage: formatter -> linkage -> unit = 
  fun fm op ->
    pp_open_box fm 0;
    let str = match op with
      | Default_extern -> ""
      | Default_storage -> ""
      | Extern -> "extern"
      | Extern_Inline -> "extern __inline"
      | Static_Inline -> "static __inline"
      | Static -> "static"
      | Auto -> "auto"
      | Register -> "register" (** todo **)
      | Inline -> "__inline"
      | Type_alias -> "typedef"
      | Thread -> "__thread"
      | Extern_Thread -> "extern __thread" 
      | Static_Thread -> "static __thread"
    in
    if str <> "" then
      begin
	pp_print_string fm str;
	pp_print_space fm ();
      end;
    pp_close_box fm ()

let string_of_fbits = function
  | F32 -> "bits32"
  | F64 -> "bits64"
  | F80 -> "bits80"

let string_of_ibits = function
  | I8 -> "bits8"
  | I16 -> "bits16"
  | I32 -> "bits32"
  | I64 -> "bits64"

let string_of_ubits = function
  | I8 -> "bits8"
  | I16 -> "bits16"
  | I32 -> "bits32"
  | I64 -> "bits64"

let string_of_fbinop = function
  | F_ADD -> "fadd"
  | F_SUB -> "fsub"
  | F_DIV -> "fdiv"
  | F_MUL -> "fmul"

let string_of_frel = function
  | F_EQ -> "feq"
  | F_NE -> "fne"
  | F_GT -> "fgt"
  | F_GE -> "fge"
  | F_LT -> "flt"
  | F_LE -> "fle"
      

let string_of_bin_op = function
  | ADD -> "add"
  | SUB -> "sub"
  | MUL -> "mul"
  | MULU -> "mulu"
  | DIV -> "div"
  | DIVU  -> "divu"
  | MOD -> "mod"
  | MODU -> "modu"
  | BAND -> "band"
  | BOR -> "bor"
  | BXOR -> "bxor"
  | SHL -> "shl"
  | SHR -> "shr"
  | SHRU -> "shru"
      
and string_of_rel = function
  | EQ -> "eq"
  | NE -> "ne"
  | GT -> "gt"
  | GE -> "ge"
  | LT -> "lt"
  | LE -> "le"
  | GTU -> "gtu"
  | GEU -> "geu"
  | LTU -> "ltu"
  | LEU -> "leu"

and string_of_logic = function
  | AND -> "and"
  | OR -> "or"


(** *********************************************************************************** **)

type c_construct =
  | CONST of c_const040 * bool
  | EXPR of expr * bool
  | REXPR of rexpr * bool
  | CEXPR of cexpr * bool
  | CONST_EXPR of c_constant_expression
  | DECL of c_declaration
  | LOCAL_DECL of c_local_declaration
  | TYPE of (string option * c_type)
  | STMT of c_stmt010
  | COMPOUND_STMT of c_compound_stmt010
  | ASM_STMT of string list * asm_details option
  | CALL_EXPR of (lval option * rval * rval list)
  | RVAL of rval
  | TRUE_COND of true_cond



      
let pp_print_c_construct: formatter -> c_construct -> unit = 
  fun fm c_construct ->
    let rec pp_print_c_init_expression: formatter -> c_init_expression -> unit =
      fun fm expr ->
	match expr with
	  | Static_init e -> 
	      pp_print_c_constant_expression fm e
		
	  | Static_init_none ->
	      (** 6.7.8 If there are fewer initializers in a brace-enclosed list
		  ..., the remainder of the aggregate shall be initialized 
		  implicitly the same as objects that have static storage 
		  duration. gcc.c-torture/execute/20050613-1.c test this 
	      **)
	      pp_print_int fm 0

    and pp_print_laddr (fm:formatter) laddr =
      match laddr with
	| Nlbl ce ->
	    pp_print_string fm "(";
	    pp_print_string fm "&";
	    pp_print_c_identifier fm ce;
	    pp_print_string fm ")"
	      
	| Nctn ce ->
	    pp_print_c_identifier fm ce	  	  

    and c_init_expression_has_values = function
      | Static_init_none -> false
      | Static_init _ -> true
	  
    and pp_print_c_initializer: formatter -> c_initializer -> unit = 
      fun fm expr ->
	Typ_mem_printer.pp_print_t fm 
	  pp_print_c_init_expression 
	  c_init_expression_has_values
	  expr
	  
    and pp_print_c_const040: formatter -> 
    need_paren:bool -> c_const040 -> unit =
      fun fm ~need_paren (c_type, (c,s, suffix_opt)) ->
	pp_open_box fm 0;
	begin
	  if need_paren then pp_print_string fm "(";
	  pp_print_string fm "(";
	  pp_print_c_type fm c_type;
	  pp_print_string fm ")";
	  pp_print_string fm s;
	  if need_paren then pp_print_string fm ")";
	end;
	pp_close_box fm ()
	  
    and pp_print_rval: formatter -> rval -> unit =
      fun fm expr ->
	pp_open_hvbox fm indent;
	pp_print_rval_ fm expr;
	pp_close_box fm ()

    and pp_print_rval_: formatter -> rval_ -> unit =
      fun fm expr ->
	pp_open_hvbox fm indent;
	begin
	  match expr with
	    | Rladdr laddr ->
		pp_print_laddr fm laddr
		  
	    | Rreg str -> 
		pp_print_c_identifier fm str
		  
	    | Rsizeof (typ, c_const040) ->
		pp_print_c_const040 fm ~need_paren:true c_const040

	    | Ralignof (typ, c_const040) ->
		pp_print_c_const040 fm ~need_paren:true c_const040
		  
	    | Rconst c_const040 -> 
		pp_print_c_const040 fm ~need_paren:true c_const040
		  
	    (*| Rdir str -> 
		pp_print_c_identifier fm str*)
		  
	    | Rindir str ->
		pp_print_string fm "(";
		pp_print_string fm "*";
		pp_print_c_identifier fm str;
		pp_print_string fm ")"

	    | Rfun str -> 
		pp_print_c_identifier fm str
		  
	    | Rvct str ->
		pp_print_c_identifier fm str
		
	    (*
	      | Rlbl str -> 
		pp_print_string fm "&";
		pp_print_c_identifier fm str
		  *)
		  
	    | Rcode_label str ->
		pp_print_string fm ("&&" ^ str)
		  
	    | Rvoid -> 
		pp_print_string fm "/*void*/"

	    | Rbyte_value str ->
		pp_print_string fm str

	    | Rcexpr expr -> 
		pp_print_cexpr fm ~need_paren:true expr
	end;
	pp_close_box fm ()

    and pp_print_lval: formatter -> lval -> unit =
      fun fm expr ->
	pp_open_hvbox fm indent;
	pp_print_lval_ fm expr; 
	pp_close_box fm ()
	  
    and pp_print_lval_: formatter -> lval_ -> unit =
      fun fm expr ->
	pp_open_hvbox fm indent;
	begin
	  match expr with
	    | Lreg ce -> 
		pp_print_c_identifier fm ce
		  
	    | Lladdr (Nlbl ce) -> 
		pp_print_c_identifier fm ce
		  
	    | Lladdr (Nctn ce) -> 
		pp_print_string fm "(";
		pp_print_string fm "*";
		pp_print_c_identifier fm ce;
		pp_print_string fm ")"
	end;
	pp_close_box fm ()

    and pp_print_cexpr: formatter -> need_paren:bool -> cexpr -> unit =
      fun fm ~need_paren expr ->
	pp_open_hvbox fm indent;
	begin
	  if need_paren then pp_print_string fm "(";
	  pp_print_cexpr_ fm ~need_paren:true expr; 
	  if need_paren then pp_print_string fm ")";
	end;
	pp_close_box fm ()
	  
    and pp_print_cexpr_ (fm:formatter) ~(need_paren:bool) (expr:cexpr_) =
      pp_open_hvbox fm indent;
      begin
	match expr with
	  | Cconst c_const040 ->
	      pp_print_c_const040 fm ~need_paren:true c_const040
		
	  | Csizeof (c_type, c_const040) ->
	      pp_print_c_const040 fm ~need_paren:true c_const040
		
	  | Calignof (c_type, c_const040) ->
	      pp_print_c_const040 fm ~need_paren:true c_const040
		
	  | Cvct str -> 
	      pp_print_c_identifier fm str
		
	  | Cvar_lbl str -> 
	      pp_print_string fm "&";
	      pp_print_c_identifier fm str
		
	  | Ccode_lbl str ->
	      pp_print_string fm ("&&" ^ str)

	  | Cfun_lbl str ->
	      pp_print_c_identifier fm str
		
	  | CCast (c_type, expr) -> 
	      begin
		if need_paren then pp_print_string fm "(";
		pp_print_string fm "(";
		pp_print_c_type fm c_type;
		pp_print_string fm ")";
		pp_print_cexpr fm ~need_paren:true expr;
		if need_paren then pp_print_string fm ")";
	      end

	  | CQuestion (cond, expr0, expr1) -> 
	      begin
		if need_paren then pp_print_string fm "(";
		pp_print_string fm "(";
		pp_print_cexpr fm ~need_paren:true cond;
		pp_print_string fm ")";
		pp_print_string fm "?";
		pp_print_cexpr fm ~need_paren:true expr0;
		pp_print_string fm ":";
		pp_print_cexpr fm ~need_paren:true expr1;
		if need_paren then pp_print_string fm ")";
	      end

	  | CLogic_not expr ->
	      begin
		if need_paren then pp_print_string fm "(";
		pp_print_string fm "!";
		pp_print_cexpr fm ~need_paren:true expr;
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CLogic (op, e0, e1) ->
	      begin
		let opstr = string_of_logic op
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_" ^ opstr ^ "(");
		pp_print_cexpr fm ~need_paren:true e0;
		pp_print_string fm ",";
		pp_print_cexpr fm ~need_paren:true e1;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")"
	      end
		
	  | CFneg (fbits, rexpr) -> 
	      begin
		let str = string_of_fbits fbits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_fneg_" ^ str ^ "(");
		pp_print_cexpr fm ~need_paren:true rexpr;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CFbin (fbin_op, fbits, e0, e1) -> 
	      begin
		let opstr = string_of_fbinop fbin_op
		and tystr = string_of_fbits fbits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		pp_print_cexpr fm ~need_paren:true e0;
		pp_print_string fm ",";
		pp_print_cexpr fm ~need_paren:true e1;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  (* integer *)
	  | CIneg (ibits, rexpr) -> 
	      begin
		let str = string_of_ibits ibits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_neg_" ^ str ^ "(");
		pp_print_cexpr fm ~need_paren:true rexpr;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CIbnot (ibits, rexpr) -> 
	      begin
		let str = string_of_ibits ibits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_bnot_" ^ str ^ "(");
		pp_print_cexpr fm ~need_paren:true rexpr;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CIbin (ibin_op, ibits, e0, e1) ->
	      begin
		let opstr = string_of_bin_op ibin_op
		and tystr = string_of_ibits ibits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		pp_print_cexpr fm ~need_paren:true e0;
		pp_print_string fm ",";
		pp_print_cexpr fm ~need_paren:true e1;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  (* floating point relation expressions *)
	  | CFrel (rel, fbits, e0, e1) ->
	      begin
		let opstr = string_of_frel rel
		and tystr = string_of_fbits fbits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		pp_print_cexpr fm ~need_paren:true e0;
		pp_print_string fm ",";
		pp_print_cexpr fm ~need_paren:true e1;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CIrel (rel, ibits, e0, e1) ->
	      begin
		let opstr = string_of_rel rel
		and tystr = string_of_ibits ibits
		in
		if need_paren then pp_print_string fm "(";
		pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		pp_print_cexpr fm ~need_paren:true e0;
		pp_print_string fm ",";
		pp_print_cexpr fm ~need_paren:true e1;
		pp_print_string fm ")";
		if need_paren then pp_print_string fm ")";
	      end
		
	  | CPTR_TO_PTR (c_type, expr) ->
	      begin
		if need_paren then pp_print_string fm "(";
		pp_print_string fm "(";
		pp_print_c_type fm c_type;
		pp_print_string fm ")";
		pp_print_cexpr fm ~need_paren:true expr;
		if need_paren then pp_print_string fm ")";
	      end

	  | CPTR_TO_UI (expr) ->
	      begin
		if need_paren then pp_print_string fm "(";
		pp_print_string fm "(unsigned int)";
		pp_print_cexpr fm ~need_paren:true expr;
		if need_paren then pp_print_string fm ")";
	      end
		
	  (* floating point types to integer types *)
	  | CF_TO_SI (rexpr, f, i) -> 
	      let istr = string_of_ibits i
	      and fstr = string_of_fbits f
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "f2i_" ^ fstr ^ "_to_" ^ istr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
	      
	  | CF_TO_UI (rexpr, f, i) -> 
	      let istr = string_of_ubits i
	      and fstr = string_of_fbits f
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "f2u_" ^ fstr ^ "_to_" ^ istr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";

	  (* bool to int *)
	  | CB_TO_SI (rexpr, s, d) -> 
	      let sstr = string_of_ubits s
	      and dstr = string_of_ubits d
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "b2i_" ^ sstr ^ "_to_" ^ dstr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";

	  | CB_TO_UI (rexpr, s, d) -> 
	      let sstr = string_of_ubits s
	      and dstr = string_of_ubits d
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "b2u_" ^ sstr ^ "_to_" ^ dstr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
	      
	  (* integer types to floating point types *)
	  | CSI_TO_F (rexpr, i, f) -> 
	      let istr = string_of_ibits i
	      and fstr = string_of_fbits f
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "i2f_" ^ istr ^ "_to_" ^ fstr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
	      
	  | CUI_TO_F (rexpr, i, f) -> 
	      let istr = string_of_ubits i
	      and fstr = string_of_fbits f
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_" ^ "u2f_" ^ istr ^ "_to_" ^ fstr ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
	      
	  (* floating point types to floating point types *)
	  | CF_TO_F (rexpr, f0, f1) -> 
	      let fstr0 = string_of_fbits f0
	      and fstr1 = string_of_fbits f1
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm 
		("op_" ^ "f2f_" ^ fstr0 ^ "_to_" ^ fstr1 ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";

	  (* sign extension *)
	  | CSXI (rexpr, srcbits, destbits) -> 
	      let fstr0 = string_of_ibits srcbits
	      and fstr1 = string_of_ibits destbits
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_sx" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";

	  | CZXI (rexpr, srcbits, destbits) -> 
	      let fstr0 = string_of_ubits srcbits
	      and fstr1 = string_of_ubits destbits
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm ("op_zx" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
	      
	  | CLOBITS (rexpr, srcbits, destbits) -> 
	      let fstr0 = string_of_ibits srcbits
	      and fstr1 = string_of_ibits destbits
	      in
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm 
		("op_lobits" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
	      pp_print_cexpr fm ~need_paren:true rexpr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")";
      end;
      pp_close_box fm ()

    and pp_print_rexpr: formatter -> need_paren:bool -> rexpr -> unit =
      fun fm ~need_paren expr ->
	pp_open_hvbox fm indent;
	begin
	  if need_paren then pp_print_string fm "(";
	  pp_print_rexpr_ fm ~need_paren:true expr;
	  if need_paren then pp_print_string fm ")";
	end;
	pp_close_box fm ()
	  
    and pp_print_rexpr_: formatter -> need_paren:bool -> rexpr_ -> unit =
      fun fm ~need_paren expr ->
	pp_open_hvbox fm indent;
	begin
	  match expr with
	    | Rval_cast (typ, rval) -> 
		pp_print_string fm "(";
		Tent_c_printer.pp_print_c_type_name fm typ;
		pp_print_string fm ")";
		pp_print_rval fm rval
		  
	    | Rval rval -> pp_print_rval fm rval			    
		
	    | Logic_not expr ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_string fm "!";
		  pp_print_rval fm expr;
		  if need_paren then pp_print_string fm ")";
		end

	    | Logic (op, e0, e1) ->
		begin
		  let opstr = string_of_logic op
		  in
		  pp_print_string fm ("op_" ^ opstr ^ "(");
		  pp_print_rval fm e0;
		  pp_print_string fm ",";
		  pp_print_rval fm e1;
		  pp_print_string fm ")";
		end
		  
	    | Fneg (fbits, rexpr) -> 
		begin
		  let str = string_of_fbits fbits
		  in
		  pp_print_string fm ("op_fneg_" ^ str ^ "(");
		  pp_print_rval fm rexpr;
		  pp_print_string fm ")";
		end
		  
	    | Fbin (fbin_op, fbits, e0, e1) -> 
		begin
		  let opstr = string_of_fbinop fbin_op
		  and tystr = string_of_fbits fbits
		  in
		  pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		  pp_print_rval fm e0;
		  pp_print_string fm ",";
		  pp_print_rval fm e1;
		  pp_print_string fm ")";
		end
		  
	    (* integer *)
	    | Ineg (ibits, rexpr) -> 
		begin
		  let str = string_of_ibits ibits
		  in
		  pp_print_string fm ("op_neg_" ^ str ^ "(");
		  pp_print_rval fm rexpr;
		  pp_print_string fm ")";
		end

	    | Ibnot (ibits, rexpr) -> 
		begin
		  let str = string_of_ibits ibits
		  in
		  pp_print_string fm ("op_bnot_" ^ str ^ "(");
		  pp_print_rval fm rexpr;
		  pp_print_string fm ")";
		end
		  
	    | Ibin (ibin_op, ibits, e0, e1) ->
		begin
		  let opstr = string_of_bin_op ibin_op
		  and tystr = string_of_ibits ibits
		  in
		  pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		  pp_print_rval fm e0;
		  pp_print_string fm ",";
		  pp_print_rval fm e1;
		  pp_print_string fm ")";
		end
		  
	    (* floating point relation expressions *)
	    | Frel (rel, fbits, e0, e1) ->
		begin
		  let opstr = string_of_frel rel
		  and tystr = string_of_fbits fbits
		  in
		  pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		  pp_print_rval fm e0;
		  pp_print_string fm ",";
		  pp_print_rval fm e1;
		  pp_print_string fm ")";
		end
		  
	    | Irel (rel, ibits, e0, e1) ->
		begin
		  let opstr = string_of_rel rel
		  and tystr = string_of_ibits ibits
		  in
		  pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
		  pp_print_rval fm e0;
		  pp_print_string fm ",";
		  pp_print_rval fm e1;
		  pp_print_string fm ")";
		end
		  
	    | PTR_TO_PTR (c_type, expr) ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_string fm "(";
		  pp_print_c_type fm c_type;
		  pp_print_string fm ")";
		  pp_print_rval fm expr;
		  if need_paren then pp_print_string fm ")";
		end

	    | PTR_TO_UI (expr) ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_string fm "(unsigned int)";
		  pp_print_rval fm expr;
		  if need_paren then pp_print_string fm ")";
		end
		  
	    (* floating point types to integer types *)
	    | F_TO_SI (rexpr, f, i) -> 
		let istr = string_of_ibits i
		and fstr = string_of_fbits f
		in
		pp_print_string fm ("op_" ^ "f2i_" ^ fstr ^ "_to_" ^ istr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"
		  
	    | F_TO_UI (rexpr, f, i) -> 
		let istr = string_of_ubits i
		and fstr = string_of_fbits f
		in
		pp_print_string fm ("op_" ^ "f2u_" ^ fstr ^ "_to_" ^ istr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"

	    (* bool to int *)
	    | B_TO_SI (rexpr, s, d) -> 
		let sstr = string_of_ubits s
		and dstr = string_of_ubits d
		in
		pp_print_string fm ("op_" ^ "b2i_" ^ sstr ^ "_to_" ^ dstr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"

	    | B_TO_UI (rexpr, s, d) -> 
		let sstr = string_of_ubits s
		and dstr = string_of_ubits d
		in
		pp_print_string fm ("op_" ^ "b2u_" ^ sstr ^ "_to_" ^ dstr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"

	    (* integer types to floating point types *)
	    | SI_TO_F (rexpr, i, f) -> 
		let istr = string_of_ibits i
		and fstr = string_of_fbits f
		in
		pp_print_string fm ("op_" ^ "i2f_" ^ istr ^ "_to_" ^ fstr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"
		  
	    | UI_TO_F (rexpr, i, f) -> 
		let istr = string_of_ubits i
		and fstr = string_of_fbits f
		in
		pp_print_string fm ("op_" ^ "u2f_" ^ istr ^ "_to_" ^ fstr ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"
		  
	    (* floating point types to floating point types *)
	    | F_TO_F (rexpr, f0, f1) -> 
		let fstr0 = string_of_fbits f0
		and fstr1 = string_of_fbits f1
		in
		pp_print_string fm ("op_" ^ "f2f_" ^ fstr0 ^ "_to_" ^ fstr1 ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"

	    (* sign extension *)
	    | SXI (rexpr, srcbits, destbits) -> 
		let fstr0 = string_of_ibits srcbits
		and fstr1 = string_of_ibits destbits
		in
		pp_print_string fm ("op_sx" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"

	    | ZXI (rexpr, srcbits, destbits) -> 
		let fstr0 = string_of_ubits srcbits
		and fstr1 = string_of_ubits destbits
		in
		pp_print_string fm ("op_zx" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"
		  
	    | LOBITS (rexpr, srcbits, destbits) -> 
		let fstr0 = string_of_ibits srcbits
		and fstr1 = string_of_ibits destbits
		in
		pp_print_string fm ("op_lobits" ^ "_" ^ fstr0 ^ "_" ^ fstr1 ^ "(");
		pp_print_rval fm rexpr;
		pp_print_string fm ")"
	end;
	pp_close_box fm ()

    and pp_print_expr: formatter -> need_paren:bool -> expr -> unit =
      fun fm ~need_paren expr ->
	pp_open_hvbox fm indent;
	begin
	  match expr with
	    | Rexpr expr ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_rexpr fm ~need_paren:true expr;
		  if need_paren then pp_print_string fm ")";
		end
		  
	    | Macro_va_start (expr0, expr1) ->
		begin
		  pp_print_string fm "__builtin_va_start";
		  pp_print_space fm ();
		  pp_print_string fm "(";
		  pp_print_rval fm expr0;
		  pp_print_string fm ",";
		  pp_print_rval fm expr1;
		  pp_print_string fm ")"
		end
		  
	    | Macro_va_end expr ->
		begin
		  pp_print_string fm "__builtin_va_end";
		  pp_print_space fm ();
		  pp_print_string fm "(";
		  pp_print_rval fm expr;
		  pp_print_string fm ")"
		end

	    | Macro_va_arg (lval, expr, c_type) ->
		begin
		  pp_print_lval fm lval;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_string fm "__builtin_va_arg";
		  pp_print_space fm ();
		  pp_print_string fm "(";
		  pp_print_rval fm expr;
		  pp_print_string fm ",";
		  pp_print_c_type fm c_type;
		  pp_print_string fm ")"
		end

	    | Assign (lval, expr1) ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_lval fm lval;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_rexpr fm ~need_paren:false expr1;
		  if need_paren then pp_print_string fm ")";
		end	    

	    | Memcpy (expr0, expr1, cexpr) ->
		begin
		  pp_print_string fm "__builtin_memcpy";
		  pp_print_string fm "(";
		  pp_print_rval fm expr0;
		  pp_print_string fm ",";
		  pp_print_space fm ();
		  pp_print_rval fm expr1;
		  pp_print_string fm ",";
		  pp_print_cexpr fm ~need_paren:true cexpr;
		  pp_print_string fm ")";
		end

	    | Alloca (lval, te, expr1) ->
		begin
		  if need_paren then pp_print_string fm "(";
		  pp_print_lval fm lval;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_string fm "(";
		  pp_print_c_type fm te;
		  pp_print_string fm ")";
		  pp_print_string fm "__builtin_alloca(";
		  pp_print_rexpr fm ~need_paren:false expr1;
		  pp_print_string fm ")";
		  if need_paren then pp_print_string fm ")";
		end	    
	end;
	pp_close_box fm ()

    and pp_print_c_constant_expression: 
	formatter -> c_constant_expression -> unit = 
      fun fm expr ->
	pp_open_hvbox fm indent;
	begin
	  pp_print_cexpr fm ~need_paren:true expr
	end;
	pp_close_box fm ()
	  
    and pp_print_c_type: formatter -> c_type -> unit = 
      fun fm expr -> 
	Tent_c_printer.pp_print_c_type_name fm expr

    and pp_print_c_declaration: formatter -> c_declaration -> unit = 
      fun fm expr -> 
	pp_open_box fm 0;
	begin
	  match expr with
	    | Str_decl_init (linkage, ce, value) ->
		begin
		  pp_print_linkage fm linkage;
		  Cent_c_printer.pp_print_ce fm ce;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_c_string_literal fm value
		end

	    | Obj_decl (linkage, ce) ->
		begin
		  pp_print_linkage fm linkage;
		  Cent_c_printer.pp_print_ce fm ce
		end
		  
	    | Obj_decl_init (linkage, ce, init) ->
		begin
		  pp_print_linkage fm linkage;
		  Cent_c_printer.pp_print_ce fm ce;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_c_initializer fm init;
		end
		  
	    | Type_def c_type -> 
		begin
		  Tent_c_printer.pp_print_type_def fm c_type
		end
	    | Type_decl c_type ->
		begin
		  Tent_c_printer.pp_print_c_type_decl fm c_type
		end
	    | Type_only c_type ->
		begin
		  Tent_c_printer.pp_print_id_decl fm c_type "";
		end
	end;
	pp_close_box fm ()

    and pp_print_c_local_declaration: formatter -> c_local_declaration -> unit = 
      fun fm expr -> 
	pp_open_box fm 0;
	begin
	  match expr with
	    | Local_obj_decl (linkage, ce) ->
		begin
		  pp_print_linkage fm linkage;
		  Cent_c_printer.pp_print_ce fm ce
		end
		  
	    | Local_obj_decl_init (linkage, ce, init) ->
		begin
		  pp_print_linkage fm linkage;
		  Cent_c_printer.pp_print_ce fm ce;
		  pp_print_space fm ();
		  pp_print_string fm "=";
		  pp_print_space fm ();
		  pp_print_c_initializer fm init;
		end
		  
	    | Local_type_def c_type -> 
		Tent_c_printer.pp_print_type_def fm c_type
		  
	    | Local_type_decl c_type ->
		Tent_c_printer.pp_print_c_type_decl fm c_type
		  
	    | Local_type_only c_type ->
		Tent_c_printer.pp_print_id_decl fm c_type "";

	    | Local_register ce ->
		Cent_c_printer.pp_print_ce fm ce
	end;
	pp_close_box fm ()
	  
    and pp_print_asm (fm:formatter) (str_list, asm_details_opt) = 
      pp_open_vbox fm 0;
      begin
	pp_print_string fm "__asm__";
	pp_print_space fm ();
	pp_print_string fm "(";
	pp_print_space fm ();
	let _ = Mlite_printer.pp_print_list fm 
	  (fun fm s -> pp_print_string fm ("\"" ^ s ^ "\""))
	  pp_print_c_newline str_list
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
		    pp_print_lval fm e;
		    pp_print_string fm ")")
		  (fun fm -> pp_print_string fm ",") asm.asm_outputs;
		pp_print_space fm ();
		pp_print_string fm ":";
		Mlite_printer.pp_print_list fm 
		  (fun fm (so, s,e) ->
		    pp_print_string fm ("\"" ^ s ^ "\"");
		    pp_print_string fm " (";
		    pp_print_rval fm e;
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


    and pp_print_true_cond: formatter -> true_cond -> unit =
      fun fm true_cond ->
	match true_cond with
	  | NEQ_ZERO rval -> 
	      pp_print_rval fm rval;
	      pp_print_string fm "!= 0"
		
	  | EQ_ZERO rval -> 
	      pp_print_rval fm rval;
	      pp_print_string fm "== 0"
		
	  | IPRED (rel, ibits, e0, e1) ->
	      let opstr = string_of_rel rel
	      and tystr = string_of_ibits ibits
	      in
	      pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
	      pp_print_rval fm e0;
	      pp_print_string fm ",";
	      pp_print_rval fm e1;
	      pp_print_string fm ")";
	      
	  | FPRED (rel, fbits, e0, e1) ->
	      let opstr = string_of_frel rel
	      and tystr = string_of_fbits fbits
	      in
	      pp_print_string fm ("op_" ^ opstr ^ "_" ^ tystr ^ "(");
	      pp_print_rval fm e0;
	      pp_print_string fm ",";
	      pp_print_rval fm e1;
	      pp_print_string fm ")";

	  | FNOTPRED (rel, fbits, e0, e1) ->
	      let opstr = string_of_frel rel
	      and tystr = string_of_fbits fbits
	      in
	      pp_print_string fm ("!op_" ^ opstr ^ "_" ^ tystr ^ "(");
	      pp_print_rval fm e0;
	      pp_print_string fm ",";
	      pp_print_rval fm e1;
	      pp_print_string fm ")";
	      
	      
    and pp_print_c_stmt010: formatter -> c_stmt010 -> unit = 
      fun fm stmt ->
	pp_open_vbox fm 0;
	begin
	  match stmt with
	    | STMT_SPAN (txt, stmt) -> 
		pp_print_string fm "/*";
		pp_print_space fm ();
		pp_print_string fm txt;
		pp_print_space fm ();
		pp_print_string fm "*/";
		pp_print_space fm ();
		pp_print_c_stmt010 fm stmt
		  
	    | STMT_AT (coord, stmt) -> 
		Coordinate.pp_print_t fm coord;
		pp_print_c_stmt010 fm stmt
		  
	    | NOP -> ()
		
	    | SESE exprs ->
		begin
		  Mlite_printer.pp_print_list fm 
		    (fun fm expr -> 
		      pp_print_expr fm ~need_paren:false expr;
		      pp_print_string fm ";")
		    (fun fm -> pp_print_space fm ())
		    exprs
		end
		  
	    | COMPUTATION expr -> 
		pp_open_box fm 0;
		pp_print_expr fm ~need_paren:false expr;
		pp_print_string fm ";";
		pp_close_box fm ()
		  
		  
	    | SEQUENCE (txt_opt, c_stmt010_list) -> 
		begin
		  let _ = match txt_opt with
		    | Some txt -> fprintf fm "/* %s */" txt; 
			pp_print_space fm ();
		    | None -> ()
		  in
		  Mlite_printer.pp_print_list fm 
		    (fun fm statement ->
		      pp_print_c_stmt010 fm statement)
		    (fun fm -> pp_print_space fm ()) 
		    c_stmt010_list;
		end

	    | COMPOUND (txt_opt, c_compound_stmt010) ->
		begin
		  pp_print_string fm "{";
		  pp_open_vbox fm indent;
		  begin
		    let _ = match txt_opt with
		      | Some txt -> pp_print_space fm (); 
			  fprintf fm "/* %s */" txt
		      | None -> ()
		    in
		    pp_print_c_compound_stmt010 fm c_compound_stmt010;
		  end;
		  pp_close_box fm ();
		  pp_print_space fm ();
		  pp_print_string fm "}";
		end
		  
	    | IF (expr, then_c_stmt010, else_c_stmt010) -> 
		begin
		  pp_print_string fm "if (";
		  pp_print_true_cond fm expr;
		  pp_print_string fm ")";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm then_c_stmt010;
		  pp_print_space fm ();
		  pp_print_string fm "else";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm else_c_stmt010
		end

		  
	    | WHILE (expr, c_stmt0) ->
		begin
		  pp_print_string fm "while (";
		  pp_print_true_cond fm expr;
		  pp_print_string fm ")";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm c_stmt0
		end

	    | LOOP c_stmt0 ->
		begin
		  pp_print_string fm "for(;;)";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm c_stmt0
		end

	    | BREAK ->
		pp_print_string fm "break;"

	    | CONTINUE ->
		pp_print_string fm "continue;"
		  
	    | RETURN_VALUE expr ->
		begin
		  pp_print_string fm "return ";
		  pp_print_rexpr fm ~need_paren:true expr;
		  pp_print_string fm ";"
		end
		  
	    | RETURN ->
		pp_print_string fm "return;"

	    | EPI ret_opt ->
		begin
		  match ret_opt with
		    | Some ret -> 
			pp_open_box fm 0;
			pp_print_string fm "return";
			pp_print_space fm ();
			pp_print_rval fm ret;
			pp_print_string fm ";";
			pp_close_box fm ()
		    | None ->
			pp_print_string fm "return;"
		end
		  
	    | SWITCH (expr, c_stmt010) ->
		begin
		  pp_print_string fm "switch";
		  pp_print_string fm "(";
		  pp_print_rval fm expr;
		  pp_print_string fm ")";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm c_stmt010
		end
		  
	    | CASE (expr, c_stmt010) ->
		begin
		  pp_open_box fm 0;
		  pp_print_string fm "case";
		  pp_print_space fm ();
		  pp_print_c_constant_expression fm expr;
		  pp_print_string fm ":";
		  pp_close_box fm ();
		  pp_open_vbox fm indent;
		  begin
		    pp_print_space fm ();
		    pp_print_c_stmt010 fm c_stmt010;
		  end;
		  pp_print_string fm (";/*case:*/");
		  pp_close_box fm ()
		end
		  
	    | CASE_RANGE (e0, e1, c_statement) ->
		begin
		  pp_open_box fm 0;
		  pp_print_string fm "case ";
		  pp_print_c_constant_expression fm e0;
		  pp_print_string fm " ... ";
		  pp_print_c_constant_expression fm e1;
		  pp_print_string fm ":";
		  pp_close_box fm ();
		  pp_open_vbox fm indent;
		  begin
		    pp_print_space fm ();
		    pp_print_c_stmt010 fm c_statement;
		  end;
		  pp_print_string fm (";/*case:*/");
		  pp_close_box fm ()
		end
		  
	    | DEFAULT (c_stmt010) ->
		begin
		  pp_print_string fm "default:";
		  pp_print_space fm ();
		  pp_print_c_stmt010 fm c_stmt010;
		  pp_print_string fm (";/*default:*/");
		end
		  
	    | LABEL (lb, c_stmt010) ->
		begin
		  pp_print_string fm (lb ^ ":");
		  pp_print_c_stmt010 fm c_stmt010;
		  pp_print_string fm (";/*label:" ^ lb ^ "*/");
		end
		  
	    | GOTO lb ->
		pp_print_string fm ("goto " ^ lb ^ ";");
		
	    | GCC_GOTO expr ->
		begin
		  pp_print_string fm ("goto ");
		  pp_print_string fm "*";
		  pp_print_rval fm expr;
		  pp_print_string fm ";";
		end
		  
	    | ASM (u,v) ->
		pp_print_asm fm (u,v)

	    | CALL (lval_opt, expr, expr_list) ->
		pp_open_box fm 0;
		pp_print_call_transfer fm (lval_opt, expr, expr_list);
		pp_close_box fm ()
	end;
	pp_close_box fm ()

    and pp_print_call_transfer: formatter -> lval option * rval * rval list 
    -> unit = 
      fun fm (lval_opt, expr, expr_list) ->
	pp_open_box fm 0;
	begin
	  let _ = match lval_opt with
	    | Some lval ->
		pp_print_lval fm lval;
		pp_print_string fm "=";
	    | None -> ()
	  in
	  pp_print_rval fm expr;
	  pp_print_string fm "(";
	  Mlite_printer.pp_print_list fm 
	    pp_print_rval
	    pp_print_c_comma 
	    expr_list;
	  pp_print_string fm ");";
	end;
	pp_close_box fm ()

    and pp_print_c_compound_stmt010: formatter -> c_compound_stmt010 -> unit = 
      fun fm (BLOCK (labels, c_declaration_list, stmts)) ->
	pp_open_vbox fm 0;
	begin
	  if (labels <> []) then
	    begin
	      pp_print_space fm ();
	      pp_print_string fm "__label__ ";
	      Mlite_printer.pp_print_list fm pp_print_string
		(fun fm -> pp_print_string fm ",") labels;
	      pp_print_string fm ";";
	      pp_print_space fm ();
	    end;
	  if (c_declaration_list <> []) then
	    begin
	      pp_print_space fm ();
	      pp_print_c_local_declaration_list fm c_declaration_list;
	    end;
	  if stmts <> [] then
	    begin
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm 
		pp_print_c_stmt010
		(fun fm -> pp_print_space fm ())
		stmts;
	    end
	end;
	pp_close_box fm ()

    and pp_print_c_local_declaration_list: formatter -> 
    c_local_declaration list -> unit =
      fun fm expr ->
	pp_open_vbox fm 0;
	if expr <> [] then 
	  begin
	    Mlite_printer.pp_print_list fm 
	      pp_print_c_local_declaration pp_print_c_semicolon expr;
	    pp_print_c_semicolon fm;
	  end;
	pp_close_box fm ()
    in
    match c_construct with
      | CONST (c_constant, need_paren) ->
	  pp_print_c_const040 fm ~need_paren c_constant
      | EXPR (c_expression, need_paren) ->
	  pp_print_expr fm ~need_paren c_expression
      | REXPR (c_expression, need_paren) ->
	  pp_print_rexpr fm ~need_paren c_expression
      | CEXPR (c_expression, need_paren) ->
	  pp_print_cexpr fm ~need_paren c_expression
      | DECL c_declaration ->
	  pp_print_c_declaration fm c_declaration
      | LOCAL_DECL c_local_declaration ->
	  pp_print_c_local_declaration fm c_local_declaration
      | TYPE (string_opt, typ) -> pp_print_c_type fm typ
      | STMT c_statement ->
	  pp_print_c_stmt010 fm c_statement
      | COMPOUND_STMT c_compound_stmt010 ->
	  pp_print_c_compound_stmt010 fm c_compound_stmt010
      | CONST_EXPR c_constant_expression ->
	  pp_print_c_constant_expression fm c_constant_expression
      | ASM_STMT (strlist, asm_details_opt) ->
	  pp_print_asm fm (strlist, asm_details_opt)
      | CALL_EXPR (lval_opt, rval, rval_lst) ->
	  pp_print_call_transfer fm (lval_opt, rval, rval_lst)
      | RVAL rval ->
	  pp_print_rval fm rval
      | TRUE_COND v ->
	  pp_print_true_cond fm v
	    
	    
let pp_print_c_type: formatter -> string option -> c_type -> unit = 
  fun fm  declarator_opt expr -> 
    pp_print_c_construct fm  (TYPE (declarator_opt, expr))

let pp_print_expr: formatter -> 
  need_paren:bool -> expr -> unit = 
  fun fm  ~need_paren expr -> 
    pp_print_c_construct fm (EXPR (expr, need_paren))

let pp_print_rexpr: formatter -> 
  need_paren:bool -> rexpr -> unit = 
  fun fm  ~need_paren expr -> 
    pp_print_c_construct fm (REXPR (expr, need_paren))
    
let pp_print_cexpr: formatter -> 
  need_paren:bool -> cexpr -> unit = 
  fun fm  ~need_paren expr -> 
    pp_print_c_construct fm (CEXPR (expr, need_paren))
    
let pp_print_c_const040: formatter -> 
  need_paren:bool -> c_const040 -> unit = 
  fun fm  ~need_paren expr -> 
    pp_print_c_construct fm  (CONST (expr, need_paren))
    
      
let pp_print_c_declaration fm  decl = 
  pp_print_c_construct fm  (DECL decl)
    
let pp_print_c_constant_expression fm  expr = 
  pp_print_c_construct fm  (CONST_EXPR expr)
    
let pp_print_c_declaration_list: formatter  -> c_declaration list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    Mlite_printer.pp_print_list fm (fun fm -> pp_print_c_declaration fm ) pp_print_c_newline expr;
    pp_close_box fm ()
      
let pp_print_c_declaration_list_opt fm  expr = 
  match expr with
  | Some c_declaration_list ->
      pp_print_space fm ();
      pp_print_c_declaration_list fm  c_declaration_list 
  | None -> ()
	
let pp_print_c_stmt010: formatter  -> c_stmt010 -> unit = 
  fun fm  expr ->
    pp_print_c_construct fm  (STMT expr)
      
let pp_print_c_compound_stmt010: formatter  -> c_compound_stmt010 -> unit = 
  fun fm  expr ->
    pp_print_c_construct fm  (COMPOUND_STMT expr)


let pp_print_c_file_unit: formatter -> c_file_unit -> unit = 
  fun fm c_translation_unit ->
    let rec pp_print_c_function_definition: formatter -> c_function_definition -> unit =
      fun fm expr ->
	pp_open_vbox fm 0;
	begin
	  match expr with
	    | Function_definition (linkage, c_type, fname, c_compound_stmt010) 
	      ->
		begin
		  pp_print_linkage fm linkage;
		  Tent_c_printer.pp_print_id_decl fm c_type 
		    (QNP.to_decl_str fname);
		  pp_print_space fm ();
		  pp_print_string fm "{";
		  pp_open_vbox fm indent;
		  begin
		    pp_print_c_compound_stmt010 fm c_compound_stmt010;
		  end;
		  pp_close_box fm ();
		  pp_print_space fm ();
		  pp_print_string fm "}";
		end
	end;
	pp_close_box fm ()

    and pp_print_c_external_declaration: formatter -> c_external_declaration -> unit =
      fun fm expr ->
	pp_open_vbox fm 0;
	begin
	  match expr with
	    | External_declaration_at (coord, expr) ->
		Coordinate.pp_print_t fm coord;
		pp_print_c_external_declaration fm expr;

	    | External_declaration_1 (c_function_definition) ->
	      pp_print_c_function_definition fm c_function_definition
	    | External_declaration_2 (c_declarations) ->
	      pp_print_c_declaration_list fm c_declarations
	end;
	pp_close_box fm ()

    and pp_print_c_declaration_list: formatter -> c_declaration list -> unit =
      fun fm expr ->
	pp_open_vbox fm 0;
	if expr <> [] then 
	  begin
	    Mlite_printer.pp_print_list fm 
	      (fun fm -> pp_print_c_declaration fm ) pp_print_c_semicolon expr;
	    pp_print_c_semicolon fm;
	  end;
	pp_close_box fm ()
	  
    in
    pp_open_vbox fm 0;
    begin
      match c_translation_unit with
	| Translation_unit (l,eenv) ->
	    begin
	      pp_print_string fm Mlite_config.mlitecc_macro_op_h;
	      pp_print_space fm ();
	      Mlite_printer.pp_print_list fm 
		pp_print_c_external_declaration
		(fun fm -> pp_print_space fm ())l;
	    end
    end;
    pp_close_box fm ()

      
