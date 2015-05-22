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

let version = "ISO/IEC 9899:TC"
  (** Types are tokenized and sizes of types are computed
      c_enumeration_constants are converted to integers
  **)

let description = 
  [
    "Types are tokenized and sizes of types are computed";
    "c_enumeration_constants are converted to integers";
    "Type convertion is explicit";
  ]

let suffix = ".ea.expr"

(** A.1.3 Identifiers **)
type coord = Coordinate.t

and c_identifier = Cent.ce
    
(** A.1.4 Universal character names **)
(** A.1.5 Constants **)
(** 6.4.4.1 integer-constant **)      
and c_const040 = c_type * Const_folding.cval_ext
      
(** A.1.5 String literals **)
and c_string_literal = C_syntax_symbol.c_string_literal

(** A.2.1 Expressions **)
and binary_arithmatic = C_semantics_symbol.binary_arithmatic
      
and binary_predicate = C_semantics_symbol.binary_predicate

and binary_logic_connect = C_semantics_symbol.binary_logic_connect
      
and unary_arithmatic = C_semantics_symbol.unary_arithmatic

and lval = 
  | Lladdr of laddr
  | Lreg of c_identifier
  
and laddr =
  | Nlbl of c_identifier
  | Nctn of c_identifier

and rval =
  | Rladdr of laddr
  | Rreg of c_identifier
  | Rindir of c_identifier
  | Rfun of c_identifier
  | Rvct of c_identifier
  | Rconst of c_const040 
  | Rsizeof of c_type * c_const040 
  | Ralignof of c_type * c_const040
  | Rvoid
  | Rcode_label of string
  | Rbyte_value of string
  | Rcexpr of cexpr
      
      
and cexpr = (* cexpr *)
  | CBinary_arithm of binary_arithmatic * cexpr * cexpr
  | CBinary_predicate of binary_predicate * cexpr * cexpr
  | CBinary_logic of binary_logic_connect * cexpr * cexpr
  | CUnary_arithm of unary_arithmatic * cexpr
  | CLogic_not of cexpr
  | CCast of c_type * cexpr
  | CQuestion of cexpr * cexpr * cexpr
  | Cconst of c_const040 
  | Csizeof of c_type * c_const040 
  | Calignof of c_type * c_const040
  | Cvct of c_identifier
  | Cvar_lbl of c_identifier
  | Ccode_lbl of string
  | Cfun_lbl of c_identifier

and rexpr =  (* side effect free expression *)
  | Rval of rval
  | Binary_arithm of binary_arithmatic * rval * rval
  | Binary_predicate of binary_predicate * rval * rval
  | Binary_logic of binary_logic_connect * rval * rval
  | Unary_arithm of unary_arithmatic * rval
  | Logic_not of rval
      
    (* convertion *)
  | Cast of c_type * rval
  | Ne_cast of c_type * rval * rval


and expr = 
  | Rexpr of rexpr
  | Macro_va_start of rval * rval
  | Macro_va_end of rval
  | Macro_va_arg of lval * rval * c_type
  | Assign of lval * rexpr
  | Memcpy of rval * rval * cexpr
  | Alloca of lval * c_type * rexpr
      
and c_expression = rval 

and call_transfer = lval option * rval * rval list

and c_constant_expression = cexpr

(** A.2.2 Declarations **)

and c_type = Tent.te

and linkage = 
  | Default_extern
  | Default_storage 
  | Extern
  | Extern_Inline
  | Auto
  | Static
  | Static_Inline
  | Register
  | Inline
  | Type_alias
  | Thread 
  | Extern_Thread 
  | Static_Thread

and c_declaration = 
  | Str_decl_init of linkage * c_identifier * c_string_literal
  | Obj_decl of linkage * c_identifier 
  | Obj_decl_init of linkage * c_identifier * c_initializer
  | Type_def of c_type
  | Type_decl of c_type
  | Type_only of c_type
      
and c_local_declaration = 
  | Local_obj_decl of linkage * c_identifier
  | Local_obj_decl_init of linkage * c_identifier
      * c_initializer (* it must
			 have
			 static
			 storage *)
  | Local_type_def of c_type
  | Local_type_decl of c_type
  | Local_type_only of c_type
  | Local_register of c_identifier
      
and c_init_expression = 
  | Static_init_none
  | Static_init of cexpr
      
(** (6.7.8) **)
and c_initializer = c_init_expression Typ_mem.t

(** A.2.3 Statements **)
and asm_details =
    { 
      (* optional name, constraints and expressions for outputs *)
      asm_outputs: (string option * string * lval) list;
      (* optional name, constraints and expressions for inputs *)
      asm_inputs: (string option * string * rval) list; 
      asm_clobbers: string list (* clobbered registers *)
    }

and true_cond =
  | NEQ_ZERO of rval
  | EQ_ZERO of rval
  | PRED of binary_predicate * rval * rval


module HashRval =
  struct
    type t = rval
    let equal (s1 : t) (s2 : t) = s1 == s2
    let hash (s : t) = Hashtbl.hash s
  end
module RvalHashtbl = Hashtbl.Make(HashRval)

module HashLval =
  struct
    type t = lval
    let equal (s1 : t) (s2 : t) = s1 == s2
    let hash (s : t) = Hashtbl.hash s
  end
module LvalHashtbl = Hashtbl.Make(HashLval)


module HashCexpr =
  struct
    type t = cexpr
    let equal (s1 : t) (s2 : t) = s1 == s2
    let hash (s : t) = Hashtbl.hash s
  end
module CexprHashtbl = Hashtbl.Make(HashCexpr)

module HashRexpr =
  struct
    type t = rexpr
    let equal (s1 : t) (s2 : t) = s1 == s2
    let hash (s : t) = Hashtbl.hash s
  end
module RexprHashtbl = Hashtbl.Make(HashRexpr)

type expr_env =
    {
      rexpr_env: Tent.te RexprHashtbl.t;
      cexpr_env: Tent.te CexprHashtbl.t;
      rval_env: Tent.te RvalHashtbl.t;
      lval_env: Tent.te LvalHashtbl.t;
    }

type c_stmt010 =
  | STMT_SPAN of string * c_stmt010 
  | STMT_AT of coord * c_stmt010
  | NOP
  | COMPUTATION of expr
  | SESE of expr list
  | SEQUENCE of string option * c_stmt010 list
  | COMPOUND of string option * c_compound_stmt010
  | IF of true_cond * c_stmt010 * c_stmt010
  | WHILE of true_cond * c_stmt010
  | LOOP of c_stmt010
  | BREAK
  | CONTINUE
  | RETURN_VALUE of rexpr
  | RETURN
  | EPI of rval option
  | SWITCH of rval * c_stmt010
  | CASE of c_constant_expression * c_stmt010
  | CASE_RANGE of c_constant_expression * 
      c_constant_expression * c_stmt010
  | DEFAULT of c_stmt010
  | LABEL of string * c_stmt010
  | GOTO of string
  | GCC_GOTO of rval
  | ASM of string list * asm_details option
  | CALL of call_transfer
      
and c_compound_stmt010 = 
  | BLOCK of string list * c_local_declaration list * c_stmt010 list

(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of c_external_declaration list * expr_env
      
	
and c_external_declaration =
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration list

and c_function_definition =
  | Function_definition of linkage * c_type * Qual_name.t * c_compound_stmt010

and c_file = c_translation_unit
