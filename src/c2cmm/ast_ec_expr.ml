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

let suffix = ".ec.expr"

(** A.1.3 Identifiers **)
type coord = Coordinate.t

and c_identifier = Ast_eb_expr.c_identifier
and expr = Ast_eb_expr.expr
and rexpr = Ast_eb_expr.rexpr
and rval = Ast_eb_expr.rval
and c_type = Ast_eb_expr.c_type
and c_declaration = Ast_eb_expr.c_declaration
and c_local_declaration = Ast_eb_expr.c_local_declaration
and linkage = Ast_eb_expr.linkage
and asm_details = Ast_eb_expr.asm_details
and call_transfer = Ast_eb_expr.call_transfer
and c_constant_expression = Ast_eb_expr.c_constant_expression

and true_cond = Ast_eb_expr.true_cond

type c_stmt010 =
  | NOP
  | STMT_SPAN of string * c_stmt010 
  | STMT_AT of coord * c_stmt010
  | COMPUTATION of expr
  | SESE of expr list
  | SEQUENCE of string option * c_stmt010 list
  | COMPOUND of string option * c_stmt010 list
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
  | CASE_RANGE of c_constant_expression * c_constant_expression * c_stmt010
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
  | Translation_unit of c_external_declaration list * Ast_eb_expr.expr_env
	
and c_external_declaration =
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration list

and c_function_definition =
  | Function_definition of linkage * c_type * Qual_name.t * c_compound_stmt010

and c_file = c_translation_unit
