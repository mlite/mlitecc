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

let suffix = ".fa.stmt"

type join = JOIN of Int.gnode_id
  
and c_identifier = Ast_eb_expr.c_identifier
and expr = Ast_eb_expr.expr
and rval = Ast_eb_expr.rval
and c_type = Ast_eb_expr.c_type
and c_declaration = Ast_eb_expr.c_declaration
and c_local_declaration = Ast_eb_expr.c_local_declaration
and linkage = Ast_eb_expr.linkage
and asm_details = Ast_eb_expr.asm_details

and call_transfer = Ast_eb_expr.call_transfer
    
and c_constant_expression = Ast_eb_expr.c_constant_expression

(** A.2.3 Statements **)
and c_lab_stmt090 = Code_label.t * c_stmt090

and true_cond = Ast_eb_expr.true_cond

and c_stmt090 =
  | JOIN of Int.gnode_id * string option * Int.gnode_id
  | GOTO of Int.gnode_id * string
  | SESE_SEQUENCE of 
      Int.gnode_id * string option * expr list * Int.gnode_id
  | CALL of Int.gnode_id * call_transfer * Int.gnode_id
  (*| CMP_ZERO_JMP of Int.gnode_id * (rval * Int.gnode_id)*)
  | CMP_JMP of Int.gnode_id * (true_cond * Int.gnode_id)
  | TBL_JMP of 
      Int.gnode_id * 
	(Ast_eb_expr.rval * 
	  ((c_constant_expression * c_constant_expression) option * 
	    Int.gnode_id) list)
  | JMP of Int.gnode_id * string option * Int.gnode_id
  | JMP_JOIN of Int.gnode_id * string option * Int.gnode_id
  | JMP_JOIN_BACKWARD of Int.gnode_id * string option * Int.gnode_id
  | EPI of Int.gnode_id * rval option * Int.gnode_id
  | BEGIN_FUNCTION of 
      Int.gnode_id * c_local_declaration list ref * 
	Int.gnode_id * Int.gnode_id 
  | END_FUNCTION of Int.gnode_id 
  | BEGIN_DECL of 
      Int.gnode_id * c_local_declaration list * 
	Int.gnode_id * Int.gnode_id (* end decl *)
  | END_DECL of Int.gnode_id * Int.gnode_id
  | ASM of Int.gnode_id * string list * asm_details option * Int.gnode_id
  | POP_ARGS of Int.gnode_id * (c_type * Qual_name.t) list * Int.gnode_id
            
    
(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of c_external_declaration list * Ast_eb_expr.expr_env
	
and c_external_declaration =
  | External_declaration_at of Ast_eb_expr.coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration list
      
and c_function_definition =
    { 
      linkage: linkage;
      c_type: c_type;
      name: Qual_name.t;
      code_size: int;
      c_stmt090_list: c_lab_stmt090 list;
      line_tbl: Ast_eb_expr.coord option array;
    }

and c_file = string * c_translation_unit
