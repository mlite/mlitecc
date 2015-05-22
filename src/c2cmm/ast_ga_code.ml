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
let description = ["All code are stored in logic memory called code array";]
let suffix = ".ga.code"

type coord = Ast_eb_expr.coord

type expr = Ast_eb_expr.expr
and c_identifier = Ast_eb_expr.c_identifier
and rval = Ast_eb_expr.rval
and true_cond = Ast_eb_expr.true_cond
and c_type = Ast_eb_expr.c_type
and c_declaration = Ast_eb_expr.c_declaration
and c_local_declaration = Ast_eb_expr.c_local_declaration
and linkage = Ast_eb_expr.linkage
and asm_details = Ast_eb_expr.asm_details

and c_constant_expression = Ast_eb_expr.c_constant_expression

and call_transfer = Ast_eb_expr.call_transfer

(*and c_lab = c_type * string * int*)
    
and scope_ctrl =
  | Begin_fun of c_local_declaration list * int
  | End_fun
  | Begin_decl of c_local_declaration list * int
  | End_decl

and flow_ctrl = 
  | Jmp of int
  | Jmp_join of int
  | Jmp_join_backward of int
  (*| Cmp_zero_jmp of rval * int*)
  | Cmp_jmp of true_cond * int
  | Tbl_jmp of rval * int (* entry in jmp_table *)
      
and sese_code = 
  | Computing of expr list
  | Asm of string list * asm_details option
  | Pop_args of (c_type * Qual_name.t) list

and c_code100 = 
  | Scope of scope_ctrl
  | Call of call_transfer
  | Flow of flow_ctrl
  | Sese of sese_code list
  | Join
  | Nop
  | Epi of rval option

and jmp_table = 
    (((c_constant_expression * c_constant_expression) option * int) list) array

      
(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of c_external_declaration list * Ast_eb_expr.expr_env
      
and c_external_declaration =
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration list
      

and c_function_definition =
    { 
      linkage: linkage;
      c_type: c_type;
      name: Qual_name.t;
      code_array: (Code_label.t * c_code100 * coord option) array;
      jmp_tbl: jmp_table;
    }
      
and c_file = string * c_translation_unit 
