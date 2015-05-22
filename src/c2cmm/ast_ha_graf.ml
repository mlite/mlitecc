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

let description = []

let suffix = ".ha.graf"

(** A.1.3 Identifiers **)
type coord = Coordinate.t
type expr = Ast_eb_expr.expr
and rval = Ast_eb_expr.rval
and c_type = Ast_eb_expr.c_type
and c_declaration = Ast_eb_expr.c_declaration
and c_local_declaration = Ast_eb_expr.c_local_declaration
and linkage = Ast_eb_expr.linkage
    
and c_code100 = Ast_ga_code.c_code100
and jmp_table = Ast_ga_code.jmp_table

(** A.2.4 External definitions **)
and c_translation_unit = 
  | Translation_unit of c_external_declaration list * Ast_eb_expr.expr_env
      
and c_external_declaration =
  | External_declaration_at of coord * c_external_declaration
  | External_declaration_1 of c_function_definition
  | External_declaration_2 of c_declaration list

and c_node =
    { 
      label_att: Code_label.t;
      coord_opt: coord option;
      preds: int list;
      code: c_code100;
      succs: int list;
      idom: int option;
      domfrontiers: int list;
      mutable dominated_nodes: Collection.IntSet.t;
      scope_begin_pcs: Collection.IntSet.t;
    }

and c_function_definition =
    { 
      linkage: linkage;
      c_type: c_type;
      name: Qual_name.t;
      code_array: c_node array;
      jmp_tbl: jmp_table;
    }
and c_file = string * c_translation_unit
