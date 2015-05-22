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

let suffix = ".ia.init"

(** A.1.3 Identifiers **)
type coord = Coordinate.t
type expr = Ast_eb_expr.expr
and rval = Ast_eb_expr.rval
and c_type = Ast_eb_expr.c_type
and cexpr = Ast_eb_expr.cexpr
    
and init_storage =
    {
      sto_linkage: linkage;
      orig_type: c_type;
      orig_id: Qual_name.t;
      union_type: c_type;
      union_id: string;
      init: c_initializer;
    }
      
and c_declaration = 
  | Str_decl_init of linkage * Cent.ce * C_syntax_symbol.c_string_literal
  | Obj_decl of linkage * Cent.ce
  | Type_def of c_type
  | Type_decl of c_type
  | Type_only of c_type
  | Dat_decl_init of init_storage
      
and c_local_declaration = 
  | Local_obj_decl of linkage * Cent.ce
  | Local_type_def of c_type
  | Local_type_decl of c_type
  | Local_type_only of c_type
  | Local_dat_decl_init of init_storage
  | Local_register of Cent.ce

and linkage = Ast_eb_expr.linkage
    
and const_val =
  | I8Const of string (* print out the value of each bytes *)
  | I8Space
  | IXCexpr of cexpr

and mem_cell =
    {
      mutable addr: int;
      size: int;
      cell: const_val array;
      typ: c_type;
      offset_name: string;
    }
      
and c_initializer = string * mem_cell list

and call_transfer = Ast_eb_expr.call_transfer

and scope_ctrl =
  | Begin_fun of c_local_declaration list * int
  | End_fun
  | Begin_decl of c_local_declaration list * int
  | End_decl
	
and flow_ctrl = Ast_ga_code.flow_ctrl
and sese_code = Ast_ga_code.sese_code

and c_code100 = 
  | Scope of scope_ctrl
  | Call of call_transfer
  | Flow of flow_ctrl
  | Sese of sese_code list
  | Join
  | Nop
  | Epi of rval option

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
