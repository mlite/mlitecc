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

open Csize
open Collection
open Tent
open Cent
open Qual_name

module QN = Qual_name

type te = Tent.te
and te_tbl = Tent.te_tbl
and te_info = Tent.te_info
and ce_info = Cent.ce_info

module HashTe =
  struct
    type t = te
    let equal (s1 : t) (s2 : t) = fst s1 == fst s2 & snd s1 = snd s2
    let hash (s : t) = Hashtbl.hash s
  end

module TeHashtbl = Hashtbl.Make(HashTe)

module HashCe =
struct
  type t = ce
  let equal (s1 : t) (s2 : t) = fst s1 == fst s2 & snd s1 = snd s2
  let hash (s : t) = Hashtbl.hash s
end
  
module CeHashtbl = Hashtbl.Make(HashCe)

type builtin_scope = 
    { 
      builtin_name: Qual_name.qn_scope list;
      builtin_qname_to_te_info: te_info StringHashtbl.t;
      builtin_symtbl: ce_info StringHashtbl.t;
      mutable builtin_cnt: int;
    }
      
and file_scope = 
    { 
      file_name: Qual_name.qn_scope list;
      file_qname_to_te_info: te_info StringHashtbl.t;
      file_symtbl: ce_info StringHashtbl.t;
      mutable file_cnt: int;
    }
      
and function_scope = 
    { 
      fun_name:Qual_name.qn_scope list;
      fun_qname_to_te_info: te_info StringHashtbl.t;
      fun_symtbl: ce_info StringHashtbl.t;
      mutable fun_cnt: int;
      is_global: bool;
      tmp_symtbl_scope: temp_scope Stack.t;
      temp_pool: temp_scope;
      var_prefix: string;
      mutable tmp_serno: int;
      mutable ret_opt: ret_kind;
      ret_lbl: string;
    }

and ret_kind =
  | HIDDEN_PARAM of ce
  | NORMAL_RETURN of ce
  | VOID_RETURN
      
and block_scope = 
    { 
      block_name: Qual_name.qn_scope list;
      block_qname_to_te_info: te_info StringHashtbl.t;
      scope_symtbl: ce_info StringHashtbl.t;
      mutable scope_cnt: int;
    }

and scope = 
  | BUILTIN_SCOPE of builtin_scope
  | FILE_SCOPE of file_scope
  | FUN_SCOPE of function_scope
  | BLOCK_SCOPE of block_scope
      
and temp_scope = 
    {
      id_decls: (te * Cent.ce) list ref;
      te_decls: te list ref;
      id_pools: Cent.ce TeHashtbl.t;
    }
      
and env = 
    {       
      mutable anonymous_count: int;
      te_tbl : te_tbl;            
      ce_tbl : ce_tbl;
      mutable scope_serno: int;      
      
      (* import from syms *)
      mutable current_file_name: string;
      mutable current_file_scope: file_scope;
      mutable current_func_scope: function_scope;
      mutable undefined: unit StringHashtbl.t;
      mutable used_builtin: ce QualNameHashtbl.t;
      
      stack: scope Stack.t;
      
      anonymous_id_serno: int ref;
      fscope: function_scope Stack.t;
      mutable cstr_id_serno: int;
      cstr_id_tag: string;
      mutable default_typ: te;
      void_typ: te;
      non_typ: te;
      char_typ: te;
      wchar_t_typ: te;
      int_typ: te;
      cuint_typ: te;
      size_typ: te;
      unsigned_char_typ: te;
      cptr_cuint_typ: te;
      bool_typ: te;
    }
