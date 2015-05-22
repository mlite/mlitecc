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
open Qual_name
module QN = Qual_name

type target_metrics = 
    { 
      little_endian: bool;
      pointer_size: csize;
      pointer_alignof: csize;
      enum_size: csize;
      enum_alignof: csize;
      builtin_type_num: int;
    }

and teid = int
    
and te_kind =
  | Array  of array_te_info
  | Abs_function of abs_fun_te_info 
  | Crt_function of crt_fun_te_info
  | Pointer of teid
  | Struct of field_table
  | Struct_name 
  | Union of field_table
  | Union_name
  | Enum of enum_te_info
  | Enum_name
  | Bits of bit_te_info
  | Qualified of qual_te_info
  | Builtin
  | Typedef of typedef_te_info
  | Normal of norm_table_entry 
      (* bitfield type will be normalized to its base typ *)
  | Attribute of teid * te_attribute list

and typ_size = 
  | Byte_size of csize
  | Incomplete

and te_info =
    { 
      m_id: teid;
      m_name: QN.t;
      mutable m_kind: te_kind;
      mutable m_ptr_teid_opt: teid option;
      mutable m_size: typ_size;
      mutable m_align: csize;
      mutable m_sizeof_computed: bool;
      mutable m_alignof_computed: bool;
      mutable m_runtime: bool;
      mutable m_has_alias: bool;
      mutable m_attribute_list: te_attribute list;
    }
      
and te_attribute = 
  | GCC_transparent_union (** __attribute__((transparent_union)) **)
  | GCC_aligned of int (** __attribute__((aligned(8))) **)
  | GCC_aligned_max  (** __attribute__((aligned)) **)
  | GCC_packed (** __attribute__((packed)) **)
  | GCC_mode of te_mode
  | GCC_may_alias
  | GCC_ms_struct
  | GCC_gcc_struct
  | GCC_vector_size of int
      
and te_mode =
  | GCC_bi
  | GCC_qi
  | GCC_hi
  | GCC_psi
  | GCC_si
  | GCC_pdi
  | GCC_di
  | GCC_ti
  | GCC_oi
  | GCC_qf
  | GCC_hf
  | GCC_tqf
  | GCC_sf
  | GCC_df
  | GCC_xf
  | GCC_sd
  | GCC_dd
  | GCC_td
  | GCC_tf
  | GCC_qq
  | GCC_hq
  | GCC_sq
  | GCC_dq
  | GCC_tq
  | GCC_uqq
  | GCC_uhq
  | GCC_usq
  | GCC_udq
  | GCC_utq
  | GCC_ha
  | GCC_sa
  | GCC_da
  | GCC_ta
  | GCC_uha
  | GCC_usa
  | GCC_uda
  | GCC_uta
  | GCC_cc
  | GCC_word
  | GCC_byte
  | GCC_pointer
      
and typ_equiv_class = 
    { 
      mutable m_representive: teid;
      mutable m_set: Ud_sets.IntSet.t;
    }

and synonymous =
  | Syn_string of string
  | Syn_teid of teid
      
and typedef_te_info = (* tt *)
    { 
      m_typedef_lhs_teid: teid;
    }
      
and qual_te_info = (* qt *)
    { 
      m_qualified_teid: teid;
      m_qualifier: qualifier; (*string;*)
    }

and qualifier = 
  | QUAL_CONST
  | QUAL_VOLATILE
  | QUAL_REGISTER
  | QUAL_RESTRICT

and bit_te_info = (* bt *)
    { 
      m_bit_base_teid: teid;
      m_bit_size: csize;
      m_bit_lsb: csize;
      m_bit_left: csize;
      m_bit_right: csize;
      m_bit_mask_typ: teid;
      m_bit_mask: string;       (* e.g mask of char:3 = 00001110 *)
      m_bit_emask: string;      (* emask of char:3 = 00000111 *)
      m_bit_bnot_emask: string; (* bnot_emask of char:3 = 11111000 *)
    }
      
and array_te_info = (* at *)
    { 
      m_elmt_teid: teid;
      m_array_cardi: array_cardi; (* csize; *)
    }

and array_cardi =
  | ARRAY_FIXED of csize (* [5] *) 
  | ARRAY_VARIABLE    (* [] *)
      
and abs_fun_te_info = (* aft *)
    { 
      aft_ret_teid: teid;
      mutable aft_param_teids: teid list;
      mutable aft_va_arg: bool;
      aft_hidden_ret_param: teid option;
      aft_muton_param_pos: int list;
    }

and crt_fun_te_info = (* cft *)
    { 
      m_crt_fun_abs_teid: teid;
      m_crt_fun_in_params: param_kind (*QN.t*) list;
    }

and param_kind = 
  | THIS_PARAM of QN.t
  | HIDDEN_RETURN of QN.t
  | NORMAL_PARAM of QN.t
  | SCALAR_PARAM of qname_muton
  | STRUCT_PARAM of qname_muton
      
and qname_muton = 
    {
      orig: QN.t;
      muton: QN.t;
    }

and enum_te_info = (* ete *)
    { 
      mutable m_enum_max_value: int;
      mutable m_enum_item_infos: enum_item_info list;
    }
      
and enum_item_info = (* eitem *)
    { 
      m_enum_item_name: QN.t;
      m_enum_item_serno: int;
      m_enum_item_value: int;
    }

and field_table = 
    {
      mutable m_field_infos: field_info list
    }
      
and field_info = (* field *)
    { 
      field_sname_opt: string option;
      field_serno: int;
      mutable field_teid: teid;
      field_te_has_def: bool;
      field_is_bitfield: bool;
      mutable field_offset: csize;
    }

and abs_field_info =
    {
      abs_field_serno: int;
      mutable abs_field_teid: teid;
      abs_field_te_has_def: bool;
      abs_field_is_bitfield: bool;
      mutable abs_field_offset: csize;
    }
      
and norm_table_entry =
    { 
      m_norm_orig_teid: teid;
      m_norm_norm_teid: teid;
    }

and te_tbl = 
    {
      mutable num_of_typs_in_files: int;
      teid_to_te_info: te_info Int.Type_idHashtbl.t;
      qname_to_te_info: te_info QualNameHashtbl.t;
      mutable te_count: int;
      mutable max_preload_typ: int;
      target_metrics: target_metrics;
      typ_equiv_class_table: teid Int.Type_idHashtbl.t;
      enum_item_const_table: int QualNameHashtbl.t;
      te_tbl_name: string;
    }

      
and enum_item = 
    {
      enum_typ: te;
      enum_name: QN.t;
      enum_value: int;
    }
      
and te = te_tbl * teid


    
let eq ((te_tbl0, tid0):te) ((te_tbl1, tid1):te) =
  (te_tbl0 == te_tbl1) & (tid0 = tid1)
    
