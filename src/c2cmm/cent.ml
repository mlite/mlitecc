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
open Qual_name

module QN = Qual_name

type te = Tent.te
and ce_info =
    {
      ce_id: ceid;
      ce_qname: QN.t;
      mutable ce_te: te;
      mutable ce_used: bool;
      mutable ce_is_real_ent: bool;
      mutable ce_addr_is_taken: bool;
      mutable ce_is_register: bool;
    }
      
and ce_tbl = 
    {
      mutable max_preload_ceid: int;
      mutable num_of_ces_in_files: int;
      ceid_to_ce_info: ce_info Int.Type_idHashtbl.t;
      qname_to_ce_info: ce_info QualNameHashtbl.t;
      mutable ce_count: int;
      ce_tbl_name: string;
    }
      
and ce_attribute =
  | GCC_weak 
  | GCC_always_inline  (** __attribute__((always_inline)) **)
  | GCC_alias of string (** __attribute__((alias("foo"))) **)
  | GCC_const (** __attribute__((const)) **)
  | GCC_malloc 
  | GCC_noinline
  | GCC_noreturn
  | GCC_pure
  | GCC_section of string (** __attribute__((section("bar"))) **)
  | GCC_unused
  | GCC_nothrow
  | GCC_no_instrument_function
  | GCC_constructor
  | GCC_destructor
  | GCC_used
  | GCC_warn_unused_result
  | GCC_regparm of int
  | GCC_stdcall
  | GCC_fastcall
  | GCC_cdecl
  | GCC_format of string * int * int 
      (** __attribute__ ((format (printf, 2, 3))) **) 
  | GCC_format_arg of int
  | GCC_nonnull of int list 
  | GCC_all_nonnull  
  | GCC_deprecated
  | GCC_visibility of string
      (* variable attributes *)
  | GCC_cleanup of string
  | GCC_common
  | GCC_nocommon
  | GCC_shared

and ceid = int
    
and ce = ce_tbl * ceid

exception Ce_exist of int

module HashCe =
struct
  type t = ce
  let equal (s1 : t) (s2 : t) = fst s1 == fst s2 & snd s1 = snd s2
  let hash (s : t) = Hashtbl.hash s
end
  
module CeHashtbl = Hashtbl.Make(HashCe)
