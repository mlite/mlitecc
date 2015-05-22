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

include Int_int

module HashContour =
  struct
    type t = contour_id
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module ContourHashtbl = Hashtbl.Make(HashContour)


module HashConst_id =
  struct
    type t = const_id
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Const_idHashtbl = Hashtbl.Make(HashConst_id)


module HashStack_addr =
  struct 
    type t = stack_addr
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Stack_addrHashtbl = Hashtbl.Make(HashStack_addr)


module HashHeapaddr =
  struct 
    type t = heap_addr
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module HeapaddrHashtbl = Hashtbl.Make(HashHeapaddr)


module HashSta_proc_addr =
  struct 
    type t = sta_proc_addr
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Sta_proc_addrHashtbl = Hashtbl.Make(HashSta_proc_addr)


module HashOffset =
  struct 
    type t = offset
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module OffsetHashtbl = Hashtbl.Make(HashOffset)


module HashTmp =
  struct
    type t = tmp
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module TmpHashtbl = Hashtbl.Make(HashTmp)


module HashType_id =
  struct
    type t = type_id
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Type_idHashtbl = Hashtbl.Make(HashType_id)



module HashAbs_set =
  struct
    type t = abs_set
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Abs_setHashtbl = Hashtbl.Make(HashAbs_set)



module HashMeta_tok_id =
  struct
    type t = meta_tok_id
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Meta_tok_idHashtbl = Hashtbl.Make(HashMeta_tok_id)



module HashMeta_tok_set =
  struct
    type t = abs_set
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module Meta_tok_setHashtbl = Hashtbl.Make(HashMeta_tok_set)
