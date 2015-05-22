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

include Ustr_str

module HashSta_proc_name =
  struct
    type t = sta_proc_name
    let equal s1 s2 = s1 = s2
    let hash s = Hashtbl.hash s
  end

module Sta_proc_nameHashtbl = Hashtbl.Make(HashSta_proc_name)

module Sta_proc_nameElmt =
  struct
    type t = sta_proc_name
    let compare v0 v1 = String.compare (t_string v0) (t_string v1)
  end

module Sta_proc_nameSet = Set.Make(Sta_proc_nameElmt)

(*
module HashAddr =
  struct 
    type t = addr
    let equal s1 s2 = s1 = s2
    let hash s = t_int s
  end

module AddrHashtbl = Hashtbl.Make(HashAddr)



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

*)
