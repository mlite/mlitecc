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

module type IntSetType =
  sig
    type t
    val is_empty: t -> bool
    val elements: t -> int list
    val singleton: int -> t
    val empty: unit -> t
    val add: int -> t -> t
    val addmore: int list -> t -> t
    val union: t -> t -> t
    val diff: t -> t -> t
    val iter: (int -> unit) -> t -> unit
  end


module type Int2SetType =
  sig
    type t
    val is_empty: t -> bool
    val elements: t -> (int * int) list
    val singleton: int * int -> t
    val empty: unit -> t
    val add: int * int -> t -> t
    val addmore: (int * int) list -> t -> t
    val union: t -> t -> t
    val diff: t -> t -> t
    val iter: (int * int -> unit) -> t -> unit
  end


(*
module type Int3SetType =
  sig
    type t
    val is_empty: t -> bool
    val elements: t -> (Int.contour_id * Int.addr * Int.size) list
    val singleton: Int.contour_id * Int.addr * Int.size -> t
    val empty: unit -> t
    val add: Int.contour_id * Int.addr * Int.size -> t -> t
    val addmore: (Int.contour_id * Int.addr * Int.size) list -> t -> t
    val union: t -> t -> t
    val diff: t -> t -> t
    val iter: (Int.contour_id * Int.addr * Int.size-> unit) -> t -> unit
  end



module type Int4SetType =
  sig
    type t
    val is_empty: t -> bool
    val elements: t -> (Int.contour_id * Int.addr * Int.offset * Int.size) list
    val singleton: Int.contour_id * Int.addr * Int.offset * Int.size -> t
    val empty: unit -> t
    val add: Int.contour_id * Int.addr * Int.offset * Int.size -> t -> t
    val addmore: (Int.contour_id * Int.addr * Int.offset * Int.size) list -> t 
      -> t
    val union: t -> t -> t
    val diff: t -> t -> t
    val iter: (Int.contour_id * Int.addr * Int.offset * Int.size-> unit) -> t 
      -> unit
  end
*)


module type Int5SetType =
  sig
    type t
    val is_empty: t -> bool
    val elements: t -> 
	  (Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size) list
    val singleton: 
	Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size -> t
    val empty: unit -> t
    val add: Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size -> t -> t
    val addmore: 
	(Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size) list -> t 
	  -> t
    val union: t -> t -> t
    val diff: t -> t -> t
    val iter: 
	(Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size-> unit) -> t 
      -> unit
  end
