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

open Int

include Ud_set_types

module IntElmt = 
  struct
    type t = int
    let compare  s10 s20 = 
      s10 - s20
  end

module IntSet' = Set.Make(IntElmt)


module IntSet:IntSetType =
  struct
    type t = IntSet'.t
    let is_empty = IntSet'.is_empty
    let elements = IntSet'.elements 
    let singleton = IntSet'.singleton
    let empty () = IntSet'.empty
    let add = IntSet'.add
    let addmore elmts t =
       let n_t = ref t 
       in
       List.iter
          (fun e ->
             n_t :=  add e !n_t
          )elmts;
        !n_t

    let union = IntSet'.union
    let diff = IntSet'.diff
    let iter = IntSet'.iter
  end


module Int2Elmt = 
  struct
    type t = int * int
    let compare  (s10, s11) (s20, s21) = 
      (s10 - s20) + (s11 - s21)
  end

module Int2Set' = Set.Make(Int2Elmt)


module Int2Set:Int2SetType =
  struct
    type t = Int2Set'.t
    let is_empty = Int2Set'.is_empty
    let elements = Int2Set'.elements 
    let singleton = Int2Set'.singleton
    let empty () = Int2Set'.empty
    let add = Int2Set'.add
    let addmore elmts t =
       let n_t = ref t 
       in
       List.iter
          (fun e ->
             n_t :=  add e !n_t
          )elmts;
        !n_t

    let union = Int2Set'.union
    let diff = Int2Set'.diff
    let iter = Int2Set'.iter
  end


(*
module Int3Elmt = 
  struct
    type t = Int.contour_id * Int.addr * Int.size
    let compare  (a0, b0, c0) (a1, b1, c1) = 
      (t_int (sub a0 a1)) + (t_int (sub b0 b1)) + (t_int (sub c0 c1))
  end

module Int3Set' = Set.Make(Int3Elmt)


module Int3Set:Int3SetType =
  struct
    type t = Int3Set'.t
    let is_empty = Int3Set'.is_empty
    let elements = Int3Set'.elements 
    let singleton = Int3Set'.singleton
    let empty () = Int3Set'.empty
    let add = Int3Set'.add
    let addmore elmts t =
       let n_t = ref t 
       in
       List.iter
          (fun e ->
             n_t :=  add e !n_t
          )elmts;
        !n_t

    let union = Int3Set'.union
    let diff = Int3Set'.diff
    let iter = Int3Set'.iter
  end




module Int4Elmt = 
  struct
    type t = Int.contour_id * Int.addr * Int.offset * Int.size
    let compare  (a0, b0, c0, d0) (a1, b1, c1, d1) = 
      (t_int (sub a0 a1)) + (t_int (sub b0 b1)) + (t_int (sub c0 c1))
	+ (t_int (sub d0 d1))
  end

module Int4Set' = Set.Make(Int4Elmt)


module Int4Set:Int4SetType =
  struct
    type t = Int4Set'.t
    let is_empty = Int4Set'.is_empty
    let elements = Int4Set'.elements 
    let singleton = Int4Set'.singleton
    let empty () = Int4Set'.empty
    let add = Int4Set'.add
    let addmore elmts t =
       let n_t = ref t 
       in
       List.iter
          (fun e ->
             n_t :=  add e !n_t
          )elmts;
        !n_t

    let union = Int4Set'.union
    let diff = Int4Set'.diff
    let iter = Int4Set'.iter
  end

*)


module Int5Elmt = 
  struct
    type t = Int.tok_cat_id * Int.contour_id * Int.generic_addr * Int.offset * Int.size
    let compare  (t0, a0, b0, c0, d0) (t1, a1, b1, c1, d1) = 
      (t_int (sub t0 t1)) + (t_int (sub a0 a1)) + (t_int (sub b0 b1)) 
	+ (t_int (sub c0 c1)) + (t_int (sub d0 d1))
  end

module Int5Set' = Set.Make(Int5Elmt)


module Int5Set:Int5SetType =
  struct
    type t = Int5Set'.t
    let is_empty = Int5Set'.is_empty
    let elements = Int5Set'.elements 
    let singleton = Int5Set'.singleton
    let empty () = Int5Set'.empty
    let add = Int5Set'.add
    let addmore elmts t =
       let n_t = ref t 
       in
       List.iter
          (fun e ->
             n_t :=  add e !n_t
          )elmts;
        !n_t

    let union = Int5Set'.union
    let diff = Int5Set'.diff
    let iter = Int5Set'.iter
  end
