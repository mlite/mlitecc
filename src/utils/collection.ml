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

open Format

exception Out_of_bound of int * int

module HashString =
  struct
    type t = string
    let equal (s1 : t) (s2 : t) = s1 = s2
    let hash (s : t) = Hashtbl.hash s
  end
    
module StringHashtbl = Hashtbl.Make(HashString)

module StringHashmap = Hashmap.Make(StringHashtbl)


module HashInt = 
  struct 
    type t = int
    let equal (s1:t) (s2:t) = s1 = s2
    let hash (s:t) = s
  end

module IntHashtbl = Hashtbl.Make(HashInt)

module IntHashmap = Hashmap.Make(IntHashtbl)


module HashInt2 = 
  struct 
    type t = int * int
    let equal (a0, b0) (a1, b1) = (a0 = a1) & (b0 = b1)
    let hash (a0, b0) = a0 * 10 + b0
  end

module Int2Hashtbl = Hashtbl.Make(HashInt2)


module HashInt3 = 
  struct 
    type t = int * int * int
    let equal (a0, b0, c0) (a1, b1, c1) = (a0 = a1) & (b0 = b1) & (c0 = c1)
    let hash (a0, b0, c0) = a0 * 100 + b0 * 10 + c0
  end

module Int3Hashtbl = Hashtbl.Make(HashInt3)


module IntElmt = 
  struct 
    type t = int
    let compare  (s1:t) (s2:t) = s1 - s2
  end

module IntSet = Set.Make(IntElmt)

let pp_print_intset: formatter -> IntSet.t -> unit = 
  fun fm t ->
    pp_open_hvbox fm 0;
    IntSet.iter
      (fun v ->
	pp_print_int fm v;
	pp_print_space fm ();
      ) t;
    pp_close_box fm ()

let print_intset: IntSet.t -> unit = 
  fun t ->
    let txt = camlp4_macro_str_pp_print
	(fun fm -> pp_print_intset fm t)
    in
    print_string txt
      (*
    Format.open_box 0;
    IntSet.iter
      (fun v ->
	Format.print_int v;
	Format.print_space ();
      ) t;
    Format.close_box ()
	 *)

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)


exception Done


class ['a] vect def_val' (base_size', seg_size') init_cnt' =
  let _ = assert (init_cnt' >= 0) in
  object(self)
    val def_val = def_val'
    val base_size = base_size'
    val seg_size  = 1 
    val base_buffer = Array.make base_size' def_val'
    val seg_list = ref []
    val seg_count = ref 0
    val elmt_count = ref init_cnt'
	
    method length: unit -> int = 
      fun () ->
	!elmt_count


    method private get_seg: int -> 'a array * int = 
      fun idx ->
	let idx' = idx - base_size in 
	let seg_num = idx' / seg_size 
	and seg_idx = idx' mod seg_size
	in
	let more_seg_num = seg_num - !seg_count + 1 
	in 
	if more_seg_num > 0 then
	  begin
	    let _ = seg_count := !seg_count + more_seg_num 
	    in
	    for i = 0 to (more_seg_num - 1) do
	      seg_list := !seg_list @ [Array.make seg_size def_val]
	    done
	  end;
	try
	  let seg = List.nth !seg_list seg_num in
	  (seg, seg_idx)
	with
	  Failure _ -> 
	    Printf.fprintf stderr "seg_list length %d seg_num %d\n" (List.length !seg_list)
	      seg_num;
	    assert false

    method expand: int -> unit = 
      fun size ->
	for i = 0 to (size - 1) do
	  let _ = self#get_seg !elmt_count
	  in incr elmt_count 
	done

    method set: int -> 'a -> unit = 
      fun idx elmt ->
	let _ = elmt_count := max (idx+1) !elmt_count in
	if idx < base_size then 
	  Array.set base_buffer idx elmt
	else
	  let (seg, seg_idx) = self#get_seg idx in 
	  Array.set seg seg_idx elmt
	    
	    
    method update_get: int -> 'a = 
      fun idx ->
	let _ = elmt_count := max idx !elmt_count in
	if idx < base_size then 
	  Array.get base_buffer idx 
	else
	  let (seg, seg_idx) = self#get_seg idx in 
	  Array.get seg seg_idx



    method get: int -> 'a = 
      fun idx ->
	if idx < base_size then 
	  Array.get base_buffer idx 
	else
	  let idx' = idx - base_size in 
	  let seg_num = idx' / seg_size 
	  and seg_idx = idx' mod seg_size
	  in
	  if seg_num >= !seg_count then
	    raise (Out_of_bound (!elmt_count, idx))
	  else
	    try
	      let seg = List.nth !seg_list seg_num in
	      Array.get seg seg_idx 
	    with
	      Failure _ -> 
		Printf.fprintf stderr "get: seg_count %d seg_list length %d seg_num %d\n" 
		  !seg_count
		  (List.length !seg_list)
		  seg_num;
		assert false

    method iteri: (int -> 'a -> unit) -> unit = 
      fun fn ->
        if !elmt_count <= base_size then
	  for i = 0 to (!elmt_count - 1) do
	    let e = Array.get base_buffer i in
	    fn i e
	  done
	else
	  begin
	    Array.iteri fn base_buffer;
	    let seg_num = (!elmt_count  - base_size) / seg_size
	    and remind  = (!elmt_count  - base_size) mod seg_size
	    and count   = ref 0
	    and baseidx = ref base_size
	    in
	    try 
	      List.iter
		(fun seg_array -> 
		  begin
		    if !count = seg_num then
		      for i = 0 to (remind - 1) do
			let e = Array.get seg_array i in
			fn !baseidx e;
			incr baseidx
		      done
		    else if !count < seg_num then
		      for i = 0 to (seg_size - 1) do
			let e = Array.get seg_array i in 
			fn !baseidx e;
			incr baseidx
		      done;
		    incr count;
		    if !baseidx >= !elmt_count then 
		      raise Done;
		  end
		)
		!seg_list
	    with
	      Done -> ()
	  end
  end
