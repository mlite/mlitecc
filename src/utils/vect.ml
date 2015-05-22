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

exception Out_of_bound of (int * int)
exception Done

type 'a t = {
    def_val: 'a;
    base_size: int;
    seg_size: int;
    base_buffer: 'a array;
    seg_list:  ('a array) list ref;
    mutable seg_count: int;
    elmt_count: int ref;
  }



let init: int -> int -> 'a -> 'a t = 
  fun base_size init_cnt def_val ->
    {def_val = def_val;
     base_size = base_size;
     seg_size = 100;
     base_buffer = Array.make base_size def_val;
     seg_list = ref [];
     seg_count = 0;
     elmt_count = ref init_cnt;
   }

let make: int -> 'a -> 'a t =
  fun size def_val ->
    init size size def_val


let length: 'a t -> int = 
  fun t ->
    let i = t.elmt_count in
    !i


let clone: 'a t -> 'a t = 
  fun t ->
    let cloned = {
      def_val = t.def_val;
      base_size = t.base_size;
      base_buffer = Array.map (fun i -> i) t.base_buffer;
      seg_size = t.seg_size;
      seg_list = ref (List.map (fun x -> Array.map (fun i -> i) x) !(t.seg_list));
      seg_count = t.seg_count;
      elmt_count = ref !(t.elmt_count);
    }
    in cloned


(** private method *)
let get_seg: 'a t -> int -> 'a array * int = 
  fun t idx ->
    let idx' = idx - t.base_size in 
    let seg_num = idx' / t.seg_size 
    and seg_idx = idx' mod t.seg_size
    in
    let more_seg_num = seg_num - t.seg_count + 1 
    in 
    if more_seg_num > 0 then
      begin
	let _ = t.seg_count <- t.seg_count + more_seg_num 
	in
	for i = 0 to (more_seg_num - 1) do
	  t.seg_list := !(t.seg_list) @ [Array.make t.seg_size t.def_val]
	done
      end;
    try
      let seg = List.nth !(t.seg_list) seg_num in
      (seg, seg_idx)
    with
      Failure _ -> 
	Printf.fprintf stderr "seg_list length %d seg_num %d\n" 
            (List.length !(t.seg_list)) seg_num;
	assert false
	  

let expand: 'a t -> int -> unit = 
  fun t size ->
    for i = 0 to (size - 1) do
      let _ = get_seg t !(t.elmt_count)
      in incr t.elmt_count 
    done


let set: 'a t -> int -> 'a -> unit = 
  fun t idx elmt ->
    let _ = t.elmt_count := max (idx+1) !(t.elmt_count) in
    if idx < t.base_size then 
      Array.set t.base_buffer idx elmt
    else
      let (seg, seg_idx) = get_seg t idx in 
      Array.set seg seg_idx elmt


let append: 'a t -> 'a -> int = 
  fun t elmt ->
    let idx = !(t.elmt_count)
    in
    set t idx elmt;
    idx

let update_get: 'a t -> int -> 'a = 
  fun t idx ->
    let _ = t.elmt_count := max idx !(t.elmt_count) in
    if idx < t.base_size then 
      Array.get t.base_buffer idx 
    else
      let (seg, seg_idx) = get_seg t idx in 
      Array.get seg seg_idx



let get: 'a t -> int -> 'a = 
  fun t idx ->
    if idx < t.base_size then 
      Array.get t.base_buffer idx 
    else
      let idx' = idx - t.base_size in 
      let seg_num = idx' / t.seg_size 
      and seg_idx = idx' mod t.seg_size
      in
      if seg_num >= t.seg_count then
	raise (Out_of_bound (!(t.elmt_count), idx))
      else
	try
	  let seg = List.nth !(t.seg_list) seg_num in
	  Array.get seg seg_idx 
	with
	  Failure _ -> 
	    Printf.fprintf stderr "get: seg_count %d seg_list length %d seg_num %d\n" 
	      t.seg_count
	      (List.length !(t.seg_list))
	      seg_num;
	    assert false


	      
let iteri: (int -> 'a -> unit) -> 'a t -> unit = 
  fun fn t ->
    if !(t.elmt_count) <= t.base_size then
      for i = 0 to (!(t.elmt_count) - 1) do
	let e = Array.get t.base_buffer i in
	fn i e
      done
    else
      begin
	Array.iteri fn t.base_buffer;
	let seg_num = (!(t.elmt_count)  - t.base_size) / t.seg_size
	and remind  = (!(t.elmt_count)  - t.base_size) mod t.seg_size
	and count   = ref 0
	and baseidx = ref t.base_size
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
		  for i = 0 to (t.seg_size - 1) do
		    let e = Array.get seg_array i in 
		    fn !baseidx e;
		    incr baseidx
		  done;
		incr count;
		if !baseidx >= !(t.elmt_count) then 
		  raise Done;
	      end
	    )
	    !(t.seg_list)
	with
	  Done -> ()
      end


let map: ('a -> 'b) -> 'a t -> 'b t = 
  fun fn t ->
    let def_val = fn t.def_val 
    in
    {def_val = def_val;
     base_size = t.base_size;
     seg_size = t.seg_size;
     base_buffer = Array.make t.base_size def_val;
     seg_list = ref [];
     seg_count = t.seg_count;
     elmt_count = ref !(t.elmt_count);
   }
