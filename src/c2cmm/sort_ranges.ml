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

type range_list = (int64 * int64) list
    
    
(* i_start = inclusive start, and i_end = inclusive end *)
let add_range (l:range_list) ~(i_start:int64) ~(i_end:int64) : range_list = 
  (i_start, i_end)::l
    
    
let sort_range_list (l:range_list) = 
  let cmp (i_start0, i_end0) (i_start1, i_end1) =
    if i_start0 = i_start1 then
      begin
	assert (i_end0 = i_end1);
	0
      end
    else if i_start0 < i_start1 then
      begin
	assert (i_end0 < i_end1);
	-1
      end
    else (* i_start0 > i_start1 *)
      begin
	assert (i_end0 > i_end1);
	1
      end
  in List.sort cmp l
  

let unused_range_list (l:range_list) ~(min:int64) ~(max:int64) : range_list = 
  let sorted_list = sort_range_list l
  in
  let (ulist, cur_min) = 
    List.fold_left
      (fun (ulist, cur_min) (a,b) ->
	if (cur_min < a) then
	  ((cur_min, Int64.sub a 1L)::ulist, Int64.add b 1L)
	else if (cur_min = a) then
	  (ulist, Int64.add b 1L)
	else 
	  assert false
      ) ([], min) sorted_list
  in
  let ulist = 
    if (cur_min <= max) then
      (cur_min, max)::ulist
    else
      ulist
  in sort_range_list ulist
