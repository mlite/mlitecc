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

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
    val pp_print: Format.formatter -> t -> unit
  end
      
      
module type S =
  sig
    type elt
    type t
    val create: unit -> t
    val add: t -> elt -> unit
    val emit: t -> elt list
  end


module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t
    type t = element Stack.t
    and element = {
	data_list: elt list;
	occurrence: int;
      }

    let pp_print = Ord.pp_print
    let create () = Stack.create ()
      

    (* a > b : compare a b > 0 *)
    (* a = b : compare a b = 0 *)
    (* a < b : compare a b < 0 *)
    let compare: element -> element -> int = 
      fun lhs rhs ->
	let compare_lst l0 l1 =
	  match l0 with
	  | a::l0' ->
	      begin
		match l1 with
		| b::l1' ->
		    begin
		      let cmp = Ord.compare a b
		      in
		      if cmp = 0 then
			compare l0' l1'
		      else
			cmp
		    end
		| [] -> 1
	      end
	  | [] -> 
	      begin
		match l1 with
		| b::l1' -> (-1)
		| [] -> 0
	      end
	in
	compare_lst lhs.data_list rhs.data_list
	  
	  
    let new_elmt: elt -> element =
      fun a ->
	{ data_list = [a]; occurrence = 1; }
	  
    let concat: element -> element -> element = 
      fun fst snd ->
	assert (fst.occurrence = 1 or snd.occurrence = 1);
	{ fst with data_list = fst.data_list @ snd.data_list; }
	  
    let pop: t -> int -> element option =
      fun stack n ->
	if Stack.length stack >= n then
	  begin
	    let elmt = ref { data_list = []; occurrence = 1; }
	    and saved = Stack.create ()
	    in
	    while (List.length !elmt.data_list) < n do
	      let top = Stack.pop stack
	      in
	      let _ = Stack.push top saved
	      in 
	      elmt :=  concat top !elmt
	    done;
	    if (List.length !elmt.data_list) = n then
	      Some !elmt
	    else
	      begin
		let _ = Stack.iter (fun v -> Stack.push v stack) saved
		in
		None
	      end
	  end
	else
	  None


    let push: element -> t -> unit =
      fun elmt stack ->
	assert (elmt.occurrence = 1);
	List.iter
	  (fun v ->
	    let ne = { data_list = [v]; occurrence = 1; }
	    in
	    Stack.push ne stack
	  ) elmt.data_list


    let size: t -> int = 
      fun stack ->
	let cnt = ref 0
	in
	let _ = Stack.iter 
	    (fun e -> cnt := List.length e.data_list + !cnt) 
	    stack
	in
	!cnt
	  
    let add: t -> elt -> unit =
      fun stack a ->
	if Stack.is_empty stack then
	  Stack.push {data_list = [a]; occurrence = 1;} stack
	else
	  begin
	    let ne = ref { data_list = [a]; occurrence = 1}
	    and finished = ref false
	    in
	    while (List.length !ne.data_list <= size stack) & not !finished do
	      let top_opt = pop stack (List.length !ne.data_list)
	      in
	      match top_opt with
	      | Some top ->
		  if compare !ne top = 0 then
		    let _ = finished := true
		    in 
		    Stack.push {top with occurrence = top.occurrence + 1} stack
		  else
		    ne := concat top !ne
	      | None -> 
		  push !ne stack
	    done;
	    if not !finished then
	      push !ne stack
	  end

    let emit: t -> 'a list = 
      fun stack ->
	let lst = ref []
	in
	let _ = Stack.iter
	    (fun v -> lst := v.data_list @ !lst ) stack
	in
	!lst
  end
