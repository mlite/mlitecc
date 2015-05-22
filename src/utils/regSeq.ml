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

type elt = string
type t = group Stack.t
and group =
  | Singleton of elt
  | RepeatSequence of group list * int
  | Epsilon

let create = Stack.create 

    (* a > b : compare a b > 0 *)
    (* a = b : compare a b = 0 *)
    (* a < b : compare a b < 0 *)
let compare = String.compare



let incr_occurrance = function
  | Epsilon -> Epsilon
  | Singleton elt -> RepeatSequence ([Singleton elt], 2)
  | RepeatSequence (lst, cnt) -> RepeatSequence (lst, cnt+1)
	
let rec pp_print_group: formatter -> group -> unit = 
  fun fm g ->
    match g with
    | Epsilon -> pp_print_string fm "\\epsilon"
    | Singleton elt -> pp_print_string fm elt
    | RepeatSequence (lst, cnt) ->
	pp_open_box fm 0;
	pp_print_string fm "{";
	Mlite_printer.pp_print_list fm
	  pp_print_group
	  (fun fm -> pp_print_string fm ".")
	  lst;
	pp_print_string fm ("}_" ^ (string_of_int cnt));
	pp_close_box fm ()


let rec express_list: fst:group list -> snd:group list -> bool = 
  fun ~fst:g0s  ~snd:g1s ->
    let rec express_groups: group -> group list -> bool = 
      fun g0 g1s ->
	match g1s with
	| [] -> false
	| [g1] -> express_group g0 g1
	| _ ->
	    begin
	      match g0 with
	      | Epsilon -> false
	      | Singleton _ -> false
	      | RepeatSequence (g0s, cnt) ->
		  if (List.length g0s) = (List.length g1s) then
		    List.for_all2 (fun u v -> equals_group u v) g0s g1s
		  else
		    false
	    end
    and express_group: group -> group -> bool = 
      fun g0 g1 ->
	match g0 with
	| Epsilon -> false
	| Singleton elt0 ->
	    begin
	      match g1 with
	      | Singleton elt1 -> (compare elt0 elt1) = 0
	      | _ -> false
	    end
	| RepeatSequence ([g0'], cnt) -> g0' = g1
	| RepeatSequence _ ->
	    express_groups g0 [g1]
    and equals_group: group -> group -> bool = 
      fun g0 g1 -> g0 = g1
    in
    match g0s with
    | [g0] -> 
	(* only top level allows regular expression match*)
	express_groups g0 g1s
    | _ ->
	if List.length g0s = List.length g1s then
	  List.for_all2( fun u v -> equals_group u v) g0s g1s
	else
	  false
	    
let add: t -> elt -> unit =
  fun stack a ->
    let rec size = function 
      | Epsilon -> 0
      | Singleton _ -> 1
      | RepeatSequence (lst, cnt) -> 
	  let i = ref 0 in
	  List.iter 
	    (fun v -> i := !i + (size v)) lst;
	  !i
    and total_size lst = 
      let s = ref 0
      in
      List.iter (fun v -> s := !s + size v) lst;
      !s
    and stack_size: t -> int =
      fun stack ->
	let s = ref 0
	in
	Stack.iter
	  (fun group -> s := !s + size group
	  ) stack;
	!s
    and top_most: t -> n:int -> group list = 
      fun stack ~n ->
	let lst = ref []
	in
	while total_size !lst < n do
	  let top = Stack.pop stack
	  in
	  lst := top::!lst
	done;
	List.iter
	  (fun v -> Stack.push v stack) !lst;
	if total_size !lst = n then
	  !lst
	else
	  []
    in
    if Stack.is_empty stack then
      Stack.push (Singleton a) stack
    else
      begin
	let ne = ref [(Singleton a)]
	and finished = ref false
	in
	while (total_size !ne <= stack_size stack) & not !finished do
	  let top_n = top_most stack (total_size !ne)
	  in
	  if express_list top_n !ne then
	    begin
	      List.iter (fun v -> ignore (Stack.pop stack)) top_n;
	      let top' =
		match top_n with
		| [top_1] -> incr_occurrance top_1
		| [] -> Epsilon
		| _ -> RepeatSequence (top_n, 2)
	      in
	      Stack.push top' stack;
	      finished := true;
	    end
	  else 
	    begin
	      let top = Stack.pop stack
	      in
	      ne := top::!ne
	    end
	done;
	if not !finished then
	  List.iter
	    (fun g -> Stack.push g stack) !ne
      end

let rec flatten_list_of = function
  | Epsilon -> []
  | Singleton e -> [e]
  | RepeatSequence (lst, cnt) ->
      let l = ref []
      in
      List.iter 
	(fun c -> l := !l @ (flatten_list_of c)) lst;
      !l
      
let emit: t -> elt list = 
  fun stack ->
    let lst = ref []
    in
    let _ = Stack.iter
	(fun v -> lst := (flatten_list_of v) @ !lst) stack
    in
    !lst


let convert: t -> elt list -> elt list = 
  fun t elts ->
    List.iter (add t) elts;
    emit t
      
let add_all: t -> elt list -> unit = 
  fun t elts ->
    List.iter (add t) elts
      
let pp_print_t: formatter -> t -> unit = 
  fun fm t ->
    let lst = ref []
    in
    Stack.iter
      (fun v -> 
	lst := v::!lst) t;
    List.iter
      (fun v -> pp_print_group fm v) !lst
