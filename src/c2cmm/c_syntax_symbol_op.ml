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

open C_syntax_symbol


module HashCStrLit =
  struct
    type t = c_string_literal
    let equal (s1 : t) (s2 : t) = 
      match s1 with
	| String_literal s1 ->
	    begin
	      match s2 with
		| String_literal s2 ->
		    String.compare s1 s2 = 0
		| WString_literal s2 -> false
	    end
	| WString_literal s1 ->
	    begin
	      match s2 with
		| String_literal s2 -> false
		| WString_literal s2 ->
		    (List.length s1 = List.length s2)
		    & (List.for_all2 (fun a1 a2 -> a1 = a2) s1 s2)
	    end
    let hash (s : t) = Hashtbl.hash s
  end
    
module CStrLitHashtbl = Hashtbl.Make(HashCStrLit)
module CStrLitHashmap = Hashmap.Make(CStrLitHashtbl)

  
let parse_int_const str suffix = 
  let parse f base str = 
    let last_char = (String.length str - 1)
    and factor = ref (Big_int.unit_big_int)
    and num = ref (Big_int.zero_big_int)
    in
    for i = last_char downto 0 do
      let digit = Big_int.big_int_of_int (f (String.get str i))
      in
      let _ = num := 
	Big_int.add_big_int !num 
	  (Big_int.mult_big_int !factor digit)
      in
      factor := Big_int.mult_int_big_int base !factor
    done;
    !num
  and hex = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a'
    | 'A' -> 10
    | 'b' 
    | 'B' -> 11
    | 'c' 
    | 'C' -> 12
    | 'd' 
    | 'D' -> 13
    | 'e' 
    | 'E' -> 14
    | 'f' 
    | 'F' -> 15
    | _ -> assert false
  and dec = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> assert false
  and oct = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | _ -> assert false
  in
  match suffix with
    | DEC_NONE
    | DEC_U
    | DEC_L
    | DEC_UL
    | DEC_LL
    | DEC_ULL -> 
	parse dec 10 str
    | HEX_NONE
    | HEX_U
    | HEX_L
    | HEX_UL
    | HEX_LL
    | HEX_ULL -> (* 0x... *)
	let i = 
	  try
	    String.index str 'x'
	  with
	    | Not_found ->
		String.index str 'X'
	and len = String.length str
	in
	let hexstr = String.sub str (i+1) (len - i - 1)
	in
	parse hex 16 hexstr
    | OCT_NONE
    | OCT_U
    | OCT_L
    | OCT_UL
    | OCT_LL
    | OCT_ULL -> (* 0... *)
	let len = String.length str
	in
	let octstr = String.sub str 1 (len - 1)
	in
	parse oct 8 octstr


exception Found

let cval_of_int_const (Constant (str, suffix)) =      
  let bigint = parse_int_const str suffix
  in
  let lst = ref [] in
  let _ = 
    try
      Safe_list.iter
	(fun (v, l) ->
	  if (v = suffix) then
	    let _ = lst := l
	    in raise Found
	) Mach.integer_range_type_table
    with 
      | Found  -> ()
  in
  let tid = ref Mach.cnon_id
  and cval = ref (Const_folding.cchar_cval_of_string "0")
  in
  let _ = 
    try
      Safe_list.iter
	(fun (max, id, f) ->
	  if Big_int.le_big_int bigint max then
	    let _ = tid := id 
	    and _ = cval := f str
	    in 
	    raise Found
	) !lst
    with
      | Found -> ()
  in (!tid, !cval)
