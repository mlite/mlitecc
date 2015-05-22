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

let std_indent = "  "

let delete_spaces row_txt =
   let txt = ref "" in
   let _ = String.iter
        (function 
        '\r' | '\n' | ' ' -> ()
        | c -> 
            txt := !txt ^ (Char.escaped c)
        ) row_txt
        in
          !txt

let lst2str f lst del = 
  List.fold_left 
    (fun a b ->
      if a = "" then 
        f b
      else
        a ^ del ^ (f b)
    )
    "" lst
			       
			       
let rec indent n = 
  if n > 0 then "    " ^ (indent (n-1))
  else ""


let array2str f ary del (col, indent) = 
    let cnt = ref 0 in 
    let idx = ref 0 in
    Array.fold_left
    (fun x a -> 
     if x = "" then 
       begin
       let _ = incr cnt 
       and ret =  f !idx a  
       and _ = incr idx in
       ret
    end
     else if !cnt = col then 
       let _ = cnt := 1
       and ret = x ^ del ^ "\n" ^ indent ^ (f !idx a) 
       and _ = incr idx
       in ret
     else
       let _  = incr cnt 
       and ret = x ^ del ^ (f !idx a) 
       and _ = incr idx
       in ret
    )
    "" ary


let list_to_str f ary del (col, indent) =
    let cnt = ref 0 in
    let idx = ref 0 in
    List.fold_left
    (fun x a ->
     if x = "" then
       begin
       let _ = incr cnt
       and ret =  f !idx a
       and _ = incr idx in
       ret
    end
     else if !cnt = col then
       let _ = cnt := 1
       and ret = x ^ del ^ "\n" ^ indent ^ (f !idx a)
       and _ = incr idx
       in ret
     else
       let _  = incr cnt
       and ret = x ^ del ^ (f !idx a)
       and _ = incr idx
       in ret
    )
    "" ary



let trim_suffix: string -> string =
  fun str ->
    try 
      String.sub str 0 ((String.length str) - (String.rindex str '.'))
    with
      Not_found -> 
	str

let trim_ll_suffix: string -> string = 
  fun str ->
    let str = 
      if String.contains str 'L' then
	String.sub str 0 (String.index str 'L')
      else str
    in
    let str = 
      if String.contains str 'l' then
	String.sub str 0 (String.index str 'l')
      else str
    in str

let trim_llu_suffix: string -> string = 
  fun str ->
    let str = trim_ll_suffix str
    in
    let str = 
      if String.contains str 'U' then
	String.sub str 0 (String.index str 'U')
      else str
    in
    let str = 
      if String.contains str 'u' then
	String.sub str 0 (String.index str 'u')
      else str
    in str
      
let trim_plus_sign: string -> string = 
  fun str ->
    match String.get str 0 with
      | '+' -> String.sub str 1 ((String.length str) - 1)
      | _ -> str


let extract_str: string option -> string =
  fun str ->
    match str with
    |Some v -> v
    |None -> assert false

let extract_int: string option -> int =
  fun str -> 
     match str with
    | Some v -> int_of_string v
    | None -> assert false


let extract_n_string: string option array -> int -> string = 
  fun a i ->
    let str = Array.get a i 
    in
    match str with
    | Some v -> v
    | None -> assert false

let extract_n_int: string option array -> int -> int = 
  fun a i ->
    let str = Array.get a i 
    in
    match str with
    | Some v -> int_of_string v
    | None -> assert false
    


type sfm = {
    buffer: Buffer.t;
    formatter: Format.formatter;
  }

let create_sfm: int -> sfm =
  fun size ->
    let str_buffer = Buffer.create size
    in
    let fm  = Format.formatter_of_buffer str_buffer
    in
    { buffer = str_buffer;
      formatter = fm;
    }

let flush_sfm: sfm -> string = 
  fun sfm ->
    let _ = Format.pp_print_flush sfm.formatter ()
    in
    Buffer.contents sfm.buffer

let pp_print_str_pp_print_pos: Format.formatter -> string -> int -> unit =
  fun fm fname lnum -> 
    Format.pp_open_box fm 0;
    Format.fprintf fm "%s[%d]:>" fname lnum;
    Format.pp_close_box fm ();
    Format.pp_print_flush fm ()
