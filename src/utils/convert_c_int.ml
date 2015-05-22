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

let suffix = ['L';'U']

let max_int_string  = string_of_int (max_int)
let max_int32_string = Int32.to_string (Int32.max_int)
let max_int64_string = Int64.to_string (Int64.max_int)	

let cut_suffix: c_int:string -> char -> string =
  fun ~c_int ch ->
    if String.contains c_int ch then
      let pos = String.index c_int ch
      in
      let str = String.sub c_int 0 pos
      in
      str
    else
      c_int
	
let rec convert: c_int:string -> int =
  fun ~c_int ->
    if !Mlite_config.enable_log then
      begin
	print_string "convert ";
	print_string c_int;
	print_string " to int";
	print_newline ();
      end;
    let c_int = ref c_int
    in
    List.iter
      (fun suffix ->
	c_int := cut_suffix !c_int suffix
      ) suffix;
    if !Mlite_config.enable_log then
      begin
	print_string "convert ";
	print_string !c_int;
	print_string " to int";
	print_newline ();
      end;
    int_of_string !c_int


let rec to_int64: c_int:string -> int64 =
  fun ~c_int ->
    if !Mlite_config.enable_log then
      begin
	print_string "convert ";
	print_string c_int;
	print_string " to int";
	print_newline ();
      end;
    let c_int = ref c_int
    in
    List.iter
      (fun suffix ->
	c_int := cut_suffix !c_int suffix
      ) suffix;
    if !Mlite_config.enable_log then
      begin
	print_string "convert ";
	print_string !c_int;
	print_string " to int";
	print_newline ();
      end;
    Int64.of_string !c_int
      

let int64_max: int64 -> int64 -> int64 =
    fun x y ->
       if x>= y then x
       else y
