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

open Typ_ast
open Format
open Csize
module QNP = Qual_name_printer

let pp_print_t fm typ =
  let rec pp_print_primitive typ =
    let str = match typ with 
      | Void -> "void"
      | Void_Ptr -> "* void"
      | Char -> "char"
      | Signed_Char -> "signed char"
      | Unsigned_Char -> "unsigned char"
      | Short -> "short int"
      | Unsigned_Short -> "unsigned short int"
      | Int -> "int"
      | Unsigned_Int -> "unsigned int"
      | Long -> "long"
      | Unsigned_Long -> "unsigned long"
      | Long_Long -> "long long"
      | Unsigned_Long_Long -> "unsigned long long"
      | Float -> "float"
      | Double -> "double"
      | Long_Double -> "long double"
      | Bool -> "_Bool"
      | Complex -> "_Complex"
      | Float_Complex -> "float _Complex"
      | Double_Complex -> "double _Complex"
      | Long_Double_Complex -> "long double _Complex"
      | Va_List -> "__builtin_va_list"
      | Unknown -> "?"
    in
    pp_print_string fm str
      
  and pp_print_type_qualifier q =
    let str = match q with
      | Const -> "const"
      | Restrict -> "restrict"
      | Volatile -> "volatile"
    in
    pp_print_string fm str
      
  and pp_print_enum_item (n,v) =
    fprintf fm "%s = %d" (QNP.to_decl_str n) v

  and pp_print_field = function
    | Named (t, s, offset) ->
	QNP.pp_print_qn_decl_str fm s;
	pp_print_string fm ":";
	pp_print_string fm (string_of_csize offset);
	pp_print_string fm ":";
	pp_print_type t
	  
    | Unamed (t,offset) ->
	pp_print_string fm (string_of_csize offset);
	pp_print_string fm ":";
	pp_print_type t

  and pp_print_param = function
    | Labeled (t, s) ->
	QNP.pp_print_qn_decl_str fm s;
	pp_print_string fm ":";
	pp_print_primary_typ t
	  
    | Unlabeled t ->
	pp_print_primary_typ t
	  
  and pp_print_function_type = function
    | Fixed (typs, typ) ->
	Mlite_printer.pp_print_list fm 
	  (fun fm v -> pp_print_param v)
	  (fun fm -> pp_print_string fm "->")
	  typs;
	pp_print_string fm "->";
	pp_print_param typ
    | Varadic (typs, typ) ->
	Mlite_printer.pp_print_list fm 
	  (fun fm v -> pp_print_param v)
	  (fun fm -> pp_print_string fm "->")
	  typs;
	pp_print_string fm "->...->";
	pp_print_param typ
	
  and pp_print_c_type_decl = function
    | Struct_decl (s, size, align, typs) ->
	fprintf fm "%s:%s:%s {" (QNP.to_decl_str s) 
	  (string_of_csize size)  (string_of_csize align);
	Mlite_printer.pp_print_list fm 
	  (fun fm v -> pp_print_field v)
	  (fun fm -> pp_print_string fm ";";
	    pp_print_space fm ())
	  typs;
	pp_print_string fm "}"

    | Union_decl (s, size, align, typs) ->
	fprintf fm "%s:%s:%s {" (QNP.to_decl_str s) 
	  (string_of_csize size) (string_of_csize align);
	Mlite_printer.pp_print_list fm 
	  (fun fm v -> pp_print_field v)
	  (fun fm -> pp_print_string fm ";";
	    pp_print_space fm ())
	  typs;
	pp_print_string fm "}"
	  
    | Enum_decl (s, items) ->
	fprintf fm "%s {" (QNP.to_decl_str s);
	Mlite_printer.pp_print_list fm 
	  (fun fm v -> pp_print_enum_item v)
	  (fun fm -> pp_print_string fm ";";
	    pp_print_space fm ())
	  items;
	pp_print_string fm "}"
	  
    | Typedef (typ, s) ->
	pp_print_string fm "typedef";
	pp_print_space fm ();
	pp_print_type typ;
	pp_print_space fm ();
	QNP.pp_print_qn_decl_str fm s
	  
  and pp_print_primary_typ v = 
    pp_open_box fm 0;
    let _ = match v with
      | Primitive primitive -> pp_print_primitive primitive
      | Struct str -> fprintf fm "%s" (QNP.to_decl_str str)
      | Union str -> fprintf fm "%s" (QNP.to_decl_str str)
      | Enum str -> fprintf fm "%s" (QNP.to_decl_str str)
      | Typename str -> QNP.pp_print_qn_decl_str fm str
      | Primary typ -> 
	  pp_print_string fm "(";
	  pp_print_type typ;
	  pp_print_string fm ")"
	    
      | Bits (typ, i, lsb, mask) ->
	  pp_print_primary_typ typ;
	  pp_print_string fm ":";
	  pp_print_string fm (string_of_csize i);
	  pp_print_string fm ":";
	  pp_print_string fm (string_of_csize lsb);
	  pp_print_string fm ":";
	  pp_print_string fm mask
	  
      | v -> pp_print_type v
    in
    pp_close_box fm ()
      
  and pp_print_type v = 
    pp_open_box fm 0;
    let _ = match v with
      | Primitive typ ->
	  pp_print_primitive typ
	    
      | Pointer typ -> 
	  pp_print_string fm "*"; 
	  pp_print_space fm ();
	  pp_print_primary_typ typ
	    
      | Array (typ, i) -> 
	  pp_print_string fm ("[" ^ (string_of_csize i) ^ "]");
	  pp_print_primary_typ typ;
	    
      | Xarray typ -> 
	  pp_print_string fm "[]";
	  pp_print_primary_typ typ;
	    
      | Darray typ -> 
	  pp_print_string fm "[]";
	  pp_print_primary_typ typ;

      | Function function_type -> 
	  pp_print_function_type function_type

      | Qual (typ, q) -> 
	  pp_print_type_qualifier q;
	  pp_print_space fm ();
	  pp_print_primary_typ typ

      | Type_decl c_type_decl ->
	  pp_print_c_type_decl c_type_decl
	    
      | v -> pp_print_primary_typ v
    in
    pp_close_box fm ()
  in
  pp_print_type typ
