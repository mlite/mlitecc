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
open Ast_da_type
open Pretty_printer

type c_file_unit = Ast_da_type.c_translation_unit
let description () = description
let suffix () = suffix

let indent = 2

let pp_print_c_comma = print_c_comma
let pp_print_c_semicolon: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ";"; 
    pp_print_space fm ()

let pp_print_c_newline: formatter -> unit = 
  fun fm ->
    pp_print_space fm ()
      
let pp_print_c_identifier: formatter -> c_identifier -> unit =
  fun fm (v) ->
    pp_open_box fm 0;
    pp_print_string fm v;
    pp_close_box fm ()

let pp_print_c_identifier_opt fm id =
  match id with
  | Some c_identifier -> 
      pp_print_space fm ();
      pp_print_c_identifier fm c_identifier
  | None -> ()

let pp_print_c_constant = Ast_ca_expr_printer.pp_print_c_constant
let pp_print_c_string_literal = Ast_aa_gram_printer.pp_print_c_string_literal
let pp_print_binary_arithmatic = C_semantics_symbol_printer.pp_print_binary_arithmatic
let pp_print_binary_predicate = C_semantics_symbol_printer.pp_print_binary_predicate
let pp_print_binary_logic_connect = C_semantics_symbol_printer.pp_print_binary_logic_connect
let pp_print_unary_arithmatic = C_semantics_symbol_printer.pp_print_unary_arithmatic

let pp_print_linkage: formatter -> linkage -> unit = 
  fun fm op ->
    pp_open_box fm 0;
    let str = match op with
      | Default_extern -> ""
      | Default -> ""
      | Extern -> "extern"
      | Extern_Inline -> "extern __inline"
      | Static_Inline -> "static __inline"
      | Static -> "static"
      | Auto -> "auto"
      | Register -> "register"
      | Inline -> "__inline"
      | Type_alias -> "typedef"
      | Extern_Thread -> "extern __thread"
      | Static_Thread -> "static __thread"
      | Thread -> "__thread"
    in
    if str <> "" then
      begin
	pp_print_string fm str;
	pp_print_space fm ();
      end;
    pp_close_box fm ()


let pp_print_c_type_qualifier: formatter -> type_qualifier -> unit = 
  fun fm op ->
    pp_open_box fm 0;
    let str = match op with
      | Const -> "const"
      | Restrict -> "restrict"
      | Volatile -> "volatile"
    in
    pp_print_string fm str;
    pp_close_box fm ()


(** *********************************************************************************** **)
let pp_print_l_parens fm str = 
  match str with
    | "__attribute__" -> 
	pp_print_string fm "((";
    | _ -> 
	pp_print_string fm "("


let pp_print_r_parens fm str = 
  match str with
    | "__attribute__" -> 
	pp_print_string fm "))";
    | _ -> 
	pp_print_string fm ")"
	


let rec pp_print_attribute: formatter -> gnu_attribute -> unit = 
  fun fm (str, expr_list) ->
    begin
      pp_print_string fm str;
      pp_print_l_parens fm str;
      Mlite_printer.pp_print_list fm 
	(pp_print_c_expression ~need_paren:false)
	(fun fm -> pp_print_space fm ())
	expr_list;
      pp_print_r_parens fm str;
    end
      
and pp_print_c_expr020: formatter -> need_paren:bool -> c_expr020 -> unit =
  fun fm ~need_paren expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| Comma (expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr0;
	      pp_print_string fm ",";
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")"
	    end
	| Constant c_constant -> pp_print_c_constant fm c_constant
	    
	| String c_string_literal -> 
	    pp_print_c_string_literal fm c_string_literal
	      
	| Call (expr, expr_list) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm " (";
	      Mlite_printer.pp_print_list fm 
		(fun fm e -> pp_print_c_expr020 fm ~need_paren:true e)
		pp_print_c_comma 
		expr_list;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	| Macro_va_start (expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__builtin_va_start";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr0;
	      pp_print_string fm ",";
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	| Macro_va_arg (expr, c_type) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__builtin_va_arg";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm ",";
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	| Macro_va_end expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__builtin_va_end";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false expr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end

	| Builtin_types_compatible (t0, t1) ->
	    begin
	      pp_print_string fm "__builtin_types_compatible_p";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_type fm (None, []) t0;
	      pp_print_string fm ",";
	      pp_print_c_type fm (None, []) t1;
	      pp_print_string fm ")"
	    end

	| Builtin_constant_p (e0) ->
	    begin
	      pp_print_string fm "__builtin_constant_p";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false e0;
	      pp_print_string fm ")"
	    end

	| Builtin_expect (e0, e1) ->
	    begin
	      pp_print_string fm "__builtin_expect";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expression ~need_paren:true fm e0;
	      pp_print_string fm ",";
	      pp_print_c_constant_expression fm ~need_paren:true e1;
	      pp_print_string fm ")"
	    end
	      
	| Gnu_block block ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__extension__({";
	      pp_print_space fm ();
	      pp_print_c_compound_stmt010 fm block;
	      pp_print_string fm "})";
	      if need_paren then pp_print_string fm ")"
	    end
	      
	| Gnu_labeladdr str -> pp_print_string fm ("&&" ^ str)

	| Variable str -> pp_print_string fm str
	    
	| Memberof (expr, f) -> 
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm ".";
	      pp_print_string fm f;
	      if need_paren then pp_print_string fm ")"
	    end
	      
	| Memberof_ptr (expr, f) -> 
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm "->";
	      pp_print_string fm f;
	      if need_paren then pp_print_string fm ")"
	    end
	      
	| Indexof (base_expr, index_expr) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true base_expr;
	      pp_print_string fm "[";
	      pp_print_c_expr020 fm ~need_paren:false index_expr;
	      pp_print_string fm "]";
	      if need_paren then pp_print_string fm ")"
	    end
	| Binary_arithm (binary_arithmatic, expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr0;
	      pp_print_space fm ();
	      pp_print_binary_arithmatic fm binary_arithmatic;
	      pp_print_space fm ();
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")"
	    end
	| Binary_predicate (binary_predicate, expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr0;
	      pp_print_space fm ();
	      pp_print_binary_predicate fm binary_predicate;
	      pp_print_space fm ();
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")"
	    end
	| Binary_logic (binary_logic_connect, expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr0;
	      pp_print_space fm ();
	      pp_print_binary_logic_connect fm binary_logic_connect;
	      pp_print_space fm ();
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")"
	    end
	| Unary_arithm (unary_arithmatic, expr) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_unary_arithmatic fm unary_arithmatic;
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Logic_not expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "!";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Sizeof_expr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "sizeof";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false expr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	| Sizeof_type (c_type, has_def) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "sizeof";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end

	| Alignof_expr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__alignof__";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false expr;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	      
	| Alignof_type (c_type, has_def) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "__alignof__";
	      pp_print_space fm ();
	      pp_print_string fm "(";
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_string fm ")";
	      if need_paren then pp_print_string fm ")"
	    end
	      
	| Cast (c_type, expr) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "(";
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_string fm ")";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")";
	    end
	| Assign (expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false expr0;
	      pp_print_string fm " = ";
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")";
	    end
	| Post_decr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm "--";
	      if need_paren then pp_print_string fm ")"
	    end
	| Post_incr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      pp_print_string fm "++";
	      if need_paren then pp_print_string fm ")"
	    end
	| Pre_decr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "--";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Pre_incr expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "++";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Assign_arithm (binary_arithmatic, expr0, expr1) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:false expr0;
	      pp_print_binary_arithmatic fm binary_arithmatic;
	      pp_print_string fm "=";
	      pp_print_c_expr020 fm ~need_paren:true expr1;
	      if need_paren then pp_print_string fm ")"
	    end
	| Cast_init (c_type, c_initializer_list) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_string fm ")";
	      pp_print_string fm "{";
	      pp_print_c_initializer_list fm c_initializer_list;
	      if need_paren then pp_print_string fm "}"
	    end
	| Memof expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "*";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Addrof expr ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_string fm "&";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	      if need_paren then pp_print_string fm ")"
	    end
	| Question (expr0, expr1_opt, expr2) ->
	    begin
	      if need_paren then pp_print_string fm "(";
	      pp_print_c_expr020 fm ~need_paren:true  expr0;
	      pp_print_space fm ();
	      pp_print_string fm "?";
	      pp_print_space fm ();
	      let _ = match expr1_opt with
		| Some expr1 -> 
		    pp_print_c_expr020 ~need_paren:true fm expr1;
		    pp_print_space fm ();
		| None -> ()
	      in
	      pp_print_string fm ":";
	      pp_print_space fm ();
	      pp_print_c_expr020 fm ~need_paren:true expr2;
	      if need_paren then pp_print_string fm ")";
	    end
    end;
    pp_close_box fm ()

and pp_print_c_assignment_expression: formatter -> need_paren:bool -> 
      c_assignment_expression -> unit = 
  fun fm ~need_paren expr -> pp_print_c_expr020 fm ~need_paren expr
    
and pp_print_c_expression: formatter -> need_paren:bool -> c_expression -> unit = 
  fun fm ~need_paren expr -> pp_print_c_expr020 fm ~need_paren expr
    
and pp_print_c_constant_expression: formatter -> need_paren:bool -> c_constant_expression -> unit = 
  fun fm ~need_paren expr -> pp_print_c_expr020 fm ~need_paren expr


and pp_print_primitive_type: formatter -> primitive_type -> unit = 
  fun fm expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| Void -> pp_print_string fm "void"
	| Void_ptr -> pp_print_string fm "void *"
	| Char -> pp_print_string fm "char"
	| Short_Int -> pp_print_string fm "short int"
	| Int -> pp_print_string fm "int"
	| Default_int -> pp_print_string fm ""
	| Long_Int -> pp_print_string fm "long int"
	| Long_Long_Int -> pp_print_string fm "long long int"
	| Float -> pp_print_string fm "float"
	| Double -> pp_print_string fm "double"
	| Bool -> pp_print_string fm "_Bool"
	| Complex -> pp_print_string fm "_Complext"
	| Signed_Char -> pp_print_string fm "signed char"
	| Signed_Short_Int -> pp_print_string fm "signed short int"
	| Signed_Int -> pp_print_string fm "signed int"
	| Signed_Long_Int -> pp_print_string fm "signed long int"
	| Signed_Long_Long_Int -> pp_print_string fm "signed long long int"
	| Unsigned_Char -> pp_print_string fm "unsigned char"
	| Unsigned_Short_Int -> pp_print_string fm "unsigned short int"
	| Unsigned_Int -> pp_print_string fm "unsigned int"
	| Unsigned_Long_Int -> pp_print_string fm "unsigned long int"
	| Unsigned_Long_Long_Int -> pp_print_string fm "unsigned long long int"
	| Long_Double -> pp_print_string fm "long double"
	| Float_Complex -> pp_print_string fm "float _Complex"
	| Double_Complex -> pp_print_string fm "double _Complex"
	| Long_Double_Complex -> pp_print_string fm "long double _Complex"
	| Signed -> pp_print_string fm "signed"
	| Unsigned -> pp_print_string fm "unsigned"
	| WChar -> pp_print_string fm "wchar_t"
    end;
    pp_close_box fm ()

and pp_print_type_qualifier: formatter -> type_qualifier -> unit = 
  fun fm expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| Const -> pp_print_string fm "const"
	| Restrict -> pp_print_string fm "restrict"
	| Volatile -> pp_print_string fm "volatile"
    end;
    pp_close_box fm ()

and pp_print_field: formatter -> field -> unit = 
  fun fm expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| Regular (c_type, str) -> 
	    pp_print_c_type fm (Some str, []) c_type;
	    
	| Bits (c_type, expr, id_opt) -> 
	    begin
	      pp_print_c_type fm (None, []) c_type;
	      pp_print_space fm ();
	      let _ = match id_opt with
		| Some s -> pp_print_string fm s;
		    pp_print_space fm ()
		| None -> ()
	      in
	      pp_print_string fm ":";
	      pp_print_c_expr020 fm ~need_paren:true expr;
	    end
    end;
    pp_close_box fm ()


and pp_print_enum_item: formatter -> enum_item -> unit = 
  fun fm expr ->
    pp_open_box fm 0;
    begin
      let (str, expr_opt) = expr
      in
      pp_print_string fm str;
      let _ = match expr_opt with 
	| Some v -> 
	    pp_print_space fm ();
	    pp_print_string fm "=";
	    pp_print_space fm ();
	    pp_print_c_expr020 fm ~need_paren:false v
	| None -> ()
      in ()
    end;
    pp_close_box fm ()
      

and pp_print_param_type: formatter -> param_type -> string option list -> unit = 
  fun fm expr ids ->
    pp_open_box fm 0;
    begin
      match expr with
	| Param_type_fix c_type_list ->
	    begin
	      let typ_ids = List.map2 (fun a b -> (a,b)) c_type_list ids
	      in
	      pp_print_string fm "(";
	      Mlite_printer.pp_print_list fm 
		(fun fm (c_type, id) -> 
		  pp_print_c_type fm (id, []) c_type;
		)
		pp_print_c_comma
		typ_ids;
	      pp_print_string fm ")";
	    end
	| Param_type_va c_type_list ->
	    begin
	      let typ_ids = List.map2 (fun a b -> (a,b)) c_type_list ids
	      in
	      pp_print_string fm "(";
	      Mlite_printer.pp_print_list fm 
		(fun fm (c_type, id) -> 
		  pp_print_c_type fm (id, []) c_type
		)
		pp_print_c_comma
		typ_ids;
	      pp_print_string fm ",...";
	      pp_print_string fm ")";
	    end
	| Param_list str_list ->
	    begin
	      pp_print_string fm "(";
	      Mlite_printer.pp_print_list fm 
		pp_print_string
		pp_print_c_comma
		str_list;
	      pp_print_string fm ")";
	    end
    end;
    pp_close_box fm ()

      
and pp_print_function_type: formatter -> string option * gnu_attribute list -> function_type -> unit = 
  fun fm (declarator_opt, attributes) expr  ->
    pp_open_box fm 0;
    begin
      let fun_interface fm = 
	pp_open_hvbox fm indent;
	begin
	  let atts = expr.func_attributes @ attributes
	  in
	  let _ = match declarator_opt with
	    | Some fun_name -> 
		if atts <> [] then
		  begin
		    Mlite_printer.pp_print_list fm
		      pp_print_attribute
		      (fun fm -> pp_print_space fm ())
		      atts;
		    pp_print_space fm ();
		  end;
		pp_print_string fm fun_name

	    | None -> 
		pp_print_string fm "(*)"
	  in
	  pp_print_space fm ();
	  pp_print_param_type fm expr.param_type expr.formal_param;
	end;
	pp_close_box fm ()
      in
      let fun_interface_txt =
	camlp4_macro_str_pp_print fun_interface
      in
      pp_print_c_type fm ((Some fun_interface_txt), []) expr.return_type;
    end;
    pp_close_box fm ()
      

and pp_print_string_opt fm (str_opt, attributes) =
  match str_opt with
    | Some str -> 
	begin
	  pp_print_space fm (); 
	  if attributes <> [] then
	    begin
	      Mlite_printer.pp_print_list fm 
		pp_print_attribute 
		(fun fm -> pp_print_space fm ())
		attributes;
	      pp_print_space fm ()
	    end;
	  pp_print_string fm str;
	end
    | None -> ()
	
and pp_print_gnu_attribute_list fm attributes =
  if attributes <> [] then
    begin 
      pp_print_space fm ();
      Mlite_printer.pp_print_list fm 
	pp_print_attribute 
	(fun fm -> pp_print_space fm ())
	attributes
    end
      
and pp_print_c_type: formatter -> string option * gnu_attribute list -> c_type -> unit = 
  fun fm (declarator_opt, attributes) expr -> 
    match expr with
      | GCC_attribute_type (c_type, attributes') ->
	  pp_print_c_type fm (declarator_opt, attributes @ attributes')
	    c_type;

      | Primitive_type primitive_type -> 
	  pp_print_primitive_type fm primitive_type;
	  pp_print_string_opt fm (declarator_opt, attributes);
	  
      | Pointer c_type -> 
	  let declarator_opt = 
	    let (l, r) = match c_type with
	      | Array _
	      | Function_type _
	      | Function_type_ex _ -> ("(", ")")
	      | _ -> ("", "")
	    in
	    match declarator_opt with
	      | Some s -> Some (l ^ "*" ^ s ^ r)
	      | None -> Some (l ^ "*" ^ r)
	  in
	  pp_print_c_type fm (declarator_opt, attributes) c_type
	    
      | Array (c_type, expr) -> 
	  begin
	    let declarator_opt = 
	      Some 
		(camlp4_macro_str_pp_print 
		  (fun fm -> 
		    pp_open_box fm 0;
		    pp_print_string_opt fm (declarator_opt, []);
		    pp_print_space fm ();
		    pp_print_string fm "[";
		    pp_print_c_expr020 fm ~need_paren:false expr;
		    pp_print_string fm "]";
		    pp_close_box fm ()
		  ))
	    in
	    pp_print_c_type fm (declarator_opt, attributes) c_type;
	  end
	    
      | Xarray c_type ->
	  begin
	    let declarator_opt = 
	      Some 
		(camlp4_macro_str_pp_print 
		  (fun fm -> 
		    pp_open_box fm 0;
		    pp_print_string_opt fm (declarator_opt, []);
		    pp_print_space fm ();
		    pp_print_string fm "[]";
		    pp_close_box fm ()
		  ))
	    in
	    pp_print_c_type fm (declarator_opt, attributes) c_type;
	  end

      | Struct_type (str, fields, layout) -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "struct";
	      pp_print_gnu_attribute_list fm attributes;
	      pp_print_space fm ();
	      pp_print_string fm str;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    pp_open_vbox fm indent;
	    pp_print_space fm ();
	    begin
	      Mlite_printer.pp_print_list fm
		pp_print_field
		pp_print_c_semicolon
		fields;
	    end;
	    pp_print_c_semicolon fm;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}";
	    let _ = match layout with
	      | C_struct -> ()
	      | GCC_struct -> 
		  pp_print_string fm "__attribute__((__gcc_struct__))"
	      | MS_struct ->
		  pp_print_string fm "__attribute__((__ms_struct__))"
	    in
	    pp_print_string_opt fm (declarator_opt, [])
	  end

      | Struct_type_name str -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "struct";
	      pp_print_space fm ();
	      pp_print_string fm str;
	    end;
	    pp_close_box fm ();
	    pp_print_string_opt fm (declarator_opt, attributes)
	  end
	    
      | Union_type (str, fields) -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "union";
	      pp_print_gnu_attribute_list fm attributes;
	      pp_print_space fm ();
	      pp_print_string fm str;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    pp_open_vbox fm indent;
	    pp_print_space fm ();
	    begin
	      Mlite_printer.pp_print_list fm
		pp_print_field
		pp_print_c_semicolon
		fields;
	    end;
	    pp_print_c_semicolon fm;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}";
	    pp_print_string_opt fm (declarator_opt, [])
	  end

      | Union_type_name str -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "union";
	      pp_print_space fm ();
	      pp_print_string fm str;
	    end;
	    pp_close_box fm ();
	    pp_print_string_opt fm (declarator_opt, attributes)
	  end

      | Enum_type (str, enum_items) -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "enum";
	      pp_print_gnu_attribute_list fm attributes;
	      pp_print_space fm ();
	      pp_print_string fm str;
	    end;
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "{";
	    pp_open_vbox fm indent;
	    pp_print_space fm ();
	    begin
	      Mlite_printer.pp_print_list fm
		pp_print_enum_item
		pp_print_c_comma
		enum_items;
	    end;
	    pp_print_space fm ();
	    pp_close_box fm ();
	    pp_print_space fm ();
	    pp_print_string fm "}";
	    pp_print_string_opt fm (declarator_opt, [])
	  end

      | Enum_type_name str -> 
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_string fm "enum";
	      pp_print_space fm ();
	      pp_print_string fm str
	    end;
	    pp_close_box fm ();
	    pp_print_string_opt fm (declarator_opt, attributes)
	  end
	    
      | Function_type function_type -> 
	  begin
	    pp_print_function_type fm (declarator_opt, attributes) function_type;
	  end
	    
      | Function_type_ex (function_type, decl_list) -> 
	  begin
	    pp_open_vbox fm 0;
	    pp_print_function_type fm (declarator_opt, attributes) function_type;
	    pp_print_space fm ();
	    Mlite_printer.pp_print_list fm 
	      (fun fm (typ, str) ->
		pp_print_c_type fm ((Some str), []) typ
	      )
	      pp_print_c_comma
	      decl_list;
	    pp_print_space fm ();
	    pp_close_box fm ()
	  end
	    
      | Qualified_type (type_qualifier, c_type) ->
	  begin
	    pp_open_box fm 0;
	    begin
	      pp_print_type_qualifier fm type_qualifier;
	      pp_print_space fm ();
	      pp_print_c_type fm (declarator_opt, attributes) c_type
	    end;
	    pp_close_box fm ()
	  end
	    
      | Typename str -> 
	  pp_print_string fm str;
	  pp_print_string_opt fm (declarator_opt, attributes)
	    
      | Typeof c_expr020 ->
	  pp_print_string fm "__typeof__(";
	  pp_print_c_expression ~need_paren:false fm c_expr020;
	  pp_print_string fm ")";
	  pp_print_string_opt fm (declarator_opt, attributes)

      | Typeid _ -> 
	  assert false
	    
      | Incomplete_qualified_type (c_type_opt, type_qualifier) -> 
	  begin
	    match c_type_opt with
	      | Some c_type ->
		  begin
		    pp_open_box fm 0;
		    begin
		      pp_print_type_qualifier fm type_qualifier;
		      pp_print_space fm ();
		      pp_print_c_type fm (declarator_opt, attributes) c_type
		    end;
		    pp_close_box fm ()
		  end
	      | None -> assert false
		  (*pp_print_type_qualifier fm type_qualifier*)
	  end

      | Incomplete_gnu_attribute_type (c_type_opt, gnu_attributes) -> 
	  begin
	    match c_type_opt with
	      | Some c_type ->
		  begin
		    pp_open_box fm 0;
		    begin
		      pp_print_c_type 
			fm (declarator_opt, attributes @ gnu_attributes) c_type
		    end;
		    pp_close_box fm ()
		  end
	      | None -> assert false;
	  end


and pp_print_c_declaration: formatter -> c_declaration -> unit = 
  fun fm expr -> 
    pp_open_box fm 0;
    begin
      match expr with
	| Obj_decl (linkage, c_type, str) ->
	    begin
	      pp_print_linkage fm linkage;
	      pp_print_c_type fm ((Some str), []) c_type;
	    end
	| Obj_decl_init (linkage, c_type, str, init) ->
	    begin
	      pp_print_linkage fm linkage;
	      pp_print_c_type fm ((Some str), []) c_type;
	      pp_print_space fm ();
	      pp_print_string fm "=";
	      pp_print_space fm ();
	      pp_print_c_initializer fm init;
	    end
	| Typedef (c_type, str) -> 
	    begin
	      pp_print_string fm "typedef";
	      pp_print_space fm ();
	      pp_print_c_type fm ((Some str), []) c_type;
	    end
	| Type_decl c_type ->
	    begin
	      pp_print_c_type fm (None, []) c_type
	    end
	| Type_only c_type ->
	    begin
	      pp_print_c_type fm (None, []) c_type
	    end
    end;
    pp_close_box fm ()

and pp_print_c_initializer_list: formatter -> c_initializer_list -> unit =
  fun fm expr ->
    pp_open_hvbox fm 0;
    begin
      Mlite_printer.pp_print_list fm 
	(fun fm (c_designator_list, c_initializer) -> 
	  pp_print_c_designator_list fm c_designator_list;
	  pp_print_c_initializer fm c_initializer
	) 
	pp_print_c_comma
	expr
    end;
    pp_close_box fm ()

and pp_print_c_initializer: formatter -> c_initializer -> unit =
  fun fm expr ->
    pp_open_hvbox fm 0;
    begin
      match expr with
	| Initializer_1 c_assignment_expression ->
	    begin
	      pp_print_c_assignment_expression 
		fm ~need_paren:true c_assignment_expression
	    end
	| Initializer_2 c_initializer_list  ->
	    begin
	      pp_print_string fm "{";
	      pp_open_vbox fm indent;
	      pp_print_space fm ();
	      begin
		pp_print_c_initializer_list fm c_initializer_list;
	      end;
	      pp_close_box fm ();
	      pp_print_space fm ();
	      pp_print_string fm "}";
	    end
    end;
    pp_close_box fm ()

and pp_print_c_designator_list: formatter -> c_designator list -> unit =
  fun fm expr ->
    if expr <> [] then
      begin
	pp_open_box fm 0;
	begin
	  Mlite_printer.pp_print_list fm 
	    pp_print_c_designator 
	    (fun fm -> ())
	    expr
	end;
	pp_print_space fm ();
	pp_print_string fm "=";
	pp_print_space fm ();
	pp_close_box fm ()
      end


and pp_print_c_designator: formatter -> c_designator -> unit =
  fun fm expr ->
    pp_open_box fm 0;
    begin
      match expr with
	| Designator_1 c_constant_expression ->
	    begin
	      pp_print_string fm "[";
	      pp_print_c_constant_expression fm ~need_paren:false c_constant_expression;
	      pp_print_string fm "]"
	    end
	      
	| Designator_2 c_identifier ->
	    begin
	      pp_print_string fm ".";
	      pp_print_c_identifier fm c_identifier;
	    end
	      
	| Designator_gnu_range (e0, e1) ->
	    begin
	      pp_print_string fm "[";
	      pp_print_c_constant_expression fm ~need_paren:false e0;
	      pp_print_string fm " ... ";
	      pp_print_c_constant_expression fm ~need_paren:false e1;
	      pp_print_string fm "]"
	    end
    end;
    pp_close_box fm ()

and pp_print_asm (fm:formatter) (str_list, asm_details_opt) = 
  pp_open_vbox fm 0;
  begin
    pp_print_string fm "__asm__";
    pp_print_space fm ();
    pp_print_string fm "(";
    pp_print_space fm ();
    let _ = Mlite_printer.pp_print_list fm 
      (fun fm s -> pp_print_string fm ("\"" ^ s ^ "\""))
      pp_print_c_newline str_list
    in
    let _ = match asm_details_opt with
      | Some asm ->
	  begin
	    pp_print_space fm ();
	    pp_print_string fm ":";
	    Mlite_printer.pp_print_list fm 
	      (fun fm (so, s,e) ->
		pp_print_string fm ("\"" ^ s ^ "\"");
		pp_print_string fm " (";
		pp_print_c_expression ~need_paren:false fm e;
		pp_print_string fm ")")
	      (fun fm -> pp_print_string fm ",") asm.asm_outputs;
	    pp_print_space fm ();
	    pp_print_string fm ":";
	    Mlite_printer.pp_print_list fm 
	      (fun fm (so, s,e) ->
		pp_print_string fm ("\"" ^ s ^ "\"");
		pp_print_string fm " (";
		pp_print_c_expression ~need_paren:false fm e;
		pp_print_string fm ")")
	      (fun fm -> pp_print_string fm ",") asm.asm_inputs;
	    if (asm.asm_clobbers <> []) then
	      begin
		pp_print_space fm ();
		pp_print_string fm ":";
		Mlite_printer.pp_print_list fm 
		  (fun fm s ->
		    pp_print_string fm ("\"" ^ s ^ "\""))
		  (fun fm -> pp_print_string fm ",") asm.asm_clobbers;
	      end
	  end
      | None -> ()
    in
    pp_print_string fm ");";
  end;
  pp_close_box fm ()


and pp_print_c_stmt010: formatter -> c_stmt010 -> unit = 
  fun fm stmt ->
    pp_open_vbox fm 0;
    begin
      match stmt with
	| STMT_AT (coord, stmt) -> 
	    Coordinate.pp_print_t fm coord;
	    pp_print_c_stmt010 fm stmt
	      
	| NOP -> ()
	    
	| COMPUTATION expr ->
	    begin
	      pp_print_c_expression fm ~need_paren:false expr;
	      pp_print_string fm ";"
	    end
	      
	| SEQUENCE (txt_opt, c_stmt010_list) -> 
	    begin
	      let _ = match txt_opt with
		| Some txt -> fprintf fm "/* %s */" txt; 
		    pp_print_space fm ();
		| None -> ()
	      in
	      Mlite_printer.pp_print_list fm 
		(fun fm statement ->
		  pp_print_c_stmt010 fm statement;
		)
		(fun fm -> pp_print_space fm ()) 
		c_stmt010_list;
	    end

	| COMPOUND (txt_opt, c_compound_stmt010) ->
	    begin
	      pp_print_string fm "{";
	      pp_open_vbox fm indent;
	      begin
		let _ = match txt_opt with
		  | Some txt -> pp_print_space fm (); 
		      fprintf fm "/* %s */" txt
		  | None -> ()
		in
		pp_print_space fm ();
		pp_print_c_compound_stmt010 fm c_compound_stmt010;
	      end;
	      pp_close_box fm ();
	      pp_print_space fm ();
	      pp_print_string fm "}";
	    end
	      
	| IF (expr, then_c_stmt010, else_c_stmt010) -> 
	    begin
	      pp_print_string fm "if (";
	      pp_print_c_expression fm ~need_paren:true expr;
	      pp_print_string fm ")";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm then_c_stmt010;
	      pp_print_space fm ();
	      pp_print_string fm "else";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm else_c_stmt010
	    end

	| WHILE (expr, c_stmt0) ->
	    begin
	      pp_print_string fm "while (";
	      pp_print_c_expression fm ~need_paren:true expr;
	      pp_print_string fm ")";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt0
	    end

	| LOOP c_stmt0 ->
	    begin
	      pp_print_string fm "for(;;)";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt0
	    end

	| BREAK ->
	    pp_print_string fm "break;"

	| CONTINUE ->
	    pp_print_string fm "continue;"
	      
	| RETURN_VALUE expr ->
	    begin
	      pp_print_string fm "return ";
	      pp_print_c_expression fm ~need_paren:true expr;
	      pp_print_string fm ";"
	    end
	      
	| RETURN ->
	    pp_print_string fm "return;"
	      
	| SWITCH (expr, c_stmt010) ->
	    begin
	      pp_print_string fm "switch";
	      pp_print_string fm "(";
	      pp_print_c_expression fm ~need_paren:true expr;
	      pp_print_string fm ")";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt010
	    end
	      
	| CASE (expr, c_stmt010) ->
	    begin
	      pp_print_string fm "case";
	      pp_print_space fm ();
	      pp_print_c_constant_expression fm ~need_paren:false expr;
	      pp_print_string fm ":";
	      pp_open_vbox fm indent;
	      begin
		pp_print_space fm ();
		pp_print_c_stmt010 fm c_stmt010;
	      end;
	      pp_print_string fm (";/*case:*/");
	      pp_close_box fm ()
	    end
	      
	| CASE_RANGE (e0, e1, c_stmt010) ->
	    begin
	      pp_print_string fm "case";
	      pp_print_space fm ();
	      pp_print_c_constant_expression fm ~need_paren:false e0;
	      pp_print_string fm " ... ";
	      pp_print_c_constant_expression fm ~need_paren:false e1;
	      pp_print_string fm ":";
	      pp_open_vbox fm indent;
	      begin
		pp_print_space fm ();
		pp_print_c_stmt010 fm c_stmt010;
	      end;
	      pp_print_string fm (";/*case:*/");
	      pp_close_box fm ()
	    end
	      
	| DEFAULT (c_stmt010) ->
	    begin
	      pp_print_string fm "default:";
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt010;
	      pp_print_string fm (";/*default:*/");
	    end
	      
	| LABEL (lb, c_stmt010) ->
	    begin
	      pp_print_string fm (lb ^ ":");
	      pp_print_space fm ();
	      pp_print_c_stmt010 fm c_stmt010;
	      pp_print_string fm (";/*label:" ^ lb ^ "*/");
	    end
	      
	| GOTO lb ->
	    pp_print_string fm ("goto " ^ lb ^ ";");
	    
	| GCC_GOTO expr ->
	    begin
	      pp_print_string fm "goto ";
	      pp_print_string fm "*";
	      pp_print_c_expression fm ~need_paren:true expr;
	      pp_print_string fm ";";
	    end

	| ASM (u, v) ->
	    pp_print_asm fm (u, v)
    end;
    pp_close_box fm ()

and pp_print_c_compound_stmt010: formatter -> c_compound_stmt010 -> unit = 
  fun fm (BLOCK (labels, c_declaration_list, stmts)) ->
    pp_open_vbox fm 0;
    begin
      if (labels <> []) then
	begin
	  pp_print_string fm "__label__ ";
	  pp_print_space fm ();
	  Mlite_printer.pp_print_list fm pp_print_string
	    (fun fm -> pp_print_string fm ",") labels;
	  pp_print_string fm ";";
	end;
      pp_print_c_declaration_list fm c_declaration_list;
      if stmts <> [] then
	begin
	  Mlite_printer.pp_print_list fm 
	    pp_print_c_stmt010
	    (fun fm -> pp_print_space fm ())
	    stmts;
	end;
    end;
    pp_close_box fm ()

and pp_print_c_declaration_list: formatter -> c_declaration_list -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      if expr <> [] then
	begin
	  Mlite_printer.pp_print_list fm 
	    (fun fm (v, _) -> pp_print_c_declaration fm v) 
	    pp_print_c_semicolon expr;
	  pp_print_c_semicolon fm;
	end
    end;
    pp_close_box fm ()

and pp_print_c_function_definition: formatter -> c_function_definition -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      match expr with
	| Function_definition (linkage, 
	  c_type,
	  fname, 
	  c_compound_stmt010) 
	  ->
	    begin
	      pp_open_box fm 0;
	      begin
		pp_print_linkage fm linkage;
		pp_print_c_type fm (Some fname, []) c_type;
	      end;
	      pp_close_box fm ();
	      pp_print_space fm ();
	      pp_print_string fm "{";
	      pp_open_vbox fm indent;
	      begin
		pp_print_space fm ();
		pp_print_c_compound_stmt010 fm c_compound_stmt010;
	      end;
	      pp_close_box fm ();
	      pp_print_space fm ();
	      pp_print_string fm "}";
	    end
    end;
    pp_close_box fm ()

and pp_print_c_external_declaration: formatter -> c_external_declaration -> unit =
  fun fm expr ->
    pp_open_vbox fm 0;
    begin
      match expr with
	| External_declaration_at (coord, expr) ->
	    Coordinate.pp_print_t fm coord;
	    pp_print_c_external_declaration fm expr;
	    
	| External_declaration_1 (c_function_definition) ->
	    pp_print_c_function_definition fm c_function_definition
	| External_declaration_2 (c_declarations) ->
	    pp_print_c_declaration_list fm c_declarations
	| External_declaration_string (c_type, id, cstr_lit) ->
	    pp_open_box fm 0;
	    pp_print_c_type fm (Some id, []) c_type;
	    pp_print_space fm ();
	    pp_print_string fm "=";
	    pp_print_space fm ();
	    pp_print_c_string_literal fm cstr_lit;
	    pp_print_string fm ";";
	    pp_close_box fm ()
    end;
    pp_close_box fm ()

and pp_print_c_file_unit: formatter -> c_file_unit -> unit = 
  fun fm c_file_unit ->
    pp_open_vbox fm 0;
    begin
      match c_file_unit with
	| Translation_unit l ->
	    begin
	      Mlite_printer.pp_print_list fm 
		pp_print_c_external_declaration
		(fun fm -> pp_print_space fm ())
		l;
	    end
    end;
    pp_close_box fm ();
    pp_print_flush fm ()
