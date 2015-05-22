open Format
open Cabs

type print_switch = 
  | Print_typeSpecifier
  | Print_spec_elem 
  | Print_specifier
  | Print_decl_type
  | Print_name_group
  | Print_field_group
  | Print_init_name_group
  | Print_name
  | Print_single_name
  | Print_enum_item
  | Print_definition
  | Print_file
  | Print_block
  | Print_statement
  | Print_expression
  | Print_init_expression
  | Print_initwhat
  | Print_attribute


let pp_print_binary_operator: formatter -> binary_operator -> unit =
  fun fm binary_operator ->
    pp_open_box fm 0;
    let str = match binary_operator with
    | ADD                 (** "+" operator. *) -> "+"
    | SUB                 (** "-" operator. *) -> "-"
    | MUL                 (** "*" operator. *) -> "*"
    | DIV                 (** "/" operator. *) -> "/"
    | MOD                 (** "%" operator. *) -> "%"
    | AND                 (** "&&" operator. *) -> "&&"
    | OR                  (** "||" operator. *) -> "||"
    | BAND                (** "&" operator. *)  -> "&"
    | BOR                 (** "|" operator. *)  -> "|"
    | XOR                 (** "^" operator. *)  -> "^"
    | SHL                 (** "<<" operator. *) -> "<<"
    | SHR                 (** ">>" operator. *) -> ">>"
    | EQ                  (** "==" operator. *) -> "=="
    | NE                  (** "!=" operator. *) -> "!="
    | LT                  (** "<" operator. *)  -> "<"
    | GT                  (** ">" operator. *)  -> ">"
    | LE                  (** "<=" operator. *)  -> "<="
    | GE                  (** ">=" operator. *)  -> ">="
    | ASSIGN              (** "=" operator. *)   -> "="
    | ADD_ASSIGN          (** "+=" operator. *)  -> "+="
    | SUB_ASSIGN          (** "-=" operator. *)  -> "-="
    | MUL_ASSIGN          (** "*=" operator. *)  -> "*="
    | DIV_ASSIGN          (** "/=" operator. *)  -> "/="
    | MOD_ASSIGN          (** "%=" operator. *)  -> "%="
    | BAND_ASSIGN         (** "&=" operator. *)  -> "&="
    | BOR_ASSIGN          (** "|=" operator. *)  -> "|="
    | XOR_ASSIGN          (** "^=" operator. *)  -> "^="
    | SHL_ASSIGN          (** "<<=" operator. *) -> "<<="
    | SHR_ASSIGN          (** ">>=" operator. *) -> ">>="
    in
    pp_print_string fm str;
    pp_close_box fm ()



let pp_print_unary_operator: formatter -> unary_operator -> unit = 
  fun fm unary_operator ->
    pp_open_box fm 0;
    let str = match unary_operator with
    | MINUS -> "-"
    | PLUS -> "+"
    | NOT -> "!"
    | BNOT -> "~"
    | MEMOF -> "*"
    | ADDROF -> "&"
    | PREINCR -> "++"
    | PREDECR -> "--"
    | POSINCR -> "++"
    | POSDECR -> "--"
    in
    pp_print_string fm str;
    pp_close_box fm ()



let pp_print_constant: formatter -> constant -> unit = 
  fun fm constant ->
    pp_open_box fm 0;
    let str = match constant with
      | CONST_INT (_, s) -> s
      | CONST_FLOAT s -> s
      | CONST_CHAR _ -> ""
      | CONST_WCHAR _ -> ""
      | CONST_STRING s -> s
      | CONST_WSTRING s -> ""
    in
    pp_print_string fm str;
    pp_close_box fm ()



let rec pp_print_expression: formatter -> expression -> unit = 
  fun fm expression ->
    pp_open_box fm 0;
    let _ = match expression with
    | PAREN _ -> assert false
    | NOTHING -> ()
    | UNARY (unary_operator, expression) ->
	begin
	  pp_print_unary_operator fm unary_operator;
	  pp_print_space fm ();
	  pp_print_expression fm expression
	end
    | LABELADDR string -> assert false
    | BINARY (binary_operator, expression0, expression1) ->
	begin
	  pp_print_expression fm expression0;
	  pp_print_space fm ();
	  pp_print_binary_operator fm binary_operator;
	  pp_print_space fm ();
	  pp_print_expression fm expression1
	end
    | QUESTION (cond_expression, then_expression, else_expression) ->
	begin
	  pp_print_expression fm cond_expression;
	  pp_print_string fm "?";
	  pp_print_expression fm then_expression;
	  pp_print_string fm ":";
	  pp_print_expression fm else_expression;
	end
	  
	  (* A CAST can actually be a constructor expression *)
    | CAST ((specifier, decl_type), init_expression) ->
	()
	  (* There is a special form of CALL in which the function called is
	     __builtin_va_arg and the second argument is sizeof(T). This 
	     should be printed as just T *)

    | CALL (expression, expression_list) -> 
	()
    | COMMA expression_list ->
	()
    | CONSTANT constant ->
	pp_print_constant fm constant
    | VARIABLE string ->
	pp_print_string fm string
    | EXPR_SIZEOF expression ->
	begin
	  pp_print_string fm "sizeof";
	  pp_print_expression fm expression;
	end
    | TYPE_SIZEOF (specifier, decl_type) ->
	begin
	end
    | EXPR_ALIGNOF expression ->
	begin
	end
    | TYPE_ALIGNOF (specifier, decl_type) ->
	begin
	end
    | INDEX (base_expression, index_expression) ->
	begin
	end
    | MEMBEROF (expression, string) ->
	begin
	end
    | MEMBEROFPTR (expression, string) ->
	begin
	end
    | GNU_BODY block -> assert false
    | EXPR_PATTERN string  -> assert false
    in
    pp_close_box fm ()





let expression_to_str: expression -> string =
  fun expression ->
    pp_print_expression str_formatter expression;
    flush_str_formatter ()


let pp_print_specifier: formatter -> specifier -> unit = 
  fun fm specifier ->
    ()


let pp_print_name_group: formatter -> name_group -> unit = 
  fun fm name_group -> ()


let pp_print_field_group: formatter -> field_group -> unit = 
  fun fm field_group -> ()


let pp_print_init_name_group: formatter -> init_name_group -> unit = 
  fun fm init_name_group -> ()


let pp_print_name: formatter -> name -> unit = 
  fun fm name -> ()

