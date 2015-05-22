%{
open Typ_ast
open Csize
let version = "Typ_parser V0.1 Ning Wang"
    
let d s = Typ_handle.reduce s

let parse_error _ =
  Typ_handle.syntax_error "Typ_parser: Syntax error"
%}
  
/* (** A.1.2 Keywords **) */
%token T_CHAR T_CONST 
%token T_DOUBLE T_DOLLAR T_ENUM T_EXTERN
%token T_FLOAT T_INLINE T_INT T_LONG T_QUEST
%token T_REGISTER T_RESTRICT T_SHORT T_SIGNED 
%token T_STATIC T_STRUCT T_TYPEDEF
%token T_UNION T_UNSIGNED T_VOID T_VOLATILE
%token T_BOOL T_COMPLEX T_IMAGINARY

%token T_EQ T_PLUS_EQ T_MINUS_EQ T_STAR_EQ T_SLASH_EQ T_PERCENT_EQ
%token T_AND_EQ T_PIPE_EQ T_CIRC_EQ T_INF_INF_EQ T_SUP_SUP_EQ
%token T_ARROW T_DOT T_BUILTIN_VA_LIST

%token T_EQ_EQ T_EXCLAM_EQ T_INF T_SUP T_INF_EQ T_SUP_EQ
%token T_PLUS T_MINUS T_STAR T_SLASH T_PERCENT
%token T_TILDE T_AND T_PIPE T_CIRC
%token T_INF_INF T_SUP_SUP


%token T_RPAREN T_LPAREN T_RBRACE T_LBRACE T_LBRACKET T_RBRACKET
%token T_COLON T_SEMICOLON T_COMMA T_ELLIPSIS 
%token T_ATTRIBUTE

%left T_ARROW
%nonassoc T_LPAREN

%token <Qual_name.t> T_IDENTIFIER
%token <string> T_CONSTANT_INT
%token T_EOF

%type <Typ_ast.c_type> c_file
%type <Typ_ast.c_type> c_type
%type <Typ_ast.c_type_decl> c_type_decl
%type <Typ_ast.primitive> c_primitive

%start c_file
%%
c_file:
c_type T_EOF { $1 }
;;

c_primitive:
| T_VOID { Void }
| T_CHAR { Char }
| T_SHORT T_INT { Short }
| T_UNSIGNED T_SHORT { Unsigned_Short}
| T_SIGNED T_SHORT { Short }
| T_INT { Int }
| T_UNSIGNED T_INT { Unsigned_Int}
| T_SIGNED T_INT { Unsigned_Int}
| T_LONG { Long }
| T_UNSIGNED T_LONG { Unsigned_Long }
| T_SIGNED T_LONG { Long }
| T_LONG T_LONG { Long_Long }
| T_UNSIGNED T_LONG T_LONG { Unsigned_Long_Long }
| T_SIGNED T_LONG T_LONG { Long_Long }
| T_FLOAT { Float }
| T_DOUBLE { Double }
| T_LONG T_DOUBLE { Long_Double }
| T_BOOL { Bool }
| T_COMPLEX { Complex }
| T_FLOAT T_COMPLEX { Float_Complex }
| T_DOUBLE T_COMPLEX { Double_Complex }
| T_LONG T_DOUBLE T_COMPLEX { Long_Double_Complex }
| T_QUEST { Unknown }
| T_BUILTIN_VA_LIST { Va_List }
;;

c_type_decl:
| T_STRUCT T_IDENTIFIER T_COLON T_CONSTANT_INT T_COLON T_CONSTANT_INT T_LBRACE c_field_list T_RBRACE
    { Struct_decl ($2, csize_of_string $4, csize_of_string $6, $8) }
| T_UNION T_IDENTIFIER T_COLON T_CONSTANT_INT T_COLON T_CONSTANT_INT T_LBRACE c_field_list T_RBRACE
	{ Union_decl ($2, csize_of_string $4, csize_of_string $6, $8) }
| T_ENUM T_IDENTIFIER T_LBRACE c_enumerator_list T_RBRACE
	    { Enum_decl ($2, $4) }
| T_TYPEDEF c_type T_IDENTIFIER { Typedef ($2, $3) }
;;

c_field_list:
| c_field { [$1] }
| c_field_list T_SEMICOLON c_field { $1 @ [$3] }
;;

c_field:
| T_IDENTIFIER T_COLON T_CONSTANT_INT T_COLON c_primary_type 
    { Named ($5, $1, csize_of_string $3) }
| c_type T_COLON T_CONSTANT_INT { Unamed ($1, csize_of_string $3) }
;;

c_enumerator_list:
| c_enumerator { [$1] }
| c_enumerator_list T_COMMA c_enumerator { $1 @ [$3] }
;;

c_enumerator:
| T_IDENTIFIER T_EQ T_CONSTANT_INT { ($1, int_of_string $3) }
;;

c_type_qualifier:
| T_CONST { Const }
| T_RESTRICT { Restrict }
| T_VOLATILE { Volatile }
;;

c_primary_type:
| c_primitive { Primitive $1 }
| c_primary_type T_COLON T_CONSTANT_INT T_COLON T_CONSTANT_INT
      { Bits ($1, csize_of_string $3, csize_of_string $5, "todo") }
| T_DOLLAR T_IDENTIFIER { Typename $2 }
| T_STRUCT T_IDENTIFIER { Struct $2 }
| T_UNION T_IDENTIFIER { Union $2 }
| T_ENUM T_IDENTIFIER { Enum $2 }
| T_LBRACE c_type T_RBRACE { Primary $2 }
;;

c_type:
| c_function_type { Function $1 }
| c_primary_type { $1 }
| T_STAR c_primary_type { Pointer $2 }
| T_LBRACKET T_CONSTANT_INT T_RBRACKET c_primary_type 
      { Array ($4, csize_of_string $2) }
| T_LBRACKET T_RBRACKET c_primary_type { Xarray $3 }
| c_type_qualifier c_primary_type { Qual ($2, $1) }
| c_type_decl { Type_decl $1 }
;;

c_function_type:
| c_param T_ARROW c_param { Fixed ([$1], $3) }
| c_function_type T_ARROW c_param 
      {
	match $1 with
	  | Fixed (l,t) -> Fixed (l @[t], $3)
	  | Varadic (l, t) -> assert false

      }
| c_function_type T_ARROW T_ELLIPSIS T_ARROW c_param
	  { 
	    match $1 with
	      | Fixed (l,t) -> Varadic (l @[t], $5)
	      | Varadic (l,t) -> assert false
	  }
;;

c_param:
| T_IDENTIFIER T_COLON c_primary_type { Labeled ($3, $1) }
| c_primary_type { Unlabeled $1 }
;;
