%{
(* $Id: ncparser.mly,v 1.4 2005/07/20 23:07:25 wangn Exp $ header *)
(* The BNF grammar of normalized untyped C language              *)
(* Refer to ast.ml for the abstract syntax of the BNF grammar    *)

let d s = Logrc_handle.reduce s
let parse_error _ = Logrc_handle.syntax_error "Logrc_parser: Syntax error"
    %}
  
  %token T_EOF
  %token T_LPAREN T_RPAREN 
  %token T_LBRACE T_RBRACE T_LBRACKET T_RBRACKET 
  %token T_LANGLE_BRACKET T_RANGLE_BRACKET T_COMMA T_DOT 
  %token T_LANGLE_BRACKET_EQ T_RANGLE_BRACKET_EQ 
  %token T_COLON T_SEMICOLON
  
  %token T_LOG T_LOG_UNIT T_ITEM T_TAG T_SUFFIX T_ENABLE T_NONE T_YES T_NO

  %token<string> T_INTEGER
  %token<string> T_IDENTIFIER T_STRING_LITERAL 

  %type <Log.log_unit>   log_unit

  %start file 
  %type <Log.log_unit list>   file
  %% 

/* (* entry  points *) */
file:
  T_LOG T_LBRACE 
  opt_log_units
  T_RBRACE 
    { $3 }
;;

log_unit:
| T_LOG_UNIT T_LBRACE
    T_ENABLE yes_no 
    T_SUFFIX opt_str
    opt_items
    T_RBRACE 
      { 
	let suffix = match  $6 with
	| None -> ""
	| Some v -> v
	in
	{ Log.unit_enable = $4;
	  Log.unit_suffix = suffix;
	  Log.unit_items = $7; }
      }
;;

yes_no:
| T_YES { true }
| T_NO { false }
;;

opt_str:
| T_NONE { None }
| T_STRING_LITERAL { Some $1 }
;;

item:
| T_ITEM T_LBRACE 
    T_TAG T_STRING_LITERAL 
    T_ENABLE yes_no
    T_RBRACE 
      { { Log.item_tag = $4;
	  Log.item_enable = $6; }
      }
;;

opt_items:
| { [] }
| opt_items item { $2::$1 }
;;

opt_log_units:
| { [] }
| opt_log_units log_unit { $2::$1 }
;;

%%
(*trailer*)
