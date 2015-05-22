{
open Typ_parser
open Typ_handle
open Collection
exception Eof
exception InternalError of string
let version = "Typ_lexer V1.0 nwang@mlite.org"

module QN = Qual_name

(*
** Keyword hashtable
*)
let lexicon = StringHashtbl.create 211
let init_lexicon _ =
  StringHashtbl.clear lexicon;
  List.iter
    (fun (key, token) -> StringHashtbl.add lexicon key token)
    [
      ("void", T_VOID);
      ("char", T_CHAR);
      ("short", T_SHORT);
      ("int", T_INT);
      ("signed", T_SIGNED);
      ("unsigned", T_UNSIGNED);
      ("enum", T_ENUM);
      ("struct", T_STRUCT);
      ("typedef", T_TYPEDEF);
      ("union", T_UNION);
      ("inline", T_INLINE);
      ("const", T_CONST);
    ]

let scan_ident str =
  try 
    StringHashtbl.find lexicon str
  with Not_found -> T_IDENTIFIER 
    { 
      QN.qn_namespace = QN.QN_CLANG;
      QN.qn_span = QN.QN_AUTO;
      QN.qn_class = QN.QN_TYPE_NAME;
      QN.qn_scopes = [];
      QN.qn_init = QN.QN_NULL;
      QN.qn_sname = str
    }

(*
 ** Useful primitives
 *)
let rem_quotes str = String.sub str 1 ((String.length str) - 2)


(** escape character management **)
let scan_escape str =
  match str with
    "n" -> "\n"
  | "r" -> "\r"
  | "t" -> "\t"
  | "b" -> "\b"
  | _ -> str
let get_value chr =
  match chr with
    '0'..'9' -> (Char.code chr) - (Char.code '0')
  | 'a'..'z' -> (Char.code chr) - (Char.code 'a') + 10
  | 'A'..'Z' -> (Char.code chr) - (Char.code 'A') + 10
  | _ -> 0
let scan_hex_escape str =
  String.make 1 (Char.chr (
		 (get_value (String.get str 0)) * 16
		   + (get_value (String.get str 1))
		))
let scan_oct_escape str =
  String.make 1 (Char.chr (
		 (get_value (String.get str 0)) * 64
		   + (get_value (String.get str 1)) * 8
		   + (get_value (String.get str 2))
		))
}
  
let decdigit = ['0'-'9']
let octdigit = ['0'-'7']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let letter = ['a'- 'z' 'A'-'Z']
    
let usuffix = ['u' 'U']
let lsuffix = ['l' 'L']
let intsuffix = (
  lsuffix|usuffix|(usuffix lsuffix)|(usuffix lsuffix lsuffix) 
|(lsuffix lsuffix)|(lsuffix usuffix))?

let floatsuffix = ['f' 'F' 'l' 'L']
    
let intnum = decdigit+ intsuffix?
let octnum = '0' octdigit+ intsuffix?
let hexnum = '0' ['x' 'X'] hexdigit+ intsuffix?
    
let ident = (letter|'_')(letter|decdigit|'_')* 
let blank = [' ' '\t' '\n']
let escape = '\\' _

    rule initial =  parse   
| "/*"	{let _ = comment lexbuf in initial lexbuf}
| blank         {initial lexbuf}
| '#'           {line lexbuf}
| intnum	{T_CONSTANT_INT (Lexing.lexeme lexbuf)}
| "..."		{T_ELLIPSIS}
|"="		{T_EQ}
|"?"		{T_QUEST}
|"$"		{T_DOLLAR}
|"->"		{T_ARROW}
|'*'		{T_STAR}
|':'		{T_COLON}
|'{'		{T_LBRACE}
|'}'		{T_RBRACE}
|'['		{T_LBRACKET}
|']'		{T_RBRACKET}
|'('		{T_LPAREN}
|')'		{T_RPAREN}
|';'		{T_SEMICOLON}
|','		{T_COMMA}
|ident		{ 
                  let str = (Lexing.lexeme lexbuf)
                  in
		  scan_ident str
                }
    
|eof		{T_EOF}
|_		{syntax_error
		   "Invalid symbol";
		 initial lexbuf}

(** comment *)
and comment =
  parse "*/"	{()}
|_ 		{comment lexbuf}
    
(** # <line number> <file name> ... *)
and line =
  parse	'\n'	{initial lexbuf}
|blank		{line lexbuf}
|intnum		{ (*set_line (int_of_string (Lexing.lexeme lexbuf));*)
		 file lexbuf}
|_              {endline lexbuf}
and file =
  parse '\n'	{initial lexbuf}
|blank		{file lexbuf}
|'"' [^ '"']* '"' { (*set_name (rem_quotes (Lexing.lexeme lexbuf));*)
		   endline lexbuf}
|_              {endline lexbuf}
and endline =
  parse '\n' 	{initial lexbuf}
|_		{endline lexbuf}
    
(** str *)
and str =
  parse	'"'	{""}
|"\\0"	        
    {(String.make 1 (Char.chr 0)) ^ (str lexbuf)}
|escape		
    {
     let cur = scan_escape 
	 (String.sub
	    (Lexing.lexeme lexbuf) 1 1) 
     in cur ^ (str lexbuf)
   }
|_              
    {
     let cur = Lexing.lexeme lexbuf 
     in cur ^  (str lexbuf)} 
    
(** chr *)
and chr =
  parse	'\''	{""}
|"\\0"		
    { (String.make 1 (Char.chr 0)) ^ (chr lexbuf)}
|escape		
    { let cur = scan_escape 
	(String.sub (Lexing.lexeme lexbuf) 1 1) 
    in cur ^ (chr lexbuf)}
|_		
    { let cur = Lexing.lexeme lexbuf in cur ^ (chr lexbuf)} 
	
{
 let init () =
   init_lexicon ();
}
