(* $Id: nclexer.mll,v 1.5 2005/07/21 16:28:37 wangn Exp $ *)
  {
open Logrc_handle
open Logrc_parser
open Collection
exception Eof
    
let lexicon = StringHashtbl.create 211
let init_lexicon _ =
  StringHashtbl.clear lexicon;
  List.iter
    (fun (key, token) -> StringHashtbl.add lexicon key token)
    [
     ("log", T_LOG);
     ("log_unit", T_LOG_UNIT);
     ("item", T_ITEM);
     ("tag", T_TAG);
     ("enable", T_ENABLE);
     ("yes", T_YES);
     ("no", T_NO);
     ("none", T_NONE);
     ("suffix", T_SUFFIX);
   ]
    
let scan_ident id = try StringHashtbl.find lexicon id
with Not_found -> 
  d ("T_IDENTIFIER: " ^ id);
  T_IDENTIFIER id
}
  
(* predefined regular exprs *)

let d  = ['0' - '9']
let l  = ['a' - 'z' 'A' - 'Z']
let ident = (l|'_')(l|d|'_')*
let blank = [' ' '\t' '\n']

let escape = '\\' _
    
(* start of rules *)
    rule token = 
  parse "/*"          {let _ = comment lexbuf in token lexbuf }
|  blank              {token lexbuf}  (* Skip blanks *)
| '"'                 {let lineno = Logrc_handle.get_lineno ()
                       and code_str = str lexbuf
                       in  d ("T_STRING_LIBERAL " ^ code_str);
                       T_STRING_LITERAL code_str }
| ':'                 {d ":";   T_COLON }
| ';'                 {d ";";   T_SEMICOLON }
|  '{'		      {d "{";   T_LBRACE }
|  '}'		      {d "}";   T_RBRACE }
|  ','		      {d ",";   T_COMMA }
|  '('		      {d "(";   T_LPAREN }
|  ')'		      {d ")";   T_RPAREN }
|  '['		      {d "[";   T_LBRACKET }
|  ']'		      {d "]";   T_RBRACKET }
|  '.'		      {d ".";   T_DOT }
| ident               {d (Lexing.lexeme lexbuf); 
		       scan_ident (Lexing.lexeme lexbuf)}
| eof                 {d "eof"; T_EOF }
| _                   {syntax_error "Invalid symbol";
                       token lexbuf} 
and comment = 
  parse 
    "*/"              { d (Lexing.lexeme lexbuf);  () }
|    _                { comment lexbuf }  
    
and str =
  parse '"'           {""}
| _                   {let cur = Lexing.lexeme lexbuf in cur ^  (str lexbuf)}

    {
     let init () = 
       init_lexicon ()
   }
