{
    
module P = Cmm_parse    (* tokens are defined here *)
module E = Error

let nl lexbuf map =
    let next = (Lexing.lexeme_start lexbuf) + 1     in
        Srcmap.nl map next
    
let tab lexbuf map = ()
let location lexbuf map =
    Srcmap.location map (Lexing.lexeme_start lexbuf)
     
let error lexbuf msg = Error.error msg
let get         = Lexing.lexeme
let getchar     = Lexing.lexeme_char
let strlen      = String.length
let pos_start   = Lexing.lexeme_start
let pos_end     = Lexing.lexeme_end
let substr      = Auxfuns.substr
let keywords    = Hashtbl.create 127
let keyword s   = Hashtbl.find keywords s

let _ = Array.iter (fun (str,tok) -> Hashtbl.add keywords str tok)
    [|("aborts"         , P.ABORTS)
    ; ("align"          , P.ALIGN)
    ; ("aligned"        , P.ALIGNED)
    ; ("also"           , P.ALSO)
    ; ("as"             , P.AS)
    ; ("big"            , P.BIG)
    ; ("byteorder"      , P.BYTEORDER)
    ; ("case"           , P.CASE)
    ; ("const"          , P.CONST)
    ; ("continuation"   , P.CONTINUATION)
    ; ("cut"            , P.CUT)
    ; ("cuts"           , P.CUTS)
    ; ("else"           , P.ELSe)
    ; ("equal"          , P.EQUAL)
    ; ("export"         , P.EXPORT)
    ; ("fails"          , P.FAILS)
    ; ("foreign"        , P.FOREIGN)
    ; ("goto"           , P.GOTO)
    ; ("if"             , P.IF)
    ; ("import"         , P.IMPORT)
    ; ("in"             , P.IN)
    ; ("invariant"      , P.INVARIANT)
    ; ("jump"           , P.JUMP)
    ; ("limitcheck"     , P.LIMITCHECK)
    ; ("little"         , P.LITTLE)
    ; ("memsize"        , P.MEMSIZE)
    ; ("never"          , P.NEVER)
    ; ("pragma"         , P.PRAGMA)
    ; ("register"       , P.REGISTER)
    ; ("reads"          , P.READS)
    ; ("return"         , P.RETURN)
    ; ("returns"        , P.RETURNS)
    ; ("section"        , P.SECTION)
    ; ("semi"           , P.SEMI)
    ; ("span"           , P.SPAN)
    ; ("stackdata"      , P.STACKDATA)
    ; ("switch"         , P.SWITCH)
    ; ("target"         , P.TARGET)
    ; ("targets"        , P.TARGETS)
    ; ("to"             , P.TO)
    ; ("typedef"        , P.TYPEDEF)
    ; ("unicode"        , P.UNICODE)
    ; ("unwinds"        , P.UNWINDS)
    ; ("writes"         , P.WRITES)

    ; ("float"          , P.FLOATREPR)
    ; ("charset"        , P.CHARSET)
    ; ("pointersize"    , P.PTRSIZE)
    ; ("wordsize"       , P.WRDSIZE)

    |]
let rec decode_escape = function
    | 'a'  -> 7
    | 'b'  -> 8
    | 'n'  -> 10
    | 'r'  -> 13
    | 't'  -> 9
    | '\\' -> 92
    | '\'' -> 39
    | '"'  -> 34
    | '?'  -> 63
    |  _   -> Impossible.impossible "unknown escape sequence"

let decode_hex c = match c with 
    | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
    | '0' .. '9' -> Char.code c - Char.code '0'
    | _          -> Impossible.impossible 
                        ("not a hexadecimal character: "^Char.escaped c)
}
let digit       = ['0'-'9']
let octal       = ['0'-'7']
let hex         = ['0'-'9' 'A'-'F' 'a'-'f']

let printable   = [' '-'~']     (* add 8bit chars *)
let alpha       = ['a'-'z' 'A'-'Z']
let misc        = ['.' '_' '$' '@']

let escape      = ['\\' '\'' '"' 'a' 'b' 'f' 'n' 'r' 't' '?' ]

let sign        = ['+' '-']
let nat         = digit+
let uint        = ['1'-'9'] digit* ['u' 'U']    (* unsigned decimal *)
let zerou       = '0' ['u' 'U']                 (* unsigned decimal zero *)
let hexint      = '0' ['x' 'X'] hex+            (* hex *)
let octint      = '0' digit*                    (* octal *)
let sint        = ['1'-'9'] digit*         (* signed decimal *)
let frac        = nat '.' nat
let exp         = ['e''E'] sign? nat
let float       = frac exp? 
                | nat exp

let cxxcomment  = "//" [^ '\n']*
                
let id          = (alpha | misc) (alpha | misc | digit)*
let ws          = [' ' '\012' '\r']  (* SP FF CR *)
let nl          = '\n'          
let tab         = '\t'
rule token = parse
    eof         { fun map -> P.EOF          }
  | ws+         { fun map -> token lexbuf map }
  | tab         { fun map -> tab lexbuf map; token lexbuf map }
  | nl          { fun map -> nl lexbuf map ; token lexbuf map }
  | nl ws* '#'  { fun map -> line lexbuf map 0; token lexbuf map }
  | ws* '#'     { fun map -> 
                  if Lexing.lexeme_start lexbuf = 0 then 
                        ( line lexbuf map 0
                        ; token lexbuf map
                        )
                  else
                        error lexbuf "illegal character" 
                }
  | ";"         { fun map -> P.SEMI         }
  | ":"         { fun map -> P.COLON        }
  | "::"        { fun map -> P.CCOLON       }
  | ","         { fun map -> P.COMMA        }
  | ".."        { fun map -> P.DOTDOT       }
  
  | "("         { fun map -> P.LPAREN       }
  | ")"         { fun map -> P.RPAREN       }
  | "{"         { fun map -> P.LBRACE       }
  | "}"         { fun map -> P.RBRACE       }
  | "["         { fun map -> P.LBRACKET     }
  | "]"         { fun map -> P.RBRACKET     }
  | "%%"        { fun map -> P.PPERCENT     }

  
  | "="         { fun map -> P.EQUAL        }
  

  (* infix/prefix operators *)
  
  | "+"         { fun map -> P.PLUS(get lexbuf)      }
  | "-"         { fun map -> P.MINUS(get lexbuf)     }
  | "*"         { fun map -> P.STAR(get lexbuf)      }
  | "/"         { fun map -> P.SLASH(get lexbuf)     }
  | "%"         { fun map -> P.PERCENT(get lexbuf)   }
  | ">>"        { fun map -> P.GGREATER(get lexbuf)  }
  | "<<"        { fun map -> P.LLESS(get lexbuf)     }
  | "&"         { fun map -> P.AMPERSAND(get lexbuf) }
  | "|"         { fun map -> P.BAR(get lexbuf)       }
  | "^"         { fun map -> P.CARET(get lexbuf)     }
  | "~"         { fun map -> P.TILDE(get lexbuf)     }
  | "=="        { fun map -> P.EEQ(get lexbuf)       }
  | "!="        { fun map -> P.NEQ(get lexbuf)       }
  | "<"         { fun map -> P.LT(get lexbuf)        }
  | "<="        { fun map -> P.LEQ(get lexbuf)       }
  | ">"         { fun map -> P.GT(get lexbuf)        }
  | ">="        { fun map -> P.GEQ(get lexbuf)       }

  | "`" id "`"  { fun map -> P.INFIXOP(substr 1 (-1) (get lexbuf)) }
              
  | "bits" nat  { fun map -> 
                  let s = substr 4 0 (get lexbuf) in
                  P.BITSn (int_of_string s)
                }
  | id          { fun map ->  
                  let s  = get lexbuf in 
                  let k  = try keyword s with Not_found -> P.ID s in
                    if k = P.PRAGMA then pragma1 lexbuf map else k  
                } 
  | '%' id      { fun map ->
                  let s = substr 1 0 (get lexbuf)
                  in P.PRIMOP(s)
                }
  | float       { fun map -> P.FLT  (get lexbuf) }
  | sint        { fun map -> P.SINT (get lexbuf) }
  | uint        { fun map -> P.UINT (get lexbuf) }
  | zerou       { fun map -> P.UINT (get lexbuf) }
  | hexint      { fun map -> P.UINT (get lexbuf) }
  | octint      { fun map -> P.UINT (get lexbuf) }
  
  | "/*"        { fun map -> comment1 lexbuf map }
  | cxxcomment  { fun map -> token lexbuf map (* skip comment *) }
  
  | "\""        { fun map -> string  lexbuf map (Buffer.create 80) }
  | "'"         { fun map -> character lexbuf map } 
 
  | _           { fun map -> error lexbuf "illegal character" }
and character = parse
    '\\'                        { fun map ->
                                  let i = escape lexbuf in
                                  if i >= 256 then
                                    error lexbuf "character literal too large"
                                  else  
                                    character_end lexbuf map i
                                }  
  | printable                   { fun map -> 
                                  let c = getchar lexbuf 0 in
                                  character_end lexbuf map (Char.code c)   
                                }
                                  
  | _                           { fun map ->
                                  error lexbuf 
                                    ( "illegal character literal: "
                                    ^  get lexbuf
                                    )
                                }

and character_end = parse
    "'"                         { fun map i -> P.CHAR(i)           }
  | _                           { fun map i -> 
                                  error lexbuf 
                                    ( "illegal character literal (too many characters): "
                                    ^  get lexbuf
                                    ) 
                                }  
and escape = parse
    |  escape                   { decode_escape (getchar lexbuf 0) }
    |  ['x' 'X'] hex            { decode_hex (getchar lexbuf 1)    }
    |  ['x' 'X'] hex hex        { decode_hex (getchar lexbuf 1) * 16 + 
                                  decode_hex (getchar lexbuf 2) }
    |  octal                    { decode_hex (getchar lexbuf 0) }
    |  octal octal              { decode_hex (getchar lexbuf 0) * 8 +
                                  decode_hex (getchar lexbuf 1) }
    |  octal octal octal        { decode_hex (getchar lexbuf 0) * 64 +
                                  decode_hex (getchar lexbuf 1) * 8  +
                                  decode_hex (getchar lexbuf 2) }
    | _                         { error lexbuf "illegal escape sequence" }
and comment1 = parse
    eof                         { fun map ->
                                  error lexbuf "unterminated comment" 
                                }
  | [^ '*' '\n' '\t' '/']+      { fun map ->
                                  comment1 lexbuf map
                                }
  | nl                          { fun map ->
                                  nl lexbuf map; comment1 lexbuf map 
                                }
  | nl '#'                      { fun map -> 
                                  line lexbuf map 0; comment1 lexbuf map
                                }
  
  | tab                         { fun map -> 
                                  tab lexbuf map; comment1 lexbuf map
                                }
  | "*"                         { fun map ->
                                  comment1 lexbuf map
                                }
  | "*/"                        { fun map ->
                                  token lexbuf map 
                                }
  | _                           { fun map ->
                                  comment1 lexbuf map 
                                }
and string = parse
    eof                         { fun map buf -> 
                                  error lexbuf "unterminated string" 
                                }
  | "\""                        { fun map buf -> P.STR (Buffer.contents buf) 
                                  (* we are done *)
                                }

  | [^ '\000'-'\031'
       '\128'-'\255'
       '"' '\\' ]+              { fun map buf ->
                                  let s    = get lexbuf              in
                                  ( Buffer.add_string buf s
                                  ; string lexbuf map buf
                                  )
                                }
  | '\\'                        { fun map buf -> 
                                  let i = escape lexbuf in
                                  if i >= 256 then
                                    error lexbuf "character literal too large"
                                  else
                                    ( Buffer.add_char buf (Char.chr(i))
                                    ; string lexbuf map buf
                                    )
                                }    
  | _                           { fun map buf ->
                                  error lexbuf "illegal character in string"
                                }
and pragma1 = parse
    eof                 { fun map -> P.EOF }
  | ws+                 { fun map -> pragma1 lexbuf map }
  | tab                 { fun map -> tab lexbuf map; pragma1 lexbuf map }
  | nl                  { fun map -> nl lexbuf map;  pragma1 lexbuf map } 
  | id                  { fun map -> 
                          let s  = get lexbuf in 
                          try ( match keyword s with 
                              | _     -> pragma2 lexbuf map s
                              )
                          with Not_found -> pragma2 lexbuf map s
                        }
  | _                   { fun map -> 
                          error lexbuf "id for pragma expected" 
                        }


and pragma2 = parse
    eof                 { fun map id -> 
                          error lexbuf "pragma body expected" 
                        }
  | ws+                 { fun map id -> pragma2 lexbuf map id }
  | tab                 { fun map id -> tab lexbuf map; pragma2 lexbuf map id}
  | nl                  { fun map id -> nl  lexbuf map; pragma2 lexbuf map id}
  | '{'                 { fun map id -> 
                          pragma3 lexbuf map 0 
                        }
  | _                   { fun map id -> 
                          error lexbuf "pragma body expected" 
                        }
and pragma3 = parse
    eof                         { fun map level ->
                                  error lexbuf "unterminated pragma" 
                                }
  | [^ '{' '}'  '\n' '\t' 
       '/' '\'' '"']+           { fun map level ->
                                  pragma3 lexbuf map level
                                }
  | nl                          { fun map level ->
                                  nl lexbuf map
                                ; pragma3 lexbuf map level 
                                }
  | tab                         { fun map level -> 
                                  tab lexbuf map; pragma3 lexbuf map level
                                }
  | '{'                         { fun map level -> 
                                  pragma3 lexbuf map (level+1)
                                }
  | '}'                         { fun map level ->
                                  if   level = 0 
                                  then token lexbuf map
                                  else pragma3 lexbuf map (level-1)
                                }
  | cxxcomment                  { fun map -> pragma3 lexbuf map (* ignore *) } 
  | "/*"                        { fun map level -> 
                                  ignore (comment1 lexbuf map) 
                                ; pragma3 lexbuf map level
                                }
  | "\""                        { fun map level -> 
                                  ignore (string lexbuf map (Buffer.create 80))
                                ; pragma3 lexbuf map level
                                }
  | "'"                         { fun map level -> 
                                  ignore (character lexbuf map)
                                ; pragma3 lexbuf map level
                                }
  
  | _                           { fun map level ->
                                  pragma3 lexbuf map level 
                                }
and line = parse 
    eof                 { fun map l ->
                          error lexbuf "unterminated line directive" 
                        }
  | ws+                 { fun map l -> line lexbuf map l }
  | tab                 { fun map l -> line lexbuf map l }
  | '"'                 { fun map l ->
                          let buf      = Buffer.create 80 in
                          let _        = string lexbuf map buf in
                          let file     = Buffer.contents buf in
                          let pos      = Lexing.lexeme_start lexbuf in
                          let location = file, l-1, 1 in
                                ( Srcmap.sync map pos location
                                ; () (* return *)
                                )
                        }
  | nat                 { fun map l -> 
                          
                          (* inline'ing the l' expression caused an
                          int_of_string failure with ocamlopt *)
                          
                          let l' = int_of_string (get lexbuf)
                          in  line lexbuf map l'
                        }
  | id                  { fun map l ->
                          line lexbuf map l
                        }
  | _                   { fun map l -> 
                          error lexbuf 
                          "illegal character in line directive"
                        }
{   (* start of epilog *)
    
let scan map lexbuf =
    token lexbuf map
    let tok2str = function

    | P.ABORTS            -> "ABORTS"
    | P.ALIGN             -> "ALIGN"
    | P.ALIGNED           -> "ALIGNED"
    | P.ALSO              -> "ALSO"
    | P.AS                -> "AS"
    | P.COLON             -> "COLON"
    | P.CCOLON            -> "CCOLON"
    | P.COMMA             -> "COMMA"
    | P.CONST             -> "CONST"
    | P.CONTINUATION      -> "CONTINUATION"
    | P.CUT               -> "CUT"
    | P.CUTS              -> "CUTS"
    | P.ELSe              -> "ELSE"
    | P.EOF               -> "EOF"
    | P.EQUAL             -> "EQUAL"
    | P.EXPORT            -> "EXPORT"
    | P.FAILS             -> "FAILS"
    | P.FOREIGN           -> "FOREIGN"
    | P.GOTO              -> "GOTO"
    | P.IF                -> "IF"
    | P.IMPORT            -> "IMPORT"
    | P.IN                -> "IN"
    | P.INVARIANT         -> "INVARIANT"
    | P.JUMP              -> "JUMP"
    | P.LBRACE            -> "LBRACE"
    | P.LBRACKET          -> "LBRACKET"
    | P.LPAREN            -> "LPAREN"
    | P.NEVER             -> "NEVER"
    | P.PPERCENT          -> "PPERCENT"
    | P.PRAGMA            -> "PRAGMA"
    | P.RBRACE            -> "RBRACE"
    | P.RBRACKET          -> "RBRACKET"
    | P.READS             -> "READS"
    | P.REGISTER          -> "REGISTER"
    | P.RETURN            -> "RETURN"
    | P.RETURNS           -> "RETURNS"
    | P.RPAREN            -> "RPAREN"
    | P.SECTION           -> "SECTION"
    | P.SEMI              -> "SEMI"
    | P.SPAN              -> "SPAN"
    | P.STACKDATA         -> "STACKDATA"
    | P.TARGETS           -> "TARGETS"
    | P.TO                -> "TO"
    | P.UNICODE           -> "UNICODE"
    | P.UNWINDS           -> "UNWINDS"
    | P.WRITES            -> "WRITES"

    | P.TYPEDEF           -> "TYPEDEF" 
    | P.MEMSIZE           -> "MEMSIZE"
    | P.BYTEORDER         -> "BYTEORDER"
    | P.LIMITCHECK        -> "LIMITCHECK"
    | P.LITTLE            -> "LITTLE"
    | P.BIG               -> "BIG"
    | P.CASE              -> "CASE"
    | P.DEFAULT           -> "DEFAULT"
    | P.TARGET            -> "TARGET"
    | P.DOTDOT            -> "DOTDOT"
    | P.SWITCH            -> "SWITCH"

    | P.WRDSIZE           -> "WORSIZE"
    | P.PTRSIZE           -> "POINTERSIZE"
    | P.FLOATREPR         -> "FLOAT"
    | P.CHARSET           -> "CHARSET"
    

    | P.AMPERSAND(s)      -> "AMPERSAND(" ^ s ^ ")"
    | P.BAR(s)            -> "BAR(" ^       s ^ ")"
    | P.CARET(s)          -> "CARET(" ^     s ^ ")"
    | P.EEQ(s)            -> "EEQ(" ^       s ^ ")"
    | P.GEQ(s)            -> "GEQ(" ^       s ^ ")"
    | P.GGREATER(s)       -> "GGREATER(" ^  s ^ ")"
    | P.GT(s)             -> "GT(" ^        s ^ ")"
    | P.LEQ(s)            -> "LEQ(" ^       s ^ ")"
    | P.LLESS(s)          -> "LLESS(" ^     s ^ ")"
    | P.LT(s)             -> "LT(" ^        s ^ ")"
    | P.MINUS(s)          -> "MINUS(" ^     s ^ ")"
    | P.NEQ(s)            -> "NEQ(" ^       s ^ ")"
    | P.PERCENT(s)        -> "PERCENT(" ^   s ^ ")"
    | P.PLUS(s)           -> "PLUS(" ^      s ^ ")"
    | P.SLASH(s)          -> "SLASH(" ^     s ^ ")"
    | P.STAR(s)           -> "STAR(" ^      s ^ ")"
    | P.TILDE(s)          -> "TILDE(" ^     s ^ ")"
    | P.UMINUS(s)         -> "UMINUS" ^     s ^ ")"
    
    
    | P.ID(s)             -> "ID(" ^        s ^ ")"
    | P.STR(s)            -> "STR(" ^       String.escaped s ^ ")"
    | P.INFIXOP(s)        -> "INFIXOP(" ^   s ^ ")"
    | P.PRIMOP(s)         -> "PRIMOP(" ^    s ^ ")"
    | P.SINT(s)           -> "SINT(" ^       s ^ ")"
    | P.UINT(s)           -> "UINT(" ^       s ^ ")"
    | P.FLT(s)            -> "FLT(" ^       s ^ ")"
    | P.CHAR(i)           -> "CHAR(" ^ Char.escaped (Char.chr i) ^ ")"

    | P.BITSn(i)          -> "BITSn(" ^ string_of_int i ^ ")"

} (* end of epilog *)
