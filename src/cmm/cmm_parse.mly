%{
    module A = Cmm_ast
    module E = Error

    
let p  ()  = (Parsing.symbol_start (), Parsing.symbol_end())
let pn n   = (Parsing.rhs_start n, Parsing.rhs_end n)
let px ()  = (Parsing.symbol_start ())

let rev    = List.rev
let dep msg =
  let deprecated = Reinit.ref false in
  fun x ->
    if not (!deprecated) then
      begin
        Printf.eprintf "C-- warning: %s\n" msg;
        deprecated := true;
      end;
    x

let rdep = dep "Use of 'register' keyword is deprecated"
let noeqdep = dep "Hardware register name without '=' is deprecated"
let mkRegs v (hint,ty,regs) = List.map (fun (id,reg) -> (v, hint, ty, id, reg)) regs
let mkTypedefs ty names = List.map (fun n -> A.Typedef(n,ty)) names
let str2uint str =
  let b = Bits.U.of_string str Nativeint.size in
  try Bits.U.to_int b with Bits.Overflow ->
    Error.errorf "constant %s overflows %d-bit native integer" str (Nativeint.size-1)
let ast_mem ty exp (align, alias) = A.Mem(ty, exp, align, alias)
let uminus e =
  let rec strip = function
    | A.ExprAt(e, _) -> strip e
    | A.Sint(s, w) -> A.Sint("-" ^ s, w)
    | _ -> A.UnOp("-", e) in
  strip e
                                    
%}
%token ABORTS ALIGN ALIGNED ALSO AS AMPERSAND BIG BYTEORDER CASE COLON CCOLON
%token COMMA CONST CONTINUATION CUT CUTS DEFAULT DOTDOT ELSe EOF EQUAL
%token EXPORT FAILS FOREIGN GOTO IF IMPORT IN INFIXOP INVARIANT JUMP LBRACE
%token LBRACKET LIMITCHECK LITTLE LPAREN MEMSIZE NEVER PPERCENT RBRACE RBRACKET 
%token READS REGISTER RETURN RETURNS RPAREN SECTION SEMI SPAN STACKDATA SWITCH 
%token TARGET TARGETS TO TYPEDEF UNICODE UNWINDS WRITES
%token WRDSIZE PTRSIZE FLOATREPR CHARSET

/* pragmas */

%token PRAGMA

/* infix and prefix operators */

%token <string> EEQ NEQ LT LEQ GT GEQ
%token <string> BAR                      
%token <string> CARET                    
%token <string> AMPERSAND                
%token <string> LLESS GGREATER           
%token <string> PLUS MINUS               
%token <string> PERCENT STAR SLASH       
%token <string> TILDE UMINUS                    
%token <string> INFIXOP
%token <string> PRIMOP 

%token <string>         ID
%token <string>         STR
%token <int>            BITSn

%token <string>         SINT
%token <string>         UINT
%token <string>         FLT
%token <int>            CHAR
/* lvalue conflict resolution */
%right      ID
%right      LBRACKET

/* conflict resolution for %foo() and %foo. We ensure the '(' has
   higher precedence and gets shifted. Take the following two
   declarations out and you get a shift/reduce conflict which defaults
   to shifting. This is correct, but we also like to get rid of the
   warning. */
%nonassoc   PRIMOP
%nonassoc   LPAREN

%nonassoc   INFIXOP
%nonassoc   EEQ NEQ LT LEQ GT GEQ
%left       BAR                      
%left       CARET                    
%left       AMPERSAND                
%left       LLESS GGREATER           
%left       PLUS MINUS               
%left       PERCENT STAR SLASH       
%right      TILDE UMINUS

%start program
%type <Cmm_ast.program>program

%%

program     :   toplevels                      { rev $1}

toplevels   :   toplevels toplevelAt           { $2::$1 }
            |   /**/                           { []     }
toplevelAt  :   toplevel                       { A.ToplevelAt($1,p())       }
toplevel    :   SECTION STR LBRACE sections RBRACE  { A.Section($2, rev $4) }
            |   procedure                      { A.TopProcedure($1)         }
            |   declAt                         { A.TopDecl($1)              }
sections    :   sections sectionAt             { $2 :: $1 }
            |   /**/                           { []       }

sectionAt   :   section                        { A.SectionAt($1,p()) }
section     :   procedure                      { A.Procedure($1)     }
            |   datum                          { A.Datum($1)         }
            |   span                           { A.SSpan($1)         }
            |   declAt                         { A.Decl($1)          }

declAt      :   decl                           { A.DeclAt($1,p()) }   
decl        :   INVARIANT /*----*/ registers SEMI
                    { A.Registers(mkRegs A.Invariant $2) }
            |   /*-----*/ /*----*/ registers SEMI
                    { A.Registers(mkRegs A.Variant $1)   }
            |   INVARIANT REGISTER registers SEMI
                    { rdep (A.Registers(mkRegs A.Invariant $3)) }
            |   /*-----*/ REGISTER registers SEMI
                    { rdep (A.Registers(mkRegs A.Variant $2))   }
          
            |   EXPORT ty exports  SEMI        { A.Export(Some $2, $3) }
            |   EXPORT    exports  SEMI        { A.Export(None,$2)    }
            |   IMPORT ty imports  SEMI        { A.Import(Some $2,$3)           }
            |   IMPORT    imports  SEMI        { A.Import(None,$2)           }
            |   CONST     ID EQUAL exprAt SEMI { A.Const(None,$2,$4)       }
            |   CONST  ty ID EQUAL exprAt SEMI { A.Const(Some $2,$3,$5)    }
            |   TYPEDEF ty names SEMI          { A.Typedef($2,rev $3)      }
            |   TARGET properties SEMI         { A.Target(rev $2)          }
uint        : SINT { str2uint $1 }
            | UINT { str2uint $1 }

property    :   MEMSIZE uint                   { A.Memsize $2      }
            |   BYTEORDER BIG                  { A.ByteorderBig    }
            |   BYTEORDER LITTLE               { A.ByteorderLittle }
            |   FLOATREPR STR                  { A.FloatRepr $2    }
            |   CHARSET STR                    { A.Charset $2      }
            |   PTRSIZE uint                   { A.PointerSize $2  }
            |   WRDSIZE uint                   { A.WordSize $2     }

properties  :   properties property            { $2 :: $1 }
            |   /**/                           { []       }
span        :   SPAN sexprAt exprAt
                LBRACE sections RBRACE         { $2, $3, rev $5 }
datumAt     :   datum                          { A.DatumAt($1,p()) }
datum       :   ID COLON                       { A.Label $1 }
            |   ALIGN uint SEMI                { A.Align $2 }
            |   tyAt size opt_initAt SEMI      { A.MemDecl($1,$2,$3)}

opt_initAt : initAt { Some $1 } | { None }
initAt      :   init                           { A.InitAt($1,p()) }
init        :   LBRACE   exprs RBRACE          { A.InitExprs($2)  }
            |   LBRACE         RBRACE          { A.InitExprs([])  }
            |   STR                            { A.InitStr($1)    }
            |   string16                       { A.InitUStr($1)   }
registers   :   STR    tyAt regs opt_comma     { Some $1, $2, rev $3 }
            |          tyAt regs opt_comma     { None   , $1, rev $2 }

regs        :   regs COMMA ID optEq STR        { ($3, Some $5)::$1 }
            |   regs COMMA ID                  { ($3, None   )::$1 }
            |   ID optEq STR                   { [($1,Some $3)]    }
            |   ID                             { [($1,None   )]    }

optEq       :   { noeqdep () }
            |   EQUAL  { () }

size        :   LBRACKET exprAt RBRACKET       { A.FixSize($2) }
            |   LBRACKET       RBRACKET        { A.DynSize     }
            |                                  { A.NoSize      }

procedure   :   conv ID frmls body             {  $1, $2, $3, $4, p() }
            |        ID frmls body             {None, $1, $2, $3, p() }

body        :   LBRACE body0 RBRACE            { rev $2 }

body0       :   body0 bodyAt                   { $2 :: $1 }
            |   /**/                           { []       }

bodyAt      :   body1                          { A.BodyAt($1,p()) }
body1       :   declAt                         { A.DeclBody $1    }
            |   stackdecl                      { A.DataBody $1    }
            |   stmtAt                         { A.StmtBody $1    }

frmls       :   LPAREN  formals RPAREN         { $2 }
            |   LPAREN          RPAREN         { [] }
actls       :   LPAREN  actuals RPAREN         { $2 }
            |   LPAREN          RPAREN         { [] }

formals     :   formals_ opt_comma             { rev $1   }
formals_    :   formals_ COMMA formal          { $3 :: $1 }
            |   formal                         { [$1]     }

actuals     :   actuals_ opt_comma             { rev $1   }
actuals_    :   actuals_ COMMA actual          { $3 :: $1 }
            |   actual                         { [$1]     }

formal      :   bare_formal                    { p(), $1 }
bare_formal :   opt_kind opt_invariant opt_register tyAt ID opt_aligned
                { $1, $2, $4, $5, $6 }

opt_kind : STR { Some $1 } | { None }
opt_invariant : INVARIANT { A.Invariant } | { A.Variant }
opt_register  : REGISTER { () } | { () }

cformal     :   opt_kind ID opt_aligned        { p(), $1, $2, $3 }

cformals    :   cformals_ opt_comma            { rev $1   }
            |   /**/      opt_comma            { []       }
cformals_   :   cformals_ COMMA cformal        { $3 :: $1 }
            |   cformal                        { [$1]     }

actual      :   opt_kind exprAt opt_aligned    { $1, $2, $3 }
stackdecl   :   STACKDATA LBRACE data RBRACE   { rev $3 }

data        :   data  datumAt                  { $2 :: $1 }
            |   /**/                           { []       }

conv        :   FOREIGN STR                    { Some $2 }

aligned     :   ALIGNED uint                   { $2 }

flowAt      :   flow                           { A.FlowAt($1,p()) }
flow        :   ALSO CUTS     TO names         { A.CutsTo($4)     }
            |   ALSO UNWINDS  TO names         { A.UnwindsTo($4)  }
            |   ALSO RETURNS  TO names         { A.ReturnsTo($4)  }
            |   ALSO ABORTS                    { A.Aborts         }
            |   NEVER RETURNS                  { A.NeverReturns   }

flows       :   flows flowAt                   { $2 :: $1 }
            |   /**/                           { []       }

targets     :   TARGETS names                  { $2 }
            |   /**/                           { [] }

aliasAt     :   alias                          { A.AliasAt($1,p()) }
alias       :   READS  opt_names               { A.Reads ($2)       }
            |   WRITES opt_names               { A.Writes($2)       }

procanns    :   procanns flowAt                { A.Flow  $2 :: $1 }
            |   procanns aliasAt               { A.Alias $2 :: $1 }
            |   /**/                           { []       }
nameOrMemAt :   nameOrMem                      { A.NameOrMemAt($1,p()) } 
nameOrMem   :   nameOrMem0                     { $1 } 
            |   ID aligned                     { A.Name(None, $1, Some $2) }
            |   STR ID opt_aligned             { A.Name(Some $1,$2,$3) } 
            
nameOrMemAt0:   nameOrMem0                    { A.NameOrMemAt($1,p()) }
nameOrMem0  :   ID                            { A.Name(None, $1, None)}
            |   mem_type LBRACKET exprAt mem_properties RBRACKET { ast_mem $1 $3 $4 }

mem_type : sty { $1 }
         | ID  { A.TypeSynonym($1) }

mem_properties : aligned opt_in_ids { (Some $1, $2) }
               | IN ids opt_aligned { ($3, $2) }
               |                    { (None, []) }

opt_aligned : { None } | aligned { Some $1 }
opt_in_ids  : { [] } | IN ids { $2 }
ids         : ID { [$1] }
            | ID COMMA ids { $1 :: $3 }


nameOrMems  :   nameOrMems_ opt_comma          { rev $1   }
nameOrMems_ :   nameOrMems_ COMMA nameOrMemAt  { $3 :: $1 }
            |   nameOrMemAt                    { [$1]     }

tyAt        :   ty                             { A.TyAt($1,p()) }
ty          :   sty                            { $1             }
            |   ID                             { A.TypeSynonym($1)  }

sty         :   BITSn                          { A.BitsTy($1)     }

returnto    :   LT sexprAt SLASH sexprAt GT    { Some($2,$4) }
            |   /**/                           { None        }
stmtAt      :   stmt                           { A.StmtAt($1,p())     }
stmt        :   SEMI                           { A.EmptyStmt          }
            |   ID COLON                       { A.LabelStmt $1       }
            |   SPAN sexprAt sexprAt body      { A.SpanStmt($2,$3,$4) }
            |   nameOrMems EQUAL exprs SEMI       
                { A.AssignStmt($1,List.map (fun e -> None,e) $3)  }

            |   nameOrMems EQUAL conv PPERCENT ID actls flows SEMI
                { A.PrimStmt($1, $3, $5, $6, rev $7) }      
            
            |                 conv PPERCENT ID actls flows SEMI
                { A.PrimStmt([], $1, $3, $4, rev $5) }      
            
            |   nameOrMems EQUAL conv expr actls targets procanns SEMI
                { A.CallStmt($1, $3, $4, $5, $6, rev $7)  }
            
            |                 conv expr actls targets procanns SEMI
                { A.CallStmt([], $1, $2, $3, $4, rev $5)  }
                                          
            |   nameOrMems EQUAL      PPERCENT ID actls flows SEMI
                { A.PrimStmt($1, None, $4, $5, rev $6)    }
            
            |                      PPERCENT ID actls flows SEMI
                { A.PrimStmt( [], None, $2, $3, rev $4)}
            
            |   nameOrMems EQUAL      expr actls targets procanns SEMI
                { A.CallStmt($1, None, $3, $4, $5, rev $6)}

            |                      expr actls targets procanns SEMI
                { A.CallStmt([], None, $1, $2, $3, rev $4)}
 
            |   IF exprAt body                      {A.IfStmt($2,$3,[])}
            |   IF exprAt body  ELSe body           {A.IfStmt($2,$3,$5)}
            |   GOTO exprAt targets SEMI            {A.GotoStmt($2,$3)}
            |   CONTINUATION ID LPAREN cformals RPAREN COLON
                                                    {A.ContStmt($2,$4)}
            |   CUT TO expr actls flows SEMI        {A.CutStmt($3,$4, rev $5)  }
            |   LIMITCHECK exprAt limitfailure SEMI {A.LimitcheckStmt($2,$3)  }
            |   conv JUMP exprAt       targets SEMI {A.JumpStmt($1  ,$3,[],$4) }
            |        JUMP exprAt       targets SEMI {A.JumpStmt(None,$2,[],$3) }
            |   conv JUMP exprAt actls targets SEMI {A.JumpStmt($1  ,$3,$4,$5) }
            |        JUMP exprAt actls targets SEMI {A.JumpStmt(None,$2,$3,$4) }
            |   conv RETURN returnto       SEMI     {A.ReturnStmt($1  ,$3,[])  }
            |        RETURN returnto       SEMI     {A.ReturnStmt(None,$2,[])  }
            |   conv RETURN returnto actls SEMI     {A.ReturnStmt($1  ,$3,$4)  }
            |        RETURN returnto actls SEMI     {A.ReturnStmt(None,$2,$3)  }
            |   SWITCH srange expr LBRACE arms RBRACE  
                        {A.SwitchStmt($2,$3, rev $5)}

limitfailure : FAILS TO exprAt {Some $3}
            |  /**/            {None}
srange      :   LBRACKET range RBRACKET        {Some $2}
            |   /**/                           {None}

range       :   expr                           {A.Point $1     }
            |   expr DOTDOT expr               {A.Range ($1,$3)}

ranges      :   ranges_                        { rev $1 }
            |   ranges_ COMMA                  { rev $1 }
            |   /**/                           { [] } 

ranges_     :   ranges_ COMMA range            { $3 :: $1 } 
            |   range                          { [$1]     } 

arms        :   arms armAt                     { $2 :: $1  }
            |   /**/                           { []        }

armAt       :   arm                            { A.ArmAt($1,p()) }
arm         :   CASE ranges COLON body         { A.Case($2,$4)   } 

sexprAt     :   sexpr                          { A.ExprAt($1, p()) }
sexpr       :   SINT opt_colon_ty              { A.Sint ($1, $2)   }
            |   UINT opt_colon_ty              { A.Uint ($1, $2)   }
            |   FLT  opt_colon_ty              { A.Float($1, $2)   }
            |   CHAR opt_colon_ty              { A.Char ($1, $2)   }
            |   nameOrMemAt0                   { A.Fetch $1        }
           /*   allow to write nullary OPs like constants */
            |   PRIMOP                         { A.PrimOp($1,[])   }
            |   LPAREN exprAt RPAREN           { $2                }
opt_colon_ty : CCOLON tyAt { Some $2 } | { None }
exprAt      :   expr                           { A.ExprAt($1, p())    }
expr:       |   sexpr                          { $1                   }
            |   PRIMOP actls                   { A.PrimOp($1,$2)      }

            |   exprAt PLUS       exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt MINUS      exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt PERCENT    exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt STAR       exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt SLASH      exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt LLESS      exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt GGREATER   exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt CARET      exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt BAR        exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt AMPERSAND  exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt EEQ        exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt NEQ        exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt LT         exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt LEQ        exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt GEQ        exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt GT         exprAt       { A.BinOp($1,$2,$3) }
            |   exprAt INFIXOP    exprAt       { A.PrimOp($2,[None,$1,None;
                                                              None,$3,None]) }
            |   TILDE exprAt %prec TILDE       { A.UnOp($1,$2)     }
            |   MINUS exprAt %prec UMINUS      { uminus $2         }

exprs       :   exprs_ opt_comma               { rev $1   }
exprs_      :   exprs_ COMMA exprAt            { $3 :: $1 }
            |   exprAt                         { [$1]     }
imports     :   imports_ opt_comma             { rev $1           }
imports_    :   imports_ COMMA STR AS ID       { (Some $3,$5)::$1 }
            |   imports_ COMMA        ID       { (None   ,$3)::$1 } 
            |   STR AS ID                      { [Some $1,$3]     }
            |   ID                             { [None   ,$1]     }

exports     :   exports_ opt_comma             { rev $1           }
exports_    :   exports_ COMMA ID AS STR       { ($3,Some $5)::$1 }
            |   exports_ COMMA ID              { ($3,None)::$1    }
            |   ID AS STR                      { [$1,Some $3]     }
            |   ID                             { [$1,None]        }

names       :   names_ opt_comma               { rev $1 }
names_      :   names_ COMMA ID                { $3:: $1}
            |   ID                             { [$1] }

opt_names   :   names { $1 }
            |   /**/  { [] }

opt_comma   :   COMMA                          {}
            |   /**/                           {}

string16    :   UNICODE LPAREN STR RPAREN      { $3 }
