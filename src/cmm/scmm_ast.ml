
(* module Ast*)
type std_int = int
type std_string = string
type identifier = std_string
type big_int = std_int


type name = (std_string)
and conv = (std_string)
and hint = (std_string)
and reg = (std_string)
and target = (std_string)
and alias_set = (std_string)
and size = (std_int)
and align = (std_int)
and aligned = (std_int)
and in_alias = (std_string)
and op = (std_string)
and region = (std_int * std_int)
and ty =
    TyAt of (ty * region)
  | BitsTy of (size)
  | TypeSynonym of (name)
      
and memloc = 
  | MemAt of (memloc * region)
  | Mem of (ty * aexpr * aligned option * in_alias list)

and reg_or_memloc = 
  | Reg of name
  | Memloc of memloc

and actual = (hint option * aexpr * aligned option)
and cactual = (hint option * cexpr * aligned option)
    
and cexpr =
    CexprAt of (cexpr * region)
  | Sint of (std_string * ty option)
  | Uint of (std_string * ty option)
  | Float of (std_string * ty option)
  | Char of (std_int * ty option)
  | SymAddr of name
  | CbinOp of (cexpr * op * cexpr)
  | CunOp of (op * cexpr)
  | CprimOp of (name * cactual list)
      
and aexpr = 
  | Cexpr of cexpr
  | FetchReg of name
      
and expr =
    ExprAt of (expr * region)
  | Aexpr of aexpr
  | FetchMem of memloc
  | BinOp of (aexpr * op * aexpr)
  | UnOp of (op * aexpr)
  | PrimOp of (name * actual list)
      
and import = 
    (* "read" as sysread *)
    (std_string option * name)
      
and export = (name * std_string option)
and variance =
    Invariant
  | Invisible
  | Variant
      
and register = (variance * hint option * ty * name * reg option)
and arch =
    Memsize of (std_int)
  | ByteorderBig
  | ByteorderLittle
  | FloatRepr of (std_string)
  | Charset of (std_string)
  | WordSize of (std_int)
  | PointerSize of (std_int)
      
and decl =
    DeclAt of (decl * region)
  | Import of (ty option * import list)
  | Export of (ty option * export list)
  | Const of (ty option * name * cexpr)
  | Typedef of (ty * name list)
  | Registers of (register list)
  | Pragma
  | Target of (arch list)
      
and bare_formal = 
    (* ("unsigned", variant, bits32, iargc) *)
    (hint option * variance * ty * name * aligned option)
      
and formal = (region * bare_formal)
and memsize =
    NoSize
  | DynSize
  | FixSize of (cexpr)
      
and init =
    InitAt of (init * region)
  | InitExprs of (cexpr list)
  | InitStr of (std_string)
  | InitUStr of (std_string)
      
and datum =
    DatumAt of (datum * region)
  | Label of (name)
  | Align of (align)
  | MemDecl of (ty * memsize * init option)
      
and cformal = (region * hint option * name * aligned option)
and flow =
    FlowAt of (flow * region)
  | CutsTo of (name list)
  | UnwindsTo of (name list)
  | ReturnsTo of (name list)
  | NeverReturns
  | Aborts
      
and mem =
    AliasAt of (mem * region)
  | Reads of (name list)
  | Writes of (name list)
      
and procann =
    Flow of (flow)
  | Alias of (mem)
      
and altcont = (expr * expr)
and range =
    Point of (cexpr)
  | Range of (cexpr * cexpr)
      
and guarded = (expr option * expr)
and arm =
    ArmAt of (arm * region)
  | Case of (range list * body list)

and asm = 
    { 
      (* optional name, constraints and expressions for outputs *)
      asm_outputs: (string option * string * name) list;
      (* optional name, constraints and expressions for inputs *)
      asm_inputs: (string option * string * expr) list; 
      asm_clobbers: string list (* clobbered registers *)
    }
      
and stmt =
    StmtAt of (stmt * region)
  | IfStmt of (expr * body list * body list)
  | SwitchStmt of (range option * aexpr * arm list)
  | LabelStmt of (name)
  | ContStmt of (name * cformal list)
  | SpanStmt of (cexpr * cexpr * body list)
  | StoreStmt of (memloc * guarded)
  | SaveStmt of (name * expr)
  | LoadStmt of (name * memloc)
  | CallStmt of (reg_or_memloc option *
      conv option *
      aexpr *
      actual list *
      target list *
      procann list)
  | PrimStmt of (reg_or_memloc option *
      conv option *
      name *
      actual list *
      flow list)
  | GotoStmt of (expr * target list)
  | JumpStmt of (conv option * expr * actual list * target list)
  | CutStmt of (expr * actual list * flow list)
  | ReturnStmt of (conv option * altcont option * actual list)
  | EmptyStmt
  | CommentStmt of (std_string)
  | LimitcheckStmt of (expr * expr option)
  | AsmStmt of (std_string list * asm option)
      
and body =
    BodyAt of (body * region)
  | DeclBody of (decl)
  | StmtBody of (stmt)
  | DataBody of (datum list)
      
and proc = 
    (* 
       #foo.c (10, 2)
       foreign "C" main (formal_list) { body_list }
    *)
    (conv option * name * formal list * body list * region)

and section =
    SectionAt of (section * region)
  | Decl of (decl)
  | Procedure of (proc)
  | Datum of (datum)
  | SSpan of (cexpr * cexpr * section list)
      
and toplevel =
    ToplevelAt of (toplevel * region)
  | Section of (name * section list)
  | TopDecl of (decl)
  | TopProcedure of (proc)
      
and program = (toplevel list)
