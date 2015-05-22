type ty  = Cmm_ast.ty
type exp = Cmm_ast.expr
type loc = Cmm_ast.name_or_mem

type 'a marked = Cmm_ast.region * 'a

type name = string
type kind = string
type convention = string
type aligned    = int
type cformal  = Cmm_ast.region * kind * name * aligned option
type actual   = kind * exp * aligned option
type flow     = Cmm_ast.flow list
type alias    = Cmm_ast.mem  list
type range    = Cmm_ast.range
type procname = string
type label    = string
type stmt =
  | StmtAt of stmt * Cmm_ast.region
  | If     of exp * stmt list * stmt list
  | Switch of range option * exp * (range list * stmt list) list
  | Label  of label
  | Cont   of name * convention * cformal list
  | Span   of exp * exp * stmt list
  | Assign of loc list * Cmm_ast.guarded list
  | Call   of loc list * convention * exp  * actual list * procname list * flow * alias
  | Prim   of loc list * convention * name * actual list * flow
  | Goto   of exp * label list
  | Jump   of convention * exp * actual list * procname list
  | Cut    of convention * exp * actual list * flow 
  | Return of convention * (exp * exp) option * actual list
  | Limitcheck of convention * exp * (exp * name) option (* (cookie,(failk,recname)) *)
type typedefn  = ty * name list
type constdefn = ty option * name * exp
type compile_time_defns = {
  types     : typedefn  marked list;
  constants : constdefn marked list;
}
type proc = {
    region        : Cmm_ast.region;
    cc            : convention;
    name          : name;
    formals       : (kind * Cmm_ast.variance * ty * name * aligned option) marked list;
    locals        : Cmm_ast.register marked list;
    pdecls        : compile_time_defns;
    continuations : (name * convention * cformal list) marked list;
    labels        : name marked list;  (* code labels *)
    stackdata     : datum marked list;
    code          : stmt list;
  }
and  datum =
  | Datalabel  of name
  | Align      of int
  | ReserveMem of ty * Cmm_ast.memsize * Cmm_ast.init option (*init always none on stackdata*)
  | Procedure  of proc                               (* never on stackdata *)
  | SSpan      of exp * exp * datum marked list      (* never on stackdata *)
type section = name * datum marked list
type t = {
  target   : Cmm_ast.arch marked list;
  imports  : (Cmm_ast.region * Cmm_ast.ty option * Cmm_ast.import list) list;
  exports  : (Cmm_ast.region * Cmm_ast.ty option * Cmm_ast.export list) list;
  globals  : Cmm_ast.register marked list;
  code_labels : name marked list list;
  udecls   : compile_time_defns;
  sections : section list
}
val program : Cmm_ast.toplevel list -> t
