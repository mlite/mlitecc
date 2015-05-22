type exp = Rtl.exp
type loc = Rtl.loc * Rtl.width
type rtl = Rtl.rtl

type name = string
type kind = string
type convention = string
type aligned    = int
type actual = kind * exp * Rtl.width * aligned
type 'a kinded = kind * 'a * aligned
type 'a flow  = { cuts : 'a list; unwinds : 'a list; areturns : 'a list;
                  returns : bool; aborts : bool }
type 'a cflow = { ccuts : 'a list; caborts : bool }
type 'a alias = { reads : 'a; writes : 'a }
type range = Bits.bits * Bits.bits   (* lo `leu` x `leu` hi, as in manual *)
type procname = string
type label    = string
type linktime = Reloc.t

type stmt =
  | If         of exp * stmt list * stmt list
  | Switch     of range option * exp * (range list * stmt list) list
  | Label      of label
  | Cont       of name * convention * Fenv.variable kinded list
  | Span       of (Bits.bits * linktime) * stmt list
  | Assign     of rtl
  | Call       of loc kinded list * convention * exp * actual list * procname list
                * name flow * name list option alias
  | Call'      of convention * exp * actual list * procname list
                    (* the dog ate the annotations *)
  | Goto       of exp * label list
  | Jump       of convention * exp * actual list * procname list
  | Cut        of convention * exp * actual list * name cflow 
  | Return     of convention * int * int * actual list
  | Limitcheck of convention * exp * limitfailure option
and limitfailure = { failcont : exp; reccont : exp; recname : name; }
val elab_stmts :
  (Rtl.rtl -> string option) -> Srcmap.map -> Cmm_ast.region -> 'a Fenv.Dirty.env' -> 
  Nast.stmt list -> stmt list Error.error
val elab_cformals : 
  Cmm_ast.region -> 'a Fenv.Dirty.env' -> Nast.cformal list ->
    Fenv.variable kinded list Error.error
val codelabels : stmt list -> label list
