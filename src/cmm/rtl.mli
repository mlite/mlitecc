type aggregation = 
    Register.aggregation = 
  | BigEndian
  | LittleEndian
  | Identity
      
type space = char * aggregation * Cell.t   (* name, byte order, cell size *)
type width = int
type count = Register.count = C of int

module Private: 
sig
  type aligned   = int     (* alignment guaranteed *)
  type assertion = aligned (* may one day include alias info *)
  type opr       = string * width list
  type const     = 
    | Bool      of bool
    | Bits      of Bits.bits            (* literal constant *)
    | Link      of Symbol.t * symkind * width  (* link-time constant *)
    | Diff      of const  * const   (* difference of two constants *)
    | Late      of string * width   (* late compile time constant *)
	
  and symkind = Code | Data | Imported (* three kinds of symbol, needed for PIC *)
      
  type exp        = 
    | Const     of const               
    | Fetch     of loc * width 
    | App       of opr * exp  list
	
  and  count     = Register.count = C of int
  and  loc       = 
    | Mem        of space * count * exp * assertion
    | Reg        of Register.t (* space * int * count *)         
        
    | Var        of string    (* name from C-- source   *)
        *  int       (* index for run-time API *)
        *  width
	
    | Global    of string * int * width (* global C-- variable *)
    | Slice     of width     (* number of bits in loc *) 
	*  int  (* index of least-significant bit of slice *)
        *  loc  (* location from which slice is drawn *)
	
  type effect     = 
    | Store     of loc * exp  * width
    | Kill      of loc
	
  type guarded    = exp  * effect
      
  type rtl        = Rtl of guarded list
end

type exp          (* denotes a compile-time or run-time value *)
type loc          (* mutable container of a bit vector *)
type rtl          (* effect of a computation *)
type opr          (* a pure function on values *)
type assertion    (* a claim about the run-time value of an address *)

val bool      : bool -> exp                    
val bits      : Bits.bits -> width -> exp      
val codesym   : Symbol.t -> width -> exp    (* locally defined code label (incl proc) *)
val datasym   : Symbol.t -> width -> exp    (* locally defined data label *)
val impsym    : Symbol.t -> width -> exp    (* imported symbol *)
val late      : string -> width -> exp         (* late compile time constant *)
val fetch     : loc -> width -> exp
val app       : opr -> exp list -> exp

val opr       : string -> width list -> opr
val none     : assertion
val aligned  : int -> assertion
val shift    : int -> assertion -> assertion
val shift_multiple : int -> assertion -> assertion
val alignment: assertion -> int
val mem      : assertion -> space -> count -> exp -> loc
val reg      : Register.t -> loc
val regx     : Register.x -> loc
val var      : string -> index:int -> width -> loc
val global   : string -> index:int -> width -> loc
val slice    : width -> lsb:int -> loc -> loc
val store     : loc -> exp -> width -> rtl
val kill      : loc -> rtl
val guard     : exp -> rtl -> rtl
val par       : rtl list -> rtl
val null      : rtl
val fetch_cvt : loc -> width -> exp
val store_cvt : loc -> exp -> width -> rtl
val locwidth : loc -> width


module Dn: 
sig 
  val exp:        exp         -> Private.exp
  val loc:        loc         -> Private.loc       
  val rtl:        rtl         -> Private.rtl
  val opr:        opr         -> Private.opr
  val assertion:  assertion   -> Private.assertion
end

module Up: 
sig 
  val exp:        Private.exp       -> exp
  val loc:        Private.loc       -> loc
  val rtl:        Private.rtl       -> rtl
  val opr:        Private.opr       -> opr
  val assertion:  Private.assertion -> assertion
  val effect :    Private.effect    -> rtl
  val const  :    Private.const     -> exp
end
