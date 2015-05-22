type t
type stage
type counter = string
type counterenv
type result =
  { overflow    : Block.t
  ; regs_used   : Register.Set.t
  ; mems_used   : Rtl.loc list
  ; align_state : int   (* final alignment state of overflow block *)
  }
type width = int
type kind  = string
type loc = Rtlutil.aloc =
    { fetch  : width -> Rtl.exp
    ; store  : Rtl.exp -> width -> Rtl.rtl
    }
type methods =
    { allocate : width: int -> alignment: int -> kind: string -> loc
    ; freeze   : Register.Set.t -> Rtl.loc list -> result
    } 
type choice_predicate = int -> string -> counterenv -> bool
type cc_spec  = { call : stage; results : stage; cutto : stage }
type cc_specs = (string * cc_spec) list
val allocate : t -> width:int -> kind:string -> align:int -> loc
val freeze   : t -> result
val fetch  : loc ->            width -> Rtl.exp
val store  : loc -> Rtl.exp -> width -> Rtl.rtl
val of_loc : Rtl.loc -> loc
val at         : Rtl.space -> start:Rtl.exp -> stage -> t
val of_methods : methods -> t
val ( *> ) : stage -> stage -> stage
val wrap   : (methods -> methods) -> stage
val overflow : growth:Memalloc.growth -> max_alignment:int -> stage
val widths : int list -> stage
val widen  : (int -> int) -> stage
val align_to : (int -> int) -> stage
val useregs : Register.t list -> bool -> stage
val bitcounter  : counter -> stage
val regs_by_bits: counter -> Register.t list -> bool -> stage

val argcounter  : counter -> stage
val regs_by_args: counter -> Register.t list -> bool -> stage

val pad         : counter -> stage
val postprocess : stage -> (result -> result) -> stage
val choice : (choice_predicate * stage) list -> stage
val counter_is : counter -> (int -> bool) -> choice_predicate
val is_kind    : kind                     -> choice_predicate
val is_width   : int                      -> choice_predicate
val is_any     :                             choice_predicate
val as_stage : t -> stage
val first_choice : (choice_predicate * stage) list -> stage
val unit : stage
val debug : counter -> (int -> string -> int -> int -> unit) -> stage
val init_cc : cc_specs
