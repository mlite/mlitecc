type regkind       = 
    RReg  of string   (* hardware reg *)
  | RKind of string   (* calling convention kind *)
  | RNone             (* none of above *)
      
type variable =    
    { index:        int
    ; rkind:        regkind
    ; loc:          Rtl.loc
    ; variance:     Cmm_ast.variance
    }
      
type symclass      = 
    Proc          of Symbol.t
  | Code          of Symbol.t    
  | Data          of Symbol.t
  | Stack         of Rtl.exp  (* address of slot *)
      
type denotation    = 
    Constant      of Bits.bits
  | Label         of symclass
  | Import        of string * Symbol.t
      (* external name, assembly symbol *)
  | Variable      of variable
  | Continuation  of continuation
      
and continuation = 
    { 
      base       : Block.t;   (* always empty; used only for address *)
      convention : string;
      formals    : (string * variable * aligned) list;
      (* kinded, aligned formals *)
      mutable escapes     : bool;  (* used as rvalue *)
      mutable cut_to      : bool;  (* mentioned in annotation *)
      mutable unwound_to  : bool;  (* mentioned in annotation *)
      mutable returned_to : convention list;
      (* list every convention cc such that there exists
         a call site with convention cc and that call site
         `also returns to' the continuation *)
    } (* might need a convention here *)
      
and convention   = string
    
and aligned      = int
    
module type Env = sig
  type 'a info
  type 'a partial
  val  bad: unit -> 'a info
    
  type 'proc env'
    
  (* type env = Proc.t env' *)
  type scope
  and  ventry        = Srcmap.rgn * (denotation * Types.ty) info
  and  tentry         = Srcmap.rgn * Types.ty info
  val map : ('b -> 'a) -> 'a env' -> 'b env'

  val empty   : Srcmap.map -> Metrics.t -> 'proc Asm.assembler -> 'proc env'
    (* empty scope stack *)
  val srcmap  : 'proc env' -> Srcmap.map
  val asm     : 'proc env' -> 'proc Asm.assembler
  val metrics : 'proc env' -> Metrics.t
  val emptyscope: scope   
  val top:        'p env' -> scope            (* top empty = assert false *)
  val pop:        'p env' -> 'p env'              (* pop empty = assert false *)
  val push:       'p env' -> scope -> 'p env'
  val foldv:      (string -> ventry -> 'a -> 'a) -> scope -> 'a -> 'a
  val bindv           : string -> ventry  -> 'p env' -> 'p env'
  val rebindv         : string -> ventry  -> 'p env' -> 'p env'
  val rebindv'        : string -> ventry  -> 'p env' -> unit  (* mutates *)
  val bindt           : string -> tentry  -> 'p env' -> 'p env'
  val findv           : string -> 'p env' -> ventry   (* Error.ErrorExn *)
  val findt           : string -> 'p env' -> tentry   (* Error.ErrorExn *)
  val is_localv       : string -> 'p env' -> bool     (* *)
  val flagError       : 'p env' -> 'p env'
  val errorFlag       : 'p env' -> bool
  val import: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* import g as f *)
  val export: Srcmap.rgn -> string -> string -> 'p env' -> 'p env' (* export f as g *)
  val symbol:   'p env' -> string -> Symbol.t
end

module Dirty : Env
  with type 'a info    = 'a Error.error
with type 'a partial = 'a option
  
module Clean : Env
  with type 'a info    = 'a 
with type 'a partial = 'a 
  
val clean : 'proc Dirty.env' -> 'proc Clean.env'
val denotation's_category : denotation -> string
