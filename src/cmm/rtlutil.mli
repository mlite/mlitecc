type aloc = 
    { fetch  : Rtl.width -> Rtl.exp
    ; store  : Rtl.exp -> Rtl.width -> Rtl.rtl
    }
val add  : Rtl.width -> Rtl.exp -> Rtl.exp -> Rtl.exp
val addk : Rtl.width -> Rtl.exp -> int     -> Rtl.exp
val fetch : Rtl.loc -> Rtl.exp
val store : Rtl.loc -> Rtl.exp -> Rtl.rtl
val reloc : Reloc.t -> Rtl.width -> Rtl.exp
val is_hardware : Rtl.Private.loc -> bool
val to_hardware : Rtl.Private.loc -> Register.x
module Eq : sig
  val space : Rtl.space         -> Rtl.space         -> bool
  val const : Rtl.Private.const -> Rtl.Private.const -> bool
  val loc   : Rtl.Private.loc   -> Rtl.Private.loc   -> bool
  val exp   : Rtl.Private.exp   -> Rtl.Private.exp   -> bool
  val rtl   : Rtl.Private.rtl   -> Rtl.Private.rtl   -> bool
end
module Compare : sig
  val space : Rtl.space         -> Rtl.space         -> int
  val const : Rtl.Private.const -> Rtl.Private.const -> int
  val loc   : Rtl.Private.loc   -> Rtl.Private.loc   -> int
  val exp   : Rtl.Private.exp   -> Rtl.Private.exp   -> int
  val rtl   : Rtl.Private.rtl   -> Rtl.Private.rtl   -> int
end
module Width: sig
    val loc:    Rtl.loc -> int
    val exp:    Rtl.exp -> int
    val exp':   Rtl.Private.exp -> int
end 
module Time: sig
    val compile:    Rtl.exp -> bool (* true, iff compile-time const *)
    val link:       Rtl.exp -> bool (* true, off compile-time or link-time *)
end
module ReadWrite: sig
    type 'a observert = Register.t -> 'a -> 'a
    type 'a observerx = Register.x -> 'a -> 'a
    val fold: read: 'a observerx -> write: 'a observerx -> Rtl.rtl -> 'a -> 'a
    val fold_promote: read: 'a observert -> write: 'a observert -> Rtl.rtl -> 'a -> 'a
    val sets: Rtl.rtl -> (Register.SetX.t * Register.SetX.t)
    val sets_promote: Rtl.rtl -> (Register.Set.t * Register.Set.t)
end
module ReadWriteKill: sig
    type 'a observert = Register.t -> 'a -> 'a
    type 'a observerx = Register.x -> 'a -> 'a
    val fold: read: 'a observerx -> write: 'a observerx -> kill: 'a observerx ->
                    Rtl.rtl -> 'a -> 'a
    val fold_promote: read: 'a observert -> write: 'a observert -> kill: 'a observert ->
                      Rtl.rtl -> 'a -> 'a
    val sets: Rtl.rtl -> Register.SetX.t * Register.SetX.t * Register.SetX.t
    val sets_promote: Rtl.rtl -> Register.Set.t * Register.Set.t * Register.Set.t
end
module FullReadWriteKill: sig
    type 'a observer = Rtl.Private.loc -> 'a -> 'a
    val fold: read: 'a observer -> write: 'a observer -> kill: 'a observer ->
                    Rtl.rtl -> 'a -> 'a
end
module Exists : sig
  module Opr : sig
    val rtl : (Rtl.opr -> bool) -> Rtl.rtl -> bool
  end
  module Loc : sig
    val exp : (Rtl.Private.loc -> bool) -> Rtl.Private.exp -> bool
    val rtl : (Rtl.Private.loc -> bool) -> Rtl.Private.rtl -> bool
  end
  module Const : sig
    val rtl : (Rtl.Private.const -> bool) -> Rtl.rtl -> bool
  end
end 
module Find : sig
  module Loc : sig
    val exp : (Rtl.Private.loc -> bool) -> Rtl.Private.exp -> Rtl.Private.loc option
  end
end 
module Fold : sig
  module RegX : sig
    val loc : (Register.x -> 'a -> 'a) -> Rtl.loc -> 'a -> 'a
  end
  module LocFetched : sig
    val rtl : (Rtl.Private.loc -> 'a -> 'a) -> Rtl.Private.rtl -> 'a -> 'a
  end
end
module MayAlias : sig
  val regs   : Register.t -> Register.t -> bool   
  val locs   : Rtl.loc -> Rtl.loc -> bool  (* useful to partially apply *)
  val locs'  : Rtl.Private.loc -> Rtl.Private.loc -> bool (* partially apply *)
  val exp    : Rtl.loc -> Rtl.exp -> bool  (* useful to partially apply *)
  val exp'   : Rtl.Private.loc -> Rtl.Private.exp -> bool (* partially apply *)
  val store_uses' :
        Rtl.Private.loc -> (Rtl.Private.loc * Rtl.Private.exp * int) -> bool
end
module Subst: sig
    val loc:        guard:(Rtl.Private.loc -> bool)               
                    -> map:(Rtl.Private.loc -> Rtl.Private.loc) 
                    -> Rtl.rtl -> Rtl.rtl
    
    val aloc:       guard:(Rtl.Private.loc -> bool)               
                    -> map:(Rtl.Private.loc -> aloc) 
                    -> Rtl.rtl -> Rtl.rtl
    
    val exp:        guard:(Rtl.Private.exp -> bool)
                    -> map:(Rtl.Private.exp -> Rtl.Private.exp) 
                    -> Rtl.rtl -> Rtl.rtl
    
    val exp_of_exp: guard:(Rtl.Private.exp -> bool)
                    -> map:(Rtl.Private.exp -> Rtl.Private.exp) 
                    -> Rtl.exp -> Rtl.exp
    val exp_of_loc: guard:(Rtl.Private.exp -> bool)
                    -> map:(Rtl.Private.exp -> Rtl.Private.exp) 
                    -> Rtl.loc -> Rtl.loc
    val loc_of_loc: guard:(Rtl.Private.loc -> bool)
                    -> map:(Rtl.Private.loc -> Rtl.Private.loc) 
                    -> Rtl.loc -> Rtl.loc
    
    val reg:        map:(Register.t -> Register.t) -> Rtl.rtl -> Rtl.rtl
    
    val reg_def:    map:(Register.t -> Register.t) -> Rtl.rtl -> Rtl.rtl
    
    val reg_use:    map:(Register.t -> Register.t) -> Rtl.rtl -> Rtl.rtl

    val reg_to_mem: map:(Register.t -> Rtl.Private.loc) -> Rtl.rtl -> Rtl.rtl

    module Fetch : sig   (* substitute for Fetch(l, w) *)
      val rtl:      guard:(Rtl.Private.loc -> bool) ->
                    fetch:(Rtl.Private.loc -> Rtl.width -> Rtl.Private.exp) ->
                    Rtl.rtl -> Rtl.rtl
      val exp':     guard:(Rtl.Private.loc -> bool) ->
                    fetch:(Rtl.Private.loc -> Rtl.width -> Rtl.Private.exp) ->
                    Rtl.Private.exp -> Rtl.Private.exp
    end
end
module RTLType: sig
    val singleAssignment:    Rtl.rtl -> (Register.t * Register.t) option
end
module ToAST: sig
    type verbosity =    Low | High             (* default is High   *)
    val verbosity:      verbosity -> verbosity (* returns old value *)

    val expr:           Rtl.exp  -> Cmm_ast.expr 
    val rtl:            Rtl.rtl  -> Cmm_ast.stmt 
end
module ToUnreadableString: sig
    val regx: Register.x -> string
    val reg:  Register.t -> string
    val rtl:  Rtl.rtl -> string
    val exp:  Rtl.exp -> string
    val loc:  Rtl.loc -> string
    val agg:  Rtl.aggregation -> string
end
module ToString: sig
    val reg: Register.t -> string
    val rtl: Rtl.rtl -> string
    val exp: Rtl.exp -> string
    val loc: Rtl.loc -> string
    val const: Rtl.Private.const -> string
end
