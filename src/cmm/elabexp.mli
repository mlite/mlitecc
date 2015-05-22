type nm_or_mem = Cmm_ast.name_or_mem
type link = Reloc.t
val aligned : Metrics.t -> Rtl.width -> Cmm_ast.aligned option -> Cmm_ast.aligned
                                                                     (* raises Error *)
val elab_ty : 'a Fenv.Dirty.env' -> Cmm_ast.ty   -> Rtl.width               Error.error
val elab_loc: 'a Fenv.Dirty.env' -> nm_or_mem-> (Rtl.loc   * Rtl.width) Error.error
val elab_exp: 'a Fenv.Dirty.env' -> Cmm_ast.expr -> (Rtl.exp   * Types.ty)  Error.error
val elab_con: 'a Fenv.Dirty.env' -> Cmm_ast.expr -> (Bits.bits * Rtl.width) Error.error
val elab_link:'a Fenv.Dirty.env' -> Cmm_ast.expr -> (link      * Rtl.width) Error.error
val elab_kinded_name: 
  'a Fenv.Dirty.env' -> nm_or_mem -> (string * (Rtl.loc * Rtl.width) * int) Error.error
val loc_region : Cmm_ast.region -> nm_or_mem -> Cmm_ast.region
val exp_region : Cmm_ast.region -> Cmm_ast.expr  -> Cmm_ast.region
