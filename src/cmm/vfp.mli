val mk_space : Rtl.width       -> Space.t
val mk       : Rtl.width       -> Rtl.exp
val is_vfp   : Rtl.Private.loc -> bool
val replace_with : sp:Rtl.loc -> Zipcfg.graph -> Zipcfg.graph * bool
