type rtl = Rtl.rtl
module type BOXER = sig
  type t
  val box   : t -> rtl
  val unbox : rtl -> t (* possible assertion failure *)
end
module Exp      : BOXER with type t = Rtl.exp
module ExpList  : BOXER with type t = Rtl.exp list
module Guard    : BOXER with type t = Rtl.exp
module Combine (Box1 : BOXER) (Box2 : BOXER) : BOXER with type t = Box1.t * Box2.t
module GuardExp : BOXER with type t = Rtl.exp * Rtl.exp
val assert_not_boxed : rtl -> unit
