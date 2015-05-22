module B  = Bits
module BO = Bits.Ops
module DG = Dag
module R  = Rtl
module RU = Rtlutil

let (<:>) = DG.(<:>)

type width = Rtl.width
type exp   = Rtl.exp
type loc   = Rtl.loc
type block = exp Postexpander.block
type temp  = Register.t

module Ops = 
struct
  (* This code generated automatically by Rtlop.Emit.creators *)
  let _NaN w w' x = Rtl.app (Rtl.opr "NaN" [w;w';]) [x; ]
  let add w x y = Rtl.app (Rtl.opr "add" [w;]) [x; y; ]
  let addc w x y z = Rtl.app (Rtl.opr "addc" [w;]) [x; y; z; ]
  let add_overflows w x y = Rtl.app (Rtl.opr "add_overflows" [w;]) [x; y; ]
  let _and w x y = Rtl.app (Rtl.opr "and" [w;]) [x; y; ]
  let bit x = Rtl.app (Rtl.opr "bit" []) [x; ]
  let bool x = Rtl.app (Rtl.opr "bool" []) [x; ]
  let borrow w x y z = Rtl.app (Rtl.opr "borrow" [w;]) [x; y; z; ]
  let carry w x y z = Rtl.app (Rtl.opr "carry" [w;]) [x; y; z; ]
  let com w x = Rtl.app (Rtl.opr "com" [w;]) [x; ]
  let conjoin x y = Rtl.app (Rtl.opr "conjoin" []) [x; y; ]
  let disjoin x y = Rtl.app (Rtl.opr "disjoin" []) [x; y; ]
  let div w x y = Rtl.app (Rtl.opr "div" [w;]) [x; y; ]
  let div_overflows w x y = Rtl.app (Rtl.opr "div_overflows" [w;]) [x; y; ]
  let divu w x y = Rtl.app (Rtl.opr "divu" [w;]) [x; y; ]
  let eq w x y = Rtl.app (Rtl.opr "eq" [w;]) [x; y; ]
  let f2f w w' x y = Rtl.app (Rtl.opr "f2f" [w;w';]) [x; y; ]
  let f2f_implicit_round w w' x = Rtl.app (Rtl.opr "f2f_implicit_round" [w;w';]) [x; ]
  let f2i w w' x y = Rtl.app (Rtl.opr "f2i" [w;w';]) [x; y; ]
  let fabs w x = Rtl.app (Rtl.opr "fabs" [w;]) [x; ]
  let fadd w x y z = Rtl.app (Rtl.opr "fadd" [w;]) [x; y; z; ]
  let fcmp w x y = Rtl.app (Rtl.opr "fcmp" [w;]) [x; y; ]
  let fdiv w x y z = Rtl.app (Rtl.opr "fdiv" [w;]) [x; y; z; ]
  let feq w x y = Rtl.app (Rtl.opr "feq" [w;]) [x; y; ]
  let fge w x y = Rtl.app (Rtl.opr "fge" [w;]) [x; y; ]
  let fgt w x y = Rtl.app (Rtl.opr "fgt" [w;]) [x; y; ]
  let fle w x y = Rtl.app (Rtl.opr "fle" [w;]) [x; y; ]
  let float_eq = Rtl.app (Rtl.opr "float_eq" []) []
  let float_gt = Rtl.app (Rtl.opr "float_gt" []) []
  let float_lt = Rtl.app (Rtl.opr "float_lt" []) []
  let flt w x y = Rtl.app (Rtl.opr "flt" [w;]) [x; y; ]
  let fmul w x y z = Rtl.app (Rtl.opr "fmul" [w;]) [x; y; z; ]
  let fmulx w x y = Rtl.app (Rtl.opr "fmulx" [w;]) [x; y; ]
  let fne w x y = Rtl.app (Rtl.opr "fne" [w;]) [x; y; ]
  let fneg w x = Rtl.app (Rtl.opr "fneg" [w;]) [x; ]
  let fordered w x y = Rtl.app (Rtl.opr "fordered" [w;]) [x; y; ]
  let fsqrt w x y = Rtl.app (Rtl.opr "fsqrt" [w;]) [x; y; ]
  let fsub w x y z = Rtl.app (Rtl.opr "fsub" [w;]) [x; y; z; ]
  let funordered w x y = Rtl.app (Rtl.opr "funordered" [w;]) [x; y; ]
  let ge w x y = Rtl.app (Rtl.opr "ge" [w;]) [x; y; ]
  let geu w x y = Rtl.app (Rtl.opr "geu" [w;]) [x; y; ]
  let gt w x y = Rtl.app (Rtl.opr "gt" [w;]) [x; y; ]
  let gtu w x y = Rtl.app (Rtl.opr "gtu" [w;]) [x; y; ]
  let i2f w w' x y = Rtl.app (Rtl.opr "i2f" [w;w';]) [x; y; ]
  let le w x y = Rtl.app (Rtl.opr "le" [w;]) [x; y; ]
  let leu w x y = Rtl.app (Rtl.opr "leu" [w;]) [x; y; ]
  let lobits w w' x = Rtl.app (Rtl.opr "lobits" [w;w';]) [x; ]
  let lt w x y = Rtl.app (Rtl.opr "lt" [w;]) [x; y; ]
  let ltu w x y = Rtl.app (Rtl.opr "ltu" [w;]) [x; y; ]
  let minf w = Rtl.app (Rtl.opr "minf" [w;]) []
  let (mod) w x y = Rtl.app (Rtl.opr "mod" [w;]) [x; y; ]
  let modu w x y = Rtl.app (Rtl.opr "modu" [w;]) [x; y; ]
  let mul w x y = Rtl.app (Rtl.opr "mul" [w;]) [x; y; ]
  let mulux w x y = Rtl.app (Rtl.opr "mulux" [w;]) [x; y; ]
  let mulx w x y = Rtl.app (Rtl.opr "mulx" [w;]) [x; y; ]
  let mul_overflows w x y = Rtl.app (Rtl.opr "mul_overflows" [w;]) [x; y; ]
  let mulu_overflows w x y = Rtl.app (Rtl.opr "mulu_overflows" [w;]) [x; y; ]
  let mzero w = Rtl.app (Rtl.opr "mzero" [w;]) []
  let ne w x y = Rtl.app (Rtl.opr "ne" [w;]) [x; y; ]
  let neg w x = Rtl.app (Rtl.opr "neg" [w;]) [x; ]
  let not x = Rtl.app (Rtl.opr "not" []) [x; ]
  let (or) w x y = Rtl.app (Rtl.opr "or" [w;]) [x; y; ]
  let pinf w = Rtl.app (Rtl.opr "pinf" [w;]) []
  let popcnt w x = Rtl.app (Rtl.opr "popcnt" [w;]) [x; ]
  let pzero w = Rtl.app (Rtl.opr "pzero" [w;]) []
  let quot w x y = Rtl.app (Rtl.opr "quot" [w;]) [x; y; ]
  let quot_overflows w x y = Rtl.app (Rtl.opr "quot_overflows" [w;]) [x; y; ]
  let rem w x y = Rtl.app (Rtl.opr "rem" [w;]) [x; y; ]
  let rotl w x y = Rtl.app (Rtl.opr "rotl" [w;]) [x; y; ]
  let rotr w x y = Rtl.app (Rtl.opr "rotr" [w;]) [x; y; ]
  let round_down = Rtl.app (Rtl.opr "round_down" []) []
  let round_nearest = Rtl.app (Rtl.opr "round_nearest" []) []
  let round_up = Rtl.app (Rtl.opr "round_up" []) []
  let round_zero = Rtl.app (Rtl.opr "round_zero" []) []
  let shl w x y = Rtl.app (Rtl.opr "shl" [w;]) [x; y; ]
  let shra w x y = Rtl.app (Rtl.opr "shra" [w;]) [x; y; ]
  let shrl w x y = Rtl.app (Rtl.opr "shrl" [w;]) [x; y; ]
  let sub w x y = Rtl.app (Rtl.opr "sub" [w;]) [x; y; ]
  let subb w x y z = Rtl.app (Rtl.opr "subb" [w;]) [x; y; z; ]
  let sub_overflows w x y = Rtl.app (Rtl.opr "sub_overflows" [w;]) [x; y; ]
  let sx w w' x = Rtl.app (Rtl.opr "sx" [w;w';]) [x; ]
  let unordered = Rtl.app (Rtl.opr "unordered" []) []
  let xor w x y = Rtl.app (Rtl.opr "xor" [w;]) [x; y; ]
  let zx w w' x = Rtl.app (Rtl.opr "zx" [w;w';]) [x; ]
  let bitExtract w w' x y = Rtl.app (Rtl.opr "bitExtract" [w;w';]) [x; y; ]
  let bitInsert w w' x y z = Rtl.app (Rtl.opr "bitInsert" [w;w';]) [x; y; z; ]
  let bitTransfer w x y z u v = Rtl.app (Rtl.opr "bitTransfer" [w;]) [x; y; z; u; v; ]
  let signed   w n = R.bits (B.S.of_int n w) w
  let unsigned w n = R.bits (B.U.of_int n w) w
end
module O = Ops


let div_overflows w x y =
  let minint = R.bits (BO.shl (B.U.of_int 1 w) (B.U.of_int (w-1) w)) w in
  O.conjoin (O.eq w x minint) (O.eq w y (O.signed w (-1)))

let (mod) w x y = O.sub w x (O.mul w y (O.div  w x y))
let modu  w x y = O.sub w x (O.mul w y (O.divu w x y))
let rem   w x y = O.sub w x (O.mul w y (O.quot w x y))
let sxlo n w x =
  let shamt = O.unsigned w (w-n) in
  O.shra w (O.shl w x shamt) shamt

let zxlo n w x =
  let shamt = O.unsigned w (w-n) in
  O.shrl w (O.shl w x shamt) shamt
let popcnt w ~dst x =
  if w <> 32 then
    Unsupported.popcnt ~notok:w ~ok:32;
  let u      = O.unsigned 32 in
  let (@:=) l r = DG.Rtl (R.store l r 32) in
  let (&)   l r = O._and 32 l r in
  let (>>)  l r = O.shrl 32 l (u r) in
  let (-)   l r = O.sub 32 l r in
  let (+)   l r = O.add 32 l r in
  let ( * ) l r = O.mul 32 l r in
  let tmploc = dst in
  let tmpval = R.fetch tmploc 32 in
  let mask3  = u 0x33333333 in
  let mask5  = R.bits (Bits.U.of_int64 0x55555555L 32) 32 in   (* Caml sign bit set *)
  let maskF  = u 0x0F0F0F0F in 
  let mask01 = u 0x01010101 in 
  tmploc @:= x - ((x >> 1) & mask5) <:>
  tmploc @:= (tmpval & mask3) + ((tmpval >> 2) & mask3)  <:>
  tmploc @:= ((tmpval + (tmpval >> 4)) & maskF)  <:>
  tmploc @:= ((tmpval * mask01) >> 24)   (* AMD guide *)
let regpair ~hi ~lo =
  let hw = RU.Width.exp hi in
  let lw = RU.Width.exp lo in
  let w = hw + lw in
  O.(or) w (O.shl w (O.zx hw w hi) (O.unsigned w lw)) (O.zx lw w lo)
let slice w n ~lsb e =
  if lsb = 0 then
    O.lobits w n e
  else
    O.lobits w n (O.shrl w e (O.unsigned w lsb))
type 'a pair = { hi : 'a; lo : 'a }

let splitu w ~lsb:n e =
  assert (0 < n && n < w);
  let lo = O.zx n w (O.lobits w n e) in
  let n  = O.unsigned w n in
  let hi = O.shl w (O.shrl w e n) n in
  { hi = hi; lo = lo }
let splits w ~lsb:n e =
  assert (0 < n && n < w);
  let lo     = O.sx n w (O.lobits w n e) in
  let adjust = O.zx 1 w (slice w 1 ~lsb:(n-1) e) in
  let n      = O.unsigned w n in
  let hi     = O.shl w (O.add w (O.shrl w e n) adjust) n in
  { hi = hi; lo = lo }
let div2 w ~dst x y =
  let return e = DG.Rtl (R.store dst e w) in
  let one  = O.signed w 1 in
  let zero = O.signed w 0 in
  DG.If (DG.cond (O.ge w y zero),
         DG.If (DG.cond (O.ge w x zero),
                return (O.quot w x y),
                return (O.sub w (O.quot w (O.add w x one) y) one)),
         DG.If (DG.cond (O.gt w x zero),
                return (O.sub w (O.quot w (O.sub w x one) y) one),
                return (O.quot w x y)))

let div1_3 w ~dst x y =
  let return e = DG.Rtl (R.store dst e w) in
  let one  = O.signed w 1 in
  let zero = O.signed w 0 in
  DG.If (DG.cond (O.ge w (O.xor w x y) zero),
         return (O.quot w x y),
  DG.If (DG.cond (O.ge w y zero),
         return (O.sub w (O.quot w (O.add w x one) y) one),
  DG.If (DG.cond (O.eq w x zero),
         return zero,
         return (O.sub w (O.quot w (O.sub w x one) y) one))))

let div' w ~dst x y ~quot ~rem =
  let return e = DG.Rtl (R.store dst e w) in
  DG.If (DG.cond (O.conjoin (O.lt w (O.xor w x y) (O.signed w 0))
                            (O.ne w rem (O.signed w 0))),
         return (O.sub w quot (O.signed w 1)),
         return quot)

