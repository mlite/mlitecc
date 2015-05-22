include Common
type symbol = Symbol.t * (Symbol.t -> Rtl.width -> Rtl.exp)
    
let eqsym (s,_) (s',_) = s#mangled_text =$= s'#mangled_text

type exp = Pos of symbol * Rtl.width | Neg of symbol * Rtl.width

let neg = function Pos (s, w) -> Neg (s, w) | Neg (s, w) -> Pos (s, w)

type t  = exp list * Bits.bits
let of_const c    = ([], c)
let of_sym s w    = ([Pos (s, w)], Bits.zero w)
let add (xs, xc) (ys, yc) = (xs @ ys, Bits.Ops.add xc yc)
let sub (xs, xc) (ys, yc) = (xs @ List.map neg ys, Bits.Ops.sub xc yc)
let fold ~const ~sym ~add ~sub (ss, c) =
  let extend e = 
    function Pos (s, w) -> add e (sym s)
      | Neg (s, w) -> sub e (sym s) 
  in
  match ss with
    | Pos (s, w) :: ss when Bits.is_zero c -> List.fold_left extend (sym s)   ss
    | _                                    -> List.fold_left extend (const c) ss
	
let width (_, b) = Bits.width b

let if_bare = function ([], b) -> Some b | (_::_, _) -> None
  
let as_simple a =
  let w = Bits.width (snd a) in
  let const bits = (None, bits) in
  let sym (s, _) = (Some s, Bits.zero w) in
  let add (s, b) (s', b') = match s, s' with
    | Some s, None   -> (Some s, Bits.Ops.add b b')
    | None, Some s   -> (Some s, Bits.Ops.add b b')
    | None, None     -> (None, Bits.Ops.add b b')
    | Some _, Some _ -> Impossible.impossible "added symbols in simple reloc" 
  in
  let sub (s, b) (s', b') = match s' with
    | None -> (s, Bits.Ops.sub b b')
    | Some _ -> Impossible.impossible "subtracted symbols in simple reloc" 
  in
  fold ~const ~sym ~add ~sub a
