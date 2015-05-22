include Common
module I = Int64        (* signed   operations *)
exception Overflow
let impossf fmt = Printf.kprintf Impossible.impossible fmt

let (<=) = Pervasives.(<=)
let (>=) = Pervasives.(>=)
let (<)  = Pervasives.(<)
let (>)  = Pervasives.(>)

type width      = int
type bits       = int64 * width
type t          = bits 

let width (_,w) = w
let check w = if w <= 0 || w > 64 then impossf "unsupported bitwidth %d" w
let zero w  = check w; (I.zero, w)

let sx64 i w =
  if w = 64 then i
  else
    let w' = 64-w in
    I.shift_right (I.shift_left i w') w'

let zx64 i w =
  if w = 64 then i
  else
    let w' = 64-w in
    I.shift_right_logical (I.shift_left i w') w'

let to_string (i,w) = Printf.sprintf "0x%Lx::bits%d" i w 
let to_decimal_string (i,w) = Printf.sprintf "%Ld" i
let to_hex_or_decimal_string ~declimit (i,w) =
  assert (declimit >= 0);
  if Uint64.lt i (I.of_int declimit) then
    Printf.sprintf "%Ld" i
  else
    Printf.sprintf "0x%Lx" (zx64 i w)


module Ops = 
struct
  let add (b, w) (b', w') =
    assert (w = w');
    (Int64.add (sx64 b w) (sx64 b' w), w)

  let eq (b, w) (b', w') =
    assert (w = w');
    (sx64 b w) =*= (sx64 b' w)

  let lt (b, w) (b', w') =
    assert (w = w');
    let n  = sx64 b  w  in
    let n' = sx64 b' w' in
    Pervasives.(<) n n'
  let gt x y = lt y x

  let ltu (b, w) (b', w') =
    assert (w = w');
    let n  = zx64 b  w  in
    let n' = zx64 b' w' in
    Uint64.lt n n'
  let gtu x y = ltu y x

  let neg (b,w) = (Int64.neg (sx64 b w), w)
    (* THIS SX SHOULD BE UNNECESSARY -- CONSULT KEVIN *)

  let sub (b,w) (b', w') =
    assert (w = w');
    let n  = sx64 b  w  in
    let n' = sx64 b' w' in
    (Int64.sub n n', w)

  let mul (b,w) (b', w') =
    assert (w = w');
    let n  = sx64 b  w  in
    let n' = sx64 b' w' in
    (Int64.mul n n', w)

  let divu (b,w) (b', w') =
    assert (w = w');
    let n  = zx64 b  w  in
    let n' = zx64 b' w' in
    (Uint64.div n n', w)

  let eq (b,w) (b',w') =
    assert (w = w');
    let n  = zx64 b w  in
    let n' = zx64 b' w' in
    n =*= n'

  let ne x y = not (eq x y)

  let sx k (b,w) =
    assert (k >= w);
    let n  = sx64 b w in
    (n ,k)

  let zx k (b,w) =
    assert (k >= w);
    let n = zx64 b w in
    (n, k)

  let lobits k (b, w) =
    if k <= w then (sx64 b k, k)
    else (Printf.eprintf "lobits error: k: %d, w: %d\n" k w;
    Impossible.impossible "lobits: k > w")

  let shra (b,w) (b',w') =
    let n  = zx64 b w in
    (Int64.shift_right b (Int64.to_int b'), w)

  let shrl (b,w) (b',w') =
    (Int64.shift_right_logical b (Int64.to_int b'), w)

  let shl (b,w) (b',w') =
    (zx64 (Int64.shift_left b (Int64.to_int b')) w, w)

  let or' (b,w) (b',w') =
    assert (w = w');
    (zx64 (Int64.logor b b') w, w)

  let and' (b,w) (b',w') =
    assert (w = w');
    (zx64 (Int64.logand b b') w, w)

  let xor (b,w) (b',w') =
    assert (w = w');
    (zx64 (Int64.logxor b b') w, w)

  let com (b,w) = (Int64.lognot (zx64 b w), w)
end


let eq (b, w) (b', w') = (w = w') && (sx64 b w) =*= (sx64 b' w)
let compare (b, w) (b', w') =
  match compare w w' with
  | 0 -> Pervasives.compare (sx64 b w) (sx64 b' w)
  | diff -> diff
let is_zero (b, w) = zx64 b w =*= I.zero


let sint str w =
  check w;
  let len = String.length str in
  try  
    if len > 2 && str.[0] =<= '0' && (str.[1] =<= 'x' || str.[1] =<= 'X') then 
      I.of_string str
    else if len > 2 && str.[0] =<= '0' then
      I.of_string ("0o"^str)
    else
      I.of_string str
  with Failure s -> raise Overflow  (* either that, or we let through bad syntax *)


let uint str w = 
  check w;
  try Uint64.of_string str
  with
  | Failure "overflow" -> raise Overflow
  | Failure s -> impossf "bad unsigned integer literal '%s'" str


let float str w = 
  try 
    let x = float_of_string str in
    match w with
    | 32 -> Uint64.Cast.float32 x
    | 64 -> Int64.bits_of_float x
    | _  -> Unsupported.floatlit w
  with Failure s -> impossf "caml function '%s' failed on literal %s" s str


let char str w =
  let str = String.sub str 1 (String.length str - 2) in
  let len = String.length str in
    try
        if   len = 0 
        then assert false
        else if str.[0] =<= '\\' && len > 1 
        then match str.[1] with
        | 'a'  when len = 2 -> I.of_int 7
        | 'b'  when len = 2 -> I.of_int 8
        | 'n'  when len = 2 -> I.of_int 10
        | 'r'  when len = 2 -> I.of_int 13
        | 't'  when len = 2 -> I.of_int 9
        | '\\' when len = 2 -> I.of_int 92
        | '\'' when len = 2 -> I.of_int 39
        | '"'  when len = 2 -> I.of_int 34
        | '?'  when len = 2 -> I.of_int 63
        | 'x'  when len = 2 -> I.of_string ("0"  ^ str)
        | '0' .. '7'        -> I.of_string ("0o" ^ str) 
        | _                 -> impossf "bad character literal '%s'" str
        else                   I.of_int (Char.code str.[0])
    with Failure s -> impossf "caml function '%s' failed on literal %s" s str


let string int (s:string) (w:int) =
    assert (String.length s > 0);
    if s.[0] =<= '\'' then
        char s w 
    else if (  String.length s >= 2 
            && s.[0] =<= '0' 
            && (s.[1] =<= 'x' || s.[1] =<= 'X')
            ) then
        int s w        
    else if (String.contains s '.' || String.contains s 'e' ||
             String.contains s 'E')  then
        float s w
    else
        int s w


module S = 
struct
  let fits_signed i w =
    w = 64 or
    let i' = I.shift_right i (w-1) in i' =*= I.zero || i' =*= I.minus_one

  let fits w' (i, w) = w <= w' || fits_signed i w'

  let of_int i w =
    check w;
    let i' = I.of_int i in
    if fits_signed i' w then (i',w) else raise Overflow 

  let of_int32 i w =
    check w;
    let i' = I.of_int32 i in
    if fits_signed i' w then (i',w) else raise Overflow 
    
  let of_native i w =
    check w;
    let i' = I.of_nativeint i in
    if fits_signed i' w then (i',w) else raise Overflow

  let of_int64 i w =
    check w;
    if fits_signed i w then (i,w) else raise Overflow

  let of_string str w =
    check w;
    let i = string sint str w in
    if fits_signed i w then (i,w) else raise Overflow

  let to_int (i,w) =
    if I.of_int min_int <= i && i <= I.of_int max_int then
      I.to_int i
    else
      raise Overflow

  let to_native (i,w) =
    if  I.of_nativeint Nativeint.min_int <= i && 
      i <= I.of_nativeint Nativeint.max_int 
    then
      I.to_nativeint i
    else
      raise Overflow

  let to_int64 (i,w) = i
end

module U = struct
  let mask i w =
    if w < 64 then 
      let m = I.shift_right_logical I.minus_one (64-w) in
      I.logand i m
    else
      i

  let fits_unsigned i w =
    assert (w = 64 || i >= I.zero);
    w = 64 or (I.shift_right_logical i w) =*= I.zero

  let fits w' (i, w) = w <= w' || fits_unsigned i w'

  let of_int i w =
    check w;
    assert (i >= 0);
    let i' = I.of_int i in 
    let i' = if i' < I.zero 
    then I.sub i' (I.shift_left (I.of_int min_int) 1)  
    else i' 
    in
    if fits_unsigned i' w then (sx64 i' w, w) else raise Overflow

  let of_int32 i w =
    check w;
    assert (i >= Int32.zero);
    let i' = I.of_int32 i in 
    let i' = if i' < I.zero 
    then I.sub i' (I.shift_left (I.of_int32 Int32.min_int) 1)  
    else i' 
    in
    if fits_unsigned i' w then (sx64 i' w, w) else raise Overflow

  let of_native i w =
    check w;
    assert (i >= Nativeint.zero);
    let i' = I.of_nativeint i in 
    let i' = if i' < I.zero 
    then I.sub i' (I.shift_left (I.of_nativeint Nativeint.min_int) 1) 
    else i' 
    in
    if fits_unsigned i' w then (sx64 i' w, w) else raise Overflow

  let of_int64 i w =
    check w;
    if fits_unsigned i w then (sx64 i w, w) else raise Overflow

  let of_string str w =
    check w;
    let i = string uint str w in
    if fits_unsigned i w then (i, w) else raise Overflow

  let to_int (i,w) = 
    let i = mask i w in
    if i <= I.of_int max_int then
      I.to_int i
    else 
      let i' = I.add i (I.shift_left (I.of_int min_int) 1)  in
      if i' < I.of_int max_int then
        I.to_int i'
      else
        raise Overflow
 
  let to_native (i,w) = 
    let i = mask i w in
    if i <= I.of_nativeint Nativeint.max_int then
      I.to_nativeint i
    else 
      let i' = I.add i (I.shift_left (I.of_nativeint Nativeint.min_int) 1) in
      if i' < I.of_nativeint Nativeint.max_int then
        I.to_nativeint i'
      else
        raise Overflow

  let to_int64 (i,w) = 
    let i = mask i w in
    i 
end
