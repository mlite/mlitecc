module SM = Strutil.Map
module T  = Types
  
let impossf fmt = Printf.kprintf Impossible.impossible fmt

type opr = Rtl.Private.opr
type exp = Rtl.Private.exp
type 'a hi = Hi of 'a
type 'a lo = Lo of 'a
type ('temp, 'warg) t =
  | Binop  of 'temp * 'temp
  | Unop   of 'temp
  | Binrm  of 'temp * 'temp * 'warg
  | Unrm   of 'temp * 'warg
  | Cmp    of 'temp * 'temp
  | Dblop  of 'temp * 'temp
  | Wrdop  of 'temp * 'temp * 'warg
  | Wrdrop of 'temp * 'temp * 'warg
  | Width
  | Fpcvt  of 'temp * 'warg
  | Bool
  | Nullary
      
let shape scheme = match scheme with
  | [T.Bits (T.Var 1); T.Bits (T.Var 1)           ], T.Bits (T.Var 1)               -> Binop ((), ())
  | [T.Bits (T.Var 1)                   ], T.Bits (T.Var 1)               -> Unop ()
  | [T.Bits (T.Var 1); T.Bits (T.Var 1); T.Bits (T.Const 2)   ], T.Bits (T.Var 1)               -> Binrm ((), (), ())
  | [T.Bits (T.Var 1); T.Bits (T.Const 2)           ], T.Bits (T.Var 1)               -> Unrm ((), ())
  | [T.Bits (T.Var 1); T.Bits (T.Var 1)           ], T.Bool               -> Cmp ((), ())
  | [T.Bits (T.Var 1); T.Bits (T.Var 1)           ], T.Bits (T.Double 1)  -> Dblop ((), ())
  | [T.Bits (T.Var 1); T.Bits (T.Var 1); T.Bits (T.Const 1)], T.Bits (T.Var 1)               -> Wrdop ((), (), ())
  | [T.Bits (T.Var 1); T.Bits (T.Var 1); T.Bits (T.Const 1)], T.Bits (T.Const 1)            -> Wrdrop ((), (), ())
  | [T.Bits (T.Var 1)                   ], T.Bits (T.Var 2)               -> Width
  | [T.Bits (T.Var 1); T.Bits (T.Const 2)           ], T.Bits (T.Var 2)               -> Fpcvt ((), ())
  | [T.Bool|T.Bits (T.Const 1)         ], (T.Bool | T.Bits (T.Const 1)) -> Bool
  | [T.Bool; T.Bool           ], T.Bool               -> Bool
  | [                         ], T.Bool               -> Bool
  | [                         ], (T.Bits (T.Var 1)|T.Bits (T.Const 2))   -> Nullary
  | _ -> impossf "unrecognized operator type %s" (Types.scheme_string scheme)

let shapetab = Rtlop.fold (fun o t z -> SM.add o (shape t) z) SM.empty

let of_opr (o, _) =
  try SM.find o shapetab 
  with _ -> impossf "operator %%%s has no shape" o
    
let args () = impossf "wrong number of arguments to operator"
  
let appfun t w = function
  | Binop  _ -> (function [x; y]    -> Binop  (t x, t y)      | _ -> args())
  | Unop   _ -> (function [x]       -> Unop   (t x)           | _ -> args())
  | Binrm  _ -> (function [x; y; r] -> Binrm  (t x, t y, w r) | _ -> args())
  | Unrm   _ -> (function [x;r ]    -> Unrm   (t x, w r)      | _ -> args())
  | Cmp    _ -> (function [x; y]    -> Cmp    (t x, t y)      | _ -> args())
  | Dblop  _ -> (function [x; y]    -> Dblop  (t x, t y)      | _ -> args())
  | Wrdop  _ -> (function [x; y; c] -> Wrdop  (t x, t y, w c) | _ -> args())
  | Wrdrop _ -> (function [x; y; c] -> Wrdrop (t x, t y, w c) | _ -> args())
  | Width    -> (function [x]       -> Width                  | _ -> args())
  | Fpcvt  _ -> (function [x;r ]    -> Fpcvt  (t x, w r)      | _ -> args())
  | Bool     -> (function _         -> Bool)
  | Nullary  -> (function []        -> Nullary                | _ -> args())
      
let cappfun t w = 
  let binop cs es =
    match es, cs with
      | [x; y], [xc; yc] ->
          let x = t xc x in
          let y = t yc y in
          Binop  (x, y)
      | _ -> args() 
  in
  let unop cs es =
    match es, cs with [x], [xc] -> Unop (t xc x) | _ -> args() 
  in
  let binrm cs es =
    match es, cs with
      | [x; y; r], [xc; yc; rc] ->
          let x = t xc x in
          let y = t yc y in
          let r = w rc r in
          Binrm (x, y, r)
      | _ -> args() 
  in
  let unrm cs es =
    match es, cs with
      | [x; r], [xc; rc] ->
          let x = t xc x in
          let r = w rc r in
          Unrm (x, r)
      | _ -> args() in
  let cmp cs es =
    match es, cs with
      | [x; y], [xc; yc] ->
          let x = t xc x in
          let y = t yc y in
          Cmp (x, y)
      | _ -> args() in
  let dblop cs es =
    match es, cs with
      | [x; y], [xc; yc] ->
          let x = t xc x in
          let y = t yc y in
          Dblop (x, y)
      | _ -> args() in
  let wrdop cs es =
    match es, cs with
      | [x; y; z], [xc; yc; zc] ->
          let x = t xc x in
          let y = t yc y in
          let z = w zc z in
          Wrdop(x, y, z)
      | _ -> args() in
  let wrdrop cs es =
    match es, cs with
      | [x; y; z], [xc; yc; zc] ->
          let x = t xc x in
          let y = t yc y in
          let z = w zc z in
          Wrdrop(x, y, z)
      | _ -> args() in
  let width cs es = 
    match es, cs with [x], [xc] -> Width | _ -> args() in
  let fpcvt cs es =
    match es, cs with
      | [x; r], [xc; rc] ->
          let x = t xc x in
          let r = w rc r in
          Fpcvt (x, r)
      | _ -> args() in
  let nullary cs es = 
    match es, cs with [], [] -> Width | _ -> args() 
  in
  function
    | Binop  _ -> binop
    | Unop   _ -> unop
    | Binrm  _ -> binrm
    | Unrm   _ -> unrm
    | Cmp    _ -> cmp
    | Dblop  _ -> dblop
    | Wrdop  _ -> wrdop
    | Wrdrop _ -> wrdrop
    | Width    -> width
    | Fpcvt  _ -> fpcvt
    | Bool     -> (fun _ _ -> Bool)
    | Nullary  -> nullary

let capply to_temp to_warg opr = cappfun to_temp to_warg (of_opr opr)
