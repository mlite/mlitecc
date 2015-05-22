include Common
let impossf fmt = Printf.kprintf Impossible.impossible fmt

type 'c op = string * 'c list * 'c        (* op, arguments, result *)
    
let nonboolops ~int ~fp ~rm =
  [ "add"           , [int; int], int
  ; "addc"          , [int; int; int], int
  ; "and"           , [int; int], int
  ; "bitExtract"    , [int; int], int
  ; "bitInsert"     , [int; int; int], int
  ; "bitTransfer"   , [int; int; int; int; int], int
  ; "borrow"        , [int; int; int], int
  ; "carry"         , [int; int; int], int
  ; "com"           , [int], int
  ; "div"           , [int; int], int
  ; "divu"          , [int; int], int
  ; "f2f"           , [fp; rm], fp
  ; "f2f_implicit_round"     , [fp], fp
  ; "f2i"           , [fp; rm], fp  (* conversion done in float unit?? *)
  ; "fabs"          , [fp], fp
  ; "fadd"          , [fp; fp; rm], fp
  ; "fcmp"          , [fp; fp], fp
  ; "fdiv"          , [fp; fp; rm], fp
  ; "float_eq"      , [], fp
  ; "float_gt"      , [], fp
  ; "float_lt"      , [], fp
  ; "fmul"          , [fp; fp; rm], fp
  ; "fmulx"         , [fp; fp], fp
  ; "fneg"          , [fp], fp
  ; "fsqrt"         , [fp; rm], fp
  ; "fsub"          , [fp; fp; rm], fp
  ; "i2f"           , [fp; rm], fp  (* conversion done in float unit? *)
  ; "lobits"        , [int], int
  ; "minf"          , [], fp
  ; "mod"           , [int; int], int
  ; "modu"          , [int; int], int
  ; "mul"           , [int; int], int
  ; "mulux"         , [int; int], int
  ; "mulx"          , [int; int], int
  ; "mzero"         , [], fp
  ; "NaN"           , [int], int  (* implemented in simplifier using integer ops *)
  ; "neg"           , [int], int
  ; "or"            , [int; int], int
  ; "pinf"          , [], fp
  ; "popcnt"        , [int], int
  ; "pzero"         , [], fp
  ; "quot"          , [int; int], int
  ; "rem"           , [int; int], int
  ; "round_down"    , [], rm
  ; "round_nearest" , [], rm
  ; "round_up"      , [], rm
  ; "round_zero"    , [], rm
  ; "rotl"          , [int; int], int
  ; "rotr"          , [int; int], int
  ; "shl"           , [int; int], int
  ; "shra"          , [int; int], int
  ; "shrl"          , [int; int], int
  ; "sub"           , [int; int], int
  ; "subb"          , [int; int; int], int
  ; "sx"            , [int], int
  ; "unordered"     , [], fp
  ; "xor"           , [int; int], int
  ; "zx"            , [int], int
  ]

let boolops ~int ~fp ~bool =
  [ "add_overflows" , [int; int], bool
  ; "bit"           , [bool], int
  ; "bool"          , [int], bool
  ; "conjoin"       , [bool; bool], bool
  ; "disjoin"       , [bool; bool], bool
  ; "div_overflows" , [int; int], bool
  ; "eq"            , [int; int], bool
  ; "feq"           , [fp; fp], bool
  ; "fge"           , [fp; fp], bool
  ; "fgt"           , [fp; fp], bool
  ; "fle"           , [fp; fp], bool
  ; "flt"           , [fp; fp], bool
  ; "fne"           , [fp; fp], bool
  ; "ge"            , [int; int], bool
  ; "geu"           , [int; int], bool
  ; "gt"            , [int; int], bool
  ; "gtu"           , [int; int], bool
  ; "le"            , [int; int], bool
  ; "leu"           , [int; int], bool
  ; "lt"            , [int; int], bool
  ; "ltu"           , [int; int], bool
  ; "mul_overflows" , [int; int], bool
  ; "mulu_overflows", [int; int], bool
  ; "ne"            , [int; int], bool
  ; "not"           , [bool], bool
  ; "quot_overflows", [int; int], bool
  ; "sub_overflows" , [int; int], bool
  ]

let except base exns =
  let keep (o, _, _) = not (List.exists (fun (o', _, _) -> o =$= o') exns) 
  in
  exns @ List.filter keep base

let nonbool ~int ~fp ~rm ~overrides = except (nonboolops ~int ~fp ~rm) overrides
  
let full ~int ~fp ~rm ~bool ~overrides =
  except (nonboolops ~int ~fp ~rm @ boolops ~int ~fp ~bool) overrides
    
type t = (Talloc.Multiple.t -> int -> Register.t) * (Register.t -> bool)
    
let spacename (s, _, _) = s
  
let of_space s =
  match s.Space.classification with
    | Space.Temp { Space.stands_for = ok } ->
	let name = spacename s.Space.space in
	let alloc = Talloc.Multiple.reg name in
	let ok ((s', _, _) as r) = spacename s' =<= name || ok r in
	alloc, ok
    | _ -> impossf "context from non-temporary space"

let rec of_spaces l = match l with
  | [] -> impossf "context from empty list of spaces"
  | [s] -> of_space s
  | s :: ss ->
      let a,  o  = of_space  s  in
      let a', o' = of_spaces ss in
      let acellwidth =
	let (_, _, cell) = s.Space.space in Cell.to_width cell (Cell.C 1) in
      let alloc m =
	let a  = a  m in
	let a' = a' m in
	fun w -> if w = acellwidth then a w else a' w in
      alloc, (fun r -> o r || o' r)
	
module SM = Strutil.Map
  
let functions ops =
  let resmap = List.fold_left (fun m (n, a, r)-> SM.add n r m) SM.empty ops in
  let argmap = List.fold_left (fun m (n, a, r)-> SM.add n a m) SM.empty ops in
  let arg_contexts   (n, _) =
    try SM.find n argmap with Not_found -> impossf "no arg context for %s" n in
  let result_context (n, _) =
    try SM.find n resmap with Not_found -> impossf "no result context for %s" n 
  in
  arg_contexts, result_context
