module T = Types        (* save external Types module here *)
let (-->) = T.proc 
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let predefined = 
  (* the four values below characterize the proper interpretation of a
     result, but the only information we actually use is a Boolean
     telling us if the result is a floating-point value *)
  let float = true in
  let int   = false in
  let code2 = false in
  let bool  = false 
  in
  [  "NaN"          , int,   [T.var 1] --> T.var 2
  ;  "add"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "addc"         , int,   [T.var 1; T.var 1; T.fixbits 1] --> T.var 1
  ;  "add_overflows", bool,  [T.var 1; T.var 1] --> T.bool
  ;  "and"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "bit"          , int,   [T.bool] --> T.fixbits 1
  ;  "bool"         , bool,  [T.fixbits 1] --> T.bool
  ;  "borrow"       , int,   [T.var 1; T.var 1; T.fixbits 1] --> T.fixbits 1
  ;  "carry"        , int,   [T.var 1; T.var 1; T.fixbits 1] --> T.fixbits 1
  ;  "com"          , int,   [T.var 1] --> T.var 1
  ;  "conjoin"      , bool,  [T.bool; T.bool] --> T.bool
  ;  "disjoin"      , bool,  [T.bool; T.bool] --> T.bool
  ;  "div"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "div_overflows", bool,  [T.var 1; T.var 1] --> T.bool
  ;  "divu"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "eq"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "f2f"          , float, [T.var 1; T.fixbits 2] --> T.var 2
  ;  "f2f_implicit_round", float, [T.var 1] --> T.var 2
  ;  "f2i"          , int,   [T.var 1; T.fixbits 2] --> T.var 2
  ;  "fabs"         , float, [T.var 1] --> T.var 1
  ;  "fadd"         , float, [T.var 1; T.var 1; T.fixbits 2] --> T.var 1
  ;  "false"        , bool,  [] --> T.bool
  ;  "fcmp"         , code2, [T.var 1; T.var 1] --> T.fixbits 2
  ;  "fdiv"         , float, [T.var 1; T.var 1; T.fixbits 2] --> T.var 1
  ;  "feq"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fge"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fgt"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fle"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "float_eq"     , code2, [] --> T.fixbits 2
  ;  "float_gt"     , code2, [] --> T.fixbits 2
  ;  "float_lt"     , code2, [] --> T.fixbits 2
  ;  "flt"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fmul"         , float, [T.var 1; T.var 1; T.fixbits 2] --> T.var 1
  ;  "fmulx"        , float, [T.var 1; T.var 1] --> T.double 1
  ;  "fne"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fneg"         , float, [T.var 1] --> T.var 1
  ;  "fordered"     , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "fsqrt"        , float, [T.var 1; T.fixbits 2] --> T.var 1
  ;  "fsub"         , float, [T.var 1; T.var 1; T.fixbits 2] --> T.var 1
  ;  "funordered"   , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "ge"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "geu"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "gt"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "gtu"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "i2f"          , float, [T.var 1; T.fixbits 2] --> T.var 2
  ;  "le"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "leu"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "lobits"       , int,   [T.var 1] --> T.var 2
  ;  "lt"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "ltu"          , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "minf"         , float, [] --> T.var 1
  ;  "mod"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "modu"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "mul"          , int,   [T.var 1; T.var 1] --> T.var 1 
  ;  "mulux"        , int,   [T.var 1; T.var 1] --> T.double 1 
  ;  "mulx"         , int,   [T.var 1; T.var 1] --> T.double 1 
  ;  "mul_overflows", bool,  [T.var 1; T.var 1] --> T.bool
  ;  "mulu_overflows", bool, [T.var 1; T.var 1] --> T.bool
  ;  "mzero"        , float, [] --> T.var 1
  ;  "ne"           , bool,  [T.var 1; T.var 1] --> T.bool
  ;  "neg"          , int,   [T.var 1] --> T.var 1
  ;  "not"          , bool,  [T.bool] --> T.bool
  ;  "or"           , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "pinf"         , float, [] --> T.var 1
  ;  "popcnt"       , int,   [T.var 1] --> T.var 1
  ;  "pzero"        , float, [] --> T.var 1
  ;  "quot"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "quot_overflows", bool,  [T.var 1; T.var 1] --> T.bool
  ;  "rem"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "rotl"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "rotr"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "round_down"   , code2, [] --> T.fixbits 2
  ;  "round_nearest", code2, [] --> T.fixbits 2
  ;  "round_up"     , code2, [] --> T.fixbits 2
  ;  "round_zero"   , code2, [] --> T.fixbits 2
  ;  "shl"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "shra"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "shrl"         , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "sub"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "subb"         , int,   [T.var 1; T.var 1; T.fixbits 1] --> T.var 1
  ;  "sub_overflows", bool,  [T.var 1; T.var 1] --> T.bool
  ;  "sx"           , int,   [T.var 1] --> T.var 2
  ;  "true"         , bool,  [] --> T.bool
  ;  "unordered"    , code2, [] --> T.fixbits 2
  ;  "xor"          , int,   [T.var 1; T.var 1] --> T.var 1
  ;  "zx"           , int,   [T.var 1] --> T.var 2
    (* not C-- operators, but might show up in RTLs *)    
  ;  "bitExtract"   , int,   [T.var 1; T.var 1] --> T.var 2
  ;  "bitInsert"    , int,   [T.var 1; T.var 1; T.var 2] --> T.var 1
  ;  "bitTransfer"  , int,   [T.var 1; T.var 1; T.var 1; T.var 1; T.var 1] --> T.var 1 
                    
  ] 
    
let optypes  = ref (Strutil.assoc2map (List.map (fun (o, _, t) -> (o, t)) predefined))
let opfloats = ref (Strutil.assoc2map (List.map (fun (o, f, _) -> (o, f)) predefined))
let add_operator ~name:o ~result_is_float:f t =
  if Strutil.Map.mem o (!optypes) then
    impossf "registered a duplicate RTL operator %%%s" o;
  optypes  := Strutil.Map.add o t (!optypes);
  opfloats := Strutil.Map.add o f (!opfloats)
    
let print_shapes () = 
  Strutil.Map.iter (fun o t ->
    Printf.printf "%16s : %s\n" o (Types.scheme_string t)) (!optypes)
    
let visible = function
  | "bitExtract" | "bitInsert" | "bitTransfer" | "f2f_implicit_round" -> false
  | "fcmp" | "float_lt" | "float_eq" | "float_gt" | "unordered" -> false (*expunged*)
  | _ -> true 

let fold f z =
  Strutil.Map.fold (fun o t z -> if visible o then f o t z else z) (!optypes) z

let opnames () = fold (fun o _ os -> o :: os) []
  
let findopname name t =
  try Strutil.Map.find name t
  with Not_found ->
    ( Printf.eprintf "unknown RTL operator '%%%s'" name
    ; raise Not_found
    )
      
let findop op = findopname (fst (Rtl.Dn.opr op))

let mono op = let _, ws = Rtl.Dn.opr op in Types.instantiate (findop op (!optypes)) ws

let has_floating_result op = findop op (!opfloats)
  
module Translate = 
struct
  let prefix opname argtys =
    try 
      let op, retsize = T.split opname in
      let opty        = findopname op (!optypes) in
      let argwidths   = T.widthlist opname opty argtys in
      ( match retsize with
	| None   -> T.appl opname opty argtys, Rtl.opr op argwidths
	| Some n -> T.bits n,                  Rtl.opr op (argwidths@[n])
      )            
    with
      | Not_found  -> Error.errorf "unknown operator %%%s" opname
	  
  let binops = Strutil.assoc2map 
    [  "+"      , "add"
    ;  "-"      , "sub"
    ;  "*"      , "mul"
    ;  "/"      , "divu"
    ;  "%"      , "modu"
    ;  "<<"     , "shl"
    ;  ">>"     , "shrl"
    ;  "=="     , "eq"
    ;  "<="     , "leu"
    ;  ">="     , "geu"
    ;  ">"      , "gtu"
    ;  "<"      , "ltu"
    ;  "!="     , "ne"
    ;  "&"      , "and"
    ;  "^"      , "xor"
    ;  "|"      , "or"
    ;  "!="     , "ne"
    ] 

  let unops = Strutil.assoc2map 
    [  "-"      , "neg"
    ;  "~"      , "com"
    ]

  let binary op =
    try prefix (Strutil.Map.find op binops) 
    with Not_found -> Impossible.impossible ("unknown binary infix operator "^op)

  let unary op =
    try prefix (Strutil.Map.find op unops) 
    with Not_found -> Impossible.impossible ("unknown unary symbolic operator "^op)
end

let ws = ["w"; "w'"; "w3"; "w4"; "w5"]
let args = ["x"; "y"; "z"; "u"; "v"]

let width n = try List.nth ws   n with _ -> Impossible.impossible "too many widths"
let arg   n = try List.nth args n with _ -> Impossible.impossible "too many args"


module Emit = 
struct
  let pf = Printf.printf
  let mangle = function
    | "and" -> "_and"
    | "NaN" -> "_Nan"
    | "mod" -> "_mod"
    | "or" -> "_or"
    | n -> n
          
  let emitop name (parms, _ as tyscheme) =
    let nargs   = List.length parms in
    let nwidths = T.largest_key tyscheme in
    begin
      pf "let %s" (mangle name);
      for i = 0 to nwidths - 1 do pf " %s"  (width i) done;
      for i = 0 to nargs   - 1 do pf " %s"  (arg   i) done;
      pf " = Rtl.app (Rtl.opr \"%s\" [" name;
      for i = 0 to nwidths - 1 do pf "%s;"  (width i) done;
      pf "]) [";
      for i = 0 to nargs   - 1 do pf "%s; " (arg   i) done;
      pf "]\n";
    end
  let creators () =
    pf "(* This code generated automatically by Rtlop.Emit.creators *)\n";
    Strutil.Map.iter emitop (!optypes)
end    
