include Common
module BO = Bits.Ops
module R  = Rtl
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn 
module Up = Rtl.Up

exception Error of string
type reloc     = Reloc.t

let error msg  = raise (Error msg)
let impossf fmt = Printf.kprintf Impossible.impossible fmt

module ToString = struct
  let exp  e = RU.ToString.exp (Up.exp e)
  let exp' e = RU.ToUnreadableString.exp (Up.exp e)
end

module Safe = 
struct
  let default op w args = RP.App((op,w),args)
  let to_bool b = RP.Const (RP.Bool b) 
  let const k   = RP.Const (RP.Bits k)
  let false' w args = RP.Const(RP.Bool false)
  let true'  w args = RP.Const(RP.Bool true)
  let round_down    w args = RP.Const(RP.Bits(Bits.U.of_int 1 2))
  let round_up      w args = RP.Const(RP.Bits(Bits.U.of_int 2 2))
  let round_nearest w args = RP.Const(RP.Bits(Bits.U.of_int 0 2))
  let round_zero    w args = RP.Const(RP.Bits(Bits.U.of_int 3 2))
  let rec add ws args = 
    let w = match ws with [w] -> w | _ -> Impossible.impossible "ill-typed add" in
    match args with
      | [ RP.App(("add", ws'), [x; RP.Const (RP.Bits k)]); RP.Const (RP.Bits k') ] ->  
          assert (ws=*=ws');
          add ws [x; RP.Const (RP.Bits (Bits.Ops.add k k'))]
      | [RP.Const (RP.Bits x); RP.Const (RP.Bits y)] ->  
          RP.Const (RP.Bits (Bits.Ops.add x y))
      | [x; RP.Const (RP.Bits y)] when Bits.Ops.eq (Bits.U.of_int 0 w) y -> x
      | args  ->  default "add" ws args

  let sub ws args =
    let rec remove_one y xs = match xs with
      | [] -> Impossible.impossible "did not find removable expression"
      | x :: xs when RU.Eq.exp y x -> xs
      | x :: xs -> x :: remove_one y xs 
    in
    let w = match ws with [w] -> w | _ -> Impossible.impossible "ill-typed sub" 
    in
    match args with
      | [RP.Const(RP.Bits x); RP.Const(RP.Bits y)] -> const (Bits.Ops.sub x y)
      | [RP.App(("add",ws'),args); e] when List.mem e args ->
          (match remove_one e args with
            | _::_::_ as rst -> add ws' rst
            | [a]            -> a
            | []             -> RP.Const(RP.Bits(Bits.zero w))
          )
      | [x; y] when RU.Eq.exp x y -> RP.Const(RP.Bits(Bits.zero w))
      | args      -> default "sub" ws args
	  
  let binop name w op = function
    | [RP.Const(RP.Bits x); RP.Const(RP.Bits y)] -> const (op x y)
    | args       -> default name w args
	
  let disjoin w = function
    | [RP.Const(RP.Bool true); _] -> RP.Const(RP.Bool true)
    | [_; RP.Const(RP.Bool true)] -> RP.Const(RP.Bool true)
    | [RP.Const(RP.Bool false); p] -> p
    | [p; RP.Const(RP.Bool false)] -> p
    | args       -> default "disjoin" w args

  let conjoin w = function
    | [RP.Const(RP.Bool false); _] -> RP.Const(RP.Bool false)
    | [_; RP.Const(RP.Bool false)] -> RP.Const(RP.Bool false)
    | [RP.Const(RP.Bool true); p] -> p
    | [p; RP.Const(RP.Bool true)] -> p
    | args       -> default "conjoin" w args
	
  let not' w = function
    | [RP.Const(RP.Bool b)] -> RP.Const(RP.Bool (not b))
    | args       -> default "not" w args
	
  let cmp_bits name w op = function
    | [RP.Const(RP.Bits x); RP.Const(RP.Bits y)] -> to_bool (op x y)
    | args       -> default name w args
	
  let unop name w op = function
    | [RP.Const(RP.Bits x)] -> const (op x)
    | args       -> default name w args
	
  let or' ws args = 
    let w = match ws with [w] -> w | _ -> Impossible.impossible "ill-typed or" in
    match args with
      | [RP.Const (RP.Bits x); RP.Const (RP.Bits y)] ->  
          RP.Const (RP.Bits (Bits.Ops.or' x y))
      | [x; RP.Const (RP.Bits y)] when Bits.Ops.eq (Bits.U.of_int 0 w) y -> x
      | [RP.Const (RP.Bits x); y] when Bits.Ops.eq (Bits.U.of_int 0 w) x -> y
      | args  ->  default "or" ws args

  let and' ws args = 
    let w = match ws with [w] -> w | _ -> Impossible.impossible "ill-typed and" in
    match args with
      | [RP.Const (RP.Bits x); RP.Const (RP.Bits y)] ->  
          RP.Const (RP.Bits (Bits.Ops.and' x y))
      | [x; RP.Const (RP.Bits y) as zero] when Bits.Ops.eq (Bits.U.of_int 0 w) y -> zero
      | [RP.Const (RP.Bits x) as zero; y] when Bits.Ops.eq (Bits.U.of_int 0 w) x -> zero
      | args  ->  default "and" ws args
	  
  let rec zx w args   = match w, args with
    | [n;w],[RP.Const (RP.Bits x)] -> const (Bits.Ops.zx w x)
    | [n;w],[RP.App (("zx", [n';w']), [x])] -> assert (w' = n); zx [n';w] [x]
    | [n;w], [x] when n = w -> x
    | _         -> default "zx" w args

  let rec sx w args   = match (w,args) with
    | [i;j],[RP.Const(RP.Bits x)] -> const (Bits.Ops.sx j x)
    | [n;w],[RP.App (("sx", [n';w']), [x])] -> assert (w' = n); sx [n';w] [x]
    | [n;w],[RP.App (("zx", [n';w']), [x])] when w' > n' -> assert (w' = n); zx [n';w] [x]
    | [i;j], [x] when i = j -> x
    | _         -> default "sx" w args
	
  let rec lobits ws args   = match (ws,args) with
    | [i;j],[RP.Const(RP.Bits x)] -> const (Bits.Ops.lobits j x)
    | [w;n],[RP.Fetch(RP.Mem ((sp, Rtl.LittleEndian, mcell) as mspace, ct, addr, assn), w')] when Cell.divides mcell n ->
        RP.Fetch (RP.Mem (mspace, Cell.to_count mcell n, addr, assn), n)
    | [w;n],[RP.Fetch(RP.Mem ((sp, Rtl.BigEndian, mcell) as mspace, ct, addr, assn), w'')] when Cell.divides mcell (w - n) ->
        let R.C offset = Cell.to_count mcell (w - n) in
        let addr = Dn.exp (RU.addk (RU.Width.exp' addr) (Up.exp addr) offset) in
        let assn = Alignment.gcd assn offset in
        RP.Fetch (RP.Mem (mspace, Cell.to_count mcell n, addr, assn), n)
    | [w;n],[RP.App(("shrl", [_]),
      [RP.Fetch(RP.Mem ((sp, Rtl.LittleEndian, mcell) as mspace, ct, addr, assn), w'); RP.Const(RP.Bits b)])] ->
        let shamt = Bits.U.to_int b in
        if Cell.divides mcell shamt then
          let R.C offset = Cell.to_count mcell shamt in
          let addr = Dn.exp (RU.addk (RU.Width.exp' addr) (Up.exp addr) offset) in
          let assn = Alignment.gcd assn offset in
          lobits [w-shamt; n]
            [RP.Fetch (RP.Mem (mspace, Cell.to_count mcell (w-shamt), addr, assn),w-shamt)]
        else
          default "lobits" ws args
    | [w;n],[RP.App(("shrl", [_]), [RP.Fetch(RP.Mem ((sp, Rtl.BigEndian, mcell) as mspace, ct, addr, assn), w''); RP.Const(RP.Bits b)])] ->
        let shamt = Bits.U.to_int b in
        if Cell.divides mcell shamt then
          lobits [w-shamt; n]
            [RP.Fetch (RP.Mem (mspace, Cell.to_count mcell (w-shamt), addr, assn),w-shamt)]
        else
          default "lobits" ws args
    | [w;n],[RP.Fetch(RP.Reg ((sp, Rtl.LittleEndian, rcell) as rspace, index, count), w'')] when Cell.divides rcell n ->
        RP.Fetch (RP.Reg (rspace, index, Cell.to_count rcell n), n)
    | [w;n],[RP.Fetch(RP.Reg ((sp, Rtl.BigEndian,    rcell) as rspace, index, count), w'')] when Cell.divides rcell (w - n) ->
        let R.C offset = Cell.to_count rcell (w - n) in
        let index      = index + offset in
        RP.Fetch (RP.Reg (rspace, index, Cell.to_count rcell n), n)
    | [w;n],[RP.App(("shrl", [_]),
      [RP.Fetch(RP.Reg ((sp, Rtl.LittleEndian, rcell) as rspace, index, count), w''); RP.Const(RP.Bits b)])] ->
        let shamt = Bits.U.to_int b in
        if Cell.divides rcell shamt then
          let R.C offset = Cell.to_count rcell shamt in
          let index      = index + offset in
          lobits [w-shamt; n]
            [RP.Fetch (RP.Reg (rspace, index, Cell.to_count rcell (w-shamt)), w-shamt)]
        else
          default "lobits" ws args
    | [w;n],[RP.App(("shrl", [_]), [RP.Fetch(RP.Reg ((sp, Rtl.BigEndian,    rcell) as rspace, index, count), w''); RP.Const(RP.Bits b)])] ->
        let shamt = Bits.U.to_int b in
        if Cell.divides rcell shamt then
          lobits [w-shamt; n]
            [RP.Fetch (RP.Reg (rspace, index, Cell.to_count rcell (w-shamt)), w-shamt)]
        else
          default "lobits" ws args
    | [i;j], [x] when i = j -> x
    | _         -> default "lobits" ws args

  let () = Cmm_debug.register "simp-lobits" "simplification of %lobits expressions"

  let lobits =
    if Cmm_debug.on "simp-lobits" then
      (fun ws args ->
        let e = RP.App (("lobits", ws), args) in
        let answer = lobits ws args in
        Printf.eprintf "simplified %s -> %s\n" (ToString.exp e) (ToString.exp answer);
        answer)
    else
      lobits

  let unsigned n w = RP.Const (RP.Bits (Bits.U.of_int n w))
  let null = function [] -> true | _ :: _ -> false

  let zero1 = Bits.U.of_int 0 1
  let one1  = Bits.U.of_int 1 1
  let bool w args = assert (null w); match args with
    | [RP.Const (RP.Bits x)] -> RP.Const (RP.Bool (Bits.Ops.ne x zero1))
    | [RP.App (("lobits", [w; 1]), [x])] ->
        RP.App (("ne", [w]), [RP.App (("and", [w]), [x; unsigned 1 w]); unsigned 0 w])
    | _ -> default "bool" w args


  let bit w args = assert (null w); match args with
    | [RP.Const (RP.Bool b)] -> RP.Const (RP.Bits (if b then one1 else zero1))
    | _ -> default "bit" w args
	
  let pzero w args = assert (null args); match w with
    | [(32|64) as w] -> const (Bits.zero w)
    | _ -> default "pzero" w args

  let mzero w args = assert (null args); match w with
    | [(32|64) as w] -> const (BO.shl (Bits.U.of_int 1 w) (Bits.U.of_int (w-1) w))
    | _ -> default "mzero" w args

  let pinf w args = assert (null args); match w with
    | [32] -> const (BO.shl (Bits.U.of_int 0xff  32) (Bits.U.of_int 23 32))
    | [64] -> const (BO.shl (Bits.U.of_int 0x7ff 64) (Bits.U.of_int 52 64))
    | _ -> default "pinf" w args

  let minf w args = assert (null args); match w with
    | [32|64] -> or' w [pinf w args; mzero w args]
    | _ -> default "minf" w args

  let nan w args =
    let swidth = function
      | 32 -> 23
      | 64 -> 52
      | _  -> Impossible.unimp "NaN at width other than 32 or 64" in
    match w, args with
      | [n;w], [x] when n = swidth w -> or' [w] [pinf [w] []; zx [n; w] [x]]
      | [n;w], [x] -> impossf "significand in NaN has width %d; should be %d" n (swidth w)
      | _ -> Impossible.impossible "ill-formed NaN"
	  
  let x86_carrybit w args = match args with
    | [RP.App (("x86_setcarry", _), [_; e])] -> e
    | _ -> default "x86_carrybit" w args

        
  let app o w args = match o with
    | "NaN"           -> nan w args
    | "add"           -> add w args
    | "and"           -> binop o w BO.and' args
    | "bit"           -> bit  w args
    | "bool"          -> bool w args
    | "borrow"        -> default o w args
    | "carry"         -> default o w args
    | "com"           -> unop o w BO.com args
    | "conjoin"       -> conjoin w args
    | "disjoin"       -> disjoin w args
    | "div"           -> default o w args
    | "divu"          -> binop o w BO.divu args
    | "eq"            -> cmp_bits o w BO.eq args
    | "f2f"           -> default o w args
    | "f2i"           -> default o w args
    | "fabs"          -> default o w args
    | "fadd"          -> default o w args
    | "false"         -> false'    w args
    | "fcmp"          -> default o w args
    | "fdiv"          -> default o w args
    | "feq"           -> default o w args
    | "fge"           -> default o w args
    | "fgt"           -> default o w args
    | "fle"           -> default o w args
    | "float_eq"      -> default o w args
    | "float_gt"      -> default o w args
    | "float_lt"      -> default o w args
    | "flt"           -> default o w args
    | "fmul"          -> default o w args
    | "fmulx"         -> default o w args
    | "fne"           -> default o w args
    | "fneg"          -> default o w args
    | "fordered"      -> default o w args
    | "fsqrt"         -> default o w args
    | "fsub"          -> default o w args
    | "funordered"    -> default o w args
    | "ge"            -> cmp_bits o w (fun x y -> BO.lt  y x) args
    | "geu"           -> cmp_bits o w (fun x y -> BO.ltu y x) args
    | "gt"            -> cmp_bits o w BO.gt args
    | "gtu"           -> cmp_bits o w BO.gtu args
    | "i2f"           -> default o w args
    | "le"            -> cmp_bits o w (fun x y -> BO.gt  y x) args
    | "leu"           -> cmp_bits o w (fun x y -> BO.gtu y x) args
    | "lobits"        -> lobits w args
    | "lt"            -> cmp_bits o w BO.lt args
    | "ltu"           -> cmp_bits o w BO.ltu args
    | "minf"          -> minf w args
    | "mod"           -> default o w args
    | "modu"          -> default o w args
    | "mul"           -> binop o w BO.mul args
    | "mulux"         -> default o w args
    | "mulx"          -> default o w args
    | "mzero"         -> mzero w args
    | "ne"            -> cmp_bits o w BO.ne args
    | "neg"           -> unop  o w BO.neg args
    | "not"           -> not' w args
    | "or"            -> binop o w BO.or' args
    | "pinf"          -> pinf w args
    | "popcnt"        -> default o w args
    | "pzero"         -> pzero w args
    | "quot"          -> default o w args
    | "rem"           -> default o w args
    | "rotl"          -> default o w args
    | "rotr"          -> default o w args
    | "round_down"    -> round_down w args
    | "round_nearest" -> round_nearest w args
    | "round_up"      -> round_up w args
    | "round_zero"    -> round_zero w args
    | "shl"           -> binop o w BO.shl args
        (* likely to break machine invariant on PPC? *)

    | "shra"          -> binop o w BO.shra args
    | "shrl"          -> binop o w BO.shrl args
    | "sub"           -> sub w args
    | "sx"            -> sx w args
    | "true"          -> true' w args
    | "unordered"     -> default o w args
    | "xor"           -> binop o w BO.xor args
    | "zx"            -> zx w args
    | "bitExtract"    -> default o w args
    | "bitInsert"     -> default o w args
    | "bitTransfer"   -> default o w args
    | "x86_carrybit"  -> x86_carrybit w args
    | o               -> default o w args
                          
  let rec exp e =
    match e with
      | RP.Const _ as c       -> c
      | RP.Fetch (l, w)       -> RP.Fetch (loc l, w)
      | RP.App ((o,w),args)   -> app o w (List.map exp args) 
          
  and loc = function          
    | RP.Mem(sp, w, e, ass) -> RP.Mem(sp, w, exp e, ass)
    | RP.Slice(n,lsb,l)     -> (match loc l with
        | RP.Mem ((sp, Rtl.BigEndian, mcell) as mspace, ct, addr, assn)
            when Cell.divides mcell n && Cell.divides mcell lsb ->
            let w          = Cell.to_width mcell ct in
            let R.C offset = Cell.to_count mcell (w - n - lsb) in
            let addr       = Dn.exp (RU.addk (RU.Width.exp' addr) (Up.exp addr) offset) in
            let assn       = Alignment.gcd assn offset in
            RP.Mem (mspace, Cell.to_count mcell n, addr, assn)
        | RP.Mem ((sp, Rtl.LittleEndian, mcell) as mspace, ct, addr, assn) 
            when Cell.divides mcell n && Cell.divides mcell lsb ->
            let R.C offset = Cell.to_count mcell lsb in
            let addr       = Dn.exp (RU.addk (RU.Width.exp' addr) (Up.exp addr) offset) in
            let assn       = Alignment.gcd assn offset in
            RP.Mem (mspace, Cell.to_count mcell n, addr, assn)
        | RP.Reg ((sp, Rtl.BigEndian,    rcell) as rspace, index, count) 
            when Cell.divides rcell n && Cell.divides rcell lsb ->
            let w          = Cell.to_width rcell count in
            let R.C offset = Cell.to_count rcell (w - n - lsb) in
            RP.Reg (rspace, index + offset, Cell.to_count rcell n)
        | RP.Reg ((sp, Rtl.LittleEndian, rcell) as rspace, index, count) 
            when Cell.divides rcell n && Cell.divides rcell lsb ->
            let R.C offset = Cell.to_count rcell lsb in
            RP.Reg (rspace, index + offset, Cell.to_count rcell n)
        | l -> RP.Slice(n,lsb,l))
    | RP.Reg(sp, i, w) as v -> v
    | RP.Var   (s,i,w) as v -> v
    | RP.Global(s,i,w) as v -> v
                                
  let effect = function       
    | RP.Store(l,e,w)       -> RP.Store(loc l, exp e, w)
    | RP.Kill(l)            -> RP.Kill(loc l)
                                
  let guarded (e, eff)        = (exp e, effect eff)
  let rtl (RP.Rtl(es))        = RP.Rtl(List.map guarded es)
end
  
module Unsafe = 
struct
  let zero w = RP.Const(RP.Bits (Bits.U.of_int 0 w))
  let reg_offset = function
    | RP.App(("add", _), [RP.Fetch(RP.Reg r,_); RP.Const _ as k]) -> Some (r,k)
    | RP.Fetch(RP.Reg r, w) -> Some (r, zero w)
    | _                     -> None
	
  let rec app o w args = match Safe.app o w args with
    | RP.App (("ne"|"eq"|"ltu"|"gtu"|"lt"|"gt" as op, w), [left;right]) as x ->
        ( match reg_offset left, reg_offset right with
          | Some(r1,k1), Some(r2,k2) when Register.eq r1 r2 -> 
              let r      = app op w [k1;k2] in
              let _dbg() = Printf.eprintf "Simplify2.Unsafe.app: %s `%s` %s = %s\n"
                (ToString.exp k1) op (ToString.exp k2) (ToString.exp r) in
              r
                
          | _  -> x
        )
    | x -> x

  let rec exp = function
    | RP.Const _ as c       -> c
    | RP.Fetch (l, w)       -> RP.Fetch (loc l, w)
    | RP.App ((o,w),args)   -> app o w (List.map exp args) 
        
  and loc = function          
    | RP.Mem(sp, w, e, ass) -> RP.Mem(sp, w, exp e, ass)
    | RP.Slice(w,i,l)       -> RP.Slice(w, i, loc l)
    | RP.Reg(sp, i, w) as v -> v
    | RP.Var   (s,i,w) as v -> v
    | RP.Global(s,i,w) as v -> v
                                
  let effect = Safe.effect    
                                
  let guarded (e, eff)        = (exp e, effect eff)
  let rtl' (RP.Rtl(es))       =
    let add_guarded e es = match guarded e with
      | RP.Const(RP.Bool false), _ -> es
      | e                          -> e :: es in
    RP.Rtl(List.fold_right add_guarded es [])

    (*let rtl' (RP.Rtl es) = RP.Rtl (List.map guarded es)*)

  let rtl r  = try Up.rtl (rtl' (Dn.rtl r)) 
  with Error msg -> Impossible.impossible msg
end


let rtl r  = 
  try Up.rtl (Safe.rtl (Dn.rtl r))
  with Error msg -> Impossible.impossible msg
    
let exp e  = 
  try Up.exp (Safe.exp (Dn.exp e))
  with Error msg -> Impossible.impossible msg

let rec location_category = function
  | RP.Mem _ -> "memory"
  | RP.Reg _ -> "a machine register"
  | RP.Var (name, _, _) | RP.Global (name, _, _) ->
      Printf.sprintf "register variable '%s'" name
  | RP.Slice (_, _, loc) -> location_category loc

let bits e =
  let rec cvt = function
    | RP.Const (RP.Bits b) -> b
    | RP.Const (RP.Bool _) -> Error.error "a constant expression may not be a Boolean"
    | RP.Const (RP.Link (s, _, _)) ->
        Error.errorf "constant %s not resolvable until link time" s#original_text
    | RP.Const (RP.Diff (_, _)) ->
        Error.errorf "difference of two constants not resolvable until link time"
    | RP.Const (RP.Late (s, _)) ->
        Error.errorf "late compile-time constant %s is not a constant yet" s
    | RP.Fetch (l, _) ->
        Error.errorf "a constant expression may not refer to %s"
          (location_category l)
    | RP.App ((opr, _), es) ->
        List.iter (fun e -> ignore (cvt e)) es;
        Error.errorf "cannot evaluate operator %%%s at compile time" opr 
  in
  cvt (Safe.exp (Dn.exp e))
    
let bool e = 
  match Safe.exp (Dn.exp e) with
    | RP.Const(RP.Bool(b)) -> b
    | _                    -> Error.error "not a constant condition"

let mklink kind s w = Up.exp (RP.Const (RP.Link(s, kind, w)))

let link e =
  let rec const c = match c with
    | RP.Bits b   -> Reloc.of_const b
    | RP.Link(l,kind,w) -> Reloc.of_sym (l, mklink kind) w
    | RP.Diff(c1,c2) -> Reloc.sub (const c1) (const c2)
    | _ -> Error.errorf "Bad link-time constant %s" (Rtlutil.ToString.const c) in
  let rec exp e = match e with
    | RP.App(("add",_),[x; y]) -> Reloc.add (exp x) (exp y)
    | RP.App(("sub",_),[x; y]) -> Reloc.sub (exp x) (exp y)
    | RP.Const c -> const c
    | _ -> Error.errorf "Bad link-time constant expression %s"
        (Rtlutil.ToString.exp (Up.exp e)) in
  exp (Safe.exp (Dn.exp e)) 

let compile_time_ops = 
  [ "add"; "and"; "com"; "divu"; "eq"; "ge"; "geu"; "gt"; "gtu"; "le"; "leu";
  "lobits"; "lt"; "ltu"; "mul"; "ne"; "neg"; "or"; "shl"; "shra";
  "shrl"; " sub"; "sx"; "xor"; "zx"]
