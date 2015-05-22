(* This is not actually a noweb file, but it *is* included in front
   of every generated .ml file.  It turns off evil polymorphic functions *)

   (* permit bare = on integers only *)
let (=|=) (x:int)    (y:int)    = (=) x y
let (=<=) (x:char)   (y:char)   = (=) x y
let (=$=) (x:string) (y:string) = (=) x y
let (=:=) (x:bool)   (y:bool)   = (=) x y
let (=*=) = (=)
let (=) = (=|=)

  (* mnemonics:  |  I  int
                 <  C  char
                 $  S  string
                 :  B  bool
                 *     any type
   *)

let (<>) (x:int) (y:int) = (<>) x y


let comparei (x:int)    (y:int)    = compare x y
let comparec (x:char)   (y:char)   = compare x y
let compares (x:string) (y:string) = compare x y
let compareb (x:bool)   (y:bool)   = compare x y
let compare = comparei

let (<.)  (x:float) (y:float) = (<) x y
let (>.)  (x:float) (y:float) = (>) x y
let (<=.) (x:float) (y:float) = (<=) x y
let (>=.) (x:float) (y:float) = (>=) x y
let (<>.) (x:float) (y:float) = Pervasives.(<>) x y

let (<)  (x:int) (y:int) = (<) x y
let (>)  (x:int) (y:int) = (>) x y
let (<=) (x:int) (y:int) = (<=) x y
let (>=) (x:int) (y:int) = (>=) x y
module R     = Rtl.Private
let fprintf  = Printf.fprintf

let (<<) f g = fun x -> f (g x)
let quoted        q = "\"" ^ q ^ "\""

let parenthesized p = "("  ^ p ^ ")"
let luafunc       f args = "CMM." ^ f ^ parenthesized args ^ "\n"

let luapush  kind c = luafunc ("push_" ^ kind) c

let valstack    op s = op ^ s
let argspace    op s = op ^ s
let globalspace op s = op ^ s
let localspace  op s = op ^ s
let commentout  _  _ = "comment"

let tybits b = 
  match b with
  | Types.Bool    -> "bool"
  | Types.Bits(i) -> "bits" ^ string_of_int i

let hexbits v      = quoted (Printf.sprintf "0x%Lx" (Bits.U.to_int64 v))
(* const : Rtl.Private.const -> string *)
let const c =
    match c with
    | R.Bool(true)    -> luapush "literal" (quoted "0x1" ^ ",1")
    | R.Bool(false)   -> luapush "literal" (quoted "0x0" ^ ",1")
    | R.Bits b        -> luapush "literal" (hexbits b ^ "," ^ 
                                      string_of_int (Bits.width b))

    | R.Link(sym,_,_) -> luapush "symbol" (quoted sym#mangled_text)
    | R.Diff _        -> Impossible.impossible "PIC not supported"
    | R.Late(name, _) -> luapush "symbol" (quoted name)
(* opr : string -> width list -> string *)
let opr op w = 
  let opty, result = Rtlop.mono (Rtl.opr op w) in
  luafunc "apply_operator"  ((quoted op) ^ "," ^
                             (quoted 
                                ((String.concat "," (List.map tybits opty)) ^ 
                                 ":" ^ (tybits result))))
(* byteorder : Rtl.aggregation -> string *)
let byteorder = function
  | Rtl.BigEndian    -> quoted "NATIVE"
  | Rtl.LittleEndian -> quoted "NATIVE"
  | Rtl.Identity     -> Impossible.impossible "rtltolua cannot handle Identity"

let alignment _ = "0" (* string_of_int i *)
(* expr' : Rtl.Private.exp -> string *)
let rec expr' e =
    match e with
    | R.Const c           -> const c
    | R.Fetch(loc, _)     -> 
        let (prefix, func_f, func_suffix, args) = location' loc in
        prefix ^ luafunc (func_f "fetch" func_suffix) args
    | R.App((op, ww), ee) -> (String.concat "\n" (List.map expr' ee)) ^ 
                             opr op ww
(* location' : Rtl.Private.loc -> string * (string -> string -> string) *
                                  string * string                            *)
and location' = function
    | R.Mem ((sp, agg, ms), c, e, ass) -> 
        ( expr' e
        , valstack
        , ""
        , string_of_int (Cell.to_width ms c) ^ "," ^ byteorder agg ^ "," ^ alignment ass
        )

    | R.Reg   (('A',_,_), i, _)        ->  (* argument-passing register space *)
        ( ""
        , argspace
        , "_arg"
        , string_of_int i
        )

    | R.Reg   (('V',_,_), 0, _)            ->  (* VFP space *)
        ( ""
        , valstack
        , "_sp"
        , ""
        )

    | R.Reg (('d',_,ms), 0, R.C 1) when Cell.size ms = 2 ->
        (* floating-pt rounding mode space *)
        ( ""
        , valstack
        , "_rounding"
        , ""
        )

    | R.Reg   ((sp,_,ms), i, c)         ->
        ( ""
        , commentout
        , "_register"
        , "'" ^ String.make 1 sp ^ "'," ^ string_of_int i)
                                        
    | R.Var   (name, index, width)   -> 
        ( ""
        , localspace
        , "_local"
        , string_of_int index
        )

    | R.Global (name, index, width)   -> 
        ( ""
        , globalspace
        , "_global"
        , string_of_int index
        )

    (* NOTE: should R.Slice just fail an assertion? *)                 
    | R.Slice (w, i, loc)              -> 
        let (p, ff, s, a) = location' loc in ("-- slice\n" ^ p, ff, s, a)
let expr printer rtl = printer (expr' (Rtl.Dn.exp rtl))
let location         = location' << Rtl.Dn.loc
(* guard : Rtl.Private.exp -> string * string *)
let guard = function
  | R.Const (R.Bool true) -> (""     , "" )
  | g                     -> (expr' g, "g")
let rtl' printer gg =
   let get_store = function
                 | (_ , R.Kill _)              -> 
                     Impossible.impossible "rtltolua : Rtl.Kill"
                 | (gd, R.Store (loc, exp, w)) ->
                     (guard gd, location' loc, exp, w)                     in
   let gg_locs   = List.map get_store gg                                   in
   let gg_stores = List.rev_map
                     (fun ((gd_prefix, gd_f_prefix), 
                           (prefix, func_f, func_suffix, args), exp, w
                          ) -> 
                       ( printer (gd_prefix ^ prefix ^ expr' exp)
                       ; luafunc (gd_f_prefix ^ func_f "store" func_suffix)
                                 args
                       )
                     )
                     gg_locs                                               in
    List.iter printer gg_stores
(* exported functions *)
let rtl printer r = 
   let clean = fun glist g -> match (glist, g) with
     | (glist, (gd, R.Store (R.Reg(('c',_,_), _, _), exp, w))) -> glist
     | (glist, (gd, R.Store (R.Reg(('t',_,_), _, _), exp, w))) -> glist
     | (glist, g                                       ) -> g::glist

   in let (R.Rtl guardeds) = Rtl.Dn.rtl r
   in let guardeds         = List.fold_left clean [] guardeds
   in rtl' printer guardeds

let exp    = expr
