include Common
module R   = Rtl
module RP  = Rtl.Private
module Up  = Rtl.Up
module Dn  = Rtl.Dn
module Rg  = Register
module RS  = Register.Set
module RSX = Register.SetX

let id     x = x
let falsef x = false

let unimpf fmt = Printf.kprintf Impossible.unimp fmt
let sprintf = Printf.sprintf

type aloc = 
    { 
      fetch  : Rtl.width -> Rtl.exp;
      store  : Rtl.exp -> Rtl.width -> Rtl.rtl
    }
      
module Width = 
struct
  let loc = Rtl.locwidth
  let rec const = function
    | RP.Bool _       -> Impossible.impossible "asked width of Boolean"
    | RP.Bits b       -> Bits.width b
    | RP.Link (_,_,w) -> w
    | RP.Diff (c,_)   -> const c
    | RP.Late (_,w)   -> w

  let exp' = function
    | RP.Const(c)   -> const c
    | RP.Fetch(_,w) -> w
    | RP.App (op,_) -> 
        ( match snd (Rtlop.mono (Rtl.Up.opr op)) with
          | Types.Bits n -> n
          | Types.Bool   -> Impossible.impossible "asked width of Boolean operator"
        ) 
  let exp e = exp' (Rtl.Dn.exp e)
end

let fetch l   = R.fetch l   (Width.loc l)
let store l e = R.store l e (Width.loc l)

let fold handle_reg handle_slice handle_mem ~read ~write ~kill r z = 
  let rec rtl (z:'a) (RP.Rtl gs)  = List.fold_left guarded z gs
  and guarded z (g, eff) = exp (effect z eff) g
  and effect z eff = match eff with
    | RP.Store (lhs, rhs, _)         -> loc write (exp z rhs) lhs
    | RP.Kill lhs                    -> loc kill z lhs
  and loc which z l = match l with
    | RP.Mem (space, w, addr, _) as m -> exp (handle_mem which z m) addr
    | RP.Reg r                        -> handle_reg       which z r
    | RP.Slice (w, i, l)              -> handle_slice loc which z (w, i, l)
    | RP.Var _                        -> z
    | RP.Global _                     -> z
  and exp z e = match e with
    | RP.Const _                      -> z
    | RP.Fetch (l, w)                 -> loc read z l
    | RP.App (_, es)                  -> List.fold_left exp z es
  in
  rtl z (Rtl.Dn.rtl r)

let fold_regx ~read ~write ~kill r z =
  fold (fun which z r -> which (Rg.Reg r) z)
    (fun loc which z (w, i, l) -> match l with
      | RP.Reg r -> which (Rg.Slice (w, i, r)) z
      | _        -> loc which z l)
    (fun _ z _ -> z)
    ~read ~write ~kill r z

let fold_regt ~read ~write ~kill r z =
  fold (fun which z r -> which r z)
    (fun loc which z (w, i, l) -> loc which z l)
    (fun _ z _ -> z)
    ~read ~write ~kill r z

module ReadWrite = 
struct
  type 'a observert = Register.t -> 'a -> 'a
  type 'a observerx = Register.x -> 'a -> 'a

  let fold         ~read ~write r z = fold_regx ~read ~write ~kill:write r z
  let fold_promote ~read ~write r z = fold_regt ~read ~write ~kill:write r z

  let mk_sets fold add empty rtl =
    let add_left  r (left,right) = (add r left, right) in
    let add_right r (left,right) = (left, add r right) in
    let empty = (empty, empty) 
    in
    fold ~read:add_left ~write:add_right rtl empty
  let sets         rtl = mk_sets fold         RSX.add RSX.empty rtl
  let sets_promote rtl = mk_sets fold_promote RS.add  RS.empty  rtl
end
  
module ReadWriteKill = 
struct
  type 'a observert = Register.t -> 'a -> 'a
  type 'a observerx = Register.x -> 'a -> 'a
  let fold         = fold_regx
  let fold_promote = fold_regt
    
  let mk_sets fold add empty rtl =
    let read  reg (r, w, k) = (add reg r, w, k) in
    let write reg (r, w, k) = (r, add reg w, k) in
    let kill  reg (r, w, k) = (r, w, add reg k) in
    fold ~read ~write ~kill rtl (empty, empty, empty)
      
  let sets         rtl = mk_sets fold         RSX.add RSX.empty rtl
  let sets_promote rtl = mk_sets fold_promote RS.add  RS.empty  rtl
end

module FullReadWriteKill = 
struct
  type 'a observer = RP.loc -> 'a -> 'a
  let fold ~read ~write ~kill r z =
    fold (fun which z r -> which (RP.Reg r) z)
      (fun loc which z (w, i, l) ->  (* ??? *)
        match l with
          | RP.Reg r -> which (RP.Slice (w, i, l)) z
          | _        -> loc which z l)
      (fun which z m -> which m z)
      ~read ~write ~kill r z
end
  
module Subst = 
struct
  module DownUp = 
  struct
    let rtl f r = Rtl.Up.rtl (f (Rtl.Dn.rtl r))
    let loc f l = Rtl.Up.loc (f (Rtl.Dn.loc l))
    let exp f e = Rtl.Up.exp (f (Rtl.Dn.exp e))
  end
    
  let loc_gen' guard map rtl ~def ~use = 
    let rec exp = function
      | RP.Fetch (l, width)  -> RP.Fetch(loc use l, width)
      | RP.App(opr, exprs)   -> RP.App(opr, List.map exp exprs)
      | x                    -> x
    and loc act l = if act && guard l then map l else match l with
      | RP.Mem (sp,w,e,ass)  -> RP.Mem(sp, w, exp e, ass)
      | RP.Slice (w,i,l)     -> RP.Slice(w,i,loc act l)
      | x                    -> x
    and effect = function      
      | RP.Store (l,e,width) -> RP.Store(loc def l, exp e, width)
      | RP.Kill  (l)         -> RP.Kill (loc def l)
    and guarded (e,eff)        = (exp e, effect eff)
    and subst (RP.Rtl rtl)     = RP.Rtl (List.map guarded rtl)
    in    
    subst rtl
      
  let loc'     guard map rtl = loc_gen' guard map rtl ~def:true  ~use:true
  let loc_def' guard map rtl = loc_gen' guard map rtl ~def:true  ~use:false
  let loc_use' guard map rtl = loc_gen' guard map rtl ~def:false ~use:true
      
  let aloc guard map rtl = 
    let rec exp = function
      | RP.Fetch (l, width)  -> fetch l width
      | RP.App(opr, exprs)   -> RP.App(opr, List.map exp exprs)
      | RP.Const c as e      -> e
    and fetch l   w = if guard l then Dn.exp((map l).fetch w) else RP.Fetch (loc l, w)
    and store l e w =
      if guard l then
        match Dn.rtl ((map l).store (Up.exp e) w) with
          | RP.Rtl [RP.Const (RP.Bool true), eff] -> eff
          | _ -> unimpf "aloc substitution on fancy location"
      else
        RP.Store (loc l, e, w)
    and loc = function
      | RP.Mem (sp,w,e,ass)  -> RP.Mem(sp, w, exp e, ass)
      | RP.Slice (w,i,l)     -> if guard l then unimpf "aloc slice"
        else RP.Slice(w,i,loc l)
      | (RP.Global _ | RP.Var _ | RP.Reg _) as l -> l
    and effect = function      
      | RP.Store (l,e,width) -> store l (exp e) width
      | RP.Kill  (l)         -> if guard l then unimpf "aloc kill " else RP.Kill (loc l)
    and guarded (e,eff)        = (exp e, effect eff)
    and subst (RP.Rtl rtl)     = RP.Rtl (List.map guarded rtl) in    
    subst rtl


  let subst_exp_loc ~eguard ~emap ~lguard ~lmap =
    let rec subst_exp e = if eguard e then emap e else match e with
      | RP.Fetch (l, width)  -> RP.Fetch(subst_loc l, width)
      | RP.App(opr, exprs)   -> RP.App(opr, List.map subst_exp exprs)
      | x                    -> x
    and subst_loc l = if lguard l then lmap l else match l with
      | RP.Mem (sp,w,e,ass)  -> RP.Mem(sp, w, subst_exp e, ass)
      | RP.Slice (w,i,l)     -> RP.Slice(w,i,subst_loc l)
      | x                    -> x                                    
    in
    (subst_exp, subst_loc)
      
  let exp' eguard emap rtl =
    let (exp, loc) = subst_exp_loc ~eguard ~emap ~lguard:falsef ~lmap:id in
    let effect = function
      | RP.Store (l,e,width)     -> RP.Store(loc l, exp e, width)
      | RP.Kill  (l)             -> RP.Kill (loc l)               in
    let guarded (e,eff)            = (exp e, effect eff)            in
    let subst (RP.Rtl rtl)         = RP.Rtl (List.map guarded rtl)  
    in
    subst rtl
      
  let reg_gen' map rtl loc' =
    let map = function RP.Reg r -> RP.Reg(map r) | _ -> assert false in
    let is_register = function RP.Reg _ -> true | _ -> false in
    loc' is_register map rtl

  let reg_to_mem_gen' map rtl loc' =
    let map = function RP.Reg r -> map r | _ -> assert false in
    let is_register = function RP.Reg _ -> true | _ -> false in
    loc' is_register map rtl
      
  let reg' map rtl     = reg_gen' map rtl loc'
  let reg_def' map rtl = reg_gen' map rtl loc_def'
  let reg_use' map rtl = reg_gen' map rtl loc_use'
  let reg_to_mem' map rtl     = reg_to_mem_gen' map rtl loc'

  let exp        ~guard  ~map rtl = DownUp.rtl (exp' guard  map) rtl
  let exp_of_exp ~guard  ~map exp =
    let (subst_exp,_) = subst_exp_loc ~eguard:guard ~emap:map ~lguard:falsef ~lmap:id in
    DownUp.exp subst_exp exp

  let exp_of_loc ~guard  ~map loc =
    let (_,subst_loc) = subst_exp_loc ~eguard:guard ~emap:map ~lguard:falsef ~lmap:id in
    DownUp.loc subst_loc loc

  let loc_of_loc ~guard  ~map loc =
    let (_,subst_loc) = subst_exp_loc ~eguard:falsef ~emap:id ~lguard:guard ~lmap:map in
    DownUp.loc subst_loc loc
      
  let loc        ~guard  ~map rtl = DownUp.rtl (loc' guard  map) rtl 
  let aloc       ~guard  ~map rtl = DownUp.rtl (aloc guard  map) rtl 
  let reg                ~map rtl = DownUp.rtl (reg'        map) rtl 
  let reg_def            ~map rtl = DownUp.rtl (reg_def'    map) rtl 
  let reg_use            ~map rtl = DownUp.rtl (reg_use'    map) rtl 
  let reg_to_mem         ~map rtl = DownUp.rtl (reg_to_mem' map) rtl
  
  module Fetch = 
  struct
    let rec exp ~guard ~fetch = function
      | RP.Fetch (l, width) when guard l -> fetch l width
      | RP.Fetch (l, width) -> RP.Fetch (loc ~guard ~fetch l, width)
      | RP.App(opr, exprs)  -> RP.App(opr, List.map (exp ~guard ~fetch) exprs)
      | RP.Const _ as e     -> e
    and loc ~guard ~fetch l = match l with
      | RP.Mem (sp, w, e, ass) -> RP.Mem(sp, w, exp ~guard ~fetch e, ass)
      | RP.Slice (w,i,l)       -> RP.Slice(w,i,loc ~guard ~fetch l)
      | RP.Reg _ | RP.Var _ | RP.Global _ -> l 
	  
    let rtl ~guard ~fetch rtl =
      let RP.Rtl effs = Dn.rtl rtl in
      let loc = loc ~guard ~fetch in
      let exp = exp ~guard ~fetch in
      let effect = function
        | RP.Store (l,e,width)     -> RP.Store(loc l, exp e, width)
        | RP.Kill  (l)             -> RP.Kill (loc l) in
      let guarded (e,eff)  = (exp e, effect eff) 
      in
      Up.rtl (RP.Rtl (List.map guarded effs))
	
    let exp' = exp
  end
end

module ToString = 
struct
  module R = Rtl.Private
  module A = Cmm_ast
  module T = Types
  let rec const = function
    | R.Bool true     ->  "true"
    | R.Bool false    ->  "false"
    | R.Bits b        ->  Int64.to_string (Bits.S.to_int64 b)
    | R.Link(sym,_,w) -> sym#mangled_text
    | R.Diff(c1,c2)   -> (const c1) ^ "-" ^ (const c2)
    | R.Late(name,w)  -> "<" ^ name ^ ">"
  let opr op w = match op, w with
    | ("sx"  | "zx"  | "lobits" |
	  "i2f" | "f2i" |"f2f" ), [w1;w2]  -> "%" ^ op ^ string_of_int w2
    | _, _                              -> "%" ^ op
	
  let join = String.concat
    
  let rec expr' = function
    | R.Const c         -> const c
    | R.Fetch(loc,_)    -> location' loc
    | R.App((op,ww),ee) -> match ee with
        | [l; r] when op =$= "add" || op =$= "sub" ->
            let op = if op =$= "add" then " + " else " - " in
            brexpr' l ^ op ^ brexpr' r
        | _ -> opr op ww ^ "(" ^ join ", " (List.map expr' ee) ^ ")"
  and brexpr' e = match e with (* bracketed expression *)
    | R.App ((op,ww), _) when op =$= "add" || op =$= "sub" -> "(" ^ expr' e ^ ")"
    | _ -> expr' e
  and agg = function
    | Rtl.BigEndian    -> "B"
    | Rtl.LittleEndian -> "L"
    | Rtl.Identity     -> "I"
  and location' = function 
    | R.Mem ((sp,a,ms),c,e,ass) ->
        let space = if sp =<= 'm' then "bits" ^ string_of_int (Cell.to_width ms c)
        else "$" ^ Char.escaped sp in
        sprintf "%s%s[%s]" space (agg a) (expr' e)
    | R.Reg r -> reg r
    | R.Var    (name,_,width)       -> name
    | R.Global (name,_,width)       -> name
    | R.Slice (w,i,loc)          ->
	
        location' loc ^ "@[" ^ string_of_int i ^ ".." ^ string_of_int (i+w-1) ^ "]"
	  
  and reg ((sp,a,_),i,R.C n) =
    if n = 1 then
      sprintf "$%s[%d]" (Char.escaped sp) i
    else
      sprintf "$%s[%d:%d%s]" (Char.escaped sp) i n (agg a)
	
        
  let (<<) f g  = fun x -> f (g x)
  let expr       = expr'     << Rtl.Dn.exp
  let loc = location' << Rtl.Dn.loc
  let rec effect = function
    | R.Kill   loc       -> "kill " ^ location' loc
    | R.Store  (loc,e,w) -> location' loc ^ " := " ^ expr' e
	
  let guard = function
    | R.Const (R.Bool true) -> ""
    | g -> expr' g ^ " --> " 
	
  let guarded (g, eff) = guard g ^ effect eff
    
  let rtl' (R.Rtl gg) = 
    match gg with
      | [] -> "skip"
      | _ :: _ -> join " | " (List.map guarded gg)
  let rtl = rtl' << Rtl.Dn.rtl
  let exp = expr
end

module ToAST = 
struct
  module R = Rtl.Private
  module A = Cmm_ast
  module T = Types
  type verbosity = Low | High  (* default is High --- more accurate *)
  let the_verbosity = ref High
  let verbosity v =
    let v' = !the_verbosity in
    the_verbosity := v;
    v'
      
  let bits n             = A.BitsTy n
  let rec const = function
    | R.Bool(true)   ->  A.PrimOp ("true", [])
    | R.Bool(false)  ->  A.PrimOp ("false", [])
    | R.Bits b       ->
        let w = match !the_verbosity with
          | High -> Some (bits (Bits.width b))
          | Low  -> None in
        (try A.Sint (string_of_int (Bits.S.to_int b), w)
        with Bits.Overflow -> A.Uint (Bits.to_string b, w))
    | R.Link(sym,_,w) -> A.Fetch(A.Name(None, sym#mangled_text,None))
    | R.Diff(c1,c2)   -> A.PrimOp("-", [(None,const c1,None); (None,const c2,None)])
    | R.Late(name,w)  -> A.Fetch(A.Name(None, name, None)) (*XXX ok? *)
	
  let opr op w =
    sprintf "%s[%s]" op (String.concat "," (List.map string_of_int w))

  let rec expr' = function
    | R.Const c         -> const c
    | R.Fetch(loc,_)    -> rvalue loc
    | R.App((op,ww),ee) -> 
	let actual e = (None, expr' e, None) 
	in
	match ee with
          | [l; r] when op =$= "add" || op =$= "sub" ->
              let op = if op =$= "add" then "+" else "-" in
              A.BinOp(expr' l, op, expr' r)
          | _ -> A.PrimOp(opr op ww, List.map actual ee)
	      
  and rvalue = function
    | R.Slice (w, i, loc) ->
        let e = rvalue loc in
        let e = if i = 0 then e
        else A.PrimOp("shrl", [None, e, None;
        None, A.Sint (string_of_int i, None), None]) in
        let e = A.PrimOp("lobits" ^ string_of_int w, [None, e, None]) in
        e
    | loc -> A.Fetch(location' loc) 

  and location' =
    let name n = A.Name (None, n, None) in
    function
      | R.Mem ((sp,agg,ms),c,e,ass) ->
          let width = Cell.to_width ms c in
          (match !the_verbosity with
            | High ->
                let a       =  match agg with
                  | Rtl.Identity     -> 'I'
                  | Rtl.BigEndian    -> 'B'
                  | Rtl.LittleEndian -> 'L' in
                let space   =  sprintf "$%c(%d%c)" sp width a in
                let aligned = if ass = 1 then None else Some ass in 
                A.Mem(A.TypeSynonym space,expr' e,aligned,
                if sp =<= 'm' then [] else ["'" ^ Char.escaped sp ^ "'"])
            | Low ->
                A.Mem(A.BitsTy width,expr' e, (if ass = 1 then None else Some ass),
                if sp =<= 'm' then [] else ["'" ^ Char.escaped sp ^ "'"]))
      | R.Reg((sp,_,ms),i,c) -> 
          (match !the_verbosity with
            | High ->
                let space   =  sprintf "$%c(%d)" sp (Cell.to_width ms c) in   
                let s       = string_of_int i in
                A.Mem(A.TypeSynonym space,A.Sint(s,None), None, [])
            | Low -> name (sprintf "$%c%d" sp i))
      | R.Var    (n,_,width)   -> name n
      | R.Global (n,_,width)   -> name n
      | R.Slice (w,i,loc)      ->
          name (sprintf "Slice(%d, %d, %s)" w i (ToString.location' loc))

  let (<<) f g  = fun x -> f (g x)
  let expr       = expr'     << Rtl.Dn.exp
  let location   = location' << Rtl.Dn.loc

  let rec effect guard = function
    | R.Kill   loc       -> 
        let width = Width.loc  (Rtl.Up.loc loc)       in
        let undef = R.App(("undef",[width]),[]) in
        effect guard (R.Store(loc, undef, width))
    | R.Store  (loc,e,w) ->
        (location' loc,(guard, expr' e))

  let guard = function
    | R.Const (R.Bool true) -> None
    | g -> Some (expr' g)  

  let guarded (g, eff) = effect (guard g) eff

  let rtl' (R.Rtl gg) = 
    match gg with
      | [] -> A.EmptyStmt
      | _ :: _ ->
          let pairs    = List.map (guarded ) gg in
          let lhs, rhs = List.split pairs             
	  in
          A.AssignStmt(lhs,rhs)
	    
  let rtl = rtl' << Rtl.Dn.rtl
end
  
module ToUnreadableString = 
struct
  module P  = Pp
  module R  = Rtl
  module RP = Rtl.Private 

  let (^^)   = P.(^^)
  let (^/)   = P.(^/)

  let indent x = P.nest 4 (P.break ^^ x)
  let int i    = P.text (string_of_int i)
  let str s    = P.text s
  let char c   = P.text (sprintf "'%c'" c)

  let tuple docs = 
    P.agrp (P.text "(" ^^ P.list (P.text "," ^^ P.break) id docs ^^ P.text ")")
        
  let apply c args = P.agrp(str c ^^ indent(tuple args))

  let aggregation' = function
    | R.BigEndian       -> P.text "BE"
    | R.LittleEndian    -> P.text "LE"
    | R.Identity        -> P.text "ID"

  let agg = function
    | R.BigEndian       -> "big"
    | R.LittleEndian    -> "little"
    | R.Identity        -> "none"

  let opr (name, ws) =
    tuple [str name; tuple (List.map int ws)]

  let rec const = function
    | RP.Bool(b)        -> apply "Bool"  [str (if b then "true" else "false")]
    | RP.Bits(b)        -> apply "Bits"  [ str (Int64.to_string (Bits.U.to_int64 b))
                                         ; int (Bits.width b)
      ]
    | RP.Link(r,k,w)    -> apply "Link"  [str r#mangled_text; P.text (kind k); int w]
    | RP.Diff(c1,c2)    -> apply "Diff"  [const c1; const c2]
    | RP.Late(s,w)      -> apply "Late"  [str s; int w]
	
  and kind = function RP.Code -> "Code" | RP.Data -> "Data" | RP.Imported -> "Imported"

  let rec exp = function
    | RP.Const(k)       -> apply "Const" [const k]
    | RP.Fetch(l,w)     -> apply "Fetch" [loc l; int w]
    | RP.App(o,es)      -> apply "App"   [opr o; tuple (List.map exp es)] 
        
  and loc = function
    | RP.Mem((sp, agg, ms), c, e, ass) -> 
        apply "Mem" [char sp; aggregation' agg; int (Cell.to_width ms c); exp e]
    | RP.Reg((sp,agg,ms),i,R.C c) ->
        apply "Reg" [char sp; int i; str (sprintf "C %d" c)]
    | RP.Var(name, index, w) ->
        apply "Var" [str name; int index; int w]
    | RP.Global(name, index, w) ->
        apply "Global" [str name; int index; int w]
    | RP.Slice(w, i, l) ->
        apply "Slice" [int w; int i; loc l]
            

  let effect = function
    | RP.Store(l,e,w) -> apply "Store" [loc l; exp e; int w]
    | RP.Kill(l)      -> apply "Kill"  [loc l]  

  let guarded (g, e) =
    tuple [exp g; effect e]

  let rtl' (RP.Rtl x) =
    apply "Rtl" (List.map guarded x)
        
  let rtl r =
    let pp = rtl' (R.Dn.rtl r) in
    Pp.ppToString 66 (* line width *) pp

  let exp e =
    let pp = exp (R.Dn.exp e) in
    Pp.ppToString 66 (* line width *) pp

  let reg' ((sp,agg,ms),i,R.C c) =
    tuple [char sp; int i; str (sprintf "C %d" c)]

  let reg r =
    Pp.ppToString 66 (P.agrp (reg' r))


  let regx r = match r with
    | Rg.Reg r -> Pp.ppToString 66 (P.agrp (reg' r))
    | Rg.Slice (w, lsb, r) -> 
        Pp.ppToString 66 (P.agrp (  reg' r ^^ str "@" ^^ int lsb ^^ str ":" ^^ int w))

  let loc l = Pp.ppToString 66 (loc (Dn.loc l))
end

module Time = 
struct
  let compile e = match Dn.exp e with
    | RP.Const(RP.Bits _) -> true
    | RP.Const(RP.Bool _) -> true
    | RP.Const(RP.Late _) -> true
    | _                   -> false
	
  let link e = match Dn.exp e with     
    | RP.Const(RP.Bits _) -> true
    | RP.Const(RP.Bool _) -> true
    | RP.Const(RP.Late _) -> true
    | RP.Const(RP.Link _) -> true       
    | RP.Const(RP.Diff _) -> true
    | RP.App(("add",_), [RP.Const(RP.Link _); RP.Const(RP.Bits _)])
      -> true
    | RP.App(("add",_), [RP.Const(RP.Diff _); RP.Const(RP.Bits _)])
      -> true
    | _                   -> false
end 
  
let rec is_hardware = function
  | RP.Reg r -> true
  | RP.Slice (w, lsb, r) -> is_hardware r
  | _ -> false

let rec to_hardware = function
  | RP.Reg r -> Rg.Reg r
  | RP.Slice (w, lsb, r) ->
      (match to_hardware r with
      | Rg.Reg r -> Rg.Slice (w, lsb, r)
      | Rg.Slice (w', lsb', r) -> Rg.Slice(w', lsb+lsb', r))
  | l -> Printf.kprintf Impossible.impossible
           "to_hardware given non-harware location %s" (ToString.location' l)
	
module RTLType = 
struct
  let singleAssignment rtl = 
    let is_truth = function RP.Const(RP.Bool true) -> true | _ -> false in
    let is_move = function (* effect is an uncond. move *)
      | (g,RP.Store(RP.Reg(dst), RP.Fetch(RP.Reg(src),_),_)) 
          when is_truth g -> Some(dst,src)
      | _                 -> None in
    let is_kill = function (* effect is an uncond. kill *) 
      | (g, RP.Kill _) -> is_truth g
      | _              -> false in 
    match Rtl.Dn.rtl rtl with
      | RP.Rtl([])       -> None
      | RP.Rtl(fst::rst) -> if List.for_all is_kill rst then is_move fst else None
end
  
module Down = Rtl.Dn
  
module MayAlias = 
struct
  let regs ((s,_,_), i, R.C c) ((s',_,_), i', R.C c') =
    s =<= s' && not (i + c <= i' || i' + c' <= i)
    
  let () = Cmm_debug.register "may-alias" "check register may-alias function"
    
  let regs =
    if Cmm_debug.on "may-alias" then
      let regs ((s,_,_), i, R.C c as r) ((s',_,_), i', R.C c' as r') =
        let good = regs r r' in
        let what p = if p then "may" else "may not" in
        ( Printf.eprintf "Comparing $%c[%d:%d] and $%c[%d:%d]: %s alias\n"
          s i c s' i' c' (what good)
        ; good
        ) in
      regs
    else
      regs

  let with_reg r = function
    | RP.Reg r' -> regs r r'
    | _ -> false

  let with_vari vi = function
    | RP.Var (_, vi', _) -> vi = vi'
    | _ -> false

  let with_globali vi = function
    | RP.Global (_, vi', _) -> vi = vi'
    | _ -> false

  type slot = string * int  (* symbol and offset *)

  let stack_slot = function
    | RP.Fetch(RP.Reg(('V',_,_), 0, _), _) -> Some ("", 0)
    | RP.App (("add", [_]), [RP.Fetch(RP.Reg(('V',_,_), 0, _), _); offset]) ->
        (match offset with
          | RP.Const (RP.Bits k') -> Some ("", Bits.S.to_int k')
          | RP.Const (RP.Late (s, _)) -> Some (s, 0)
          | RP.App (("add", [_]), [RP.Const l; RP.Const r]) ->
              (match l, r with
		| RP.Late (s, _), RP.Bits k -> Some (s, Bits.S.to_int k)
		| RP.Bits k, RP.Late (s, _) -> Some (s, Bits.S.to_int k)
		| _ -> None)
          | _ -> None)
    | _ -> None

  let rec is_initialized_data = function
    | RP.Const (RP.Link(_, _, _)) -> true
    | RP.Const (RP.Diff(_, _)) -> true
    | RP.App ((("add"|"sub"), [_]), [RP.Const _ as l; RP.Const _ as r]) ->
        is_initialized_data l || is_initialized_data r
    | _ -> false

  let with_mem (s,_,_) (R.C c) e = function  (* different stack slots don't alias *)
    | RP.Mem ((s',_,_), R.C c', e', _) ->
        s =<= s' && (match stack_slot e, stack_slot e' with
          | None,   Some _ -> not (is_initialized_data e)
          | Some _, None   -> not (is_initialized_data e')
          | None,   None   -> true
          | Some (sym, n), Some(sym', n') ->
	      (* Printf.eprintf "--> comparing stack slots %s+%d and %s+%d\n" sym n sym' n';
	      *)
              sym =$= sym' && not (n + c <= n' || n' + c' <= n))
    | _ -> false

  let unslice f l =
    let rec un = function
      | RP.Slice (_, _, l) -> un l
      | l -> l
    in f (un l)

  let rec locs' l = match l with
    | RP.Reg r            -> unslice (with_reg  r)
    | RP.Var    (_, i, _) -> unslice (with_vari i)
    | RP.Global (_, i, _) -> unslice (with_globali i)
    | RP.Mem (s, w, e, _) -> unslice (with_mem s w e)
    | RP.Slice (_, _, l)  -> locs' l
	
  and locs l =
    let alias = locs' (Down.loc l) in
    fun l' -> alias (Down.loc l')
      
  let has_loc f e =
    let rec has es ess = match es with
      | [] -> (match ess with [] -> false | es :: ess -> has es ess)
      | RP.Const _ :: es -> has es ess
      | RP.Fetch (l, w) :: es ->
          f l || (match l with
            | RP.Mem (_, _, e, _) -> has (e :: es) ess
            | _ -> has es ess)
      | RP.App (_, es') :: es -> has es (es' :: ess) in
    has [e] []

  let exp' l =
    let may_alias = has_loc (locs' l) in
    fun e ->
      let answer = may_alias e in
      let module S = ToString in
      let _debug () = 
        Printf.eprintf "*** Expression %s %s alias with location %s\n"
          (S.exp (Rtl.Up.exp e)) (if answer then " may " else " may not ")
          (S.loc (Rtl.Up.loc l)) in
      answer

  let exp l =
    let alias = exp' (Down.loc l) in
    fun e -> alias (Down.exp e)


  let store_uses' l (l', r', _) =
    exp' l r' || match l' with RP.Mem (_, _, e, _) -> exp' l e | _ -> false
end

let add' w =
  let addop = R.opr "add" [w] in
  fun x y -> R.app addop [x; y]

let rec is_sum_of_constants = function
  | RP.Const (RP.Bits _ | RP.Late (_, _)) -> true
  | RP.App   ((("add"), [w]), es)   -> List.for_all is_sum_of_constants es
  | _ -> false
      
let addc w =
  let add' = add' w in
  let addconst x y = match x, y with
    | RP.Bits b, RP.Bits b' -> R.bits (Bits.Ops.add b b') w
    | c, c' -> add' (Up.const c) (Up.const c') in
  fun x y ->
    match Down.exp x with
      | RP.App (("add", [w']), [x1; RP.Const c]) -> add' (Up.exp x1) (addconst c y)
      | RP.App (("add", [w']), [x1; x2]) when is_sum_of_constants x2 ->
          add' (Up.exp x1) (add' (Up.exp x2) (Up.const y))
      | _ -> add' x (Up.const y) 

let addk w =
  let add = addc w in
  fun x k -> if k = 0 then x else add x (RP.Bits (Bits.S.of_int k w))

let add w =
  let add = add' w in
  fun x y -> match (Down.exp y) with
    | RP.Const c -> addc w x c
    | _ -> add x y

	
module Exists = 
struct
  module Loc = 
  struct
    let exp = MayAlias.has_loc
    let rtl p =
      let exp = exp p in
      let effect = function
        | RP.Store (l, r, w) -> exp r || exp (RP.Fetch (l, w))
        | RP.Kill l          -> exp (RP.Fetch (l, 0)) in (* width cheat is OK *)
      let ge (guard, eff) = exp guard || effect eff in
      fun (RP.Rtl ges) -> List.exists ge ges
  end
    
  module Opr = 
  struct
    let rtl p =
      let rec rtl (RP.Rtl gs) = List.exists guarded gs
      and guarded (g, eff) = exp g || effect eff
      and effect = function
        | RP.Store (lhs, rhs, _) -> loc lhs || exp rhs
        | RP.Kill lhs            -> loc lhs
      and loc = function
        | RP.Mem (_, _, addr, _) -> exp addr
        | RP.Reg r               -> false
        | RP.Slice (_, _, l)     -> loc l
        | RP.Var _               -> false
        | RP.Global _            -> false
      and exp = function         
        | RP.Const _             -> false
        | RP.Fetch (l, w)        -> loc l
        | RP.App (opr, es)       -> p (Rtl.Up.opr opr) || List.exists exp es in
      fun r -> rtl (Rtl.Dn.rtl r)
  end

  module Const = 
  struct
    let rtl const_ftn =
      let rec rtl (RP.Rtl gs) = List.exists guarded gs
      and guarded (e, eff) = exp e || effect eff
      and effect eff = match eff with
        | RP.Store(l, e, _) -> loc l || exp e
        | RP.Kill l -> loc l
      and loc l = match l with
        | RP.Mem(_,_,e,_)   -> exp e
        | RP.Slice(_,_,l')  -> loc l'
        | RP.Reg _ | RP.Var _ | RP.Global _ -> false
      and exp e = match e with
        | RP.Const c     -> const c
        | RP.Fetch(l, _) -> loc l
        | RP.App(_, es)  -> List.exists exp es
      and const c = const_ftn c in
      fun r -> rtl (R.Dn.rtl r)
  end
end 
  
module Find = 
struct
  module Loc = 
  struct
    let exp f e =
      let rec has es ess = 
	match es with
          | [] -> (match ess with [] -> None | es :: ess -> has es ess)
          | RP.Const _ :: es -> has es ess
          | RP.Fetch (l, w) :: es ->
              if f l then Some l
              else ( match l with
		| RP.Mem (_, _, e, _) -> has (e :: es) ess
		| _ -> has es ess)
          | RP.App (_, es') :: es -> has es (es' :: ess) 
      in
      has [e] []
  end
end 

module Fold = 
struct
  module RegX = 
  struct
    let loc f l z =
      let rec exp z = function
        | RP.Const _ -> z
        | RP.Fetch (l, _) -> loc z l
        | RP.App (_, es') -> List.fold_left exp z es'
      and loc z = function
        | RP.Reg   r                -> f (Rg.Reg r) z
        | RP.Slice (w, i, RP.Reg r) -> f (Rg.Slice (w, i, r)) z
        | RP.Slice (_, _, l)        -> loc z l
        | _ -> z in
      loc z (Rtl.Dn.loc l)
  end

  module LocFetched = 
  struct
    let rtl f rtl z = 
      let rec exp z e =
        let rec fold z es ess = match es with
          | [] -> (match ess with [] -> z | es :: ess -> fold z es ess)
          | RP.Const _ :: es -> fold z es ess
          | RP.Fetch (l, w) :: es -> let z = f l z in fold (loc z l) es ess
          | RP.App (_, es') :: es -> fold z es (es' :: ess) in
        fold z [e] [] 
      and loc z = function
        | RP.Mem (_, _, e, _) -> exp z e
        | _ -> z in
      let effect z = function
        | RP.Kill l -> loc z l
        | RP.Store (l, r, w) -> exp (loc z l) r in
      let guarded z (g, e) = effect (exp z g) e in
      let RP.Rtl effs = rtl in
      List.fold_left guarded z effs
  end
end 

module Compare = 
struct
  let less    = -1
  let equal   = 0
  let greater = 1

  let space (s, _, _) (s', _, _) = comparec s s'

  let rec const c c' = match c, c' with
    | (RP.Bool l, RP.Bool r) -> compareb l r
    | (RP.Bits l, RP.Bits r) -> Bits.compare l r
    | (RP.Link (l1, l2, l3), RP.Link (r1, r2, r3)) ->  
	(match Pervasives.compare l1 r1 with
	  | 0 ->
              (match Pervasives.compare l2 r2 with
		| 0 -> compare l3 r3
		| diff -> diff)
	  | diff -> diff)
    | (RP.Diff (l1, l2), RP.Diff (r1, r2)) ->  
	(match const l1 r1 with
	  | 0 -> const l2 r2
	  | diff    -> diff)
    | (RP.Late (l1, l2), RP.Late (r1, r2)) ->  
	(match compares l1 r1 with
	  | 0   -> comparei l2 r2
	  | diff    -> diff)
    | (RP.Bool _, RP.Bits _) -> less
    | (RP.Bool _, RP.Link _) -> less
    | (RP.Bool _, RP.Diff _) -> less
    | (RP.Bool _, RP.Late _) -> less
    | (RP.Bits _, RP.Bool _) -> greater
    | (RP.Bits _, RP.Link _) -> less
    | (RP.Bits _, RP.Diff _) -> less
    | (RP.Bits _, RP.Late _) -> less
    | (RP.Link _, RP.Bool _) -> greater
    | (RP.Link _, RP.Bits _) -> greater
    | (RP.Link _, RP.Diff _) -> less
    | (RP.Link _, RP.Late _) -> less
    | (RP.Diff _, RP.Bool _) -> greater
    | (RP.Diff _, RP.Bits _) -> greater
    | (RP.Diff _, RP.Link _) -> greater
    | (RP.Diff _, RP.Late _) -> less
    | (RP.Late _, RP.Bool _) -> greater
    | (RP.Late _, RP.Bits _) -> greater
    | (RP.Late _, RP.Link _) -> greater
    | (RP.Late _, RP.Diff _) -> greater
	
  and symkind k k' = match k, k' with
    | (RP.Code, RP.Code) -> equal
    | (RP.Data, RP.Data) -> equal
    | (RP.Imported, RP.Imported) -> equal
    | (RP.Code, RP.Data) -> less
    | (RP.Code, RP.Imported) -> less
    | (RP.Data, RP.Code) -> greater
    | (RP.Data, RP.Imported) -> less
    | (RP.Imported, RP.Code) -> greater
    | (RP.Imported, RP.Data) -> greater

  let rec exp e e' = match e, e' with
    | (RP.Const l, RP.Const r) -> const l r
    | (RP.Fetch (l1, l2), RP.Fetch (r1, r2)) ->  
        (match loc l1 r1 with
          | 0   -> compare l2 r2
          | diff    -> diff)
    | (RP.App (l1, l2), RP.App (r1, r2)) ->  
        (match Pervasives.compare l1 r1 with
          | 0   -> Auxfuns.compare_list exp l2 r2
          | diff    -> diff)
    | (RP.Const _, RP.Fetch _) -> less
    | (RP.Const _, RP.App _) -> less
    | (RP.Fetch _, RP.Const _) -> greater
    | (RP.Fetch _, RP.App _) -> less
    | (RP.App _, RP.Const _) -> greater
    | (RP.App _, RP.Fetch _) -> greater
	
  and count (RP.C l) (RP.C r) = compare l r
    
  and loc l l' = match l, l' with
    | (RP.Mem (l1, l2, l3, l4), RP.Mem (r1, r2, r3, r4)) ->
        (match space l1 r1 with
          | 0   ->  
              (match count l2 r2 with
		| 0   ->  
                    (match exp l3 r3 with
                      | 0   -> Pervasives.compare l4 r4
                      | diff    -> diff)
		| diff    -> diff)
          | diff    -> diff)
    | (RP.Reg l, RP.Reg r) -> Register.compare l r
    | (RP.Var (l1, l2, l3), RP.Var (r1, r2, r3)) -> comparei l2 r2
    | (RP.Global (l1, l2, l3), RP.Global (r1, r2, r3)) ->  comparei l2 r2
    | (RP.Slice (l1, l2, l3), RP.Slice (r1, r2, r3)) ->  
        (match comparei l2 r2 with
          | 0   ->  
              (match comparei l1 r1 with
		| 0   -> loc l3 r3
		| diff    -> diff)
          | diff    -> diff)
    | (RP.Mem _, RP.Reg _) -> less
    | (RP.Mem _, RP.Var _) -> less
    | (RP.Mem _, RP.Global _) -> less
    | (RP.Mem _, RP.Slice _) -> less
    | (RP.Reg _, RP.Mem _) -> greater
    | (RP.Reg _, RP.Var _) -> less
    | (RP.Reg _, RP.Global _) -> less
    | (RP.Reg _, RP.Slice _) -> less
    | (RP.Var _, RP.Mem _) -> greater
    | (RP.Var _, RP.Reg _) -> greater
    | (RP.Var _, RP.Global _) -> less
    | (RP.Var _, RP.Slice _) -> less
    | (RP.Global _, RP.Mem _) -> greater
    | (RP.Global _, RP.Reg _) -> greater
    | (RP.Global _, RP.Var _) -> greater
    | (RP.Global _, RP.Slice _) -> less
    | (RP.Slice _, RP.Mem _) -> greater
    | (RP.Slice _, RP.Reg _) -> greater
    | (RP.Slice _, RP.Var _) -> greater
    | (RP.Slice _, RP.Global _) -> greater

  let effect e e' = match e, e' with
    | (RP.Store (l1, l2, l3), RP.Store (r1, r2, r3)) ->  
	(match loc l1 r1 with
	  | 0   ->  
              (match exp l2 r2 with
		| 0   -> compare l3 r3
		| diff    -> diff)
	  | diff    -> diff)
    | (RP.Kill l, RP.Kill r) -> loc l r
    | (RP.Store _, RP.Kill _) -> -1
    | (RP.Kill _, RP.Store _) -> 1
  let guarded (l1, l2) (r1, r2) =
    (match exp l1 r1 with
      | 0   -> effect l2 r2
      | diff    -> diff)
  let rtl (RP.Rtl l) (RP.Rtl r) = Auxfuns.compare_list guarded l r
end
  
module Eq = 
struct
  let space  ((s:char), _, _)  ((s':char), _, _) = s =<= s'
  let const c c' = Compare.const c c' = 0
  let loc   l l' = Compare.loc   l l' = 0
  let exp   e e' = Compare.exp   e e' = 0
  let rtl   r r' = Compare.rtl   r r' = 0
end
  
let reloc addr w =
  let const b = R.bits b w in
  let sym (s, mk) = mk s w in
  let infix op a b = R.app (R.opr op [w]) [a; b] in
  let add, sub = infix "add", infix "sub" in
  Reloc.fold ~const ~sym ~add ~sub addr
