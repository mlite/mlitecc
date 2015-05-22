module A = Cmm_ast
type ty  = Cmm_ast.ty
type exp = Cmm_ast.expr
type loc = Cmm_ast.name_or_mem

type 'a marked = Cmm_ast.region * 'a

type name = string
type kind = string
type convention = string
type aligned    = int
type cformal  = Cmm_ast.region * kind * name * aligned option
type actual   = kind * exp * aligned option
type flow     = Cmm_ast.flow list
type alias    = Cmm_ast.mem  list
type range    = Cmm_ast.range
type procname = string
type label    = string
type stmt =
  | StmtAt of stmt * Cmm_ast.region
  | If     of exp * stmt list * stmt list
  | Switch of range option * exp * (range list * stmt list) list
  | Label  of label
  | Cont   of name * convention * cformal list
  | Span   of exp * exp * stmt list
  | Assign of loc list * Cmm_ast.guarded list
  | Call   of loc list * convention * exp  * actual list * procname list * flow * alias
  | Prim   of loc list * convention * name * actual list * flow
  | Goto   of exp * label list
  | Jump   of convention * exp * actual list * procname list
  | Cut    of convention * exp * actual list * flow 
  | Return of convention * (exp * exp) option * actual list
  | Limitcheck of convention * exp * (exp * name) option (* (cookie,(failk,recname)) *)
      
type typedefn  = ty * name list
    
type constdefn = ty option * name * exp
    
type compile_time_defns = 
    { types     : typedefn  marked list
    ; constants : constdefn marked list
    }
      
type proc = 
    { region        : Cmm_ast.region
    ; cc            : convention
    ; name          : name
    ; formals       : (kind * Cmm_ast.variance * ty * name * aligned option) marked list
    ; locals        : Cmm_ast.register marked list
    ; pdecls        : compile_time_defns
    ; continuations : (name * convention * cformal list) marked list
    ; labels        : name marked list  (* code labels *)
    ; stackdata     : datum marked list
    ; code          : stmt list
    }
      
and  datum =
  | Datalabel  of name
  | Align      of int
  | ReserveMem of ty * Cmm_ast.memsize * Cmm_ast.init option (*init always none on stackdata*)
  | Procedure  of proc                               (* never on stackdata *)
  | SSpan      of exp * exp * datum marked list      (* never on stackdata *)
      
type section = name * datum marked list
    
type t = 
    { target   : Cmm_ast.arch marked list
    ; imports  : (Cmm_ast.region * Cmm_ast.ty option * Cmm_ast.import list) list
    ; exports  : (Cmm_ast.region * Cmm_ast.ty option * Cmm_ast.export list) list
    ; globals  : Cmm_ast.register marked list
    ; code_labels : name marked list list
    ; udecls   : compile_time_defns
    ; sections : section list
    }
      
let default_proc_section = "text"
  
let id ss = ss
  
let null = function [] -> true | _ :: _ -> false
  
let rflatten xs =
  let add (r, xs) tail = 
    List.fold_right (fun a t -> (r, a) :: t) xs tail 
  in
  List.fold_right add xs []
    
    
    
let cformal (r, kind, name, aligned) = 
  (r, Auxfuns.Option.get "" kind, name, aligned)
    
let formal (r, (kind, variance, ty, name, aligned)) =
  (r, (Auxfuns.Option.get "" kind, variance, ty, name, aligned))
    
let actual (kind, name, aligned) = (Auxfuns.Option.get "" kind, name, aligned)
  
let convention = Auxfuns.Option.get "C--"
  
let rec add_datum r d ds = match d with
  | A.DatumAt (d, r) -> add_datum r d ds
  | A.Label l -> (r, Datalabel l) :: ds
  | A.Align n -> (r, Align n) :: ds
  | A.MemDecl (t, s, init) -> (r, ReserveMem (t, s, init)) :: ds
      
let fdata r = List.fold_right (add_datum r) 
  
type ('a) accumulation_disaster =
    { imps : (A.region * A.ty option * A.import list) list
    ; exps : (A.region * A.ty option * A.export list) list
    ; lbls : 'a
    ; ks : (Cmm_ast.region * (A.name * convention * (A.region * A.hint * A.name * aligned option) list)) list
    ; consts : (A.region * (A.ty option * A.name * A.expr)) list
    ; tys : (A.region * (A.ty * A.name list)) list
    ; regs : (A.region * A.register list) list
    ; archs : (A.region * A.arch list) list
    ; data : ((A.region * datum) list) 
    }
    
let rec decl r accums d k = match d with
  | A.DeclAt(x,r)   -> decl r accums x k
  | A.Typedef d     -> k {accums with tys = ((r,d) :: accums.tys)}
  | A.Import (t,ii) -> k {accums with imps = ((r,t,ii)::accums.imps)}
  | A.Export (t,ee) -> k {accums with exps = ((r,t,ee)::accums.exps)}
  | A.Const d       -> k {accums with consts = ((r,d) :: accums.consts)}
  | A.Registers rs  -> k {accums with regs = ((r, rs) :: accums.regs)}
  | A.Target    t   -> k {accums with archs = ((r, t) :: accums.archs)}
  | A.Pragma        -> k accums
      
let rec kmap f cons r accums xs k = match xs with
  | []      -> k [] accums
  | x :: xs ->
      kmap f cons r accums xs
        (fun xs accums -> f r accums x (fun x accums -> k (cons x xs) accums))
	
let kmap_none f r accums xs k =
  let xk f r accums x k = f r accums x (k []) 
  in
  kmap (xk f) (fun _ _ -> []) r accums xs (fun _ -> k)
    
let rec body r accums b k = match b with
  | A.BodyAt(b,r) -> body r accums b k
  | A.DeclBody d  -> decl r accums d (k id)
  | A.StmtBody s  -> 
      (let rec stmt r s k = 
	let cons s ss = StmtAt (s, r) :: ss in
	let atomic s = k (cons s) accums in
        match s with
	  | A.AsmStmt _ -> assert false
	  | A.StmtAt(s,r) -> stmt r s k
          | A.IfStmt (c,b1,b2)   ->
	      bodies r accums b2
                (fun ss2 accums ->
                  bodies r accums b1 (fun ss1 accums -> k (cons(If(c,ss1,ss2))) accums))
          | A.SwitchStmt (range,e,aa) ->
	      arms r accums aa
                (fun aa accums ->
                  let stmt = cons (Switch (range, e, aa)) in
                  k stmt accums)
          | A.LabelStmt l ->
	      k (cons (Label l)) {accums with lbls = ((r, l) :: accums.lbls)}
          | A.ContStmt (n, formals) ->
	      let formals = List.map cformal formals in
	      let cc      = convention None in
	      let stmt    = cons (Cont (n, cc, formals)) in
	      k stmt {accums with ks = ((r, (n,cc,formals)) :: accums.ks)}
          | A.SpanStmt(key,v,bs)   -> 
	      bodies r accums bs (fun ss accums -> k (cons (Span (key,v,ss))) accums)
          | A.AssignStmt (ls, rs) ->
	      atomic (Assign (ls, rs))
          | A.CallStmt (ls, cc, p, a's, tgts, flows) ->
	      let add a (flows, mems) = match a with
                | A.Flow f -> (f :: flows, mems)
                | A.Alias m -> (flows, m :: mems) in
	      let flows, mems = List.fold_right add flows ([], []) in
	      atomic (Call (ls, convention cc, p, List.map actual a's, tgts, flows, mems))
          | A.PrimStmt (ls, cc, p, a's, flows) ->
	      atomic (Prim (ls, convention cc, p, List.map actual a's, flows))
          | A.GotoStmt (e, tgts) ->
	      atomic (Goto (e, tgts))
          | A.JumpStmt (cc, p, a's, tgts) ->
	      atomic (Jump (convention cc, p, List.map actual a's, tgts))
          | A.CutStmt (e, a's, flows) ->
	      atomic (Cut (convention None, e, List.map actual a's, flows))
          | A.ReturnStmt (cc, alt, a's) ->
	      atomic (Return (convention cc, alt, List.map actual a's))
          | A.EmptyStmt
          | A.CommentStmt _   -> k id accums
          | A.LimitcheckStmt (cookie, cont) -> 
	      let cc   = convention None in
	      match cont with
                | None -> atomic (Limitcheck (cc, cookie, None))
                | Some cont ->
                    (* construct continuation for recovery *)
                    let formals = [] in
                    let name = Idgen.label "overflow-recovery continuation" in
                    let stmt = cons (Limitcheck (cc, cookie, Some (cont, name))) in
                    k stmt {accums with ks = ((r, (name,cc,formals)) :: accums.ks)} in
      stmt r s k)
  | A.DataBody ds -> k id {accums with data = (fdata r ds accums.data)}
      
and bodies r = kmap body (fun add ss -> add ss) r
  
and arm r accums a k = match a with
  | A.ArmAt (a,r) -> arm r accums a k
  | A.Case (ranges,bs) -> 
      bodies r accums bs (fun stmts accums -> k ((ranges, stmts)) accums)
	
and arms r = kmap arm (fun x y -> x::y) r
  
  
let rec section r accums s k = match s with
  | A.SectionAt (s,r) ->
      section r accums s k
	
  | A.Decl d ->
      decl r accums d k
	
  | A.SSpan(key, v, ss) ->
      sections r {accums with data = []} ss
	(fun saccums ->
          let data' = (r, SSpan(key,v, saccums.data)) :: accums.data in
          k {saccums with data = data'})
	
  | A.Datum d ->
      k {accums with data = (add_datum r d accums.data)}
	
  | A.Procedure(cc,p,fs,bs,r) ->
      bodies r { imps = accums.imps ; exps = accums.exps
	       ; lbls = [] ; ks = [] ; consts = [] ; tys = []
	       ; regs  = [] ; archs = accums.archs ; data = [] }
        bs
	(fun ss paccums ->
	  let cdecls = { constants = paccums.consts; types = paccums.tys } 
	  in
	  let p = 
	    { cc = convention cc
	    ; name = p
	    ; formals = List.map formal fs
	    ; code = ss
	    ; continuations = paccums.ks
	    ; labels = paccums.lbls
	    ; region = r
	    ; locals = rflatten paccums.regs
	    ; pdecls = cdecls
	    ; stackdata = paccums.data; 
	    } 
	  in
	  k { accums with imps  = paccums.imps
	    ; exps  = paccums.exps
	    ; lbls  = paccums.lbls :: accums.lbls
	    ; archs = paccums.archs
	    ; data  = (r,Procedure p)::accums.data
	  })
	
and sections r = kmap_none section r

let rec toplevel r accums t k =
  let cons s ss = (s, r) :: ss in
  match t with
    | A.ToplevelAt(t,r) -> toplevel r accums t k
	
    | A.Section(name, ss) ->
	sections r {accums with data = []} ss
          (fun saccums -> k (cons (name, saccums.data))
            {saccums with data = accums.data})
	  
    | A.TopDecl d -> decl r accums d (k id)
	
    | A.TopProcedure p ->
	let t = A.Section(default_proc_section, [A.Procedure p]) in
	toplevel r accums t k
	  
let program ts =
  let checknull what l =
    if not (null l) then Impossible.impossible ("some toplevel " ^ what) 
  in
  kmap toplevel (fun add t -> add t)
    Srcmap.null
    { imps = [] ; exps = [] 
    ; lbls = [] ; ks = [] 
    ; consts = [] ; tys = [] 
    ; regs = [] ; archs = [] 
    ; data = [] 
    }
    ts
    (fun ss saccums ->
      let _ = checknull "data" saccums.data in
      let _ = checknull "continuations" saccums.ks in
      { target = rflatten saccums.archs
      ; imports = saccums.imps; exports = saccums.exps
      ; code_labels = saccums.lbls
      ; globals = rflatten saccums.regs
      ; sections = List.map fst ss
      ; udecls = { types = saccums.tys; constants = saccums.consts }
      })
