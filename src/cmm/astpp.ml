include Common
module A = Cmm_ast
module P = Pp
module E = Error

let (^^)   = P.(^^)
let (^/)   = P.(^/)
let (~~) x = x
let nonempty = function [] -> false | _ :: _ -> true
let nest       = P.nest 4 

let commablock f xs =
  nest begin
    ~~ P.list (P.text "," ^^ P.break) f xs
  end
    
let angrp x =
  P.agrp begin 
    ~~ nest begin
      ~~ x
    end
  end

let fngrp x =
  P.fgrp begin 
    ~~ nest begin
      ~~ x
    end
  end
    
let unzip = List.split
let zip   = List.combine


let id i        = P.text i
let int n       = P.text (string_of_int n)
let semi        = P.text ";"
let comment x   = P.vgrp (P.text "// " ^^ P.text x)
let str s       = P.text ("\"" ^ String.escaped s ^ "\"")
let char i      = P.text ("'"  ^ Char.escaped(Char.chr i) ^ "'")

let rec ty = function
  | A.TyAt(x,_)       -> ty x
  | A.BitsTy(n)       -> P.text "bits"  ^^ int n
  | A.TypeSynonym(name)   -> P.text name
      
let aligned = function
  | Some i -> P.text " aligned" ^/ int i 
  | None   -> P.empty

let rec lvalue = function
  | A.NameOrMemAt(x,_)  -> lvalue x
  | A.Name(None,x,a) -> P.agrp (id x ^^ aligned a)
  | A.Name(Some h,x,a) -> P.agrp (str h ^/ id x ^^ aligned a)
  | A.Mem(t,e,a,aliasing) -> 
      ~~ ty t 
      ^^ P.text "["
      ^^ expr e 
      ^^ aligned a
      ^^ begin match aliasing with 
        | [] -> P.empty 
        | n :: ns ->
            let head = P.break ^^ P.text "in" ^/ P.text n in
            List.fold_left (fun h n -> h ^^ P.text "," ^/ P.text n) head ns
      end  
      ^^ P.text "]"
	
and glvalue = function
  | x, None   -> lvalue x
  | x, Some e -> P.agrp(lvalue x  ^/ P.text "when" ^/ expr e)
      
and actual = function
  | ( Some hint, e, a) -> P.agrp (str hint ^/ expr e ^^ aligned a)
  | ( None     , e, a) -> P.agrp (expr e ^^ aligned a)
      
and actuals xs = P.agrp (P.text "(" ^^ P.commalist actual xs ^^ P.text ")")

and expr e = 
  let with_ty t p = match t with None -> p | Some t -> p ^^ P.text "::" ^^ ty t in
  match e with
    | A.ExprAt(x,_)       -> expr x
    | A.Sint( i, t)       -> with_ty t (P.text i)
    | A.Uint( i, t)       ->
        with_ty t (if String.get i 0 =<= '0' then P.text i else P.text (i^"U"))
    | A.Float( f, t)      -> with_ty t (P.text f)
    | A.Char( i, t)       -> with_ty t (char i)
    | A.Fetch(v)          -> lvalue v
    | A.BinOp(l,op,r)     -> P.agrp begin
        ~~ P.text "(" ^^ expr l
        ^^ nest begin
          ~~ P.text op
          ^^ (if op =$= "%" then P.break else P.empty)
          ^^ P.breakWith ""
          ^^ expr r
        end
        ^^ P.text ")"
      end
    | A.UnOp(op,e)        -> P.agrp (P.text op ^^ expr e)
    | A.PrimOp(n,xs)      -> P.agrp (P.text "%" ^^ id n ^^ actuals xs)
	
let memsize = function
    | A.NoSize        -> P.empty
    | A.DynSize       -> P.text "[]"
    | A.FixSize(e)    -> P.text "[" ^^ expr e ^^ P.text "]"

let rec init = function
    | A.InitAt(x,_) -> init x
    | A.InitExprs(es) -> 
            P.fgrp begin
            ~~ P.text "{"
            ^/ nest begin
               ~~ P.commalist expr es
               end
            ^/ P.text "}"
            end
    | A.InitStr(s) -> str s
    | A.InitUStr(s) -> 
            P.agrp begin
            ~~ P.text "unicode"
            ^/ P.text "("
            ^/ nest begin
               ~~ str s
               end
            ^/ P.text ")"
            end

let rec datum = function
    | A.DatumAt(x,_)        -> datum x
    | A.Label(n)            -> id n ^^ P.text ":"
    | A.Align(a)            -> P.agrp (P.text "align" ^/ int a ^^ semi)
    | A.MemDecl(t,m,Some i) -> P.agrp (ty t ^^ memsize m ^/ init i ^^ semi)
    | A.MemDecl(t,m,None)   -> P.agrp (ty t ^^ memsize m ^^ semi) 

let formal (_, (h, v, t, n, a)) =
    P.agrp begin
    ~~ (match h with Some hint -> str hint ^^ P.break | None -> P.empty)
    ^^ (if v =*= A.Invariant then P.text "invariant" ^^ P.break else P.empty)
    ^^ ty t
    ^/ id n
    ^^ aligned a
    end

let formals xs = P.agrp (P.text "(" ^^ P.commalist formal xs ^^ P.text ")")

let cformal (_, h, n, a) =
    P.agrp begin
    ~~ (match h with Some hint -> str hint ^^ P.break | None -> P.empty)
    ^^ id n
    ^^ aligned a
    end

let cformals xs = P.agrp (P.text "(" ^^ P.commalist cformal xs ^^ P.text ")")
    

let register is_global (v , hint, t, n, reg) = 
  angrp begin 
    ~~ (if v =*= A.Invariant then P.text "invariant" ^^ P.break else P.empty)
    ^^ (if is_global         then P.text "register"  ^^ P.break else P.empty)
    ^^ (match hint with Some h -> P.break ^^ str h | None -> P.empty)
    ^^ ty t
    ^/ id n
    ^^ (match reg with Some r -> P.break ^^ str r | None -> P.empty)
    ^^ semi
  end 

let altcont (e1,e2) =   
            P.agrp begin 
            ~~ P.text "<" 
            ^^ expr e1 
            ^^ P.text "/" 
            ^/ expr e2
            ^^ P.text ">"
            end

let targets = function
    | []    -> P.empty
    | ts    -> P.agrp (P.text "targets" ^/ nest (P.commalist id ts))
    
let rec flow f =
  let also s ns = P.agrp (P.text "also" ^/ P.text s ^/ P.text "to" 
  ^/ nest (P.commalist id ns)) in
  match f with
    | A.FlowAt(x,_)     -> flow x
    | A.CutsTo(ns)    when nonempty ns -> also "cuts"    ns
    | A.UnwindsTo(ns) when nonempty ns -> also "unwinds" ns
    | A.ReturnsTo(ns) when nonempty ns -> also "returns" ns
    | A.Aborts                         -> P.text "also aborts"
    | _                                -> P.empty

let rec alias f =
  let ann s ns = P.agrp (P.text s ^/ nest (P.commalist id ns)) 
  in
  match f with
    | A.AliasAt(x,_)     -> alias x
    | A.Reads  ns -> ann "reads"  ns
    | A.Writes ns -> ann "writes" ns

let pann = function
  | A.Alias a -> alias a
  | A.Flow f  -> flow f
      
let flows = function
    | []    -> P.empty
    | xs    -> nest (P.agrp (P.list P.break flow xs))

let panns = function
    | []    -> P.empty
    | xs    -> nest (P.agrp (P.list P.break pann xs))

let conv = function
  | Some cc       -> P.agrp (P.text "foreign" ^/ str cc)
  | None          -> P.empty 

    
let opt_ty t = 
  ( match t with 
    | Some t -> P.break ^^ ty t
    | None   -> P.empty
  )
    
let export t ns =
  let export' = function
    | (x, Some y) -> P.agrp (id x ^/ P.text "as" ^/ str y)
    | (x, None  ) -> id x
  in
  P.agrp begin
    ~~ P.agrp begin 
      ~~ P.text "export" 
      ^^ opt_ty t
    end
    ^/ commablock export' ns
    ^^ semi
  end


let architecture = function
  | A.Memsize(i)      -> P.agrp (P.text "memsize" ^/ int i)
  | A.ByteorderBig    -> P.agrp (P.text "byteorder" ^/ P.text "big")
  | A.ByteorderLittle -> P.agrp (P.text "byteorder" ^/ P.text "little")
  | A.WordSize i      -> P.agrp (P.text "wordsize" ^/ int i)
  | A.PointerSize i   -> P.agrp (P.text "pointersize" ^/ int i)
  | A.FloatRepr s     -> P.agrp (P.text "float" ^/ str s)
  | A.Charset s       -> P.agrp (P.text "charset" ^/ str s)
    


let range = function
  | A.Point(e)        -> expr e
  | A.Range(e1,e2)    -> P.agrp(expr e1 ^/ P.text ".." ^/ expr e2)

let rec stmt = function
  | A.AsmStmt _ -> assert false
  | A.StmtAt(x,_)   -> stmt x
      
  | A.IfStmt ( e, ss1, ss2)    ->
      P.agrp begin
        ~~ P.agrp (P.text "if" ^/ expr e)
        ^/ P.block (body false) ss1
        ^^ begin match ss2 with
          | []     -> P.empty
          | ss     -> P.break ^^ P.text "else" ^/ P.block (body false) ss
        end
      end
  | A.LabelStmt(n)            -> 
      P.agrp begin
        ~~ id n ^^ P.text ":"
      end
        
  | A.ContStmt(n,cf)          ->
      P.agrp begin
        ~~ P.text "continuation"
        ^^ angrp begin
          ~~ P.break
          ^^ id n
          ^^ cformals cf
          ^^ P.text ":"
        end
      end

  | A.SpanStmt(e1,e2,ss)      -> 
      P.agrp begin
        ~~ P.agrp (P.text "span" ^/ expr e1 ^/ expr e2)
        ^/ P.block (body false) ss
      end
  | A.AssignStmt(lhs,rhs)          -> 
      let rec combine = function (* error tolerant *)
        | []   , []    -> []
        | x    , []    -> []
        | []   , x     -> []
        | x::xx, y::yy -> (x,y)::combine (xx,yy)
      in
      let guards, rhs = List.split rhs       in
      let lhs         = combine (lhs,guards) in
      P.agrp begin
        ~~ fngrp begin 
          ~~ P.commalist glvalue lhs 
          ^/ P.text "="
        end
        ^^ fngrp begin
          ~~ P.break
          ^^ P.commalist expr rhs
          ^^ semi
        end
      end

  | A.CallStmt(lhs, cc, e, args, ts, fs) -> 
      angrp begin
        ~~ ( if nonempty lhs
        then fngrp begin 
          ~~ P.commalist lvalue lhs 
          ^/ P.text "="
          ^^ P.break
        end
        else P.empty
        )
        ^^ ( match cc with 
          | Some c -> P.agrp (P.text "foreign" ^/ str c ^^ P.break)
          | None   -> P.empty
        ) 
        ^^ angrp begin 
          ~~ expr e
          ^^ fngrp begin 
            ~~ P.break 
            ^^ P.text "(" 
            ^^ P.commalist actual args 
            ^^ P.text ")"
          end
        end
        ^^ (if nonempty ts then P.break ^^ targets ts else P.empty)
        ^^ (if nonempty fs then P.break ^^ panns fs else P.empty)
        ^^ semi
      end
  | A.PrimStmt(lhs, cc, n, args, fs)     -> 
      angrp begin
        ~~ ( if nonempty lhs then 
          ~~ P.commalist lvalue lhs 
          ^/ P.text "="
          ^^ P.break
        else P.empty
        )
        ^^ ( match cc with 
          | Some c -> P.agrp (P.text "foreign" ^/ str c ^^ P.break)
          | None   -> P.empty
        ) 
        ^^ fngrp begin 
          ~~ P.text "%%" 
          ^^ id n
          ^^ P.break 
          ^^ P.text "(" 
          ^^ P.commalist actual args 
          ^^ P.text ")"
        end
        ^^ (if nonempty fs then P.break ^^ flows fs else P.empty)
        ^^ semi
      end
	
  | A.GotoStmt(e,ts)                      -> 
      angrp begin
        ~~ P.text "goto"
        ^/ expr e
        ^^ (if nonempty ts then P.break ^^ targets ts else P.empty)
        ^^ semi
      end
  | A.CutStmt(e, args, fs)                -> 
      angrp begin
        ~~ P.text "cut to"
        ^/ expr e 
        ^^ fngrp begin 
          ~~ P.break 
          ^^ P.text "(" 
          ^^ P.commalist actual args 
          ^^ P.text ")"
        end
        ^^ (if nonempty fs then P.break ^^ flows fs else P.empty)
        ^^ semi
      end
  | A.ReturnStmt(cc, alt, args)           -> 
      angrp begin
        ~~ ( match cc with 
          | Some c -> P.text "foreign" ^/ str c ^^ P.break
          | None   -> P.empty
        )
        ^^ P.text "return"
        ^^ ( match alt with
          | Some a -> P.break ^^ altcont a ^^ P.break
          | None   -> P.empty
        )
        ^^ fngrp (P.break ^^ P.text "(" ^^ P.commalist actual args ^^ P.text ")") 
        ^^ semi 
      end
  | A.JumpStmt(cc,e,args,ts)              -> 
      angrp begin
        ~~ ( match cc with 
          | Some c -> P.agrp (P.text "foreign" ^/ str c) ^^ P.break
          | None   -> P.empty
        )
        ^^ fngrp begin 
          ~~ (P.text "jump" ^/ expr e)
          ^^ ( if nonempty args then 
            ~~ P.break 
            ^^ P.text "(" 
            ^^ P.commalist actual args 
            ^^ P.text ")"
          else 
            P.empty
          )
          ^^ (if nonempty ts then P.break ^^ targets ts else P.empty)
        end
        ^^ semi
      end
  | A.EmptyStmt      -> semi
  | A.CommentStmt(s) -> comment s

  | A.LimitcheckStmt (cookie, cont)    ->
      P.agrp begin
        ~~ P.agrp (P.text "limitcheck" ^/ expr cookie)
        ^/ begin match cont with
          | None   -> P.empty
          | Some e -> P.text "fails to" ^/ expr e
        end
      end

  | A.SwitchStmt (r,e,arms) -> 
      P.agrp begin
        ~~ P.agrp begin 
          ~~ P.text "switch"
          ^^ ( match r with
            | Some r -> P.agrp begin 
                ~~ P.break 
                ^^ P.text "[" 
                ^^ range r 
                ^^ P.text "]"
              end
            | None   -> P.empty
          )
          ^/ expr e
        end
        ^/ P.agrp (P.block arm arms)
      end
        
and body is_global = function
  | A.BodyAt(x, _)    -> body is_global x
  | A.DeclBody(d)     -> decl is_global d
  | A.StmtBody(s)     -> stmt s
  | A.DataBody(dd)    -> 
      P.agrp begin
        ~~ P.text "stackdata"
        ^/ P.block datum dd
      end
	
and proc (cc,n,fs,ss,_) =  
  P.agrp begin 
    ~~ P.agrp begin
      ~~ begin match cc with 
        | Some c -> P.text "foreign" ^/ str c ^^ P.break
        | None   -> P.empty
      end
      ^^ id n
      ^^ formals fs
    end
    ^/ P.text "{"
    ^^ nest begin
      ~~ P.break
      ^^ P.list P.break (body false) ss
    end 
    ^/ P.text "}" 
  end

and decl is_global = function
  | A.DeclAt(x,_) -> decl is_global x
  | A.Import( t, ns) -> 
      let import' = function
        | (Some x, y) -> P.agrp (str x ^/ P.text "as" ^/ P.text y)
        | (None  , y) -> id y
      in
      P.agrp begin
        ~~ P.agrp (P.text "import" ^/ opt_ty t)
        ^/ commablock import' ns
        ^^ semi
      end
  | A.Export( t, ns) -> export t ns
  | A.Const (t,n,e) ->
      P.agrp begin 
        ~~ P.text "const"
        ^^ ( match t with
          | Some t -> P.break ^^ ty t
          | None   -> P.empty
        )
        ^/ id n
        ^/ P.text "="
        ^/ expr e
        ^^ semi
      end
  | A.Registers( rs) ->
      P.vgrp begin
        ~~ P.list P.break (register is_global) rs
      end
  | A.Typedef (t,nn)  -> 
      angrp begin
        ~~ P.text "typedef"
        ^/ ty t
        ^/ P.text "="
        ^/ commablock id nn
        ^^ semi
      end 
	
  | A.Target (arch) -> 
      angrp begin
        ~~ P.text "target"
        ^/ P.list P.break architecture arch
        ^^ semi
      end
  | A.Pragma        -> comment "pragma"

and arm = function
  | A.ArmAt(x,_)          -> arm x
  | A.Case(ranges, stmts) ->
      P.agrp begin
        ~~ P.text "case"
        ^/ P.agrp (P.list (P.text "," ^^ P.break) range ranges)
        ^^ P.text ":"
        ^/ P.block (body false) stmts
      end 

and section  = function (* inside a section *)
  | A.SectionAt(x,_)  -> section x
  | A.Decl(d)         -> decl true d
  | A.Datum( d)       -> datum d
  | A.Procedure(p)    -> proc p
  | A.SSpan( e1, e2, ss) ->  
      P.agrp begin
        ~~ P.agrp (P.text "span" ^/ expr e1 ^/ expr e2)
        ^/ P.block section ss
      end
       
       
let rec toplevel = function
    | A.ToplevelAt(x, _)  -> toplevel x
    | A.Section(name, ss) -> 
            P.agrp begin 
            ~~ P.agrp (P.text "section" ^/ str name)
            ^/ P.block section ss
            end
    | A.TopDecl(d)        -> decl true d
    | A.TopProcedure(p)   -> proc p

let program ds = P.vgrp begin
                  ~~ P.list (P.break ^^ P.break) toplevel ds
                  ^^ P.break
                  end
                                            
let pp = program
let emit fd ~width tl =
  List.iter 
    (fun t -> ( P.ppToFile fd width (toplevel t)
              ; output_string fd "\n\n"
    )) tl
