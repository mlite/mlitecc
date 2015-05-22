let debug   = false 
let strlen  = String.length

let nl      = "\n"

type gmode =
  | GFlat             (* hgrp *)
  | GBreak            (* vgrp *)
  | GFill             (* fgrp *)
  | GAuto             (* agrp *)


type doc =
  | DocNil
  | DocCons           of doc * doc
  | DocText           of string
  | DocNest           of int * doc
  | DocBreak          of string
  | DocGroup          of gmode * doc


let (^^) x y            = DocCons(x,y)
let empty               = DocNil
let text s              = DocText(s)
let nest i x            = DocNest(i,x)
let break               = DocBreak(" ")
let breakWith s         = DocBreak(s)

let hgrp d              = DocGroup(GFlat, d)
let vgrp d              = DocGroup(GBreak,d)
let agrp d              = 
  if   debug
  then DocGroup(GAuto, text "[" ^^ d ^^ text "]")
  else DocGroup(GAuto, d)
    
let fgrp d              = 
  if   debug
  then DocGroup(GFill, text "{" ^^ d ^^ text "}")
  else DocGroup(GFill, d)
                          
type sdoc =
  | SNil
  | SText             of string * sdoc
  | SLine             of int    * sdoc    (* newline + spaces *)

let sdocToString sdoc =
  let buf = Buffer.create 256 in
  let rec loop = function
    | SNil              -> ()
    | SText(s,d)        -> ( Buffer.add_string buf s
                           ; loop d)
	
    | SLine(i,d)        -> let prefix = String.make i ' ' in
      ( Buffer.add_char   buf '\n'
      ; Buffer.add_string buf prefix
      ; loop d
      )
  in
  ( loop sdoc
  ; Buffer.contents buf
  )

let sdocToFile oc doc = 
  let pstr = output_string oc in
  let rec loop = function
    | SNil          -> () 
    | SText(s,d)    -> pstr s; loop d
    | SLine(i,d)    -> 
	let prefix = String.make i ' ' 
	in  pstr nl;
	pstr prefix;
	loop d
  in
  loop doc

type mode =
  | Flat
  | Break
  | Fill

let rec fits w = function
  | _ when w < 0                   -> false
  | []                             -> true
  | (i,m,DocNil)              :: z -> fits w z
  | (i,m,DocCons(x,y))        :: z -> fits w ((i,m,x)::(i,m,y)::z)
  | (i,m,DocNest(j,x))        :: z -> fits w ((i+j,m,x)::z)
  | (i,m,DocText(s))          :: z -> fits (w - strlen s) z
  | (i,Flat, DocBreak(s))     :: z -> fits (w - strlen s) z
  | (i,Fill, DocBreak(_))     :: z -> true 
  | (i,Break,DocBreak(_))     :: z -> true
  | (i,m,DocGroup(_,x))       :: z -> fits w ((i,Flat,x)::z)


(* format is cps to avoid stack overflow *)
let cons  s post z = post (SText (s, z))
let consl i post z = post (SLine (i, z))
let rec format w k l post = match l with
  | []                             -> post SNil
  | (i,m,DocNil)              :: z -> format w k z post
  | (i,m,DocCons(x,y))        :: z -> format w k ((i,m,x)::(i,m,y)::z) post
  | (i,m,DocNest(j,x))        :: z -> format w k ((i+j,m,x)::z) post
  | (i,m,DocText(s))          :: z -> format w (k + strlen s) z (cons s post)
  | (i,Flat, DocBreak(s))     :: z -> format w (k + strlen s) z (cons s post)
  | (i,Fill, DocBreak(s))     :: z -> 
      let l = strlen s in
      if   fits (w - k - l) z 
      then format w (k+l) z (cons s post)
      else format w  i    z (consl i post)
	
  | (i,Break,DocBreak(s))     :: z -> format w i z (consl i post)
  | (i,m,DocGroup(GFlat ,x))  :: z -> format w k ((i,Flat ,x)::z) post
  | (i,m,DocGroup(GFill ,x))  :: z -> format w k ((i,Fill ,x)::z) post
  | (i,m,DocGroup(GBreak,x))  :: z -> format w k ((i,Break,x)::z) post
  | (i,m,DocGroup(GAuto, x))  :: z -> 
      if fits (w-k) ((i,Flat,x)::z)
      then format w k ((i,Flat ,x)::z) post
      else format w k ((i,Break,x)::z) post
	
let ppToString  w doc = format w 0 [0,Flat,agrp(doc)] sdocToString
let ppToFile oc w doc = format w 0 [0,Flat,agrp(doc)] (sdocToFile oc) 
  
let rec list sep f xs =
  let rec loop acc = function
    | []    -> acc
    | [x]   -> acc ^^ f x 
    | x::xs -> loop (acc ^^ f x ^^ sep) xs
  in loop empty xs 

let commalist f = list (text "," ^^ break) f

let (^/) x y   = x ^^ break ^^ y 
let (~~) x     = x

let block f xs =   
  text "{"
  ^^ nest 4 begin
    ~~ break
    ^^ list break f xs
  end 
  ^/ text "}"

