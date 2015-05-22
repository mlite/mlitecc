type aggregation = 
    Register.aggregation = 
  | BigEndian
  | LittleEndian
  | Identity
      
type space = char * aggregation * Cell.t   (* name, byte order, cell size *)
type width = int
type count = Register.count = C of int

module Private = 
struct
  type aligned   = int     (* alignment guaranteed *)
  type assertion = aligned (* may one day include alias info *)
  type opr       = string * width list
  type const     = 
    | Bool      of bool
    | Bits      of Bits.bits            (* literal constant *)
    | Link      of Symbol.t * symkind * width  (* link-time constant *)
    | Diff      of const  * const   (* difference of two constants *)
    | Late      of string * width   (* late compile time constant *)
	
  and symkind = Code | Data | Imported (* three kinds of symbol, needed for PIC *)

  type exp        = 
    | Const     of const               
    | Fetch     of loc * width 
    | App       of opr * exp  list
	
  and  count     = Register.count = C of int
    
  and  loc       = 
    | Mem       of space * count *  exp *  assertion
    | Reg       of Register.t (* space * int * count *)         
    | Var       of string(* name from C-- source   *) * int(* index for run-time API *) * width
    | Global    of string * int * width (* global C-- variable *)
    | Slice     of width(* number of bits in loc *) 
	* int(* index of least-significant bit of slice *)
        *  loc  (* location from which slice is drawn *)
	
  type effect     = 
    | Store     of loc * exp  * width
    | Kill      of loc
	
  type guarded    = exp  * effect
  type rtl        = Rtl of guarded list
end

open Private    (* never do that *)
type exp       = Private.exp     
type loc       = Private.loc   
type rtl       = Private.rtl        
type opr       = Private.opr        
type assertion = Private.assertion  
let aligned k = k
  
let alignment k = k
  
let shift k' k = 
  let rec gcd (m:int) (n:int) =
    let rec g m n = if n = 0 then m else g n (m mod n) in
    if  n < 0 then gcd m (- n)
    else if m < n then gcd n m
    else g m n 
  in
  gcd k k'
    
let shift_multiple = shift  (* OK for this rep, which is lossy *)
  
let none = aligned 1
  
let par rtls   = Rtl (List.concat (List.map (fun (Rtl x) -> x) rtls))
  
let slice width ~lsb loc = Slice (width, lsb, loc) 
  
let conjunction = ("conjoin", [])   (* logical and *)
  
let conjoin g g' = match g with
  | Const(Bool true)  -> g'
  | Const(Bool false) -> g
  | _ -> match g' with      
      | Const(Bool true)  -> g
      | Const(Bool false) -> g'
      | _ -> App(conjunction, [g; g'])
	  
let guard expr (Rtl geffects) =
  let conjunct (guard,effect) = (conjoin expr guard, effect) 
  in
  Rtl (List.map conjunct geffects)  
    
let true'  = Const (Bool true)
let false' = Const (Bool false)
  
let bool p = if p then true' else false'
  
let bits b width            = 
  ( assert (Bits.width b = width)
  ; Const (Bits b)
  )
    
let codesym name width      = Const (Link(name,Code,width))
let datasym name width      = Const (Link(name,Data,width))
let impsym  name width      = Const (Link(name,Imported,width))
let diff const1 const2      = Const (Diff(const1,const2))
let late name width         = Const (Late(name,width))
let fetch loc w             = Fetch(loc, w)
let app op exprs            = App(op,exprs)
let opr name widths         = (name,widths)

(* location *)
let mem assertion sp count expr = Mem(sp,count,expr,assertion)
let var    name ~index width    = Var (name,index,width)
let global name ~index width    = Global (name,index,width)
let reg r                       = Reg r
let regx = function
  | Register.Reg r -> Reg r
  | Register.Slice (w, lsb, r) -> Slice(w, lsb, Reg r)
      
(* effect *)
let rtl effect              = Rtl [(bool true, effect)]
let store loc expr width    = rtl (Store(loc, expr, width))
let kill  loc               = rtl (Kill(loc))
let null                    = Rtl []
  
let locwidth = function
  | Mem((_,_,ms),c,_,_) -> Cell.to_width ms c
  | Reg((_,_,ms),_,c)   -> Cell.to_width ms c
  | Var(_,_,w)            -> w
  | Global(_,_,w)         -> w
  | Slice(w, _, _)        -> w 

let rec fetch_cvt loc n = 
  match loc with
    | Slice(n', lsb, loc) ->
	assert (n'=n);
	let w = locwidth loc in
	if lsb = 0 then 
          App(("lobits", [w; n]), [fetch loc n])
	else
          App(("bitExtract", [w; n]), [Const (Bits (Bits.U.of_int lsb w)); fetch loc n])
    | _ -> Fetch(loc,n)
	
let store_cvt loc expr n = 
  match loc with
    | Slice(n', lsb, loc) ->
	assert (n'=n);
	let w = locwidth loc in
	let wide = Fetch(loc, w) in
	let lsb = Const (Bits (Bits.U.of_int lsb w)) in
	let wide' = App(("bitInsert", [w; n]), [lsb; wide; expr]) in
	rtl (Store(loc, wide', w))
    | _ -> rtl (Store(loc,expr,n))              



module Dn = 
struct    
  let id          = fun x -> x
  let exp         = id
  let loc         = id
  let rtl         = id
  let opr         = id
  let assertion   = id
end

module Up = 
struct    
  let effect      = rtl (* from above *)
  let id          = fun x -> x
  let exp         = id
  let loc         = id
  let rtl         = id
  let opr         = id
  let assertion   = id
  let const c     = Const c
end

(*
module Convert = Dn (* deprecated, for backward compatibility *)
module Revert  = Up (* deprecated, for backward compatibility *) 
*)
