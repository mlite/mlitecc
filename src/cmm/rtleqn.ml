include Common
module Dn = Rtl.Dn
module Up = Rtl.Up
module RP = Rtl.Private

exception Can'tSolve
type solution =
    { 
      known:        (string * Rtl.exp) list;
      dependent:    (string * Rtl.exp) list
    }
      
type term =
  | Const of Rtl.exp
  | Var   of string
  | Unit  
	
type sum = (int * term) list
type t   = sum                  (* equation, sum == 0 *)
module ToString = 
struct
  let term = function
    | Const(exp) -> "(" ^ Rtlutil.ToString.exp exp ^ ")"
    | Var(s)     -> s
    | Unit       -> "1"
	
  let summand (i,t) = match i with
    | 1 -> term t
    | n ->
        match t with
          | Unit -> Printf.sprintf "%d" i
          | _    -> Printf.sprintf "%d * %s" i (term t) 

  let t sum = (* no thought for efficiency *)
    let pos = List.filter (fun (i, n) -> i >= 0)  sum in
    let neg = List.filter (fun (i, n) -> i <  0)  sum in
    let neg = List.map    (fun (i, n) -> (-i, n)) neg in
    let side = function
      | [] -> "0"
      | sum -> String.concat " + " (List.map summand sum) in
    side pos ^ " = " ^ side neg
end
  
let to_string = ToString.t
let sym x   = [(1, Var x)]
let int i   = if i = 0 then [] else [(i, Unit)]
let const k = [(1, Const (Up.exp k))]
let add x y = x @ y
let sub x y = x @ List.map (fun (i,y) -> (-i,y)) y
  
let exp e = 
  let rec exp = function
    | RP.Const(RP.Bits(b))      -> int (Bits.S.to_int b)
    | RP.Const(RP.Late(x,_))    -> sym x
    | RP.Const(_) as x          -> const x
    | RP.App(("add",_),[e1;e2]) -> add (exp e1) (exp e2)
    | RP.App(("sub",_),[e1;e2]) -> sub (exp e1) (exp e2)
    | x                         -> const x
  in
  exp (Dn.exp e)
    
let equate e1 e2 = 
  let t  = sub (exp e1) (exp e2) in
  let () = if Cmm_debug.on "rtleqn" then
    Printf.eprintf "Rtleqn.equate: %s === %s\n"
      (ToString.t (exp e1)) (ToString.t (exp e2)) in
  t
    
let () = Cmm_debug.register "rtleqn" "RTL equation solver"
  
module T = 
struct
  type t = term
  let variable = function         (* identify variable *)
    | Var s -> Some s
    | _     -> None
	
  let compare t t' = match t, t' with
    | (Const l, Const r) -> Rtlutil.Compare.exp (Dn.exp l) (Dn.exp r)
    | (Var l, Var r) -> compares l r
    | (Unit, Unit) -> 0
    | (Const _, Var _) -> -1
    | (Const _, Unit) -> -1
    | (Var _, Const _) -> 1
    | (Var _, Unit) -> -1
    | (Unit, Const _) -> 1
    | (Unit, Var _) -> 1

  let print   = ToString.term
end
  
module Solver = Eqn.Make(T)         (* equation solver over terms *)
let rtl ~(width:int) (e:sum) = 
  let add    = Rtlutil.add width in
  let mult   = Rtl.opr "mul" [width] in 
  let bits i = Rtl.bits (Bits.S.of_int i width) width    in 
  let late x = Rtl.late x width in
  let summand = function
    | (1,Var x)   -> late x 
    | (i,Var x)   -> Rtl.app mult [bits i; late x] 
    | (i,Unit)    -> bits i 
    | (1,Const k) -> k
    | (i,Const k) -> Rtl.app mult [bits i;k] in
  let rec loop e = function
    | []    -> e
    | [s]   -> add e (summand s)
    | s::ss -> loop (add e (summand s)) ss
  in
  match e with
    | []    -> Rtl.bits (Bits.zero width) width
    | s::ss -> loop (summand s) ss 
	
let solve ~width sums =
  try
    let eqns   = List.fold_right Solver.make_zero sums Solver.empty in
    let result = Solver.solve eqns in
    let to_rtl = function (v,sum) -> (v,rtl width sum) in
    { known     = List.map to_rtl result.Solver.known
    ; dependent = List.map to_rtl result.Solver.dependent
    }
  with
      Solver.Can'tSolve _ -> raise Can'tSolve
