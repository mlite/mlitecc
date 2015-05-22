type key  = int
type size =
  | Const  of int
  | Var    of key
  | Double of key      
  | Half   of key
      
type 'a t = 
  | Bool
  | Bits of 'a 
      
type ty   = int t
type tyscheme = (size t) list * (size t)
type monotype = (int  t) list * (int  t)  (* types from the interface
					     definition *)
    
module E = Error
module S = Map.Make(struct type t=key let compare=compare end)

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let to_string = function
  | Bool -> "bool"
  | Bits n -> "bits" ^ string_of_int n

let lookup key env = S.find key env 
  
let dump env       = let f key data res = (key,data)::res in S.fold f env []
							       
let match' opname sigma expected actual =
  let to_string' = function
    | Bool -> to_string Bool
    | Bits (Const x) -> to_string (Bits x)
    | Bits (Var x) when S.mem x sigma -> to_string (Bits (lookup x sigma))
    | Bits _ -> "bitsxxx (not easily identified)" 
  in
  let badarg () =
    E.errorf
      "operator %%%s expected argument of type %s, got %s (in unspecified position)"
      opname (to_string' expected) (to_string actual) 
  in
  match expected, actual with
    | Bool          , Bool                      -> sigma
    | Bits(Const x) , Bits y when x = y         -> sigma

    | Bits(Var x)   , Bits y when S.mem x sigma -> if lookup x sigma = y then sigma
      else badarg()
    | Bits(Var x)   , Bits y                    -> S.add x y sigma
	
    | Bits(Double x), Bits y when S.mem x sigma ->
        if (lookup x sigma) * 2 = y then sigma else badarg()
    | Bits(Double x), Bits y                    -> S.add x (y/2) sigma
	
    | Bits(Half x)  , Bits y when S.mem x sigma ->
        if (lookup x sigma) / 2 = y then sigma else badarg()
    | Bits(Half x)  , Bits y                     -> S.add x (y*2) sigma
	
    | _             , _                          -> badarg()
	
let subst opname sigma = function
  | Bool             -> Bool
  | Bits(Const x)    -> Bits x
  | Bits(Var x)      -> (try Bits(lookup x sigma) with Not_found -> 
      E.errorf "operator %%%s is polymorphic;\n    to indicate the size, \
                  use a suffix such as %%%s32" opname opname)
  | Bits(Double x)   -> (try Bits(2*(lookup x sigma)) with Not_found ->
      impossf "internal error (2) in application")
  | Bits(Half x)     -> (try Bits((lookup x sigma)/2) with Not_found ->
      impossf "internal error (3) in application")
      
let wrongargs opname args' args =
  E.errorf "operator %s expected %d arguments, got %d"
    opname (List.length args') (List.length args)

let appl opname (args',r) args =
  let sigma = 
    try List.fold_left2 (match' opname) S.empty args' args 
    with Invalid_argument _ -> wrongargs opname args' args 
  in
  subst opname sigma r
    
let widthlist opname (args',r) args =
  let sigma = 
    try List.fold_left2 (match' opname) S.empty args' args 
    with | Invalid_argument _ -> wrongargs opname args' args 
  in
  let sorted = List.sort (fun (key1,_) (key2,_) -> compare key1 key2) (dump sigma) 
  in
  List.map snd sorted
    
let rtlop = Str.regexp "^\\([A-Za-z0-9_]*[A-Za-z_]\\)\\([0-9]+\\)?$"

let split op =
  let matched n l = Str.matched_group n l in  
  if Str.string_match rtlop op 0 then
    let basename = matched 1 op in
    let size     = try Some (int_of_string (matched 2 op))
    with Not_found -> None in
    (basename, size)
  else
    impossf "illegal operator %%%s?" op
      
let largest_key (args, res) =
  let rec count k = function
    | [] -> k
    | (Bool | Bits (Const _)) :: ws -> count k ws
    | (Bits (Var n) | Bits (Double n) | Bits (Half n)) :: ws -> count (max k n)	ws 
  in
  count 0 (res :: args)
    
let instantiate ((args,ret):tyscheme) ~widths  =
  if List.length widths <> largest_key (args, ret) then
    impossf "instantiated %d-key type scheme with %d widths"
      (largest_key (args, ret)) (List.length widths);
  let inst = function
    | Bits (Var i)   -> 
	( try Bits (List.nth widths (i-1)) with 
          | Failure _ -> assert false
	) 
    | Bits (Double i)-> 
	( try Bits (2 * (List.nth widths (i-1))) with 
          | Failure _ -> assert false
	)
    | Bits (Half i)  -> 
	( try Bits ((List.nth widths (i-1))/2) with 
          | Failure _ -> assert false
	)
    | Bits (Const k) -> Bits k
    | Bool           -> Bool in
  (List.map inst args, inst ret)
    
let fixbits x        = assert (x > 0); Bits(Const x)
let var    x         = assert (x > 0); Bits(Var x)
let double x         = assert (x > 0); Bits(Double x)
let half   x         = assert (x > 0); Bits(Half x)
let bool             = Bool
let bits x           = Bits x
let proc args res    = (args,res)
let keyname = function
  | 1 -> "n"
  | 2 -> "m"
  | n -> Printf.sprintf "t%d" (n-2)

let scheme_string (args, result) =
  let spr = Printf.sprintf in
  let ty = function 
    | Bits (Var i)    -> spr "#%s bits"   (keyname i)
    | Bits (Double i) -> spr "2*#%s bits" (keyname i)
    | Bits (Half i)   -> spr "#%s/2 bits" (keyname i)
    | Bits (Const k)  -> spr "#%d bits"   k
    | Bool            -> "bool" 
  in
  let scheme = match args with
    | _ :: _ -> String.concat " * " (List.map ty args) ^ " -> " ^ ty result 
    | [] -> ty result 
  in
  match largest_key (args, result) with
    | 0 -> scheme
    | n -> spr "\\/ %s . %s" (String.concat ", " (List.map keyname (Auxfuns.from 1 ~upto:n)))
        scheme
