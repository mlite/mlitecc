include Common
module type EXP = sig
  type t                                      (* a term *)
  val variable: t -> string option            
  val compare: t -> t -> int                  (* -1/0/1 *)
  val print: t -> string                      (* for debugging *)
end
  
module type S = sig
  type t                                      (* set of equations *)
  type term
  type sum      = (int * term) list
      
  exception Can'tSolve of t

  type solution = 
      { known:     (string * sum) list
      ; dependent: (string * sum) list
      }
	
  val empty:          t                       (* empty set of equations *)
  val make_zero:      sum -> t -> t           (* add equation *)
  val solve:          t -> solution           (* Can'tSolve *)
end

module Make (E: EXP) = struct
  type term     = E.t

  type sum   = (int * E.t) list                 (* invariant: ordered *)
  type assoc = string * sum

  type solution = 
      { known     : (string * sum) list
      ; dependent : (string * sum) list
      }
	
  module SM = Map.Make (struct type t = string let compare = compares end)
  type t   = 
      { set  : sum list          (* invariant: normalized *)
      ; env  : sum SM.t
      ; deps : string list SM.t
      }

  let empty = 
    { set  = []
    ; env  = SM.empty
    ; deps = SM.empty
    }

  exception Can'tSolve of t
  let error msg = raise (Can'tSolve msg)
  let dump t =
    let spr = Printf.sprintf in
    let product (i,term) =
      if i = 1 then
        E.print term
      else
        spr "%d * %s" i (E.print term) in
    let sum = function
      | [] -> "0"
      | s  -> String.concat " + " (List.map product s) in
    let eqn s =
      let pos = List.filter (fun (i, t) -> i >= 0)  s in
      let neg = List.filter (fun (i, t) -> i <  0)  s in
      let neg = List.map    (fun (i, t) -> (-i, t)) neg in
      spr "  %s = %s" (sum pos) (sum neg) 
    in
    let assoc v s rst = spr "  %s :-> %s" v (sum s) :: rst in
    let multiple f s = String.concat "\n" (List.map f s) in
    let set f s = String.concat "\n" (SM.fold assoc s []) in
    Printf.printf
      "Eqn.t: unsolved equations\n%s\n------ solved variables:\n%s\n------ Eqn.t ends\n"
      (multiple eqn t.set) (set assoc t.env);
    flush stdout
    let rec gcd (m:int) (n:int) =
      let rec g m n = if n = 0 then m else g n (m mod n) in
      if  n < 0 then
        gcd m (- n)
      else if m < n then gcd n m
      else g m n

    let normalize = function
      | [] -> []
      | ((c,_)::rest) as sum ->
          let g = List.fold_left (fun k (c,_) -> gcd k c) c rest in
          if g > 1 then
            List.map (fun (c,t) -> (c / g, t)) sum
          else
            sum
	      
    let combine (k1:int) (sum1:sum) (sum2:sum) =
      let rec loop = function 
        | []                 , [] -> []
        | ((c1,x1)::s1 as _x) , ((c2,x2)::s2 as y) when E.compare x1 x2 < 0 ->
            let k = k1 * c1 in
            if k = 0 then loop (s1, y) else
              (k, x1) :: loop (s1, y) 
        | ((c1,x1)::s1) , ((c2,x2)::s2) when E.compare x1 x2 = 0 -> 
            let k = k1 * c1 + c2 in 
            if k = 0 then loop (s1, s2) else
              (k, x1) :: loop (s1, s2)
        | ((c1,x1)::s1 as x) , ((c2,x2)::s2 as _y)  (*   x1 > x2 *) ->
            let k = c2 in
            if k = 0 then loop (x, s2) else
              (k, x2) :: loop (x, s2)
        | ((c1,x1)::s1) , [] ->
            let k = k1 * c1 in
            if k = 0 then loop (s1, []) else
              (k, x1) :: loop (s1, [])
        | []                 ,  ((c2,x2)::s2) ->
            let k = c2 in
            if k = 0 then loop ([], s2) else
              (k, x2) :: loop ([], s2)
      in
      loop (sum1,sum2)
	
    let split (v:string) (sum:sum) =
      let rec loop a = function
        | []             -> (0, sum)
        | (c,t as ct)::s -> 
            ( match E.variable t with
              | Some v' when v =$= v' -> (c, List.rev a @ s)
              | _                     -> loop (ct::a) s
            )
      in 
      loop [] sum
	
    let elim (v:string) (vsum:sum) (sum:sum) =  
      match split v sum with
        | (0,_)    -> sum
        | (c,sum') -> combine c vsum sum'
	    
    let vars (sum:sum) =
      let rec loop vs = function
        | (_, t)::s ->
            ( match E.variable t with
              | Some v -> loop (v::vs) s
              | _      -> loop vs s
            )
        | [] -> vs in 
      loop [] sum
	
    let elim_all env (sum:sum) =
      let vs = vars sum in
      List.fold_left (fun sum v -> try elim v (SM.find v env) sum
      with Not_found -> sum) sum vs
    let rec zero = function
       | []                        -> true
       | (0,_)::rest               -> zero rest
       | _                         -> false
	   
    let candidate t found finished =
      let negate          = List.map (fun (c,trm) -> (-c,trm))                       in
      let unitvar (c,trm) = Auxfuns.Option.is_some (E.variable trm) && (c = 1 || c = -1) in
      let rec loop accum_set = function
        | []        -> (* all eqns scanned *)
            (match accum_set with
              | [] -> finished { t with set = accum_set }
              | _  -> error    { t with set = accum_set }
            )
        | sum::sums -> 
            let sum = normalize (elim_all t.env sum) in
            if zero sum then
              loop accum_set sums
            else 
              try let v      = match E.variable (snd (List.find unitvar sum)) with
                | Some v -> v
                | None   -> assert false in   
              let c,vsum = split v sum in 
              match c with
                | -1 -> found (v, vsum,        { t with set = accum_set @ sums })
                |  1 -> found (v, negate vsum, { t with set = accum_set @ sums })
                | _  -> assert false    (* c is a unit, i.e. +/-1 *)
              with Not_found -> loop (sum::accum_set) sums (* check next equation *)
      in
      loop [] t.set
	
    let update (v:string) (vsum:sum) t =
      let find x map = try SM.find x map with Not_found -> [] in
      let depends_on_v = find v t.deps in
      let env = List.fold_left (fun env v' -> SM.add v' (elim v vsum (SM.find v' env)) env)
        t.env depends_on_v in
      let vs = vars vsum in
      let new_deps = v :: depends_on_v in
      let add deps v' = SM.add v' (new_deps @ find v' deps) deps in
      { set  = t.set
      ; env  = SM.add v vsum env
      ; deps = List.fold_left add t.deps vs
      }

    let solver t =
      let rec loop t =
        candidate t (fun (v, vsum, t') -> loop (update v vsum t')) (fun t' -> t') in
      loop t
	
    let solve t =
      (* let () = dump t in *)
      let t         = try solver t with Can'tSolve x -> (dump x; error x)in
      (* let () = dump t in *)
      let known sum = List.for_all (fun (_,e) -> Auxfuns.Option.is_none (E.variable e)) sum in
      let k,d       =
        SM.fold (fun s sum (k,d) -> if known sum then ((s,sum)::k,d) else (k, (s,sum)::d))
          t.env ([], []) in
      { known     = k
      ; dependent = d
      }
	
    let rec merge sum =  match sum with
      | []       -> []
      | [_] as x -> x
      | (c1,t1 as ct1)::((c2,t2)::rest1 as rest2)  ->
          if E.compare t1 t2 = 0 then
            if c1+c2 <> 0 then
              merge ((c1+c2,t1)::rest1)
            else
              merge rest1     (* drop zero coefficient *)
          else
            ct1 :: merge rest2

    let make_zero (sum:sum) (t:t) =
      let cmp (_,tx) (_,ty) = E.compare tx ty in 
      let eqn = normalize (merge (List.sort cmp sum)) in
      let r   = { t with set = eqn::t.set } in
      (* dump t; *)
      match eqn with [] -> t | _ :: _ -> r
        
end    

module Test = struct
  module E = struct
    type t = 
      | Var of string
      | Unit
      | Const of string
	  
    let variable = function
      | Var s  -> Some s 
      | _      -> None
          
    let compare : t -> t -> int = Pervasives.compare

    let print = function
      | Var(s) -> s
      | Unit     -> "1"
      | Const(s) -> s
  end

  module S = Make(E)

  let test eqns =
    let t = List.fold_left (fun t eqn -> S.make_zero eqn t) S.empty eqns
    in
    S.solve t

  let eqns1 =
    [ [3,E.Var "x";5   ,E.Var "y"]
    ; [2,E.Var "y";3   ,E.Var "z"]
    ; [3,E.Var "x";(-3),E.Var "x"]
    ; [2,E.Var "x";(-5),E.Var "z"]
    ; [3,E.Unit   ;1   ,E.Var "y"]
    ]
  let eqns2 =
    [ [-3,E.Unit;1, E.Var "x"; 1, E.Var "y"; 1, E.Var "z"]
    ; [-1,E.Unit;1, E.Var "x";-1, E.Var "y";-1, E.Var "z"]
    ; [4, E.Unit;1, E.Var "z"]
    ]


  let eqns3 =
    [ [3,E.Var "x";5   ,E.Var "y"; 3,E.Const "k1"]
    ; [2,E.Var "y";3   ,E.Var "z"]
    ; [3,E.Var "x";(-3),E.Var "x"]
    ; [2,E.Var "x";(-5),E.Var "z"; 2,E.Const "k1"]
    ; [3,E.Unit   ;1   ,E.Var "y"; 3,E.Const "k3"]
    ]
      
(*
  let _ = assert 
  (test eqns1 = 
  { S.known = ["x", [5, E.Unit]; "z", [2, E.Unit]; "y", [-3, E.Unit]]
  ; S.dependent = []
  })
  
  let _ = assert 
  (test eqns2 = 
  { S.known = ["y", [5, E.Unit]; "x", [2, E.Unit]; "z", [-4, E.Unit]]
  ; S.dependent = []
  })
*)        

end
