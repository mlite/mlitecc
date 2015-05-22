include Common

module type GRAPHINFO = 
sig
  type t
  type result
  val getNodeNumber : t -> int
  val getSuccs      : t -> int list array 
  val getPreds      : t -> int list array
  val translate      : int array -> t -> result
end
  
module type DOMINATORTREE = 
  functor (G : GRAPHINFO) -> sig
    val dominatorTree : G.t -> G.result
  end
    
type tree = 
  | Leaf of Zipcfg.label option * Zipcfg.Rep.labelkind option
  | Node of Zipcfg.label option * Zipcfg.Rep.labelkind option * tree list
      
module LengauerTarjan : DOMINATORTREE   = functor (G : GRAPHINFO) -> 
struct
  module A = Array
  module L = List
      
  let dominatorTree graph = 
    let nodeNumber = G.getNodeNumber graph 
    and succs =      G.getSuccs graph 
    and preds =      G.getPreds graph in 
    let vertex =     A.init nodeNumber (fun i -> -1)
    and parent =     A.init nodeNumber (fun i -> -1)
    and bucket =     A.init nodeNumber (fun i -> []) 
    and dfnum =      A.init nodeNumber (fun i -> 0)
    and semi =       A.init nodeNumber (fun i -> -1)
    and ancestor =   A.init nodeNumber (fun i -> -1)
    and idom =       A.init nodeNumber (fun i -> -1)
    and samedom =    A.init nodeNumber (fun i -> -1) 
    and counter =    ref 0 
    in 
    let rec dfs p n =
      if A.get dfnum n = 0 then (
        A.set dfnum   n !counter ; 
        A.set vertex !counter n ; 
        A.set parent  n  p ;
        counter := !counter + 1 ;
        L.iter (fun w -> dfs n w) (A.get succs n)
      ) 
    in
    let ancestorWithLowestSemi v = 
      let u = ref v 
      and v = ref v in 
      while A.get ancestor !v != -1 do
        if A.get dfnum (A.get semi !v) < A.get dfnum (A.get semi !u)
        then u := !v ;
        v := A.get ancestor !v ;
      done ;
      !u
    and link p n = 
      A.set ancestor n p in 
    let dominators () =
      dfs (-1) 0 ;
      for i = !counter - 1 downto 1 do 
        let n  = A.get vertex i in  
        let p  = A.get parent n in  
        let s  = ref p 
        and s' = ref p in 
        L.iter 
          (fun v -> 
            if dfnum.(v) <= dfnum.(n) 
            then s' := v 
            else s' := A.get semi (ancestorWithLowestSemi v) ;
            if A.get dfnum !s' < A.get dfnum !s then  s := !s' ;
            A.set semi n !s ;
            A.set bucket !s (L.append (A.get bucket !s) [n]) ;
          ) (A.get preds n) ;
        link p n ;
        L.iter 
          (fun v -> 
            let y = ancestorWithLowestSemi v in
            if A.get semi y = A.get semi v 
            then A.set idom v p 
            else A.set samedom v y ;
          ) (A.get bucket p) ;
	
        A.set bucket p [] ;
      done;
      for i = 1 to !counter - 1 do 
        let n = A.get vertex i in 
        if A.get samedom n != -1
        then A.set idom n (A.get idom (A.get samedom n)) 
      done ; in
    dominators () ; 
    (* We should not have the graph appearing here ...  *)
    G.translate idom graph  
  end
    
module ZipGraph : GRAPHINFO with type t = Zipcfg.graph 
                            and type result = tree = 
struct
  module A = Array
  module L = List 
  module G = Zipcfg
  module B = Zipcfg.Rep
  module M = Map 

  type t = G.graph     
  type result = tree
      
  let getNodeNumber graph = L.length (G.postorder_dfs graph)
    
  let succsOfLast last = 
    match last with 
      | B.Branch (_,lbl) -> [Some lbl]
      | B.Cbranch (_,lbl1,lbl2)-> [Some lbl1;Some lbl2]
      | B.Mbranch (_,l) -> L.map (fun e -> Some e) l
      | _ -> []

  let intToLabel graph = 
    let l = G.postorder_dfs graph in 
    let l = A.of_list (L.map (fun b -> (B.blocklabel b,B.blockkind b)) l) in
    fun elt -> A.get l elt 

  let position elt l = 
    let rec find l pos=
      match l with
          [] -> Impossible.impossible "label missing ine ZipGraph"
	| e :: l -> if e =*= elt then pos else find l (pos+1) in
    find l 0

  let labelToInt graph = 
    let l = G.postorder_dfs graph in
    let l = L.map B.blocklabel l in 
    fun label -> position label l 
      

  let getSuccs graph = 
    let l = G.postorder_dfs graph in
    let labelToInt = labelToInt graph in
    let f block = 
      let zblock = B.unzip block in
      let last = B.last zblock in
      let succs = succsOfLast last in
      L.map labelToInt succs in
    A.of_list (L.map f l)

  let getPreds graph =
    let succs = getSuccs graph in
    let max = getNodeNumber graph in
    let f num = 
      let result = ref [] in
      for i = 0 to max do 
        if L.mem num succs.(i) then result := i :: !result ;
      done ;
      !result in
    A.init (getNodeNumber graph) f
      
  (* We really should not have the graph here as an argument, I think
     that it would be much better here if I could have the function 
     IntToLabel somewhere in a global mutable state *)

  let translate idom graph = 
    let name = intToLabel graph in
    let find idom num = 
      let l = A.to_list idom in 
      L.filter (fun i -> num = i) l in
    let rec build num =
      let (lbl, kind) = name num in 
      let l = find idom num in 
      if L.length l = 0 then Leaf (lbl, kind)
      else Node (lbl,kind,L.map build l)
    in
    build 0
end
