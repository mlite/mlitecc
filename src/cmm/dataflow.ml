module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Unique.Prop
module RS = Rtlutil.ToString
module UM = Unique.Map
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimpf  fmt = Printf.kprintf Impossible.unimp      fmt
let dprintf fmt = Cmm_debug.eprintf "dataflow" fmt
let _ = Cmm_debug.register "dataflow" "execution of generic dataflow engine"
  
type 'a answer = Dataflow of 'a | Rewrite of Zipcfg.graph
type txlimit = int
type 'a fact = 
    {
      fact_name : string;                     (* documentation *)
      init_info : 'a;                         (* lattice bottom element *)
      add_info  : 'a -> 'a -> 'a;             (* lattice join (least upper bound) *)
      changed   : old:'a -> new':'a -> bool;  (* is new one bigger? *)
      prop      : 'a Unique.Prop.t;           (* access to mutable state by uid *)
    }
      
type 'a fact' = 
    {
      fact_name' : string;                     (* documentation *)
      init_info' : 'a;                         (* lattice bottom element *)
      add_info'  : 'a -> 'a -> 'a;             (* lattice join (least upper bound) *)
      changed'   : old:'a -> new':'a -> bool;  (* is new one bigger? *)
      get'       : Zipcfg.uid -> 'a;
      set'       : Zipcfg.uid -> 'a -> unit;
    }
      
let to_fact' f =
  { fact_name' = f.fact_name
  ; init_info' = f.init_info
  ; add_info' = f.add_info
  ; changed' = f.changed
  ; get' = P.get f.prop
  ; set' = P.set f.prop
  }
    
let to_fact f f' =
  {fact_name = f'.fact_name'; init_info = f'.init_info'; add_info = f'.add_info';
   changed = f'.changed'; prop = f.prop}
    
let run dir name fact changed entry_fact do_block txlim blocks =
  let init () = 
    List.iter (fun b -> fact.set' (GR.id b) fact.init_info') blocks;
    fact.set' GR.entry_uid entry_fact in
  let rec iterate n =
    let () = changed := false in
    let txlim = List.fold_left do_block txlim blocks in
    if !changed then
      if n < 1000 then iterate (n + 1)  (* starts from original txlim *)
      else impossf "%s didn't converge in %n %s iterations" name n dir
    else
      (Cmm_debug.eprintf name "%s converged in %d %s iterations\n" name n dir; txlim) in
  Cmm_debug.eprintf name "starting %s dataflow pass %s\n" dir name;
  init();
  Cmm_debug.eprintf name "post-init %s dataflow pass %s\n" dir name;
  iterate 1
    
let update fact changed =
  fun u a ->
    let old_a = fact.get' u in
    let new_a = fact.add_info' a old_a in
      if fact.changed' ~old:old_a ~new':new_a then
        begin
          dprintf "Dataflow fact changed at unknown uid\n";
          fact.set' u new_a;
          if not (Unique.eq u GR.entry_uid) then (* no need to re-run on new entry *)
            changed := true
        end

let _ = (update : 'a fact' -> bool ref -> G.uid -> 'a -> unit)
  
let without_changing_entry fact go =
  let restore =
    try
      let entry = fact.get' GR.entry_uid in
      fun () -> fact.set' GR.entry_uid entry
    with Not_found ->
      fun () -> () in
  let output = go () in
  let answer = fact.get' GR.entry_uid in
  restore();
  output, answer
    
let add_blocks map list =
  let add _ b bs = b :: bs in
  UM.fold add map list
    
let ( << ) f g = fun x -> f (g x)
  
let eqfact fact a a' = (* poor man's approximation of equality *)
  not (fact.changed' a a' or fact.changed' a' a)
    
let limit_fun f i n txlim = if txlim > 0 then f i n else None
  
let limit_last f n txlim  = if txlim > 0 then f n else None
  
module B = 
struct
  type ('i, 'o) computation =
      { 
	name      : string;
	last_in   : Zipcfg.Rep.last -> 'o;
	middle_in : 'i -> Zipcfg.Rep.middle -> 'o;
	first_in  : 'i -> Zipcfg.Rep.first -> 'o;
      } 
	
  type 'a analysis       = 'a fact  * ('a, 'a)                   computation
  type 'a analysis'      = 'a fact' * ('a, 'a)                   computation
  type 'a transformation =           ('a, Zipcfg.graph option)   computation
  type 'a pass           = 'a fact  * ('a, txlimit -> 'a answer) computation
  type 'a pass'          = 'a fact' * ('a, txlimit -> 'a answer) computation
      
  let to_analysis' (f, c) = (to_fact' f, c)
    
  let y = (to_analysis' : 'a analysis -> 'a analysis')
    
  let to_pass'     (f, c) = (to_fact' f, c)
    
  let comp_with_exit comp exit_fact =
    let last_in l txlimit = match l with
    | GR.Exit -> Dataflow exit_fact
    | _ -> comp.last_in l txlimit 
    in
    { comp with last_in = last_in } 
      
  let check_property_match fact a block =
    match (fst block) with
    | GR.Entry -> () (* needn't match *)
    | GR.Label ((_, l), _, _) -> 
        let old_a = fact.get' (GR.id block) in
        let new_a = fact.add_info' a old_a in
        if not (eqfact fact old_a new_a) then
          impossf "property at label '%s' changed after supposedly reaching fixed point" l
	    
  let run_anal' (fact, comp) graph =
    let changed = ref false in
    let update = update fact changed in
    let set_block_fact () b =
      let h, l = GR.goto_end (GR.unzip b) in
      let block_in = (* 'in' fact for the block *)
        let rec head_in h out = match h with
          | GR.Head (h, m) -> head_in h (comp.middle_in out m)
          | GR.First f -> comp.first_in out f in
        head_in h (comp.last_in l) in
      update (GR.id b) block_in in
    let blocks = List.rev (G.postorder_dfs graph) in
    run "backward" comp.name fact changed fact.init_info' set_block_fact ()
    blocks
      
  let run_anal anal graph = run_anal' (to_analysis' anal) graph
    
  let rec solve_graph fact comp txlim graph exit_fact =
    without_changing_entry fact (fun () ->
      general_backward fact (comp_with_exit comp exit_fact) txlim graph)
      
  and general_backward fact comp txlim graph =
    let changed = ref false in
    let update = update fact changed in
    let set_block_fact txlim b =
      let txlim, block_in = 
        let rec head_in txlim h out = match h with
          | GR.Head (h, m) ->
              (dprintf "Solving middle node %s\n" (RS.rtl (GR.mid_instr m));
              match comp.middle_in out m txlim with
		| Dataflow a -> head_in txlim h a
		| Rewrite g ->
                    let txlim, a = solve_graph fact comp (txlim-1) g out in
                    head_in txlim h a)
          | GR.First f -> 
              match comp.first_in out f txlim with
		| Dataflow a -> txlim, a
		| Rewrite g -> solve_graph fact comp (txlim-1) g out in
        let h, l = GR.goto_end (GR.unzip b) in
        match comp.last_in l txlim with
          | Dataflow a -> head_in txlim h a
          | Rewrite g ->
              let txlim, a = solve_graph fact comp (txlim-1) g fact.init_info' in
              head_in txlim h a in
      update (GR.id b) block_in;
      txlim in
    let blocks = List.rev (G.postorder_dfs graph) in
    run "backward" comp.name fact changed fact.init_info' set_block_fact txlim
      blocks
      
  let rec solve_and_rewrite fact comp txlim graph exit_fact changed =
    let _, a = solve_graph fact comp txlim graph exit_fact in          (* pass 1 *)
    let txlim, g, c =                                                  (* pass 2 *)
      backward_rewrite fact (comp_with_exit comp exit_fact) txlim graph changed  in
    txlim, a, (g, c) 
      
  and backward_rewrite (fact : 'a fact') comp txlim graph changed =
    let rec rewrite_blocks txlim rewritten fresh changed : txlimit * G.graph * bool =
      match fresh with
	| [] -> txlim, G.of_block_list rewritten, changed
	| b :: bs ->
            let rec rewrite_next_block txlim =
              let h, l = GR.goto_end (GR.unzip b) in
              match comp.last_in l txlim with
		| Dataflow a -> propagate txlim h a (GR.Last l) rewritten changed
		| Rewrite g ->
                    let txlim, a, (g, _) =
                      solve_and_rewrite fact comp (txlim-1) g fact.init_info' changed in
                    let t, g = G.remove_entry g in
                    let rewritten = add_blocks (G.to_blocks g) rewritten in
                    (* continue at entry of g *)
                    propagate txlim h a t rewritten true
            and propagate : txlimit -> GR.head -> 'a -> GR.tail -> GR.block list -> bool ->
          txlimit * G.graph * bool =
              fun txlim h out tail rewritten changed -> match h with
		| GR.Head (h, m) -> (
                    dprintf "Rewriting middle node %s\n" (RS.rtl (GR.mid_instr m));
                    match comp.middle_in out m txlim with
                      | Dataflow a -> propagate txlim h a (GR.Tail (m, tail)) rewritten changed
                      | Rewrite g ->
			  dprintf "Rewriting middle node...\n";
			  let txlim, a, (g, _) =
			    solve_and_rewrite fact comp (txlim-1) g out changed in
			  dprintf "Rewrite of middle node completed\n";
			  let t, g = G.splice_tail g tail in
			  let rewritten = add_blocks (G.to_blocks g) rewritten in
			  propagate txlim h a t rewritten true)
		| GR.First f ->
                    match comp.first_in out f txlim with
                      | Dataflow a ->
			  let b = (f, tail) in
			  check_property_match fact a b;
			  rewrite_blocks txlim (b :: rewritten) bs changed
                      | Rewrite g -> impossf "rewriting a label in backward dataflow" in
            rewrite_next_block txlim in
    rewrite_blocks txlim [] (List.rev (G.postorder_dfs graph)) changed

  let rewrite' (fact, comp) g =
    let txlim = Tx.remaining () in
    let txlim', _, gc = solve_and_rewrite fact comp txlim g fact.init_info' false in
    Tx.decrement ~name:comp.name ~from:txlim ~to':txlim';
    gc
      
  let rewrite pass g = rewrite' (to_pass' pass) g
    
  let debug' s (f, comp) =
    let pr = Printf.eprintf in
    let fact dir node a = pr "%s %s for %s = %s\n" f.fact_name' dir node (s a) in
    let rewr node g = pr "%s rewrites %s to <not-shown>\n" comp.name node in
    let wrap f nodestring node txlim =
      let answer = f node txlim in
      let () = match answer with
	| Dataflow a -> fact "in " (nodestring node) a
	| Rewrite g  -> rewr (nodestring node) g in
      answer in
    let wrapout f nodestring out node txlim =
      fact "out" (nodestring node) out;
      wrap (f out) nodestring node txlim in
    let last_in = wrap comp.last_in (RS.rtl << GR.last_instr) in
    let middle_in = wrapout comp.middle_in (RS.rtl << GR.mid_instr) in
    let first_in  =
      let first = function GR.Entry -> "<entry>" | GR.Label ((u, l), _, _) -> l in
      wrapout comp.first_in first in
    f, { comp with last_in = last_in; middle_in = middle_in; first_in = first_in; }
      
  let debug s ((f, comp) as pass) =
    let (f', comp') = debug' s (to_pass' pass) in
    (to_fact f f', comp')
      
  let anal (fact, comp) =
    let wrap  f node txlim = Dataflow (f node) in
    let wrap2 f out node txlim = Dataflow (f out node) in
    fact,
    { name = comp.name; last_in = wrap comp.last_in;
      middle_in = wrap2 comp.middle_in; first_in = wrap2 comp.first_in; }
      
  let anal' = anal
    
  let a_t' (fact, comp) tx =
   let last_in l txlim =
     if txlim > 0 then
       match tx.last_in l with
       | Some g -> Rewrite g
       | None   -> Dataflow (comp.last_in l)
     else
       Dataflow (comp.last_in l) in
   let middle_in out m txlim =
     if txlim > 0 then
       match tx.middle_in out m with
       | Some g -> Rewrite g
       | None   -> Dataflow (comp.middle_in out m)
     else
       Dataflow (comp.middle_in out m) in
   let first_in out f txlim =
     if txlim > 0 then
       match tx.first_in out f with
       | Some g -> Rewrite g
       | None   -> Dataflow (comp.first_in out f)
     else
       Dataflow (comp.first_in out f) in
   fact, 
    { name    = Printf.sprintf "%s and %s" comp.name tx.name
    ; last_in = last_in; middle_in = middle_in; first_in  = first_in;
    }
  let a_t x tx = a_t' x tx
  let limit_anal (fact, comp) =
    fact,
    { name = comp.name
    ; first_in  = (fun i n _ -> comp.first_in i n)
    ; middle_in = (fun i n _ -> comp.middle_in i n)
    ; last_in   = (fun l _   -> comp.last_in l);
    }
  let limit_tx tx =
    { name = tx.name
    ; first_in = limit_fun tx.first_in
    ; middle_in = limit_fun tx.middle_in
    ; last_in = limit_last tx.last_in;
    }


  module XXX = 
  struct 
    let comp_with_exit comp exit_fact =
      let last_in l = match l with
        | GR.Exit -> Dataflow exit_fact
        | _ -> comp.last_in l in
      { comp with last_in = last_in } 

    let run f c e d blocks = run "backward" "comp.name" f c e (fun () b -> d b) () blocks

    let rec solve_graph fact comp graph exit_fact =
      general_backward fact (comp_with_exit comp exit_fact) graph;
      fact.get' GR.entry_uid 
	
    and general_backward fact comp graph =
      let changed = ref false in
      let update = update fact changed in
      let set_block_fact b =
        let block_in = 
          let rec head_in h out = match h with
            | GR.Head (h, m) ->
                let a = match comp.middle_in out m with
                  | Dataflow a -> a
                  | Rewrite g -> solve_graph fact comp g out in
                head_in h a
            | GR.First f -> 
                match comp.first_in out f with
                  | Dataflow a -> a
                  | Rewrite g -> solve_graph fact comp g out in
          let h, l = GR.goto_end (GR.unzip b) in
          let a = match comp.last_in l with
            | Dataflow a -> a
            | Rewrite g -> solve_graph fact comp g fact.init_info' in
          head_in h a in
        update (GR.id b) block_in in
      let blocks = List.rev (G.postorder_dfs graph) in
      run fact changed fact.init_info' set_block_fact blocks

    let rec solve_and_rewrite fact comp graph exit_fact changed =
      let a = solve_graph fact comp graph exit_fact in          (* pass 1 *)
      let g, c =                                                (* pass 2 *)
        backward_rewrite fact (comp_with_exit comp exit_fact) graph changed  
      in
      a, (g, c) 
	
    and backward_rewrite fact comp graph changed =
      let rec rewrite_blocks rewritten fresh changed : G.graph * bool =
        match fresh with
          | [] -> G.of_block_list rewritten, changed
          | b :: bs ->
              let rec rewrite_next_block () =
                let h, l = GR.goto_end (GR.unzip b) in
                match comp.last_in l with
                  | Dataflow a -> propagate h a (GR.Last l) rewritten changed
                  | Rewrite g ->
                      let a, (g, _) =
                        solve_and_rewrite fact comp g fact.init_info' changed in
                      let t, g = G.remove_entry g in
                      let rewritten = add_blocks (G.to_blocks g) rewritten in
                      (* continue at entry of g *)
                      propagate h a t rewritten true
              and propagate : GR.head -> 'a -> GR.tail -> GR.block list -> bool ->
            G.graph * bool =
                fun h out tail rewritten changed -> match h with
                  | GR.Head (h, m) -> (
                      match comp.middle_in out m with
                        | Dataflow a -> propagate h a (GR.Tail (m, tail)) rewritten changed
                        | Rewrite g ->
                            let a, (g, _) =
                              solve_and_rewrite fact comp g out changed in
                            let t, g = G.splice_tail g tail in
                            let rewritten = add_blocks (G.to_blocks g) rewritten in
                            propagate h a t rewritten true)
                  | GR.First f ->
                      match comp.first_in out f with
                        | Dataflow a ->
                            let b = (f, tail) in
                            rewrite_blocks (b :: rewritten) bs changed
                        | Rewrite g -> impossf "rewriting a label in backward dataflow" in
              rewrite_next_block () 
      in
      rewrite_blocks [] (List.rev (G.postorder_dfs graph)) changed


    module Unique = 
    struct
      module Map = 
      struct
        let empty = G.of_blocks Unique.Map.empty
        let union g1 g2 = G.of_blocks (Unique.Map.union (G.to_blocks g1) (G.to_blocks g2))
        let add k v g = G.of_blocks (Unique.Map.add k v (G.to_blocks g))
      end
    end

    let rec solve_and_rewrite fact comp graph exit_fact =
      let a = solve_graph fact comp graph exit_fact in          (* pass 1 *)
      let g =                                                (* pass 2 *)
        backward_rewrite fact (comp_with_exit comp exit_fact) graph in
      a, g 
    and backward_rewrite fact comp graph =
      let rec rewrite_blocks rewritten fresh =
        match fresh with
          | [] -> rewritten
          | b :: bs ->
              let rec rewrite_next_block () =
                let h, l = GR.goto_end (GR.unzip b) in
                match comp.last_in l with
                  | Dataflow a -> propagate h a (GR.Last l) rewritten
                  | Rewrite g ->
                      let a, g = solve_and_rewrite fact comp g fact.init_info' in
                      let t, g = G.remove_entry g in
                      let rewritten = Unique.Map.union g rewritten in
                      (* continue at entry of g *)
                      propagate h a t rewritten 
              and propagate : GR.head -> 'a -> GR.tail -> G.graph -> G.graph =
                fun h out tail rewritten -> match h with
                  | GR.Head (h, m) -> (
                      match comp.middle_in out m with
                        | Dataflow a -> propagate h a (GR.Tail (m, tail)) rewritten 
                        | Rewrite g ->
                            let a, g = solve_and_rewrite fact comp g out in
                            let t, g = G.splice_tail g tail in
                            let rewritten = Unique.Map.union g rewritten in
                            propagate h a t rewritten)
                  | GR.First f ->
                      match comp.first_in out f with
                        | Dataflow a ->
                            let b = (f, tail) in
                            rewrite_blocks (Unique.Map.add (GR.id b) b rewritten) bs 
                        | Rewrite g -> impossf "rewriting a label in backward dataflow" in
              rewrite_next_block () 
      in
      rewrite_blocks Unique.Map.empty (List.rev (G.postorder_dfs graph)) 
  end
end
  
module F = 
struct
  type 'a edge_fact_setter = (Zipcfg.uid -> 'a -> unit) -> unit

  type ('i, 'om, 'ol) computation =
      { 
	name       : string;
	middle_out : 'i -> Zipcfg.Rep.middle -> 'om;
	last_outs  : 'i -> Zipcfg.Rep.last   -> 'ol;
      } 
	
  type 'a analysis  = 'a fact  * ('a, 'a,                'a edge_fact_setter) computation
  type 'a analysis' = 'a fact' * ('a, 'a,                'a edge_fact_setter) computation
  type 'a transformation =     ('a, Zipcfg.graph option, Zipcfg.graph option) computation
  type 'a pass =
    'a fact *
      ('a, txlimit -> 'a answer, txlimit -> 'a edge_fact_setter answer) computation
  type 'a pass' =
    'a fact' *
      ('a, txlimit -> 'a answer, txlimit -> 'a edge_fact_setter answer) computation
      
  let to_analysis' (f, c) = (to_fact' f, c)
  let to_pass'     (f, c) = (to_fact' f, c)
  let comp_with_exit comp exit_fact_ref =
    let last_outs in' l txlimit = match l with
    | GR.Exit -> Dataflow (fun set -> exit_fact_ref := in')
    | _ -> comp.last_outs in' l txlimit in
    { comp with last_outs = last_outs } 
      
  let check_property_match fact u a =
    let old_a = fact.get' u in
    let new_a = fact.add_info' a old_a in
    if not (eqfact fact old_a new_a) then
      impossf "property '%s' changed after supposedly reaching fixed point"
        fact.fact_name'

  let run_anal' (fact, comp) ~entry_fact graph =
    let changed = ref false in
    let update = update fact changed in
    let set_successor_facts () b =
      let rec forward in' t = match t with
	| GR.Tail (m, t) -> forward (comp.middle_out in' m) t
	| GR.Last l -> comp.last_outs in' l update in
      let f, t = b in
      let blockname =
        match f with GR.Entry -> "<entry>" | GR.Label ((_, l), _, _) -> l in
      dprintf "Setting successor fact of block %s\n" blockname;
      forward (fact.get' (GR.fid f)) t in
    let blocks = G.postorder_dfs graph 
    in
    run "forward" comp.name fact changed entry_fact set_successor_facts ()
      blocks
      
  let run_anal anal ~entry_fact graph = run_anal' (to_analysis' anal) entry_fact graph
    
  let rec solve_graph fact comp txlim graph in_fact =
    let exit_fact_ref = ref fact.init_info' in
    let txlim =
      general_forward fact (comp_with_exit comp exit_fact_ref) txlim in_fact
	graph 
    in
    txlim, !exit_fact_ref

  and general_forward fact comp txlim entry_fact graph =
    let changed = ref false in
    let update = update fact changed in
    let set_successor_facts txlim b =
      let rec set_tail_facts txlim in' t = match t with
        | GR.Tail (m, t) ->
            (dprintf "Solving middle node %s\n" (RS.rtl (GR.mid_instr m));
            match comp.middle_out in' m txlim with
              | Dataflow a -> set_tail_facts txlim a t
              | Rewrite g -> 
                  let txlim, g = solve_graph fact comp (txlim-1) g in' in
                  set_tail_facts txlim g t)
        | GR.Last l -> 
            match comp.last_outs in' l txlim with
              | Dataflow setter -> (setter update; txlim)
              | Rewrite g -> fst (solve_graph fact comp (txlim-1) g in') in
      let f, t = b in
      let in' = match f with  (* CHANGE TO USE GR.id ? *)
	| GR.Entry -> entry_fact
	| GR.Label ((u, _), _, _) -> fact.get' u in
      set_tail_facts txlim in' t 
    in
    let blocks = G.postorder_dfs graph 
    in
    run "forward" comp.name fact changed entry_fact set_successor_facts txlim
    blocks
      
  let expectD = function
    | Dataflow d -> d
    | Rewrite _  -> impossf "expected dataflow result"
	
  let rec solve_and_rewrite modify fact comp txlim graph in_fact changed =
    let _ = solve_graph fact comp txlim graph in_fact in                   (* pass 1 *)
    let exit_ref = ref fact.init_info' in
    let txlim, g, c = forward_rewrite modify fact
      (comp_with_exit comp exit_ref) txlim graph in_fact changed 
    in
    txlim, !exit_ref, (g,c)
      
  and forward_rewrite modify (fact : 'a fact') comp txlim graph entry_fact changed =
    let rec rewrite_blocks txlim rewritten fresh changed : txlimit * G.graph * bool =
      match fresh with
	| [] -> txlim, G.of_block_list rewritten, changed
	| b :: bs ->
            let rec rewrite_next_block txlim =
              let f, t = b in
              let in' = match f with
		| GR.Entry -> entry_fact
		| GR.Label ((u, _), _, _) -> fact.get' u in
              propagate txlim (GR.First f) in' t rewritten changed
            and propagate :
		txlimit -> GR.head -> 'a -> GR.tail -> GR.block list -> bool ->
          txlimit * G.graph * bool =
              fun txlim h in' t rewritten changed -> match t with
		| GR.Tail (m, t) -> (
                    dprintf "Rewriting middle node %s\n" (RS.rtl (GR.mid_instr m));
                    match comp.middle_out in' m txlim with
                      | Dataflow a -> propagate txlim (GR.Head (h, m)) a t rewritten changed
                      | Rewrite g ->
			  dprintf "Rewriting middle node...\n";
			  let txlim, a, (g, _) =
			    if modify then
                              (txlim-1, expectD (comp.middle_out in' m 0), (g, false))
			    else solve_and_rewrite modify fact comp (txlim-1) g in' changed in
			  dprintf "Rewrite of middle node completed\n";
			  let g, h = G.splice_head h g in
			  propagate txlim h a t (add_blocks (G.to_blocks g) rewritten) true)
		| GR.Last l ->
                    dprintf "Rewriting last node %s\n" (RS.rtl (GR.last_instr l));
                    match comp.last_outs in' l txlim with
                      | Dataflow set ->
			  if not modify then set (check_property_match fact);
			  let b = GR.zip (h, GR.Last l) in
			  rewrite_blocks txlim (b :: rewritten) bs changed
                      | Rewrite g ->
			  (* could test here that [[exits g = exits (GR.Entry, GR.Last l)]] *)
			  if Cmm_debug.on "rewrite-last" then 
			    dprintf "Last node %s rewritten to:\n" (RS.rtl (GR.last_instr l));
			  let k (txlim, _, (g, changed)) =
                            let g = G.to_blocks (G.splice_head_only h g) in
                            rewrite_blocks txlim (add_blocks g rewritten) bs changed in
			  if modify then
			    k (txlim-1, expectD (comp.last_outs in' l 0), (g, true))
			  else
			    k (solve_and_rewrite modify fact comp (txlim-1) g in' true) 
	    in
            rewrite_next_block txlim 
    in
    rewrite_blocks txlim [] (G.postorder_dfs graph) changed

  let _ =
    Cmm_debug.register "rewrite-last" "show what happens when last node is rewritten forward"

  let rewrite_solved'' modify (fact, comp) ~entry_fact g =
    let txlim = Tx.remaining () in
    let txlim', g, changed = forward_rewrite modify fact comp txlim g entry_fact false in
    Tx.decrement ~name:comp.name ~from:txlim ~to':txlim';
    g, changed
      
  let rewrite_solved  pass ~entry_fact g =
    rewrite_solved'' false (to_pass' pass) entry_fact g
      
  let rewrite_solved' pass ~entry_fact g = rewrite_solved'' false pass entry_fact g
  let modify_solved'  pass ~entry_fact g = rewrite_solved'' true  pass entry_fact g

  let rewrite' (fact, comp) ~entry_fact g =
    let txlim = Tx.remaining () in
    let txlim', _, gc = solve_and_rewrite false fact comp txlim g entry_fact false in
    Tx.decrement ~name:comp.name ~from:txlim ~to':txlim';
    gc
      
  let rewrite pass ~entry_fact g = rewrite' (to_pass' pass) entry_fact g
    
  let debug' s (f, comp) =
    let pr = Printf.eprintf in
    let fact dir node a = pr "%s %s for %s = %s\n" f.fact_name' dir node (s a) in
    let setter dir node run_sets set =
      run_sets (fun u a -> pr "%s %s for %s = %s\n" f.fact_name' dir node (s a); set u a) in
    let rewr node g = pr "%s rewrites %s to <not-shown>\n" comp.name node in
    let wrap f nodestring wrap_answer in' node txlim =
      fact "in " (nodestring node) in';
      wrap_answer (nodestring node) (f in' node txlim)
    and wrap_fact n answer =
      let () = match answer with
      | Dataflow a -> fact "out" n a
      | Rewrite g  -> rewr n g in
      answer
    and wrap_setter n answer =
      match answer with
      | Dataflow set -> Dataflow (setter "out" n set)
      | Rewrite g  -> (rewr n g; Rewrite g) in
    let middle_out = wrap comp.middle_out (RS.rtl << GR.mid_instr) wrap_fact in
    let last_outs = wrap comp.last_outs (RS.rtl << GR.last_instr) wrap_setter in
    f, { comp with last_outs = last_outs; middle_out = middle_out; }
      
  let debug s ((f, comp) as pass) =
    let (f', comp') = debug' s (to_pass' pass) in
    ({fact_name = f'.fact_name'; init_info = f'.init_info'; add_info = f'.add_info';
    changed = f'.changed'; prop = f.prop}, comp')
      
  let limit_anal (fact, comp) =
    fact,
    { name = comp.name
    ; middle_out = (fun i n _ -> comp.middle_out i n)
    ; last_outs  = (fun i n _ -> comp.last_outs i n);
    }
      
  let limit_tx tx =
    { 
      name = tx.name;
      middle_out = limit_fun tx.middle_out;
      last_outs  = limit_fun tx.last_outs;
    }
  let anal (fact, comp) =
    let wrap f in' node txlim = Dataflow (f in' node) in
    fact,
    { name = comp.name;
    last_outs = wrap comp.last_outs; middle_out = wrap comp.middle_out; }
      
  let anal' = anal
  let a_t' (fact, comp) tx =
   let last_outs in' l txlim =
     if txlim > 0 then
       match tx.last_outs in' l with
       | Some g -> Rewrite g
       | None   -> Dataflow (comp.last_outs in' l)
     else
       Dataflow (comp.last_outs in' l) in  
   let middle_out in' m txlim =
     if txlim > 0 then
       match tx.middle_out in' m with
       | Some g -> Rewrite g
       | None   -> Dataflow (comp.middle_out in' m)
     else
       Dataflow (comp.middle_out in' m) in
   fact, 
   { name = Printf.sprintf "%s and %s" comp.name tx.name;
     last_outs = last_outs; middle_out = middle_out;
   }
  let a_t x tx = a_t' x tx
end
