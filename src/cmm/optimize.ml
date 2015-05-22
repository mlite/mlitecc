module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Proc
module PA = Preast2ir
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn
module Up = Rtl.Up
module SS = Strutil.Set
module T  = Target

let impossf fmt = Printf.kprintf Impossible.impossible fmt
  
let not_null = function [] -> false | _ :: _ -> true
  
let simplify_exps _ (g, proc) =
  let changed = ref false in
  let g = G.map_rtls (fun r -> changed := true; Simplify.rtl r) g in
  (g, proc), !changed

let trim_unreachable_code _ (g, proc) =
  let g' = G.postorder_dfs g in
  let changed = List.length g' < Unique.Map.size (G.to_blocks g) in
  (G.of_block_list g', proc), changed

    
let collapse_branch_chains _ = Impossible.unimp "new optimizer"
  
type limit = { mutable lim : int }
    
let remove_nops _ (g, proc) =
  let changed = ref false in
  let is_nop rtl =
    let unneeded = function
      | (_, RP.Kill _) -> true
      | (_, RP.Store (l, RP.Fetch(l', w'), w)) -> (* assert (w=w'); *)
          if w<>w' then
            impossf "width of fetch and store don't match in %s" (RU.ToString.rtl rtl)
          else
            RU.Eq.loc l l'
      | _ -> false in
    let RP.Rtl effs = Dn.rtl rtl in
    List.for_all unneeded effs in
  let remaining = Tx.remaining() in
  let tx = { lim = remaining } in
  let block (f, t) =
    let rec finish t h = match t with
    | GR.Last _ -> GR.zipht h t
    | GR.Tail (m, t) when tx.lim > 0 && GR.is_executable m && is_nop (GR.mid_instr m) ->
        (changed := true; tx.lim <- tx.lim - 1; finish t h)
    | GR.Tail (m, t) -> finish t (GR.Head (h, m)) in
    finish t (GR.First f) in
  let g = G.of_blocks (Unique.Map.map block (G.to_blocks g)) in
  let () = if tx.lim < remaining then Tx.decrement "remove_nops" remaining tx.lim 
  in
  (g, proc), !changed
    
let badrtl r =
  Printf.eprintf "non-target RTL: %s\n" (RU.ToString.rtl r)
    (* if you want to do the following, extend Target.t with a to_asm function *)
    (*  Printf.eprintf "x86 rec says %s\n" (X86rec.M.to_asm r) *)
    
let validate _ (g, proc) =
  let PA.T tgt = proc.P.target in
  let check r = if not (tgt.T.is_instruction r) then badrtl r else () in
  let first _ = () in
  let middle m = if GR.is_executable m then check (GR.mid_instr m) in
  let last l = check (GR.last_instr l) in
  G.iter_nodes first middle last g;
  (g, proc), false
