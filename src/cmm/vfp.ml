module D  = Dataflow
module G  = Zipcfg
module GR = Zipcfg.Rep
module P  = Property
module R  = Rtl
module RP = Rtl.Private
module RU = Rtlutil
module Dn = Rtl.Dn
module Up = Rtl.Up

let impossf fmt = Printf.kprintf Impossible.impossible fmt

let mk w = Rtl.fetch (Rtl.reg (('V', Rtl.Identity, Cell.of_size w), 0, Rtl.C 1)) w
let is_vfp = function
  | RP.Reg (('V', _, _), 0, _) -> true
  | _ -> false
let unknown = max_int
let matcher = { P.embed = (fun a -> P.Vfp a);
                P.project = (function P.Vfp a -> Some a | _ -> None);
                P.is = (function P.Vfp a -> true | _ -> false);
              }

let prop = Unique.Prop.prop matcher

let fact sp = {
  D.fact_name = "vfp location";
  D.init_info = (sp, unknown);
  D.add_info =
    (fun (vfp, k as a) (vfp', k' as a') ->
      let () = Cmm_debug.eprintf "vfp" "updating vfp dataflow fact\n" in
      if k = k' then a
      else if k = unknown then a'
      else if k' = unknown then a
      else (* accept inconsistency at exit and entry points to cut and unwind contn's *)
        if false (* G.kind node =*= G.Exit || G.is_non_local_join node *) then a
        else impossf "inconsistent stack-pointer location: %d and %d" k k');
  D.changed = (fun ~old:(_, k) ~new':(_, k') -> k <> k');
  D.prop = prop;
}
let replace_with ~sp =
  let w = RU.Width.loc sp in
  let spval = R.fetch sp w in
  let sp = Dn.loc sp in
  let sp_plus = RU.addk w spval in
  let sp_plus k = Dn.exp (sp_plus k) in
  let replace_vfp value =
    let is_vfp = function
      | RP.Fetch (v, _) -> is_vfp v
      | _ -> false in
    RU.Subst.exp ~guard:is_vfp ~map:(fun _ -> value) in
  let rec note_sp_changes rtl vfp k =
    if RU.Exists.Loc.rtl (RU.Eq.loc sp) rtl then
      let k' = find_k'_added_to_sp rtl in
      let post_k = k - k' in
      sp_plus post_k, post_k
    else
      vfp, k
  and find_k'_added_to_sp (RP.Rtl ges) =
    let rec find found k' = function
      | [] -> k' (* could be zero if only assignment is guarded *)
      | (RP.Const (RP.Bool b), RP.Store (sp', e, w)) :: ges when RU.Eq.loc sp sp' ->
          if not b then
            find found k' ges
          else if found then
            Impossible.impossible "multiple assignments to stack pointer"
          else
            (match e with
            | RP.App (("add", [_]), [RP.Fetch(sp', _); RP.Const (RP.Bits k')])
              when RU.Eq.loc sp' sp -> find true (Bits.S.to_int k') ges
            | RP.App (("add", [_]), [RP.Const (RP.Bits k'); RP.Fetch(sp', _)])
              when RU.Eq.loc sp' sp -> find true (Bits.S.to_int k') ges
            | RP.App (("sub", [_]), [RP.Fetch(sp', _); RP.Const (RP.Bits k')])
              when RU.Eq.loc sp' sp -> find true (- (Bits.S.to_int k')) ges
            | RP.Fetch (sp', _)
              when RU.Eq.loc sp' sp -> find true 0 ges
                    |     _ -> Printf.printf "illegal sp assignment: sp := %s\n" (RU.ToString.exp (Up.exp e)) ; 0)
  (* RRO temp
            | _ -> Impossible.impossible ("sp assigned other than sp + k: " ^
                                          RU.ToString.exp (Up.exp e)))
  *)
      | (g, RP.Store (sp', e, w)) :: ges when RU.Eq.loc sp sp' ->
          Impossible.impossible ("assigned sp with nontrivial guard " ^
                                 Rtlutil.ToString.exp (Rtl.Up.exp g))
      | (g, RP.Kill sp') :: ges when RU.Eq.loc sp sp' -> 
          Impossible.impossible "killed sp"
      | _ :: ges -> find found k' ges in
    find false 0 ges in 
  let fact = fact (Dn.exp spval) in
  let middle_out (vfp, k) m txlim =
    let simp rtl =
      let str = Rtlutil.ToString.rtl in
      let rtl' = Simplify.Unsafe.rtl rtl in
      Cmm_debug.eprintf "vfp"
        "Simplified stack adjustment from %s to %s\n" (str rtl) (str rtl');
      rtl' in
    let i = GR.mid_instr m in
    let down = Dn.rtl i in
    if RU.Exists.Loc.rtl is_vfp down then
      let simp = match m with GR.Stack_adjust _ -> simp | _ -> Simplify.rtl in
      let rtl = simp (replace_vfp vfp i) in
      D.Rewrite (G.single_middle (G.new_rtlm rtl m))
    else
      D.Dataflow (note_sp_changes down vfp k) in

  let last_outs (vfp, k) l txlim =
    let upd vfp rtl = Simplify.rtl (replace_vfp vfp rtl) in
    let set_succs (vfp, k as a) =
      let upd_cedges ces =
        let upd ce = note_sp_changes (Dn.rtl ce.G.assertion) vfp k in
        D.Dataflow (fun set -> List.iter (fun ce -> set (fst ce.G.node) (upd ce))
                                         ces) in
      match l with
      | GR.Cut  (_, ces, _) -> upd_cedges ces
      | GR.Call c           -> upd_cedges c.GR.cal_contedges
      | _ -> D.Dataflow (fun set -> GR.iter_succs (fun u -> set u a) l) in
    let rewrite_assertions l fail =
      let upd_cedges vfp k cedges succ =
        if List.exists (fun ce -> RU.Exists.Loc.rtl is_vfp (Dn.rtl ce.G.assertion))
                       cedges then
          succ (List.map (fun ce -> {ce with G.assertion = upd vfp ce.G.assertion})
                         cedges)
        else fail () in
      match l with
      | GR.Cut (i, ces, r) ->
          let upd ces = D.Rewrite (G.single_last (GR.Cut (i, ces, r))) in
          upd_cedges vfp k ces upd
      | GR.Call c ->
          let upd ces =
            D.Rewrite (G.single_last (GR.Call { c with GR.cal_contedges = ces })) in
          let vfp, k = note_sp_changes (Dn.rtl (GR.last_instr l)) vfp k in
          upd_cedges vfp k c.GR.cal_contedges upd
      | _ -> fail () in
    let i = GR.last_instr l in
    let down = Dn.rtl (GR.last_instr l) in
    let propagate () = match l with
      | GR.Cut _ ->  set_succs (vfp, k)
      | _        ->  set_succs (note_sp_changes down vfp k) in
    if RU.Exists.Loc.rtl is_vfp down then
      let l = G.new_rtll (upd vfp i) l in
      rewrite_assertions l (fun () -> D.Rewrite (G.single_last l))
    else rewrite_assertions l propagate in
  let repl =
    { D.F.name = "replace vfp";
      D.F.middle_out = middle_out; D.F.last_outs = last_outs; } in
  D.F.rewrite (fact, repl) ~entry_fact:(Dn.exp spval, 0)
let () = Cmm_debug.register "vfp" "stack adjustments for virtual frame pointer"
let mk_space w = 
    { Space.space = ('V', Rtl.Identity, Cell.of_size w)
    ; Space.doc = "holds the virtual frame pointer"
    ; Space.indexwidth = w
    ; Space.indexlimit = None
    ; Space.widths = [w]
    ; Space.classification = Space.Fixed
    }
