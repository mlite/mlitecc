module B  = Bits
module G  = Zipcfg
module GR = Zipcfg.Rep
module CT = Ctypes
module PA = Preast2ir
module RM = Register.Map
module RS = Register.Set
module R  = Rtl
module RP = Rtl.Private
module RU = Rtlutil
module W  = Rtlutil.Width
module Dn = Rtl.Dn
module Up = Rtl.Up
module S  = Spans
module T  = Target
module IM = Map.Make (struct type t = int let compare = compare end)

type tgt   = Preast2ir.tgt
type spans = S.t
exception DeadValue

let imposs = Impossible.impossible
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimp  = Impossible.unimp

(* Don't touch the user-defined spans -- not necessary *)
let upd_spans upd spans =
  let spans = S.expose spans in
  let maybe_upd l = match l with
    | Some l -> (try Some (upd l) with DeadValue -> None)
    | None   -> None 
  in
  spans.S.inalloc  <- upd spans.S.inalloc;
  spans.S.outalloc <- upd spans.S.outalloc;
  spans.S.ra       <- upd spans.S.ra;
  spans.S.csregs   <- List.map (fun (r,l) -> (r, maybe_upd l)) spans.S.csregs;
  spans.S.conts    <- List.map (fun (pc,sp,vars) -> (pc, upd sp, vars)) spans.S.conts;
  spans.S.sds      <- List.map upd spans.S.sds;
  Array.iteri (fun i l -> spans.S.vars.(i) <- maybe_upd l) spans.S.vars

let upd_all_spans upd g = Zipcfg.iter_spans (upd_spans upd) g
  
let emit_as_asm (PA.T target) asm ~procsym cfg =
  let add_spans (first, _ as b) _ spans =
    let (_, last) = GR.goto_end (GR.unzip b) in
    let add spans (_, lbl) = function Some ss -> (lbl, ss) :: spans | None -> spans in
    let spans = match first with
      | GR.Label (lbl, _, ss) -> add spans lbl (!ss)
      | GR.Entry              -> spans in
    match last with
      | GR.Call { GR.cal_spans = ss; GR.cal_contedges = {G.node = n} :: _ } ->
          add spans n ss
      | _ -> spans 
  in
  let spans = List.rev (G.fold_layout add_spans [] cfg) in
  let wordsize   = target.T.wordsize in
  let to_bits  i = Bits.S.of_int i wordsize  in 
  let zero       = Bits.zero wordsize in
  let one        = to_bits 1 in
  let simplify_loc loc =
    let vfp     = target.T.vfp in
    let check w = if w mod wordsize <> 0 || w < 0 then
      unimp (Printf.sprintf "unsupported size or alignment %d" w) in
    let is_vfp_offset e =
      let rec exp e = match e with
        | RP.Fetch (l,_)  -> loc l
        | RP.App (_,exps) -> List.fold_left (fun v e -> v || exp e) false exps
        | RP.Const _      -> false
      and loc l = if Vfp.is_vfp l then true else match l with
        | RP.Mem   (_,_,e,_) -> exp e
        | RP.Slice (_,_,l)   -> loc l
        | _                  -> false in
      exp e in
    let rec flatten offset l = match l with
      | RP.Mem ((_,_,mcell as ms),_,e,ass) when is_vfp_offset e ->
          let w = Rtlutil.Width.exp (Up.exp e) in
          check offset; check w;
          (*V.eprintf verb "Adding an offset %d of width %d\n" offset w;*)
          let e' = Rtlutil.add w (R.app (R.opr "sub" [w]) [Up.exp e; vfp])
            (R.bits (Bits.S.of_int offset w) w) in
          R.mem (Up.assertion ass) ms (Cell.to_count mcell w) (Simplify.exp e')
      | RP.Mem (ms,w,e,ass) -> (* not a VFP offset *)
          R.mem (Up.assertion ass) ms w (Simplify.exp (Up.exp e))
      | RP.Reg r ->
          let w = Register.width r in
          check w; check offset;
          let l_up = Up.loc l in
          if offset <> 0 then
            R.slice w offset l_up
          else l_up
      | RP.Slice (_,i,l)       -> flatten (offset + i) l
      | RP.Var _ | RP.Global _ -> Up.loc l in
    flatten 0 (Dn.loc loc) 
  in
  let reg_ix (((s,_,_), i, _) as r) =
    try  RM.find r (snd target.T.reg_ix_map)
    with Not_found -> imposs "Register not found in map" in
  let emit_dead_loc      () = asm#value zero 
  in
  let emit_link_const sym   = asm#addr (Reloc.of_sym sym wordsize) 
  in
  let offset_w = wordsize - 1 
  in
  let emit_offset bits = (* possibly an offset *)
    let value    = Bits.S.to_int bits in
    (try ignore (Bits.S.of_int value offset_w)
    with Bits.Overflow -> imposs "Offset from vfp is too large for run-time encoding");
    asm#value (B.Ops.or' (B.Ops.shl  one (to_bits offset_w))
      (B.Ops.shrl (B.Ops.shl (to_bits value) one) one)) 
  in
  let emit_int i = asm#value (Bits.U.of_int i wordsize) 
  in
  let emit_reg ix offset =
    let offset_w = wordsize - 2 in
    let w'       = offset_w / 2 in
    (try ignore (Bits.S.of_int ix w'); ignore(Bits.S.of_int offset w')
    with Bits.Overflow -> imposs "Reg ix or offset is too large for run-time encoding");
    asm#value (B.Ops.or' (B.Ops.shl one (to_bits offset_w))
      (B.Ops.or' (B.Ops.shl (to_bits offset) (to_bits w'))
        (to_bits ix))) 
  in
  let emit_loc l =
    let mklink kind s w = Up.exp (RP.Const (RP.Link(s, kind, w))) in
    match Dn.loc (simplify_loc l) with
      | RP.Mem (_,_, RP.Const (RP.Link (sym,kind,_)),_)  ->
	  emit_link_const (sym, mklink kind)
      | RP.Mem (_,_, RP.Const (RP.Bits bs),_)            -> emit_offset   bs
      | RP.Reg r                                         -> emit_reg  (reg_ix r) 0
      | RP.Slice (_, i, RP.Reg r)                        -> emit_reg  (reg_ix r) i
      | _ -> impossf "unexpected location for span data: %s" (RU.ToString.loc l)  in
  let emit_maybe_loc l = match l with
    | Some l -> emit_loc l
    | None   -> emit_dead_loc () in

  let nonredundant (reg, l) = match l with
    | Some r -> not (RU.Eq.loc (Dn.loc r) (RP.Reg reg))
    | None   -> true 
  in
  let emit_csreg (reg, l) =
    if nonredundant (reg, l) then
      (emit_int (reg_ix reg); emit_maybe_loc l) 
  in
  let size_cont_block conts =
    let n = List.length conts in
    let nvars = List.fold_left (fun rst (_,_,vars) -> List.length vars + rst) 0 conts in
    1 + 2 * nvars + 4 * n 
  in
  let emit_conts conts =
    let n = List.length conts in
    emit_int n;
    ignore (List.fold_left (fun offset (_,_,vs) -> emit_int offset;
      offset + 2 * List.length vs + 3)
      (n+1) conts);
    let emit_cont_block (pc, sp, vars) =
      emit_int (List.length vars);
      asm#addr (Reloc.of_sym (asm#local pc, Rtl.datasym) wordsize);
      emit_loc sp;
      List.iter (fun (h,i,a) -> emit_int i; emit_int (Ctypes.enum_int h)) vars 
    in
    List.iter emit_cont_block conts 
  in
  let sd_label = asm#local (Idgen.label "stackdata") 
  in
  let emit_sd_spans () =
    match spans with
      | (_, spans)::_ ->
          let spans = S.expose spans in
          asm#section "pcmap_data";
          asm#label   sd_label;
          asm#value   (Bits.U.of_int (List.length spans.S.sds) target.T.wordsize);
          List.iter   emit_loc spans.S.sds
      | [] -> () 
  in
  let emit_site_spans (stop_l, spans) =
    let spans = S.expose spans in
    let frame_label = asm#local (Idgen.label "frame") in
    asm#section "pcmap";
    asm#addr (Reloc.of_sym (asm#local stop_l, Rtl.codesym) wordsize);
    asm#addr (Reloc.of_sym (frame_label, Rtl.datasym)      wordsize);
    asm#section "pcmap_data";
    asm#label frame_label;
    emit_loc spans.S.inalloc;
    emit_loc spans.S.outalloc;
    emit_loc spans.S.ra;
    asm#addr (Reloc.of_sym (sd_label, Rtl.datasym) wordsize);
    emit_int (List.length (List.filter nonredundant spans.S.csregs));
    emit_int (Array.length spans.S.vars);
    let users =
      let unsorted = List.map (fun (n,e) -> (Bits.S.to_int n,e)) spans.S.users in
      let compare ((n : int), _) ((n' : int), _) = compare n n' in
      List.stable_sort compare unsorted in
    let max_span_ix = match users with [] -> -1 | _ :: _ -> fst (Auxfuns.last users) in
    let rec emit_spans_from n spans =
      if n <= max_span_ix then
        match spans with
          | (n', e) :: tail when n > n' -> emit_spans_from n tail (* skip duplicate span *)
          | (n', e) :: tail when n = n' -> asm#addr e; emit_spans_from (n+1) tail
          | _                           -> emit_int 0; emit_spans_from (n+1) spans in
    emit_int (max_span_ix + 1);
    emit_int (size_cont_block spans.S.conts);
    List.iter emit_csreg spans.S.csregs;
    Array.iter emit_maybe_loc spans.S.vars;
    emit_conts spans.S.conts;
    emit_spans_from 0 users; 
  in
  let prev_section = asm#current 
  in
  emit_sd_spans ();
  List.iter emit_site_spans spans;
  asm#section prev_section
    
let user_spans spans = (S.expose spans).S.users
let stackdata  spans = (S.expose spans).S.sds
let print_reg_map (PA.T tgt) =
  let (n, map) = tgt.T.reg_ix_map in
  Printf.printf "Target has %d registers:\n" n;
  RM.iter (fun ((s,_,_),i,_) n -> Printf.printf "  %c[%d] -> %d\n" s i n) map
    
let emit_global_properties target (asm:'c Asm.assembler) =
  let cc = Call.get_cc target "C--" in
  let growth = match cc.Call.stack_growth with Memalloc.Up -> 1 | Memalloc.Down -> -1 in
  let wordsize = target.Target.wordsize in
  let memsize  = target.Target.memsize in
  let sym = asm#export "Cmm_stack_growth" in
  asm#section "data";
  asm#align (wordsize / memsize);
  asm#label sym;
  asm#value (Bits.S.of_int growth wordsize)
  

