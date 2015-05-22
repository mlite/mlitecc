include Common

type reg  = Register.t
type rset = Register.Set.t
type temp = Register.t
type loc_pair  = { reg  : Register.t option; mem  : Rtlutil.aloc option }
type loc_pair' = { reg' : Register.t option; mem' : bool }
type use_dist = Use of (int * int)
type hwdef_num = (int * int option) (* (n, m):  nth def in basic block m *)
type hwdef =
  | Copy    of (hwdef_num * int * Register.t * hwdef)
  | NonCopy of int
      
type def_dist = 
  | HW of hwdef
  | Temp of (int * int)
		  
module RM = Register.Map
module RS = Register.Set

type t = loc_pair  RM.t * temp RM.t
type y = loc_pair' RM.t * temp list RM.t

let impossf fmt = Printf.kprintf Impossible.impossible fmt
let empty_pair       = {reg = None; mem = None}
let def_reg pair r   = {pair with reg = r}
let def_mem pair m   = {pair with mem = m}
let pair_map_find    t   m = try RM.find t m with Not_found -> empty_pair


let empty            = (RM.empty, RM.empty)
let emptyy           = (RM.empty, RM.empty)
let is_empty' (vm, lm) = RM.is_empty vm && RM.is_empty lm
  
let remove_reg t r (vm, lm) =
  let lm' = RM.remove r lm in
  try match (RM.find t vm).mem with
    | None   -> (RM.remove t vm, lm')
    | Some m as m'-> (RM.add t {reg = None; mem = m'} vm, lm')
  with Not_found -> Impossible.impossible "DLS: Tried to remove unknown reg"
    
let filt r lst = List.filter (fun r' -> not (Register.eq r r')) lst
  
let lm_rem' r t lm = 
  try RM.add r (filt t (RM.find r lm)) lm 
  with Not_found -> lm
    
let lm_add' r t lm = 
  RM.add r (t :: try RM.find r lm with Not_found -> []) lm
    
let remove_reg' t (vm, lm) =
  try 
    let lp = RM.find t vm in
    match lp.reg' with
      | Some r -> let lm' = lm_rem' r t lm in
	if lp.mem' then (RM.add t {reg' = None; mem' = true} vm, lm')
	else (RM.remove t vm, lm')
      | None -> Impossible.impossible "DLS: Tried to remove unknown reg"
  with Not_found -> Impossible.impossible "DLS: Tried to remove unknown reg"
    
let remove_mem t _ (vm, lm) =
  try match (RM.find t vm).reg with
      | None   -> (RM.remove t vm, lm)
      | Some r as r' -> (RM.add t {reg = r'; mem = None} vm, lm)
  with Not_found -> Impossible.impossible "DLS: Tried to remove unknown mem"
    
let remove_mem' t (vm, lm as m) =
  try match (RM.find t vm).reg' with
      | None -> (RM.remove t vm, lm)
      | Some r as r' -> (RM.add t {reg' = r'; mem' = false} vm, lm)
  with Not_found -> m
    
let add_reg t r (vm, lm) =
  try let lp = RM.find t vm in
      let lm' = match lp.reg with
                | Some r_old -> RM.remove r_old lm
                | None -> lm in
      (RM.add t {lp with reg = Some r} vm, RM.add r t lm')
  with Not_found -> (RM.add t {reg = Some r; mem = None} vm, RM.add r t lm)
    
let add_reg' t r (vm, lm) =
  try let lp = RM.find t vm in
  let lm' = match lp.reg' with
    | Some r_old -> lm_rem' r_old t lm
    | None -> lm in
  (RM.add t {lp with reg' = Some r} vm, lm_add' r t lm')
  with Not_found -> (RM.add t {reg' = Some r; mem' = false} vm, lm_add' r t lm)

let add_mem t m (vm, lm) =
  try (RM.add t {(RM.find t vm) with mem = Some m} vm, lm)
  with Not_found -> (RM.add t {reg = None; mem = Some m} vm, lm)
    
let add_mem' t (vm, lm) =
  try (RM.add t {(RM.find t vm) with mem' = true} vm, lm)
  with Not_found -> (RM.add t {reg' = None; mem' = true} vm, lm)

let spill t m (vm, lm) =
  let fail () = Impossible.impossible "DLS: can not spill temp" in
  try match RM.find t vm with
      | {reg = Some r} -> (RM.add t {reg = None; mem = Some m} vm, RM.remove r lm)
      | _ -> fail ()
  with Not_found -> fail ()
    
let spill' t (vm, lm) =
  let fail () = Impossible.impossible "failed to spill temp" in
  try match RM.find t vm with
    | {reg' = Some r} -> (RM.add t {reg' = None; mem' = true} vm,
      lm_rem' r t lm)
      | _ -> fail ()
  with Not_found -> fail ()

let var_locs'  (vm, _) temp = RM.find temp vm
let var_locs'' (vm, _) temp = RM.find temp vm
let temp_loc' (vm, _) t = try (RM.find t vm).reg' with Not_found -> None
let reg_contents  (_, lm) reg = try Some (RM.find reg lm) with Not_found -> None
let reg_contents' (_, lm) reg = try RM.find reg lm with Not_found -> []
  
let fold f_r f_m (vm, _) zero =
  let app f t v z = match v with Some s -> f t s z | None -> z in
  RM.fold (fun t lp z -> app f_m t lp.mem (app f_r t lp.reg z)) vm zero
    
let fold' f_r f_m (vm, _) zero =
  let app  f t v z = match v with Some s -> f t s z | None -> z in
  let app' f t v z = match v with true -> f t z | false -> z in
  RM.fold (fun t lp z -> app' f_m t lp.mem' (app f_r t lp.reg' z)) vm zero

let filter f (vm, _ as maps) =
  let remove temp {reg = reg} (vm, lm as maps) =
    if f temp then maps
    else (RM.remove temp vm, match reg with Some r -> RM.remove r lm | None -> lm) in
  RM.fold remove vm maps
    
let printReg ((s,_,_), i, Register.C n) =
  if n = 1 then Printf.sprintf "%c%d" s i
  else Printf.sprintf "%c%d:%d" s i n
    
let print msg (_, lm as map) =
  ( Printf.eprintf "%s\nOneLocVarMap:\n" msg
  ; fold (fun t (r:Register.t) () -> Printf.eprintf "%s -> %s\n" (printReg t) (printReg r))
         (fun t m () -> Printf.eprintf "%s -> mem_loc\n" (printReg t))
         map ()
  ; Printf.eprintf "  Reverse map:\n"
  ; RM.iter (fun r t -> Printf.eprintf "  %s -> %s\n" (printReg r) (printReg t)) lm
  ; flush stderr
  )
    
let print' msg (_, lm as map) =
  ( Printf.eprintf "%s\nOneLocVarMap:\n" msg
  ; fold' (fun t (r:Register.t) () -> Printf.eprintf "%s -> %s\n" (printReg t) (printReg r))
          (fun t () -> Printf.eprintf "%s -> mem_loc\n" (printReg t))
          map ()
  ; Printf.eprintf "  Reverse map:\n"
  ; RM.iter (fun r ts -> Printf.eprintf "  %s -> " (printReg r);
                         List.iter (fun t -> Printf.eprintf "%s, " (printReg t)) ts;
                         Printf.eprintf "\n") lm
  ; flush stderr
  )
    
let sync_maps inmap (om_vm,_ as outmap) =
  (RM.fold (fun temp loc_pair (im_vm',_ as inmap') ->
    match loc_pair.mem with
      | Some m ->
          if Auxfuns.Option.is_none (pair_map_find temp im_vm').mem then
            add_mem temp m inmap'
          else
            inmap'
      | None   -> inmap')
    om_vm inmap, outmap)
    
let free_reg_inregs defs live_in live_out (_, lm) regs_used t_live_past_uses r =
  let not_in set = not (RS.mem r set) in
  not (RM.mem r lm) && not_in regs_used && not_in live_in &&
    not (t_live_past_uses && (RS.mem r defs || RS.mem r live_out))

let free_reg_outregs defs live_out (_, lm) regs_used _ r =
  let not_in set = not (RS.mem r set) in
  not (RM.mem r lm) && not_in regs_used && not_in defs && not_in live_out


(* If we have found a register to spill, we may only be able to use it for the
   live-in part of the instruction, or it may be good long-term. *)
type spill = 
  | NoneYet
  | LiveInOnly of Register.t
  | LongTerm   of Register.t
      
let alloc_inreg allregs defs live_in live_out (_, lm)
    regs_used live_past_use t =
  (*printTempList "alloc_outreg allregs: " allregs;
    printTempSet "regs_used: " regs_used;
    printTempSet "live_out: " live_out;
    printTempSet "defs: " defs;
  *)
  let rec try_regs rs bst = match rs with
    | [] -> (match bst with LongTerm r -> r | LiveInOnly r -> r
        | NoneYet -> impossf "no register available for temp %s" (printReg t))
    | r::rs ->
        let liveinonly r = match bst with NoneYet -> LiveInOnly r | _ -> bst in
        let longterm r = match bst with LongTerm _ -> bst | _ -> LongTerm r in
        let not_in set = not (RS.mem r set) in
        if not_in regs_used && not_in live_in &&
          (not live_past_use || not_in live_out && not_in defs) then
            if RM.mem r lm then try_regs rs (longterm r)
            else r
              (* won't be live out: *)
        else if not_in regs_used && not_in live_in then try_regs rs (liveinonly r)
        else try_regs rs bst in
  try_regs allregs NoneYet

let alloc_outreg allregs defs live_out (_, lm) regs_used _ t =
  (*printTempList "alloc_outreg allregs: " allregs;
    printTempSet "regs_used: " regs_used;
    printTempSet "live_out: " live_out;
    printTempSet "defs: " defs;
  *)
  let rec try_regs rs bst = match rs with
    | [] -> (match bst with LongTerm r -> r | LiveInOnly r -> r
        | NoneYet -> impossf "no register available for temp %s" (printReg t))
    | r::rs ->
        let longterm r = match bst with LongTerm _ -> bst | _ -> LongTerm r in
        let not_in set = not (RS.mem r set) in
        if not_in regs_used && not_in live_out && not_in defs then
          if RM.mem r lm then try_regs rs (longterm r)
          else r
        else try_regs rs bst 
  in
  try_regs allregs NoneYet
    
let join (m1, _) (m2, _) =
  let upd t lp1 lp2 maps = match (lp1, lp2) with
    | ({reg' = Some r1; mem' = false}, {reg' = Some r2; mem' = false})
	when Register.eq r1 r2 -> add_reg' t r1 maps
    | ({reg' = Some r1; mem' = true}, {reg' = Some r2; mem' = true})
	when Register.eq r1 r2 -> add_mem' t (add_reg' t r1 maps)
    | _ -> add_mem' t maps 
  in
  RM.fold (fun temp lp z -> upd temp lp (try RM.find temp m2 with Not_found -> lp) z)
    m1 empty

let eq' ~old:(m2, _) ~new':(m1, _) =
  let is_eq m' t lp1 b =
    b && try let lp2 = RM.find t m' in
    (match (lp1.reg', lp2.reg') with
      | (Some r1, Some r2) -> Register.eq r1 r2
      | (None, None) -> true
      | _ -> false) &&
      (lp1.mem' =:= lp2.mem')
    with Not_found -> false 
  in
  RM.fold (is_eq m2) m1 true (* only equality on a subset... *)
