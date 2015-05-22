module R   = Rtl
module RP  = Rtl.Private
module RU  = Rtlutil
module Up  = Rtl.Up
module Dn  = Rtl.Dn
module S   = RU.ToString

let () = Cmm_debug.register "avail" "available expressions"

let debug = Cmm_debug.on "avail"

type t = Unknown | Known of (RP.loc * RP.exp) list

let unknown = Unknown

let smaller ~old ~new' =
  match old with
    | Unknown -> (match new' with Unknown -> false | Known _ -> true)
    | Known olds ->
	(match new' with
	  | Unknown -> false
	  | Known news -> List.length news < List.length olds)
	  
let in_loc t l = match t with
  | Unknown -> None
  | Known pairs -> try Some (List.assoc l pairs) with Not_found -> None

let has_exp t e = match t with
  | Unknown -> None
  | Known pairs ->
      try Some (fst (List.find (fun (_, e') -> RU.Eq.exp e e') pairs))
      with Not_found -> None
	
let join a a' = match a, a' with
  | Unknown, a' -> a'
  | a, Unknown  -> a
  | Known ps, Known ps' ->
      let primed (l, e) =
	List.exists (fun (l', e') -> RU.Eq.loc l l' && RU.Eq.exp e e') ps' 
      in
      Known (List.filter primed ps)
	
let locs_killed rtl =
  let add l locs = if List.mem l locs then locs else l :: locs in
  RU.FullReadWriteKill.fold ~write:add ~kill:add ~read:(fun _ locs -> locs)
    rtl []
    
let subst_exp t ls e = match t, ls with
  | _, [] -> e
  | Known pairs, _ :: _ ->
      RU.Subst.Fetch.exp' ~guard:(fun l -> List.mem l ls)
	~fetch:(fun l w -> List.assoc l pairs) e
  | Unknown, _ :: _ -> 
      Impossible.impossible "substitution with no available expressions"
	
let invalidate_pairs alocs aexps pairs =
  let invalidated (l, r) =
    List.exists (fun aloc -> aloc l) alocs || List.exists (fun aexp -> aexp r) aexps in
  if List.exists invalidated pairs then
    List.filter (fun p -> not (invalidated p)) pairs
  else
    pairs

let invalidate locs_killed t = match t with
  | Unknown -> Unknown
  | Known pairs ->
      let add l locs = Dn.loc (Rtl.regx l) :: locs in
      let locs = Register.SetX.fold add locs_killed [] in
      let alocs = List.map RU.MayAlias.locs' locs in
      let aexps = List.map RU.MayAlias.exp'  locs in
      Known (invalidate_pairs alocs aexps pairs)
	
let forward rtl t =
  let pairs = match t with Unknown -> [] | Known ps -> ps in
  let locs = locs_killed rtl in
  let alocs = List.map RU.MayAlias.locs' locs in
  let aexps = List.map RU.MayAlias.exp'  locs in
  let add_new_pair l r new_pairs =
    if List.exists (fun aexp -> aexp r) aexps then
      let badlocs = List.filter (fun l -> RU.MayAlias.exp' l r) locs in
      if List.for_all (fun l -> List.mem_assoc l pairs) badlocs then
        (l, subst_exp (Known pairs) badlocs r) :: new_pairs
      else
        new_pairs
    else
      (l, r) :: new_pairs in
  let pairs = invalidate_pairs alocs aexps pairs in
  (* massive pessimism: do registers only *)
  let extend guarded pairs = match guarded with
    | RP.Const (RP.Bool true), RP.Store(RP.Reg _ as l, r, _) -> add_new_pair l r pairs
    | _ -> pairs in
  let RP.Rtl effects = Dn.rtl rtl in
  let pairs = List.fold_right extend effects pairs in
  if debug then
    begin
      Printf.eprintf "avail: forwarding past %s yields\n" (S.rtl rtl);
      List.iter
        (fun (l, r) -> Printf.eprintf "avail:   %s == %s\n"
          (S.loc (Up.loc l)) (S.exp (Up.exp r))) pairs;
    end;
  Known pairs
    
let to_string = function
  | Unknown -> "<?>"
  | Known [] -> "<nothing-available>"
  | Known les ->
      let print (l, e) =
        Printf.sprintf "\n  %s = %s"
          (RU.ToString.loc (Up.loc l)) (RU.ToString.exp (Up.exp e)) in
      String.concat "" (List.map print les)
