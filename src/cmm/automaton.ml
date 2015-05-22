include Common
type counter    = string
type counterenv = string -> int ref option
type result =
  { overflow    : Block.t
  ; regs_used   : Register.Set.t
  ; mems_used   : Rtl.loc list
  ; align_state : int   (* final alignment state of overflow block *)
  }
type width = int
type kind  = string
type loc = Rtlutil.aloc =
    { fetch  : width -> Rtl.exp
    ; store  : Rtl.exp -> width -> Rtl.rtl
    }
type methods =
    { allocate : width: int -> alignment: int -> kind: string -> loc
    ; freeze   : Register.Set.t -> Rtl.loc list -> result
    } 
type choice_predicate = int -> string -> counterenv -> bool
type t = methods
type implementation = I of (start:Rtl.exp -> space:Rtl.space -> ctrs:counterenv -> t)
type stage = S of (implementation -> implementation)

module R   = Rtl
module RS  = Register.Set
module B   = Block
module M   = Memalloc
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimp  = Impossible.unimp
let ( *> ) (S f1) (S f2) = S (fun rhs -> f1 (f2 rhs))

let pipeline_end = I (
  fun ~start ~space ~ctrs -> 
    let block = Block.at ~base:start ~size:0 ~alignment:1 in
    { allocate = (fun ~width ~alignment ~kind -> impossf
                  "fell off end of pipeline -- multiple return values from C?")
    ; freeze   =
      (fun regs mems -> 
       {overflow = block; regs_used = regs; mems_used = mems; align_state = 0})
    })
let at space ~start (S s) =
  let I f = s pipeline_end in
    f ~start ~space ~ctrs:(fun _ -> None)

let of_methods t                     = t
let allocate   t ~width ~kind ~align = t.allocate width align kind
let freeze     t                     = t.freeze Register.Set.empty []

let fetch  loc = loc.fetch
let store  loc = loc.store
let of_loc loc = { fetch  = R.fetch loc; store = R.store loc }
let wrap  f = S (fun (I next) -> I (fun ~start ~space ~ctrs ->
                                    f (next ~start ~space ~ctrs)))
let swrap f =
  S (fun (I next) -> I (fun ~start ~space ~ctrs ->
                           f (next ~start ~space ~ctrs) ~start ~space ~ctrs))
let wrap_choice f choices = S (fun (I next) ->
  let apply (I f) = f in
  let downchoice c ~start ~space ~ctrs =
   fun meths -> apply (c (I (fun ~start ~space ~ctrs -> meths))) ~start ~space ~ctrs in
  I (fun ~start ~space ~ctrs ->
   let choices = List.map (fun (p,(S c)) ->
                            (p, downchoice c ~start ~space ~ctrs))
                 choices
   in f choices (next ~start ~space ~ctrs) ~ctrs))
type state = { mutable mem       : M.t; mutable frozen : bool
             ; mutable mems_list : R.loc list  }
let overflow ~growth ~max_alignment next =
  fun ~start ~space:((_, byteorder, memsize) as space) ~ctrs ->
    let mem = M.at start growth in
    let state = { mem = mem; frozen = false; mems_list = [] } in
    let allocate ~width ~alignment ~kind =
      if not (Cell.divides memsize width) then
       impossf "width %d not multiple of memsize %d" width (Cell.size memsize);
      if max_alignment mod alignment <> 0 then
       impossf "max alignment %d not multiple of alignment %d" max_alignment alignment;
      if state.frozen then impossf "Allocation from a frozen overflow block";
      let (R.C c) as count = Cell.to_count memsize width in
      let mem  = state.mem in
      let mem  = M.align mem alignment in
      let mem' = M.allocate mem c in
      let addr = M.current (match growth with M.Up -> mem | M.Down -> mem') in
      let loc  = (R.mem (R.aligned alignment) space count addr) in
      state.mems_list <- loc :: state.mems_list;
      state.mem <- mem';
      of_loc loc in
    let freeze regs mems =
      state.frozen <- true;
      let mem    = state.mem in
      let astate = M.num_allocated mem mod max_alignment in
      let mem    = match growth with 
                   M.Up -> mem | M.Down -> M.align mem (M.alignment mem) in
      let block  = M.freeze mem in
      { overflow = block; regs_used = regs; mems_used = state.mems_list @ mems
      ; align_state = astate } in
    { allocate = allocate; freeze = freeze }
let overflow ~growth ~max_alignment = 
  swrap (overflow ~growth ~max_alignment) 
type allocator = width: int -> alignment: int -> kind: string -> loc
let txstage f  = wrap (fun next -> 
                        { allocate = f next.allocate; freeze = next.freeze })
let _ = (txstage : (allocator -> allocator) -> stage)
let widths ws = txstage
  (fun nalloc ~width ~alignment ~kind ->
    if   List.exists ((=) width) ws then nalloc width alignment kind
                                    else Unsupported.automaton_widths width)
let narrower = 
  let narrow nopname wopname ~w ~n loc =
     let widen  = R.opr wopname [n; w] in 
     let narrow = R.opr nopname [w; n] in 
       { fetch  = (fun n   -> R.app narrow [loc.fetch w])
       ; store  = (fun e n -> loc.store (R.app widen [e]) w)
       } in
  function
  | "signed" -> narrow "lobits" "sx"
  | "float"  -> narrow "f2f_implicit_round" "f2f_implicit_round"
  | "unsigned" | "address" | "" | _ -> narrow "lobits" "zx"
let narrowf = narrower "float"

let widen f = txstage 
  (fun nalloc ~width:n ~alignment ~kind ->
    let w   = f n in
    let loc = nalloc w alignment kind in
    if w = n then loc else narrower kind ~n ~w loc)

let align_to f =
  txstage (fun nalloc ~width:w ~alignment ~kind -> nalloc w (f w) kind)
let choice choices next ~ctrs =
    let add (p, stage) alternative =
      let follows_choice = { allocate = next.allocate
                           ; freeze   = alternative.freeze } in
      let choice = stage follows_choice in
      let alloc ~width ~alignment ~kind =
        let alloc = if p width kind ctrs then choice.allocate 
                                         else alternative.allocate in
        alloc width alignment kind in
      { allocate = alloc; freeze = choice.freeze } in
    List.fold_right add choices
      { allocate = (fun ~width -> impossf "fell off end of choice stage")
      ; freeze = next.freeze }
let choice = wrap_choice choice

let first_choice choices next ~ctrs =
  let choices = List.map (fun (p, c) -> (p, c next)) choices in
  let myself = ref next in (* temporary *)
  let allocate ~width ~alignment ~kind = 
    (!myself).allocate width alignment kind in
  let freeze regs mems = (!myself).freeze regs mems in
  let first_allocate ~width ~alignment ~kind =
    let choice =
      try snd (List.find (fun (p, _) -> p width kind ctrs) choices)
      with Not_found -> impossf "missing choice" in
    myself := choice;
    allocate width alignment kind in
  let () = try
    myself := { allocate = first_allocate
              ; freeze   = (snd (List.hd choices)).freeze }
  with Failure _ -> impossf "no first choice" in
  { allocate = allocate; freeze = freeze }
let first_choice = wrap_choice first_choice

let counter_is c f _ _ env = match env c with Some i -> f (!i) | None -> false
let is_kind    h'  w h _   = h =$= h'
let is_width   w'  w h _   = w = w'
let is_any         w h _   = true
let wrap_counter f label =
  S (fun (I next) ->
       I (fun ~start ~space ~ctrs ->
           let (ctr, ctrs) = (match ctrs label with
           | Some n -> (n, ctrs)
           | None   ->
              let n = ref 0 in
              (n, fun s -> if s =$= label then Some n else ctrs s)) in
           f space ctr (next ~start ~space ~ctrs)))

let counter of_width =
  let f ctr nalloc ~width ~alignment ~kind =
    let loc = nalloc ~width ~alignment ~kind in
    (ctr := !ctr + of_width width ; loc)
  in (fun ctr next -> { allocate = f ctr next.allocate; freeze = next.freeze })
type regstate = { mutable used  : Register.Set.t }

let const w k = R.bits (Bits.U.of_int k w) w
let zx n w v = R.app (R.opr "zx" [n; w]) [v]
let orb w x y = R.app (R.opr "or"  [w]) [x; y]
let shl  w x k = R.app (R.opr "shl"  [w]) [x; const w k]
let shrl w x k = R.app (R.opr "shrl" [w]) [x; const w k]
let lobits w n x = R.app (R.opr "lobits" [w; n]) [x]
let combine msb msw lsb lsw =
  { fetch  = (fun ww -> assert (ww = msw + lsw);
                    orb ww (zx lsw ww (fetch lsb lsw))
                           (shl ww (zx msw ww (fetch msb msw)) lsw))
  ; store  = (fun exp ww -> assert (ww = msw + lsw);
                    R.par [ store lsb (lobits ww lsw exp              ) lsw
                          ; store msb (lobits ww msw (shrl ww exp lsw)) msw ])
  }

let combine_loc = function
  | Rtl.Identity     -> impossf "split without byte-ordering"
  | Rtl.LittleEndian -> (fun b1 w1 b2 w2 -> combine b2 w2 b1 w1)
  | Rtl.BigEndian    -> combine

let reg_placer of_width agg n regs reserve next =
  let state = { used = RS.empty } in
  let getreg = function
  | []      -> raise (Failure "get register")
  | r :: rs -> (state.used <- RS.add r state.used; r) in

  let rec drop i regs = if i <= 0 then regs else match regs with
  | r :: rest -> drop (i - of_width (Register.width r)) rest
  | []        -> [] in

  let rec alloc ~width ~alignment ~kind =
    try
      let ((_, _, ms), _, c) as r = getreg (drop !n regs) in
      let w = Cell.to_width ms c in
      if w = width then
         let allocated = of_loc (R.reg r) in
         ( if reserve then ignore (next.allocate width alignment kind)
         ; allocated )
      else if w < width then match agg with
             | Some endianness ->
                let w'  = width - w in
                let _   = n := !n + w in
                let ell = combine_loc endianness
                             (of_loc (R.reg r)) w
                             (alloc ~width:w' ~alignment ~kind) w'
                in ( n := !n - w ; ell )
             | None -> Unsupported.automaton_widen ~have:width ~want:w
      else unimp "auto-widening for register requests"
    with Failure _ -> next.allocate width alignment kind in
  let freeze regs mems = next.freeze (RS.union state.used regs) mems in
  { freeze = freeze; allocate = alloc }

let bitcounter   = counter (fun w -> w)
let regs_by_bits = reg_placer (fun w -> w)

let useregs regs reserve = 
  swrap (fun next ~start ~space:(_, byteorder, _) ~ctrs -> let c = ref 0 in
          (bitcounter c) ((regs_by_bits (Some byteorder) c regs reserve) next))

let bitcounter   = wrap_counter (fun _ -> bitcounter)
let regs_by_bits label rs reserve =
    wrap_counter (fun (_,bo,_) ctr -> regs_by_bits (Some bo) ctr rs reserve) label

let argcounter   = wrap_counter (fun _ -> counter (fun w -> 1))
let regs_by_args = reg_placer (fun w -> 1) None
let regs_by_args label rs reserve = 
    wrap_counter (fun _ ctr -> regs_by_args ctr rs reserve) label

let pad = 
  let padcounter space ctr next =
    let (_, _, memsize) = space in
    let f nalloc ~width ~alignment ~kind =
      let al = Cell.to_width memsize (R.C alignment) in
      ctr := Auxfuns.round_up_to ~multiple_of:al !ctr;
      nalloc ~width ~alignment ~kind
    in { next with allocate = f next.allocate }
  in wrap_counter padcounter
let as_stage inner next = 
  fun ~start ~space ~ctrs ->
    let freeze regs mems =
      { regs_used = regs; mems_used = mems;
        overflow = Block.at start 0 0; align_state = 0 } in
    { allocate = inner.allocate; freeze = freeze }
let as_stage inner = swrap (as_stage inner)
let unit = wrap (fun next -> next)
let debug f _ ctr next =
  let allocate ~width ~alignment ~kind =
    f width kind alignment (!ctr);
    next.allocate ~width ~alignment ~kind in
  { allocate = allocate; freeze = next.freeze }

let debug counter f = wrap_counter (debug f) counter
let postprocess (S stage) f = S (fun imp ->
  let I i = stage imp in
  I (fun ~start ~space ~ctrs ->
      let m = i start space ctrs in
      { allocate = m.allocate
      ; freeze = (fun rs ms -> f (m.freeze rs ms))
      } ))
type cc_spec  = { call : stage; results : stage; cutto : stage }
type cc_specs = (string * cc_spec) list
let init_cc = []
