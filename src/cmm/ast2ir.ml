module A  = Cmm_ast
module AT = Automaton
module C  = Call
module Dn = Rtl.Dn
module E  = Error
module F  = Fenv.Clean
module FE = Fenv
module G  = Zipcfg
module N  = Nelab
module R  = Rtl
module RO = Rewrite.Ops
module RP = Rtl.Private
module RU = Rtlutil
module RS = Register.Set
module RSX= Register.SetX
module S  = Elabstmt
module SM = Strutil.Map
module SP = Spans
module T  = Target
module Up = Rtl.Up
module W  = Rtlutil.Width

type aligned = int

type label   = G.uid * string 

let genlabel s =
  let s = Idgen.label s in
  (G.uid (), s)

let rsx = Register.rset_to_rxset

let (<<) f g  = fun x -> f (g x) (* function composition *)

let impossf x = Printf.kprintf Impossible.impossible x

let unimpf x = Printf.kprintf Impossible.unimp x

let ( **> ) f x = f x

let aligned_mem ~align ~base target = 
  let ( *> ) = AT.( *> ) in
  AT.at target.T.memspace ~start:base (
    AT.align_to (fun w -> min align (w / target.T.memsize)) *>
    AT.overflow ~growth:Memalloc.Up ~max_alignment:align)
    
type tgt = Preast2ir.tgt = T of (basic_proc, (Rtl.exp -> Automaton.t), Call.t) Target.t
and basic_proc = Preast2ir.basic_proc
type proc      = Preast2ir.proc

module K = 
struct
  type convention = string
      
  type 'i t =
      {         unwind_pc   : label       (* uids may or may not be used *)
      ;         unwind_sp   : Rtl.loc
      ;         cut_pc      : label
      ; mutable return_pcs  : (convention * label list) list
      ;         escapes     : bool        (* properties of how it is used *)
      ;         cut_to      : bool
      ;         unwound_to  : bool
      ;         formals     : (string * FE.variable * aligned) list
        (* args (kind, variable index) to the continuation *)
      ;         cut_in      : (Block.t -> Rtl.rtl) C.answer
        (* move vals at cut (uses rep) *)
      ;         return_in   : (Block.t -> Rtl.rtl) C.answer
        (* move vals at also returns *)
      ;         rep         : Contn.t     (* representation as C-- value (incl block) *)
      ;         base        : Block.t     (* base address; to be composed with rep *)
      ;         convention  : string
      ;         succ        : label       (* filled in by 2nd pass time *)
      ; mutable spans       : (Bits.bits  * Reloc.t) list
        (* user-defined spans at the continuation *)
      }
	
  let mk to_cc name den rep base ~formals ~cut_in ~return_in ~unwind_sp =
    let ccname cc = (to_cc cc).C.name 
    in
    { escapes     = den.FE.escapes
    ; cut_to      = den.FE.cut_to
    ; unwound_to  = den.FE.unwound_to
    ; unwind_pc   = genlabel "unwind_entry"
    ; unwind_sp   = unwind_sp
    ; cut_pc      = genlabel "cut_entry"
    ; return_pcs  = List.map (fun cc -> (ccname cc,[])) den.FE.returned_to
    ; rep         = rep
    ; base        = base
    ; convention  = den.FE.convention
    ; formals     = formals
    ; cut_in      = cut_in
    ; return_in   = return_in
    ; succ        = genlabel "start of continuation code"
    ; spans       = []
    }
      
  let get_return_pc cc k =
    let labels = 
      try List.assoc cc.C.name k.return_pcs
      with Not_found -> impossf "Unknown alt-return convention %s" cc.C.name 
    in
    let new_label = genlabel "return_entry" in
    let new_entry = (cc.C.name, new_label :: labels) 
    in
    k.return_pcs <- new_entry :: (List.remove_assoc cc.C.name k.return_pcs);
    new_label
end


type 'a nvr = { reg : 'a; tmp : 'a }   (* for callee-saves info *)
    (* I have to choose the value  *)
let headroom = ref 1024 
let set_headroom n = headroom := n

type 'a blocklists = 
    { mutable caller : 'a list
    ; mutable callee : 'a list 
    }

let translate target env ~(optimizer:proc -> unit) ~(defineglobals:bool)
    :(proc Nelab.compunit -> unit) =
  let T target = target in
  let pointersize = target.T.pointersize in
  let asm = F.asm env in
  let convert_parms kind_parm_aligned cvt =
    let add x (wkas, parms) =
      let kind, parm, a = kind_parm_aligned x in
      let parm, w = cvt parm in
      (w, kind, a) :: wkas, parm :: parms 
    in
    fun conv l ->
      let hws, parms = List.fold_right add l ([], []) 
      in conv hws parms  
  in
  let proc global_map proc =
    let proc_cc  = Call.get_cc target proc.N.cc in
    let vfp      = target.T.vfp in (* virtual frame pointer *)
    let temps    = Talloc.Multiple.for_spaces target.T.spaces in
    let uidmap =
      let add map l = SM.add l (G.uid ()) map in
      List.fold_left add SM.empty (Elabstmt.codelabels proc.N.code) in
    let uid_of s = 
      try SM.find s uidmap with Not_found -> impossf "unknown code label" in
    let exp_of_code_label (u,l) = R.codesym ((F.asm env)#local l) pointersize in
    let machine = T.boxmach in
    let m = (), machine, exp_of_code_label in
    let youngblocks        = { caller = []; callee = [] } in
    let oldblocks          = { caller = []; callee = [] } 
    in
    let add blocks party b = match party with
      | C.Caller -> blocks.caller <- b :: blocks.caller
      | C.Callee -> blocks.callee <- b :: blocks.callee in
    let constraints         = ref [] in
    let add_constraints  c  = constraints  := c :: !constraints        
    in
    let ptrcount =
      let _, _, c = target.T.memspace in Cell.to_count c target.T.pointersize 
    in
    let to_mloc exp =
      R.mem (R.aligned target.T.alignment) target.T.memspace ptrcount exp 
    in
    let low_end  block = Block.base block 
    in
    let high_end block =
      let base = Block.base block in
      Rtlutil.addk (W.exp base) base (Block.size block) 
    in
    let young_end cc = match cc.C.stack_growth with
      | Memalloc.Down -> low_end
      | Memalloc.Up   -> high_end 
    in
    let old_end cc = match cc.C.stack_growth with
      | Memalloc.Down -> high_end
      | Memalloc.Up   -> low_end 
    in
    let formals  = convert_parms (fun ((_, k, _, w, _, a) as parm) -> k, parm, a)
      (fun (i, _, _, w, n, _) -> AT.of_loc (R.var n i w), w) 
    in
    let cformals = convert_parms (fun p -> p)
      (fun v -> AT.of_loc v.FE.loc, RU.Width.loc v.FE.loc) 
    in
    let actuals  = convert_parms (fun ((k, _, _, a) as parm) -> k, parm, a)
      (fun (_, e, w, _) -> e, w) 
    in
    let results  = convert_parms (fun p -> p) (fun (l, w) -> AT.of_loc l, w) 
    in
    let extend_cont name den = 
      let cc        = Call.get_cc target den.FE.convention in
      let args      = den.FE.formals in
      let cut_in    = cformals cc.C.cut_parms.C.in' args in
      let return_in = cformals cc.C.results.C.in'   args in
      let rep       = Contn.with_overflow target cut_in.C.overflow in
      K.mk (Call.get_cc target) name den rep den.FE.base ~formals:args ~cut_in ~return_in
        ~unwind_sp:(to_mloc proc_cc.C.stable_sp_loc) 
    in
    let contenv =
      let add env (n, k) = Strutil.Map.add n (extend_cont n k) env in
      List.fold_left add Strutil.Map.empty proc.N.continuations 
    in
    let continuation l =
      try Strutil.Map.find l contenv with _ -> impossf "lost cont %s" l 
    in
    let proc_in'  = formals proc_cc.C.call_parms.C.in' proc.N.formals  in
    let nvregs    = RS.diff proc_cc.C.pre_nvregs proc_in'.C.regs in
    let nvr_temps = 
      RS.fold (fun r i -> (r, Some (proc_cc.C.saved_nvr temps r)) :: i) nvregs [] 
    in
    let convert (r,t) = 
      match t with 
	| Some t -> {reg = R.reg r; tmp = t}
	| None   -> impossf "Some nvr_temp expected" 
    in
    let nvr_info  = List.map convert nvr_temps 
    in
    let save_nvrs =
      R.par (List.map (fun i -> RU.store i.tmp (RU.fetch i.reg)) nvr_info) 
    in
    let restore_nvrs =
      R.par (List.map (fun i -> RU.store i.reg (RU.fetch i.tmp)) nvr_info) 
    in
    let save_ra, saved_ra =
      let ra_in = proc_cc.C.ra_on_entry proc_in'.C.overflow in
      let loc   = proc_cc.C.where_to_save_ra ra_in temps in
      R.store loc ra_in pointersize, loc 
    in
    let inalloc =
      match proc_cc.C.overflow_alloc.C.parameter_deallocator with
	| C.Callee -> to_mloc (old_end   proc_cc proc_in'.C.overflow)
	| C.Caller -> to_mloc (young_end proc_cc proc_in'.C.overflow) 
    in
    let nvars = List.length proc.N.formals + List.length proc.N.locals 
    in
    (* I DON'T KNOW HOW TO GET A VAR'S NAME!!!! SEE ELSEWHERE ALSO *)
    let var_array () = Array.init nvars (fun i -> Some (R.var "" ~index:i 0)) 
    in
    let sd_locs = List.map to_mloc proc.N.stacklabels in
    let continuations cc ast =
      let volregs = cc.C.volregs in                 (* volatile registers *)
      let allregs = RS.union volregs cc.C.pre_nvregs in (* all registers *)
      let (--)    = RS.diff in
      let as_cut_to k =
        let defs  = k.K.cut_in.C.regs in
        { G.defs  = rsx defs
	; G.kills = rsx (allregs -- defs)
	; G.node = k.K.cut_pc
        ; G.assertion = k.K.cut_in.C.insp (Contn.rep k.K.rep) 
	} 
      in
      let as_unwinds k =
        { G.defs  = RSX.empty
	; G.kills = rsx volregs
	; G.node = k.K.unwind_pc
        ; G.assertion = proc_cc.C.sp_on_unwind proc_cc.C.stable_sp_loc 
	} 
      in
      (* defs are vars and so can't be a register set. See [[splice_in_unwind_to_entry]] *)
      let as_returns k =
        let pc = K.get_return_pc cc k in
        let defs  = k.K.return_in.C.regs in
        { G.defs  = rsx defs
	; G.kills = rsx (volregs -- defs)
	; G.node = pc
        ; G.assertion = k.K.return_in.C.insp k.K.return_in.C.overflow 
	} 
      in
      let contmap f = List.map (f << continuation) in
      { S.cuts     = contmap as_cut_to  ast.S.cuts
      ; S.unwinds  = contmap as_unwinds ast.S.unwinds
      ; S.areturns = contmap as_returns ast.S.areturns
      ; S.returns  = ast.S.returns; S.aborts = ast.S.aborts; 
      } 
    in
    let ccontinuations cc ast =
      let ast' = 
	{ S.cuts = ast.S.ccuts
	; S.aborts = ast.S.caborts
	; S.unwinds = []
	; S.areturns = []
	; S.returns = false; 
	} 
      in
      let c = continuations cc ast' 
      in
      { S.ccuts = c.S.cuts; S.caborts = c.S.aborts } 
    in
    let rec finish_compiling_continuation k g =
      let g = 
        if k.K.cut_to then
          splice_in_cut_to_entry k g
        else if k.K.escapes then
          impossf "Continuation escapes but is not annotated with also cuts to"
        else g in
      let g = splice_in_return_to_entries k g in
      let g = if k.K.unwound_to then splice_in_unwind_to_entry k g else g 
      in
      g
	
    and splice_in_cut_to_entry k g =
      let in' = k.K.cut_in in
      let prolog =
        G.label m k.K.cut_pc **>
          G.stack_adjust in'.C.pre_sp   **>
          G.instruction  in'.C.shuffle  **>
          G.stack_adjust in'.C.post_sp  **>
          G.branch m k.K.succ     **>
          G.entry G.empty in
      G.add_blocks g prolog
	
    and splice_in_return_to_entries k g = 
      let gen cc g label =
        let in' = k.K.return_in in
        let ()  = add youngblocks cc.C.overflow_alloc.C.result_allocator in'.C.overflow 
	in
        let prolog =
          G.label m label **>
	    (* NR IS BEHIND THE TIMES AND NEEDS TO BE REMINDED WHAT HAPPENED TO ASSERTIONS *)
	    (* WE LOST:  G.assertion    g (in'.C.insp in'.C.overflow)    *)
            G.stack_adjust in'.C.pre_sp    **>
            G.instruction  in'.C.shuffle   **>
            G.stack_adjust in'.C.post_sp   **>
            G.branch m k.K.succ     **>
            G.entry G.empty in
        G.add_blocks g prolog 
      in
      let gen_labels g (ccname, labels) =
        List.fold_left (gen (Call.get_cc target ccname)) g labels 
      in
      List.fold_left gen_labels g k.K.return_pcs
	
    and splice_in_unwind_to_entry k g = 
      let prolog =
        G.label m k.K.unwind_pc **>
	  (* NR IS BEHIND THE TIMES AND NEEDS TO BE REMINDED WHAT HAPPENED TO ASSERTIONS *)
	  (* WE LOST: G.assertion g (proc_cc.C.sp_on_unwind proc_cc.C.stable_sp_loc) **> *)
          G.branch m k.K.succ     **>
          G.entry G.empty 
      in
      G.add_blocks g prolog
	(*
	  let bogus_space = ('z', Rtl.Identity, Cell.of_size 1) in
	  let use_var (h,v,a) succ =
          let w = RU.Width.loc v.FE.loc in
          G.assertion g (RU.store (R.mem R.none bogus_space (R.C w)
          (R.bits (Bits.S.of_int 0 w) w))
          (R.fetch (R.var "" v.FE.index w) w)) succ in
	  ... (List.fold_right use_var k.K.formals k.K.succ) ...
	*)
	
    and stmts props ss succ = List.fold_right (stmt props) ss succ
    and _stmt' props s  succ =
      Printf.eprintf "Translating %s\n" (
	match s with
	  | S.If (e, so, not) -> "If"
	  | S.Label n              -> "Label " ^ n
	  | S.Switch _             -> "Switch"
	  | S.Cont (name, _, _)    -> "Cont"
	  | S.Span (kv, bs)        -> "Span"
	  | S.Assign rtl           -> "Assign"
	  | S.Call  (lhs, cc, e, args, targets, conts, alias) -> "Call"
	  | S.Call' _                                         -> "Call'"
	  | S.Goto (e, labels) -> "Goto"
	  | S.Jump  (cc, e, args, targets)  -> "Jump"
	  | S.Cut (cc, k, args, conts)      -> "Cut"
	  | S.Return (cc, i, n, args)       -> "Return"
	  | S.Limitcheck (cc, cookie, cont) -> "Limitcheck");
      stmt props s succ
	
    and stmt  props s  succ =
      let rec contStmt2 label succ =
        let k = continuation label in
        k.K.spans <- props;
        G.forbidden m **>
          G.label m k.K.succ **> succ
	  
      and switchStmt range e arms props stmt_succ =
        let w = RU.Width.exp e in
        let e_in_interval (lo, hi) =
          if Bits.Ops.eq lo hi then
            RO.eq w e (Rtl.bits lo w)
          else
            RO.conjoin (RO.leu w (Rtl.bits lo w) e) (RO.leu w e (Rtl.bits hi w)) in
        let endswitch, g = G.make_target m stmt_succ in
        let default,   g = G.make_target m (G.forbidden m g) in
        let do_arm (ranges, body) (next_test, g) =
          let condition =
            Simplify.exp (
              List.fold_left (fun cond range -> RO.disjoin (e_in_interval range) cond)
                (Rtl.bool false) ranges) in
          let arm, g = G.make_target m (stmts props body (G.branch m endswitch g)) in
          G.make_target m (G.cbranch m condition ~ifso:arm ~ifnot:next_test g) in
        let (first_test, g) = List.fold_right do_arm arms (default, g) in
        g
	  
      and jump ~stack cconv e args targets g = 
        let stack_adjust = 
	  if stack then G.stack_adjust 
	  else fun _ succ -> succ 
	in
        let cc      = Call.get_cc target cconv in
        let out     = actuals cc.C.call_parms.C.out args in
        let ra_out  = cc.C.ra_on_exit saved_ra out.C.overflow temps in
        let jump_sp = cc.C.sp_on_jump out.C.overflow temps in
        let () = add oldblocks cc.C.overflow_alloc.C.parameter_deallocator out.C.overflow 
	in
        stack_adjust   out.C.pre_sp                                  **>
          G.instruction  out.C.shuffle                                 **>
          G.instruction  restore_nvrs                                  **>
          G.instruction  (RU.store ra_out (RU.fetch saved_ra))         **>
          G.instruction  (RU.store cc.C.jump_tgt_reg e)                **>
          stack_adjust   jump_sp                                       **>
          G.jump         m (RU.fetch cc.C.jump_tgt_reg) ~targets
          ~uses:(rsx (RS.union out.C.regs nvregs)) **>
          g
	  
      and call lhs cconv e args _targets conts alias succ = 
        let cc    = Call.get_cc target cconv in
        let out   = actuals cc.C.call_parms.C.out args in
        let in'   = results cc.C.results.C.in' lhs     in
        let unwind_conts =
          let f_to_cont (h,v,a) = (h,v.FE.index,a) in
          let unwind_k kname =
            let k = continuation kname in
            let args = List.map f_to_cont k.K.formals in
            (k.K.unwind_pc, k.K.unwind_sp, args) in
          List.map unwind_k conts.S.unwinds 
	in
        let conts = continuations cc conts 
	in
        add youngblocks cc.C.overflow_alloc.C.parameter_deallocator out.C.overflow;
        (* outgoing overflow parms *)
        add youngblocks cc.C.overflow_alloc.C.result_allocator      in'.C.overflow;
        (* incoming overflow results *)
        let outalloc = match cc.C.overflow_alloc.C.parameter_deallocator with
          | C.Caller -> to_mloc (young_end cc out.C.overflow)
          | C.Callee -> to_mloc (old_end   cc out.C.overflow) 
	in
        let label ((uid, l), x, y) = (l, x, y) in
        let spans = SP.to_spans ~inalloc ~outalloc ~ra:saved_ra
          ~users:props ~csregs:nvr_temps
          ~conts:(List.map label unwind_conts)
          ~sds:sd_locs ~vars:(var_array ()) 
	in
        let succ_assn = 
	  if conts.S.returns then in'.C.insp in'.C.overflow else R.par [] 
	in
        G.stack_adjust out.C.pre_sp                                        **>
          G.instruction  out.C.shuffle                                       **>
          G.stack_adjust out.C.post_sp                                       **>
          G.call m e
          ~uses:(rsx out.C.regs) ~defs:(rsx in'.C.regs) ~kills:(rsx cc.C.volregs)
          ~altrets:conts.S.areturns ~unwinds_to:conts.S.unwinds
          ~cuts_to:conts.S.cuts ~aborts:conts.S.aborts
          ~reads:alias.S.reads ~writes:alias.S.writes ~spans:(Some spans) ~succ_assn **>
          (if conts.S.returns then
            G.stack_adjust in'.C.pre_sp              (* DOUBTS ABOUT THIS *) **>
              G.instruction  in'.C.shuffle                                     **>
              G.stack_adjust in'.C.post_sp succ
          else
            G.forbidden m succ)
	  
      and return cconv i n args g = 
        let cc  = Call.get_cc target cconv in
        let out = actuals cc.C.results.C.out args  in
        let () =
          if Cmm_debug.on "return-regs" then
            let regstring ((s,_,_), i, R.C c) =
              if c = 1 then Printf.sprintf "%c%d" s i else Printf.sprintf "%c%d:%d" s i c in
            let used = List.map regstring (RS.elements out.C.regs) in
            Printf.eprintf "return statement uses regs: %s\n" (String.concat ", " used) in
        let ra_out = cc.C.ra_on_exit saved_ra out.C.overflow temps in
        let w      = Rtlutil.Width.loc saved_ra in
        let upd_ra = 
	  if i > 0 then 
	    RU.store saved_ra (RU.addk w (RU.fetch saved_ra) (i * asm#longjmp_size ()))
          else R.null 
	in
        let reti   = cc.C.return i n (RU.fetch ra_out) 
	in
        add oldblocks cc.C.overflow_alloc.C.result_allocator out.C.overflow;
        (* outgoing jump overflow = incoming *)
        G.instruction  upd_ra                  **>
          G.stack_adjust out.C.pre_sp            **>
          G.instruction  out.C.shuffle           **>
          G.instruction  (RU.store ra_out (RU.fetch saved_ra))  **>
          G.instruction  restore_nvrs            **>
          G.stack_adjust out.C.post_sp           **>
          G.return       ~exit:i reti ~uses:(rsx (RS.union out.C.regs nvregs)) **>
          g
	  
      and cut cc k args conts g = 
        let cc       = Call.get_cc target cc in
        let out      = actuals (cc.C.cut_parms.C.out k) args in
        let cut_args = Contn.cut_args target ~contn:k in
        let ()       = add_constraints (Block.constraints out.C.overflow) in
        let conts    = ccontinuations cc conts 
	in
        G.instruction out.C.shuffle **>
          G.cut_to m cut_args ~cuts_to:conts.S.ccuts ~aborts:conts.S.caborts
          ~uses:(rsx out.C.regs) **> g
	  
      and limitcheck cconv cookie cont =
        let w  = W.exp vfp in
        let cc = Call.get_cc target cconv in
        let growth = cc.Call.stack_growth in
        let younger = match growth with
          | Memalloc.Down -> Rewrite.Ops.lt w
          | Memalloc.Up -> Rewrite.Ops.gt w in
        let _extremum = unimpf "old end of stack frame" in
        let overflows = younger vfp cookie in
        let ovnode ~ifso ~ifnot = G.cbranch m overflows ~ifso ~ifnot 
	in
        G.limitcheck m ovnode (limitcheck_fails cconv cont) succ
	  
      and limitcheck_fails cconv cont succ = match cont with
	| None -> G.forbidden m succ
	| Some failure ->
            let failflow = { S.caborts = true; S.ccuts = [failure.S.recname] } 
	    in
            cut cconv failure.S.reccont [] failflow **>
              contStmt2 failure.S.recname **>
              succ 
      in
      match s with
	| S.If (e, so, not)      ->
            let cb ~ifso ~ifnot = G.cbranch m e ~ifso ~ifnot 
	    in
            G.if_then_else m cb ~t:(stmts props so) ~f:(stmts props not) succ
	      
	| S.Label n              -> G.label m (uid_of n, n) succ 
	| S.Switch (rg, e, arms) -> switchStmt rg e arms props succ
	| S.Cont (name, _, _)    -> contStmt2 name succ
	| S.Span (kv, bs)        -> stmts (kv :: props) bs succ
	| S.Assign rtl           -> G.instruction rtl succ
	| S.Call  (lhs, cc, e, args, targets, conts, alias) ->
            call lhs cc e args targets conts alias succ
	| S.Call' (cc, e, args, targets) ->
            jump ~stack:false cc e args targets succ
	| S.Goto (e, labels) ->
            (match Dn.exp e with
              | RP.Const (RP.Link (sym, _, w)) ->
		  let lbl = sym#original_text in
		  G.branch m (uid_of lbl, lbl) succ
              | _ -> (*let instr   = machine.T.goto.T.embed e in*)
                  let targets = List.map (fun l -> (uid_of l, l)) labels in
                  G.mbranch m e targets succ)
	| S.Jump  (cc, e, args, targets)  -> jump ~stack:true cc e args targets succ
	| S.Cut (cc, k, args, conts)      -> cut cc k args conts succ
	| S.Return (cc, i, n, args)       -> return cc i n args succ
	| S.Limitcheck (cc, cookie, cont) -> limitcheck cc cookie cont 
    in
    let insert_init_cont_nodes contmap init_label g =
      let one_node cname k g = 
        if k.K.escapes then
          let pc    = exp_of_code_label k.K.cut_pc in
          let sp    = proc_cc.C.stable_sp_loc      in
          let spans = SP.to_spans ~inalloc ~outalloc:(to_mloc sp) ~ra:saved_ra
            ~users:k.K.spans ~csregs:nvr_temps ~conts:[]
            ~sds:sd_locs ~vars:(var_array ()) in
          let cut_g = G.focus (fst k.K.cut_pc) (G.unfocus g) in
          let () = G.set_spans cut_g spans in
          let blockname block =
            match G.Rep.blocklabel block with
              | Some (_, s) -> s
              | None -> "<entry block>" in
          let _focused_blockname g =
            let b, _ = G.openz g in
            let b = G.Rep.zip b in
            blockname b in
          let pc_sp = { Mflow.new_pc = pc; Mflow.new_sp = sp } in
          let init = G.instruction (Contn.init_code k.K.rep pc_sp) (G.entry G.empty) 
	  in
          G.splice_focus_entry g (G.unfocus init)
        else
          g 
      in
      G.unfocus (Strutil.Map.fold one_node contmap (G.focus (fst init_label) g)) 
    in
    let bodylbl = genlabel "proc body start" in
    let contlbl = genlabel "initialize continuations" in
    let () = add oldblocks proc_cc.C.overflow_alloc.C.parameter_deallocator
      proc_in'.C.overflow 
    in
    let stack_adjust =
      if proc.N.basic_block then (fun _ succ -> succ) else G.stack_adjust 
    in
    let g = 
      G.unfocus                         **>
	stack_adjust proc_in'.C.pre_sp    **>
	G.instruction  proc_in'.C.shuffle **>
	stack_adjust proc_in'.C.post_sp   **>
	G.instruction  save_nvrs          **>
	G.instruction  save_ra            **>
	G.label m contlbl                 **>
	G.label m bodylbl                 **>
	stmts proc.N.spans proc.N.code    **>
	G.entry G.empty 
    in 
    let contblocks, g =
      if proc.N.basic_block then
        Block.cathl_list pointersize [], g
      else
        let addblock name k (blocks, g) =
          let g = finish_compiling_continuation k g in
          let blocks =
            if k.K.escapes then Block.cathl (Contn.rep k.K.rep) k.K.base :: blocks
            else blocks in
          blocks, g in
        let blocks, g = Strutil.Map.fold addblock contenv ([], g) in
        Block.cathl_list pointersize blocks, g 
    in
    let g =
      if proc.N.basic_block then
        g  (* continuations already initialized in prolog of original proc *)
      else
        insert_init_cont_nodes contenv contlbl g 
    in
    let index = ref 0 in
    let _allocate = fun ~width -> fun ~alignment -> fun ~kind ->
      let () = index := !index + 1 in 
      let spaceId = char_of_int alignment in 
      let cell = Cell.of_size target.Target.wordsize in
      let space = (spaceId, target.Target.byteorder, cell) 
      in  
      AT.of_loc (Rtl.reg (space, !index, Cell.to_count cell width)) 
	
    and _freeze arg = Impossible.impossible "you must not freeze this automaton" 
    in
    let formals =
      List.map (fun (i, h, v, w, n, a) -> i, (Some h, v, Cmm_ast.BitsTy w, n, Some a))
	proc.N.formals 
    in
    let (i : proc) =    
      g,
      { Proc.symbol      = proc.N.sym
      ; Proc.cc          = proc_cc
      ; Proc.target      = T target
      ; Proc.formals     = formals
      ; Proc.temps       = temps
      ; Proc.mk_symbol   = Fenv.Clean.symbol env
      ; Proc.cfg         = ()
      ; Proc.oldblocks   = (* parms must be first, so call List.rev *)
          { C.callee = List.rev (oldblocks.callee)
	  ; C.caller = List.rev (oldblocks.caller)
	  }
      ; Proc.youngblocks = (* order doesn't matter *)
          { C.callee = youngblocks.callee
	  ; C.caller = youngblocks.caller
	  }
      ; Proc.stackd         = proc.N.stackmem     (* stack data block *)

      ; Proc.priv           = Block.relative vfp "private"
          (aligned_mem ~align:proc_cc.C.sp_align) target
      ; Proc.sp             = Block.at proc_cc.C.stable_sp_loc 0 proc_cc.C.sp_align 
      ; Proc.eqns           = List.concat (!constraints)
      ; Proc.conts          = contblocks
      ; Proc.vars           = nvars              (* number of variables *)
      ; Proc.nvregs         = RS.cardinal nvregs (* number of non-volatile regs *)
      ; Proc.var_map        = Array.make nvars None
      ; Proc.global_map     = global_map
      ; Proc.bodylbl        = bodylbl
      ; Proc.headroom       = !headroom
      ; Proc.exp_of_lbl     = exp_of_code_label
      } 
    in
    let i, _ = Optimize.trim_unreachable_code () i 
    in
    optimizer i (* runs optimizer, freezes, and assembles proc *) 
  in
  let globals (base:Rtl.exp) vars =
    let t           = target.T.globals base in
    let decls       = Buffer.create 128 in (* initial size - grows as needed *)
    let rec assign bindings (name, var) = match var.FE.rkind with
      | FE.RNone | FE.RKind _ -> 
          (* global register w/o hardware annotation *)
          let w     = RU.Width.loc var.FE.loc in
          let sig'  = Printf.sprintf "[%s %d]" name w in
          let ()    = Buffer.add_string decls sig' in
          let loc   = AT.allocate t w "" 1 in
          (var.FE.index, loc) :: bindings   (* could do even better here *)
      | FE.RReg hw ->
          (* global register with h/w annotation *)
          (try
            let loc   = AT.of_loc (Strutil.Map.find hw target.T.named_locs) in
            let sig'  = Printf.sprintf "[%s %s]" name hw in            
            let ()    = Buffer.add_string decls sig' in
            (var.FE.index, loc) :: bindings
          with Not_found -> impossf "unknown hardware register \"%s\"" hw) 
    in
    let bindings = List.fold_left assign [] vars 
    in
    let gmap =
      let var n = 
	try List.assoc n bindings
        with Not_found -> impossf "no global register %d" n 
      in 
      Array.init (List.length vars) var 
    in
    gmap, AT.freeze t, Digest.string (Buffer.contents decls) 
  in
  let rec datum global_map asm = 
    function
      | N.Datalabel l -> asm#label l  
      | N.Align n     -> asm#align n
      | N.InitializedData es -> List.iter (fun (k, t) -> asm#addr k) es
      | N.UninitializedData n -> asm#zeroes n
      | N.Procedure p -> proc global_map p 
  in
  let program prog =
    let prog = N.rewrite target.T.tx_ast prog in
    let emit_global_register_area ~export =
      let areaname = "Cmm.global_area" in
      let base_sym = if export then asm#export areaname else F.symbol env areaname in
      let base     = Rtl.datasym base_sym pointersize in 
      let gmap, area, digest = globals base prog.N.globals in
      let area     = area.AT.overflow in
      let digest   = "Cmm.globalsig." ^ Idcode.encode digest in
      let asm      = F.asm env in
      let digest   = if export then asm#export digest else asm#import digest in
      (* must move to placevars and use gmap as a replacement map *)
      let _ = 
        if export then 
          begin
            asm#section (target.T.data_section);
            asm#comment "memory for global registers";
            asm#align (Block.alignment area);
            asm#label digest;     (* ensures desired definition is present *)
            asm#label base_sym;   (* ensures no multiple inconsistent definitions *)
            asm#addloc (Block.size area);
            asm#globals (List.length prog.N.globals);
          end
        else
          let localref = asm#local "Cmm.ref_to_global_area" in
          begin
            asm#section (target.T.data_section);
            asm#label localref;
            asm#comment "reference to global-register signature";
            asm#addr (Reloc.of_sym (digest, Rtl.impsym) pointersize);
          end in
      gmap 
    in
    let global_map = emit_global_register_area defineglobals in
    (*if defineglobals then
      Runtimedata.emit_global_properties target asm;*)
    let section asm (name, data) =
      asm#section name; List.iter (datum global_map asm) data in
    List.iter (section asm) prog.N.sections in
  program

let () = Cmm_debug.register "return-regs" "show registers used by return statememt"
