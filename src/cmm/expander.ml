include Common
module type S = 
sig
  val cfg     : 'a -> Preast2ir.proc -> Preast2ir.proc * bool
  val machine : Preast2ir.basic_proc Target.machine
  val block :
    Preast2ir.basic_proc -> Rtl.exp Dag.block   -> (Rtl.exp -> Rtl.rtl) Dag.block
  val goto :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val cbranch :
    Preast2ir.basic_proc -> Rtl.exp Dag.cbranch -> (Rtl.exp -> Rtl.rtl) Dag.cbranch
  val call :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val jump :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
  val cut :
    Preast2ir.basic_proc -> Rtl.exp Dag.branch  -> (Rtl.exp -> Rtl.rtl) Dag.branch
end 

module A   = Automaton
module BO  = Bits.Ops
module DG  = Dag
module G   = Zipcfg
module GR  = Zipcfg.Rep
module O   = Opshape 
module PA  = Preast2ir
module PX  = Postexpander
module R   = Rtl
module RP  = Rtl.Private
module RU  = Rtlutil
module Reg = Register
module Rg  = Register
module S   = Space
module T   = Target

module Up  = Rtl.Up
module Dn  = Rtl.Dn

let upassn = Rtl.Up.assertion
let impossf fmt = Printf.kprintf Impossible.impossible fmt
let unimpf  fmt = Printf.kprintf Impossible.unimp fmt
let fetch l = RP.Fetch (Dn.loc l, RU.Width.loc l)
let width e = RU.Width.exp (Up.exp e)

module D = 
struct (* debugging *)
  let () = Cmm_debug.register "expander" "code expander"
  let eprintf = Printf.eprintf 
  let sprintf = Printf.sprintf 
  let strings =
    if Cmm_debug.on "expander" then
      (fun ss -> eprintf "%s" (String.concat "" ss))
    else
      (fun ss -> ()) 
  let int  n = string_of_int n
  let rtl  r = Rtlutil.ToString.rtl r
  let brtl b = "<function: exp -> rtl>"
  let temp ((s, _, ms), n, c) =
    sprintf "$%s[%d] : %d loc" (Char.escaped s) n (Cell.to_width ms c)
  let exp = Rtlutil.ToString.exp 
  let pr_rtls rs = List.iter (fun r -> strings ["  "; rtl r; "\n"]) (List.rev rs)
  let rec cbi pa c = match c with
    | a, DG.Exit true,  DG.Exit false -> pa a
    | a, DG.Exit false, DG.Exit true  -> sprintf "!(%s)" (pa a)
    | a, DG.Exit true,  p -> sprintf "(%s || %s)" (pa a) (cbranch pa p)
    | a, p, DG.Exit false -> sprintf "(%s && %s)" (pa a) (cbranch pa p)
    | a, p, q -> sprintf "(%s ? %s : %s)" (pa a) (cbranch pa p) (cbranch pa q)
	
  and cbranch pa c = match c with
    | DG.Exit p           -> if p then "true" else "false"
    | DG.Shared (_, c)    -> sprintf "[%s]" (cbranch pa c)
    | DG.Test (DG.Nop, c) -> cbi pa c
    | DG.Test (b, c)      -> sprintf "{%s; %s}" (compact_block pa b) (cbi pa c)
	
  and compact_block pa b =
    let rec pr = function
      | DG.Rtl r        -> sprintf "%s" (rtl r)
      | DG.Seq (b, b')  -> sprintf "%s; %s" (pr b) (pr b')
      | DG.If (c, t, f) -> sprintf "if (%s) { %s; } else { %s; }"
          (cbranch pa c) (pr t) (pr f)
      | DG.While (c, b) -> sprintf "while (%s) { %s; }" (cbranch pa c) (pr b)
      | DG.Nop          -> "skip" in
    pr b
      
  let rec pr_block pa ind b =
    let rec pr = function
      | DG.Rtl r          -> eprintf "%s%s;\n" ind (rtl r)
      | DG.Seq (b, b')    -> pr b; pr b'
      | DG.If (DG.Exit p, t, f) -> pr (if p then t else f)
          (* true to semantics, but maybe not informative enough *)
      | DG.If (DG.Shared (_, c), t, f) -> pr (DG.If(c, t, f))
      | DG.If (DG.Test (DG.Nop, c), t, f) -> 
          let ind' = ind ^ "  " in
          eprintf "%sif (%s) {\n" ind (cbi pa c);
          pr_block pa ind' t;
          eprintf "%s} else {\n" ind;
          pr_block pa ind' f;
          eprintf "%s}\n" ind
      | DG.If (DG.Test (b, cbi), t, f) ->
          assert (Pervasives.(<>) b DG.Nop);
          pr (DG.Seq (b, DG.If (DG.Test(DG.Nop, cbi), t, f)))
      | DG.While (c, b) ->  (* not implemented really *)
          let ind' = ind ^ "  " in
          eprintf "%swhile (%s) {\n" ind (cbranch pa c);
          pr_block pa ind' b;
          eprintf "%s}\n" ind
      | DG.Nop -> eprintf "%s<nop>\n" ind in
    pr b
      (* renaming *)
  let cbranch pa = if Cmm_debug.on "expander" then cbranch pa else fun _ -> "<cbranch>"
  let pr_block pa = if Cmm_debug.on "expander" then pr_block pa "  " else fun _ -> ()
  let exp  e = Rtlutil.ToString.exp (Up.exp e)
  let exp' e = Rtlutil.ToString.exp e
  let loc  l = Rtlutil.ToString.loc (Up.loc l)
end
  
let _ = Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0))
  
module IntFloatAddr (Post : Postexpander.S) = 
struct
  let pc_lhs = Dn.loc Post.pc_lhs
  let pc_rhs = Dn.loc Post.pc_rhs
  let has_pc_on_left = function
    | RP.Store (pc, _, _) -> RU.Eq.loc pc pc_lhs
    | RP.Kill _ -> false 
	
  let has_pc_on_right = function
    | RP.Store (_, e, _) -> RU.Exists.Loc.exp (RU.Eq.loc pc_rhs) e
    | RP.Kill _ -> false 
	
  let (<<) f g x = f (g x) 
  let branch_condition rtl = match Dn.rtl rtl with
    | RP.Rtl [(g, RP.Store (left, right, w))] ->
	if not (RU.Eq.loc left pc_lhs) then
          impossf "conditional branch assigns to non-PC";
	g
    | _ -> impossf "ill-formed conditional branch"
	
  let alloc (allocate, check) w   = allocate w
  let ok    (allocate, check) reg = check reg

  let expand proc =
    let PA.T tgt = proc.Proc.target in
    let exchange_slot =
      let slots = ref [] in
      function w ->
        try List.assoc w (!slots)
        with Not_found ->
          let slot = Postexpander.Alloc.slot w Post.exchange_alignment in
          slots := (w, slot) :: !slots;
          slot in
    let (<:>) = DG.(<:>) 
    in
    let (<::>) is (is', branch) = (is <:> is', branch) 
    in
    let rec (<:::>) is cbranch = match cbranch with
      | DG.Exit b -> DG.Exit b
      | DG.Test (b, c) -> DG.Test (is <:> b, c)
      | DG.Shared (u, c) -> DG.Shared (u, is <:::> c) 
    in
    let strip_trivial_guards l =
      List.fold_right
        (fun (g, e) l ->
          match g with RP.Const (RP.Bool b) -> if b then e :: l else l
            | _ -> Impossible.unimp "multiple effects with a nontrivial guard") l [] 
    in
    let trivially p l =
      let rec t es' = function
        | [] -> p (List.rev es')
        | (g, e) :: ges ->
            match g with RP.Const (RP.Bool b) -> t (if b then e :: es' else es') ges
              | _ -> false in
      t [] l 
    in
    let contextmap (allocator, checker) = (allocator proc.Proc.temps, checker) in
    let icontext = contextmap Post.icontext in
    let acontext = contextmap Post.acontext in
    let fcontext = contextmap Post.fcontext in
    let rounding_mode = Dn.loc tgt.Target.rounding_mode in
    let is_not_rounding_mode arg = match arg with
      | RP.Fetch (l, 2) -> not (RU.Eq.loc l rounding_mode)
      | _ -> true in
    let cmp_context (oprname, ws) =
      if oprname.[0] =<= 'f' then fcontext else icontext 
    in
    let guess_context = function
      | RP.Const (RP.Bits b) -> contextmap (Post.constant_context (Bits.width b))
      | RP.Const k -> icontext
      | RP.Fetch(RP.Reg ((('f'|'u'), _, _), _, _),_) -> fcontext
      | RP.Fetch _ -> icontext
      | RP.App (op, args) -> contextmap (Post.result_context op) 
    in
    let looks_like_stack_rhs = function
      | RP.App (op, _) ->
          begin
            match Post.opclass op with
              | PX.Stack (_, _) -> true
              | PX.Register -> false
          end
      | RP.Fetch (l, _) -> Post.is_stack_top_proxy l
      | _ -> false 
    in
    let alloc_direct (allocator, _) = allocator proc.Proc.temps 
    in
    let temp_in_context t context instructions =
      if ok context t then
        t, instructions
      else
        let t' = alloc context (Register.width t) in
        t', instructions <:> Post.move t' t 
    in 
    let rec to_temp' room context e =
      match e with
	| RP.Const k ->
            let w = width e in
            let t = alloc context w in
            t, Post.li t k
	| RP.App (_, _) when is_compile_time_constant e ->
            let w = width e in
            let t = alloc context w in
            t, Post.lix t (Up.exp e)
	| RP.Fetch (l, w) when Post.is_stack_top_proxy l ->
            let slot = exchange_slot w in
            let t, is = to_temp room context (fetch_slot slot w) in
            t, assign_slot room slot e w <:> is
	| RP.Fetch (RP.Mem (('m', agg, ms), c, addr, assn), w) ->
            let t = alloc context w in
            let a, is = address room addr in
            assert (agg =*= Post.byte_order);
            assert (w = Cell.to_width ms c);
            t, is <:> Post.load t a (upassn assn)
	| RP.Fetch (RP.Reg r, w) ->
            assert (w = Register.width r);
            temp_in_context r context DG.Nop
	| RP.Fetch _ ->
            impossf "memory-like access to non-memory space"
	| RP.App (("sx", [n; w]), [RP.App(("lobits", [w'; n']), [x])]) when w = w' ->
            if n <> n' then Impossible.impossible "ill-typed %sx(%lobits(...))";
            to_temp' room context (Dn.exp (Rewrite.sxlo n w (Up.exp x)))
	| RP.App (("zx", [n; w]), [RP.App(("lobits", [w'; n']), [x])]) when w = w' ->
            if n <> n' then Impossible.impossible "ill-typed %zx(%lobits(...))";
            to_temp' room context (Dn.exp (Rewrite.zxlo n w (Up.exp x)))
	| RP.App (("sx", [n;w]), [RP.Fetch (RP.Mem (('m',agg,ms), c, addr, assn), n')]) ->
            assert (n = n' && n' = Cell.to_width ms c);
            let t = alloc icontext w in
            let a, is = address room addr in
            assert (agg =*= Post.byte_order);
            temp_in_context t context (is <:> Post.sxload t a n (upassn assn))
	| RP.App (("zx", [n;w]), [RP.Fetch (RP.Mem (('m',agg,ms), c, addr, assn), n')]) ->
            assert (n = n' && n' = Cell.to_width ms c);
            let t = alloc icontext w in
            let a, is = address room addr in
            assert (agg =*= Post.byte_order);
            temp_in_context t context (is <:> Post.zxload t a n (upassn assn))
	| RP.App ((("sx"|"zx" as o), [n; w]), [RP.Fetch(r, _)]) when RU.is_hardware r ->
            let r = RU.to_hardware r in
            let t = alloc icontext w in
            let fill = if o == "sx" then PX.HighS else PX.HighZ in
            t, Post.hwget ~dst:(fill,t) ~src:r 
	| RP.App (("lobits", [w;n]), [RP.App (("shrl", [w']), [e; RP.Const (RP.Bits b)])]) ->
            let shamt = Bits.U.to_int b in
            assert (w = w' && shamt < w);
            let t = alloc context n in
            let src, is = to_temp room (guess_context e) e in
            t, is <:> Post.extract ~dst:t ~lsb:shamt ~src
	| RP.App (("lobits", [w;n]), [e]) ->
            let t = alloc context n in
            let src, is = to_temp room (guess_context e) e in
            t, is <:> Post.extract ~dst:t ~lsb:0 ~src
	| RP.App ((("sx"|"zx") as xop, [n; w]),
          [RP.App((("carry"|"borrow"), [w']) as wrdop, args)]) ->
            if n <> 1 then Impossible.impossible "ill-typed %sx/%zx(...)";
            let signed = xop =$= "sx" in
            let x, y, z, is = compile_weird_args room wrdop args in
            let t = alloc_direct (Post.result_context wrdop) w in
            let fill = if signed then PX.HighS else PX.HighZ in
            t, is <:> Post.wrdrop ~dst:(fill,t) wrdop x y z 
	| RP.App ((("addc"|"subb"), [w]) as wrdop, args) ->
            let x, y, z, is = compile_weird_args room wrdop args in
            let t = alloc_direct (Post.result_context wrdop) w in
            t, is <:> Post.wrdop ~dst:t wrdop x y z 
	| RP.App ((("sx"|"zx") as op, [n; w]), [RP.App (("bit", []), [e])]) ->
            assert (n = 1);
            let t       = alloc context w in
            let nonzero = if op =$= "sx" then -1 else 1 in
            let tbranch = Post.li t (RP.Bits (Bits.S.of_int nonzero w)) in
            let fbranch = Post.li t (RP.Bits (Bits.zero w)) in
            let e = Up.exp e in
            let cond    = expand_cbranch (DG.cond e) in
            t, DG.If (cond, tbranch, fbranch)
	| RP.App ((_, [stackw; tempw]) as cvt, _) as e
            when Post.converts_stack_to_temp cvt ->
            let slot = exchange_slot tempw in
            let t, is = to_temp room context (fetch_slot slot tempw) in
            t, assign_slot room slot e tempw <:> is 
	| RP.App (("f2f_implicit_round", ws), [x]) ->
            to_temp room context (RP.App (("f2f", ws), [x; RP.Fetch(rounding_mode, 2)]))
	| RP.App (op, args) -> (match Post.opclass op with
            | PX.Stack(dir, depth) ->
                let w = Post.stack_width in
                let slot = exchange_slot w in
                let t, is = to_temp room context (fetch_slot slot w) in
                t, assign_slot room slot e w <:> is
            | PX.Register ->
                let w = width e in
                let t = alloc_direct (Post.result_context op) w in 
                let temp e c = to_temp room (contextmap c) e in
                let warg e c = to_warg room (contextmap c) e in
                let is = match O.capply temp warg op args (Post.arg_contexts op) with
                  | O.Binop ((x, b1), (y, b2))          -> b1 <:> b2 <:>        Post.binop t op x y
                  | O.Unop  ((x, b1))                   -> b1 <:>               Post.unop  t op x
                  | O.Binrm ((x, b1), (y, b2), (r, b3)) -> b1 <:> b2 <:> b3 <:> Post.binrm t op x y r
                  | O.Unrm  ((x, b1), (r, b2))          -> b1 <:> b2 <:>        Post.unrm  t op x r
                  | O.Fpcvt ((x, b1), (r, b2))          -> b1 <:> b2 <:>        Post.unrm  t op x r
                  | O.Dblop ((x, b1), (y, b2))          ->
                      Impossible.unimp "double-width multiply into single temporary"
                  | O.Wrdop ((x, b1), (y, b2), (z, b3)) -> b1 <:> b2 <:> b3 <:> Post.wrdop t op x y z
                  | O.Wrdrop((x, b1), (y, b2), (z, b3)) ->
                      impossf "single-bit result direct into temporary"
                  | O.Cmp _ | O.Width | O.Bool | O.Nullary ->
                      impossf "operator %%%s reached postexpander" (fst op) 
		in
                temp_in_context t context is )
	    
    and compile_weird_args room op args = match args, Post.arg_contexts op with
      | [x; y; z], [xc; yc; zc] ->
          let x, xis = to_temp room (contextmap xc) x in
          let y, yis = to_temp room (contextmap yc) y in
          let z, zis = to_warg room (contextmap zc) z in
          x, y, z, xis <:> yis <:> zis
      | _ -> impossf "wrong number of args or contexts to %%%s" (fst op)
	  
    and to_warg room context e = match e with
      | RP.Const (RP.Bits b) ->
          PX.WBits b, DG.Nop
      | RP.App (("lobits", [w; n]), [e]) ->
          let t, is = to_temp room context e in
          (PX.WTemp (Rg.Reg t)), is
      | RP.App ((("carry"|"borrow"), [w]) as wrdop, args) ->
          let x, y, z, is = compile_weird_args room wrdop args in
          let t = alloc_direct (Post.result_context wrdop) w in
          let t, is = 
            temp_in_context t context (is <:> Post.wrdrop (PX.HighAny, t) wrdop x y z) in
          (PX.WTemp (Rg.Reg t)), is
      | RP.Fetch (RP.Reg r, w) -> 
          (PX.WTemp (Rg.Reg r), DG.Nop)
      | RP.Fetch (RP.Slice (w, lsb, (RP.Reg r)), w') ->
          assert (w = w');
          (PX.WTemp (Rg.Slice (w, lsb, r)), DG.Nop)
      | RP.Fetch _ -> Impossible.unimp "narrow argument from memory"
      | e -> impossf "trying to put %s into a weird temporary" 
	  (RU.ToString.exp (Up.exp e))
	    
    and address room exp =
      let t, is = to_temp room acontext exp in
      R.fetch (R.reg t) (Register.width t), is
	
    and is_compile_time_constant = function
      | RP.Const (RP.Bits _ | RP.Late (_, _)) -> true
      | RP.App   ((("add"|"sub"), [w]), es)   -> List.for_all is_compile_time_constant es
      | _ -> false
	  
    and to_stack' room e =
      if room < 1 then
        impossf "machine stack overflow in code generation";
      if Rtlutil.Width.exp' e <> Post.stack_width then
        (Printf.eprintf "failing expression: %s\n" (Rtlutil.ToString.exp
          (Rtl.Up.exp e));
        Unsupported.stack_width ~have:(Rtlutil.Width.exp' e)
          ~want:Post.stack_width);
      Cmm_debug.eprintf "expander" "to_stack %s\n" (RU.ToString.exp (Up.exp e));
      match e with
	| RP.Const k -> Post.pushk k
	| RP.Fetch (l, _) when Post.is_stack_top_proxy l ->
            DG.Nop
	| RP.Fetch (RP.Mem (('m', agg, ms), c, addr, assn), w) ->
            let a, is = address room addr in
            assert (agg =*= Post.byte_order);
            assert (w = Cell.to_width ms c);
            is <:> Post.push a (upassn assn)
	| RP.Fetch (RP.Reg src, w) when Post.SlotTemp.is src ->
            assert (w = Register.width src);
            Post.SlotTemp.push src
	| RP.Fetch (RP.Reg _, w) ->
            let slot = exchange_slot w in
            assign_slot room slot e w <:> to_stack room (fetch_slot slot w)
	| RP.Fetch _ ->
            impossf "memory-like access to non-memory space"
	| RP.App (("f2f_implicit_round", ws), [x]) ->
            to_stack room (RP.App (("f2f", ws), [x; RP.Fetch(rounding_mode, 2)]))
	| RP.App ((("sx"|"zx"), [n; w]) as op,
          [RP.Fetch (RP.Mem (('m',agg,ms), c, addr, assn), n')]) ->
            assert (n = n' && n' = Cell.to_width ms c);
            assert (agg =*= Post.byte_order);
            let a, is = address room addr in
            is <:> Post.push_cvt op n a (upassn assn)
	| RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
          [RP.Fetch (RP.Mem (('m',agg,ms), c, addr, assn), n'); rm]) ->
            assert (n = n' && n' = Cell.to_width ms c);
            assert (agg =*= Post.byte_order);
            let rcontext = match Post.arg_contexts op with [_; c] -> c
              | _ -> impossf "rm context" in
            let r, is' = to_warg room (contextmap rcontext) rm in
            let a, is  = address room addr in
            is <:> is' <:> Post.push_cvt_rm op r n a (upassn assn)
	| RP.App ((("sx"|"zx"), [n; w]) as op,
          [RP.Fetch (RP.Reg src, n')]) when Post.SlotTemp.is src ->
            assert (n = n' && n' = Register.width src);
            Post.SlotTemp.push_cvt op n src
	| RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
          [RP.Fetch (RP.Reg src, n'); rm]) when Post.SlotTemp.is src ->
            assert (n = n' && n' = Register.width src);
            let rcontext = match Post.arg_contexts op with [_; c] -> c
              | _ -> impossf "rm context" in
            let r, is = to_warg room (contextmap rcontext) rm in
            is <:> Post.SlotTemp.push_cvt_rm op r n src
	| RP.App (("i2f", [iw; fw] as i2f), [e; rm]) ->
            let slot = exchange_slot iw in
            assign_slot room slot e iw <:>
              to_stack room (RP.App (i2f, [fetch_slot slot iw; rm]))
	| RP.App (("f2f", [w; _] as f2f), [e; rm]) ->
            let slot = exchange_slot w in
            assign_slot room slot e w <:>
              to_stack room (RP.App (f2f, [fetch_slot slot w; rm]))
	| RP.App (op, args) -> (Cmm_debug.eprintf "expander" "to_stack generic case for %s\n" (D.exp e);
          match Post.opclass op with
            | PX.Stack(dir, depth) ->
                let push room args =
                  let args = match dir with PX.LeftFirst -> args | PX.RightFirst -> List.rev args in
                  let (is, _) = List.fold_left (fun (is,r) e -> (is <:> to_stack r e, r-1))
                    (DG.Nop,room) args in
                  is in
                let temp e c = e in
                let warg e c = to_warg room (contextmap c) e in
                let compute room = match O.capply temp warg op args (Post.arg_contexts op) with
                  | O.Binop (x, y)          -> push room [x; y] <:>       Post.stack_op op
                  | O.Unop  (x)             -> push room [x]    <:>       Post.stack_op op
                  | O.Binrm (x, y, (r, b))  -> push room [x; y] <:> b <:> Post.stack_op_rm op r
                  | O.Unrm  (x, (r, b))     -> push room [x]    <:> b <:> Post.stack_op_rm op r
                  | O.Fpcvt (x, (r, b))     -> impossf "general fpcvt on stack"
                  | O.Dblop _ | O.Wrdop _ | O.Wrdrop _ ->
                      Impossible.unimp "weird value operator on stack"
                  | O.Cmp _ | O.Width | O.Bool | O.Nullary ->
                      impossf "operator %%%s reached postexpander" (fst op) in
                if room >= depth then
                  compute room
                else
                  save_stack room <:> compute Post.stack_depth <:> restore_stack 1 room
            | PX.Register ->
                let r, is = to_temp room (guess_context e) e in
                is <:> to_stack room (RP.Fetch (RP.Reg r, Register.width r)))
    and save_stack room =
      let _number_to_push = Post.stack_depth - room in
      Impossible.unimp "saving an overflowing machine stack"
    and restore_stack number_to_keep number_to_restore =
      Impossible.unimp "restoring an overflowing machine stack"
    and rtl' hr =
      let RP.Rtl gs = Dn.rtl hr in
      if trivially Post.don't_touch_me gs then
        DG.Rtl hr
      else
        match gs with
          | [(g, RP.Store (left, right, w))] -> guarded g left right w
          | [(g, RP.Kill _)] -> DG.Nop
          | [] -> DG.Nop (* a nop is a nop is a nop *)
          | ((_::_::_) as effs) -> (let make_shuffle effs =
              let strip_store = function
                | RP.Store (l, r, w) -> (l, r, w)
                | RP.Kill _ -> impossf "kill in shuffle" in
              let rec shuffle =
                let assign = noisy_assign in
                function
                  | [(l, r, w)] -> assign l r w
                  | [] -> DG.Nop
                  | ((l, (r:RP.exp), w) :: rest) as effs ->
                      let try_first_effect effects succ fail =
                        let rec maybe bad = function
                          | [] -> fail()
                          | (l, r, w) :: rest ->
                              let alias = RU.MayAlias.store_uses' l in
                              if not (List.exists alias bad || List.exists alias rest) then
                                succ (l, r, w) (List.rev_append bad rest)
                              else
                                maybe ((l, r, w) :: bad) rest in
                        maybe [] effects in
                      try_first_effect effs
                        (fun (l, r, w) rest -> assign l r w <:> shuffle rest)
                        (fun () ->
                          let t = alloc (guess_context r) w in
                          (assign (RP.Reg t) r w <:>
                            shuffle rest <:>
                            assign l (RP.Fetch (RP.Reg t, w)) w))
              in shuffle (List.map strip_store effs) in
            let effs = strip_trivial_guards effs in
            if Post.don't_touch_me effs then
              DG.Rtl (R.par (List.map Up.effect effs))
            else
              let _ = if List.exists has_pc_on_left effs then
                impossf "straight-line code has PC on left" in
              make_shuffle effs)
	      
    and wrap_don't_touch f r =
      let RP.Rtl gs = Dn.rtl r in
      if trivially Post.don't_touch_me gs then
        DG.Nop, r
      else f r
	
    and pre_rtl_to_call r =
      match Dn.exp (T.boxmach.T.call.T.project r) with
	| RP.Const c -> Post.call c
	| e          -> let r, is = to_temp Post.stack_depth acontext e in
          is <::> Post.callr r
	    
    and rtl_to_call' r = wrap_don't_touch pre_rtl_to_call r
      (* JUMP IS A PROBLEM -- WE DON'T DEAL WITH IT IN THE PX INTERFACE AT
      ALL *)
      
    and pre_rtl_to_jump r =
      pre_rtl_to_branch r
	
    and rtl_to_jump' r = wrap_don't_touch pre_rtl_to_jump r
      
    and pre_rtl_to_cut r =
      let cut_args = T.boxmach.T.cutto.T.project r in
      let to_t e = 
        let e     = Dn.exp e in
        let t, is = to_temp Post.stack_depth (guess_context e) e in
        let t = R.reg t in 
        R.fetch t (RU.Width.loc t), is in
      let sp_t, is1 = to_t cut_args.Mflow.new_sp in
      let pc_t, is2 = to_t cut_args.Mflow.new_pc in
      is1 <:> is2 <::> Post.cut_to {Mflow.new_sp = sp_t; Mflow.new_pc = pc_t}
	
    and rtl_to_cut' r = wrap_don't_touch pre_rtl_to_cut r
      
    and pre_rtl_to_branch r =
      match Dn.exp (T.boxmach.T.goto.T.project r) with
	| RP.Const c -> Post.b c
	| e          -> 
	    let r, is = to_temp Post.stack_depth acontext e 
	    in is <::> Post.br r
		 
    and rtl_to_branch' r = wrap_don't_touch pre_rtl_to_branch r
      
    and guarded g left right w =
      match g with
	| RP.Const (RP.Bool b) -> if b then assign left right w else DG.Nop
	| RP.Const _ -> impossf "non-bool constant as guard"
	| RP.Fetch _ -> impossf "fetch as guard"
	| RP.App (cmp, [_; _]) ->
            Printf.eprintf "guarded: %s\n" (Rtlutil.ToString.exp (Up.exp g));
            Impossible.unimp "guard on other than conditional branch"
	| RP.App (cmp, _) ->
            Printf.kprintf Impossible.unimp "non-binary comparison in guard: %s" 
	      (D.exp g)
	      
    and expand_cbinst g ktrue kfalse =
      let room = Post.stack_depth in
      let rec expand g ktrue kfalse =
        match g with
          | RP.Const (RP.Bool b) -> if b then ktrue else kfalse
          | RP.Const _ -> impossf "non-bool constant as conditional guard"
          | RP.Fetch _ -> impossf "fetch as conditional guard"
          | RP.App (("not", []), [g]) -> expand g kfalse ktrue
          | RP.App (("conjoin", []), [x;y]) ->
              let kfalse = DG.shared kfalse in expand x (expand y ktrue kfalse) kfalse 
          | RP.App (("disjoin", []), [x;y]) ->
              let ktrue  = DG.shared ktrue  in expand x ktrue (expand y ktrue kfalse)
          | RP.App (cmp, ([x; y] as args)) ->
              begin
		match Post.opclass cmp with
		  | PX.Register ->
                      let context = cmp_context cmp in
                      let xt, xis = to_temp room context x in
                      let yt, yis = to_temp room context y in
                      xis <:> yis <:::> Post.bc_of_guard (Post.bc_guard xt cmp yt) ktrue kfalse
		  | PX.Stack(dir, depth) ->
                      let args = List.filter is_not_rounding_mode args in (* cheat!!! *)
                      (* ROUNDING *)
                      let args =
			match dir with PX.LeftFirst -> args | PX.RightFirst -> List.rev args in
                      let push_args room =
			let is, _ =
			  List.fold_left (fun (is,r) e -> (is <:> to_stack r e, r-1))
			    (DG.Nop,room) args in
			is in
                      let compute room ktrue kfalse =
			push_args room <:::> Post.bc_stack cmp ~ifso:ktrue ~ifnot:kfalse in
                      if room >= depth then
			compute room ktrue kfalse
                      else
			(* PLAUSIBLE BUT COMPLETELY UNTESTED. *)
			let t = restore_stack 0 room <:::> ktrue  in
			let f = restore_stack 0 room <:::> kfalse in
			save_stack room <:::> compute Post.stack_depth t f
              end
          | RP.App (cmp, _) ->
              Printf.eprintf "rtl exp: %s\n" (Rtlutil.ToString.exp (Up.exp g));
              Impossible.unimp "non-binary comparison in conditional guard" 
      in
      expand g ktrue kfalse
	
    and expand_cbranch' c = match c with
      | DG.Exit x -> DG.Exit x
      | DG.Shared (u, c) -> DG.Shared(u, expand_cbranch' c)
      | DG.Test (b, (g, ktrue, kfalse)) ->
          let b = block b in
          let rec extend c = match c with
            | DG.Exit p -> DG.Exit p
            | DG.Test (b', c) -> DG.Test (b <:> b', c)
            | DG.Shared (u, c) -> DG.Shared(u, extend c) in
          extend (expand_cbinst (Dn.exp g) (expand_cbranch' ktrue) (expand_cbranch' kfalse))
	    
    and assign left right w = assign_room Post.stack_depth left right w
      
    and assign_room room left right w = 
      match right with
	| RP.App (("f2f_implicit_round", ws), [x]) ->
            assign left (RP.App (("f2f", ws), [x; RP.Fetch(rounding_mode, 2)])) w
	| _ ->
            if RU.Eq.loc left pc_lhs then  (* unconditional branch *)
              impossf "assignment to pc is not a branch"
            else if Post.is_stack_top_proxy left then
              to_stack room right
            else
              match left with
		| RP.Mem (('m',agg,memsize) as mspace, c, addr, assn) ->  (* a store *)
                    let w = Cell.to_width memsize c in
                    assert (agg =*= Post.byte_order || agg =*= Rtl.Identity);
                    let split_assignment ~lsw ~lw ~msw ~mw =
                      if Cmm_debug.on "expander" then
			Printf.eprintf "Splitting msw %s; lsw = %s\n" (D.exp msw) (D.exp lsw);
                      let lc, mc = Cell.to_count memsize lw, Cell.to_count memsize mw in
                      assert (Cell.divides memsize lw && w = lw + mw);
                      let addr = Up.exp addr in
                      let assn = Up.assertion assn in
                      let lsw, msw = Up.exp lsw, Up.exp msw in
                      let offset (R.C n) = RU.addk tgt.T.pointersize addr n in
                      match agg with
			| Rtl.LittleEndian ->
			    rtl (R.par [R.store (R.mem assn mspace lc addr) lsw lw;
                            R.store (R.mem assn mspace mc (offset lc)) msw mw])
			| Rtl.BigEndian ->                           
			    rtl (R.par [R.store (R.mem assn mspace mc addr) msw mw;
                            R.store (R.mem assn mspace lc (offset mc)) lsw lw])
			| Rtl.Identity ->
			    impossf "bad aggregation in split assignment" in
                    let a, is' = address room addr in
                    (match right with
                      | RP.App (("lobits", [w; n]), [rhs]) ->
			  let r, is = to_temp room icontext rhs in
			  is <:> is' <:> Post.lostore a r n (upassn assn)
                      | RP.Const (RP.Bits b) when w < Post.itempwidth ->
			  let rhs = RP.Const (RP.Bits (Bits.Ops.zx Post.itempwidth b)) in
			  let r, is = to_temp room icontext rhs in
			  is <:> is' <:> Post.lostore a r w (upassn assn)
                      | ( RP.App (("or", [ww]), [RP.App (("zx", [nn;ww']), [lsw]);
                        RP.App (("shl", [ww'']),
                        [RP.App (("zx", [dd;ww''']), [msw]);
                        RP.Const (RP.Bits nnb)])])
			| RP.App (("or", [ww]), [RP.App (("shl", [ww'']),
                          [RP.App (("zx", [dd;ww''']), [msw]);
                          RP.Const (RP.Bits nnb)]);
                          RP.App (("zx", [nn;ww']), [lsw])])
			) when w > Post.itempwidth && Pervasives.(<>) agg Rtl.Identity && ww = ww' && ww = ww''
			  && ww = ww''' && dd = ww - nn && BO.eq nnb (Bits.U.of_int nn ww) ->
                          split_assignment ~lsw ~lw:nn ~msw ~mw:dd
                      | RP.Const (RP.Bits b) when w > Post.itempwidth ->
			  let lw = Post.itempwidth in
			  let mw = w - lw in
			  let lsw = RP.Const (RP.Bits (BO.lobits lw b)) in
			  let msw = RP.Const (RP.Bits (BO.lobits mw (BO.shrl b (Bits.U.of_int lw w)))) in
			  split_assignment ~lsw ~lw ~msw ~mw
                      | RP.App ((("mulx"|"mulux"), [nw]) as opr, [x; y]) when w = 2 * nw ->
			  let xcon, ycon = match Post.arg_contexts opr with
			    | [x; y] -> contextmap x, contextmap y
			    | _ -> impossf "arity of extended multiply (arg contexts)" in
			  let rcon = contextmap (Post.result_context opr) in
			  let tx, i1 = to_temp room xcon x in
			  let ty, i2 = to_temp room ycon y in
			  let thi, tlo = alloc rcon nw, alloc rcon nw in
			  let i3 = Post.dblop ~dsthi:thi ~dstlo:tlo opr tx ty in
			  let v tmp = RP.Fetch (RP.Reg tmp, nw) in
			  i1 <:> i2 <:> i3 <:> split_assignment ~lsw:(v tlo) ~lw:nw ~msw:(v thi) ~mw:nw
                      | RP.App ((("mulx"|"mulux"), _), _) as e ->
			  impossf "unsupported extended multiply %s at width %d"
                            (RU.ToUnreadableString.exp (Up.exp e)) w
                      | _ ->
			  if looks_like_stack_rhs right then
			    (match right with
			      | RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
                                [RP.Fetch (RP.Mem ((_,agg,ms), c, srcaddr, srcassn), n'); rm]) ->
				  assert (n = n' && n' = Cell.to_width ms c);
				  assert (agg =*= Post.byte_order);
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is' = to_warg room (contextmap rcontext) rm in
				  let srca, is = address room srcaddr in
				  is <:> is' <:> Post.push_cvt_rm op r n srca (upassn srcassn) <:>
				    Post.store_pop a (upassn assn)
                              | RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
                                [RP.Fetch (RP.Reg src, n'); rm]) when Post.SlotTemp.is src ->
				  assert (n = n' && n' = Register.width src);
				  assert (agg =*= Post.byte_order);
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is = to_warg room (contextmap rcontext) rm in
				  is <:> Post.SlotTemp.push_cvt_rm op r n src <:> Post.store_pop a (upassn assn)
			      | RP.App ((("sx" | "zx"), [wsrc; wdst]) as op, [rhs]) ->
				  (* what goes wrong if wsrc is not the width of the stack? *)
				  let is = to_stack room rhs in
				  is <:> is' <:> Post.store_pop_cvt op wdst a (upassn assn)
                              | RP.App ((("f2f" | "f2i" | "i2f"), [wsrc; wdst]) as op, [rhs; rm]) ->
				  (* what goes wrong if wsrc is not the width of the stack? *)
				  let is = to_stack room rhs in
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is'' = to_warg room (contextmap rcontext) rm in
				  is <:> is' <:> is'' <:> Post.store_pop_cvt_rm op r wdst a (upassn assn)
			      | _ -> 
				  let is = to_stack room right in
				  is <:> is' <:> Post.store_pop a (upassn assn))
			  else
			    match right with
			      | RP.Fetch (RP.Mem(('m', _, _), ct, addr', assn'), w) ->
				  let a', is = address room addr' in
				  is <:> is' <:>
				    Post.block_copy a (upassn assn) a' (upassn assn') w
			      | RP.Fetch (RP.Reg src, _) ->
				  is' <:> Post.store a src (upassn assn)
			      | _ ->
				  let r, is = to_temp room (guess_context right) right in
				  is <:> is' <:> Post.store a r (upassn assn))
		| RP.Mem (_, _, _, _) ->
                    Impossible.unimp "memory space other than 'm'"
		| RP.Reg dst when Post.SlotTemp.is dst -> (* store to a stack slot *)
                    (*<definition of [[split_assignment_slot]]>*)
                    (match right with
                      | RP.App (("lobits", [w; n]), [rhs]) ->
			  impossf "slot-temp := %%lobits(e)   [extend postexpander?]"
                      | RP.Const (RP.Bits b) when w < Post.itempwidth ->
			  impossf "slot-temp := %%lobits(k)   [extend postexpander?]"
                      | ( RP.App (("or", [ww]), [RP.App (("zx", [nn;ww']), [lsw]);
                        RP.App (("shl", [ww'']),
                        [RP.App (("zx", [dd;ww''']), [msw]);
                        RP.Const (RP.Bits nnb)])])
			| RP.App (("or", [ww]), [RP.App (("shl", [ww'']),
                          [RP.App (("zx", [dd;ww''']), [msw]);
                          RP.Const (RP.Bits nnb)]);
                          RP.App (("zx", [nn;ww']), [lsw])])
			) when w > Post.itempwidth && ww = ww' && ww = ww''
			  && ww = ww''' && dd = ww - nn && BO.eq nnb (Bits.U.of_int nn ww) ->
                          impossf "splittable assignment to stack-slot temporary"
                      | RP.Const (RP.Bits b) when w > Post.itempwidth ->
                          unimpf "assigned wide constant to stack-slot temporary"
                      | RP.App ((("mulx"|"mulux"), [nw]), [x; y]) when w = 2 * nw ->
			  impossf "extended multiply into stack-slot temporary"
                      | RP.App ((("mulx"|"mulux"), _), _) as e ->
			  impossf "unsupported extended multiply %s at width %d"
                            (RU.ToUnreadableString.exp (Up.exp e)) w
                      | _ ->
			  if looks_like_stack_rhs right then
			    match right with
			      | RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
                                [RP.Fetch (RP.Mem ((_,agg,ms), c, srcaddr, srcassn), n'); rm]) ->
				  assert (n = n' && n' = Cell.to_width ms c);
				  assert (agg =*= Post.byte_order);
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is' = to_warg room (contextmap rcontext) rm in
				  let srca, is = address room srcaddr in
				  is <:> is' <:> Post.push_cvt_rm op r n srca (upassn srcassn) <:>
				    Post.SlotTemp.store_pop dst
                              | RP.App ((("f2f"|"f2i"|"i2f"), [n; w]) as op,
                                [RP.Fetch (RP.Reg src, n'); rm]) when Post.SlotTemp.is src ->
				  assert (n = n' && n' = Register.width src);
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is = to_warg room (contextmap rcontext) rm in
				  is <:> Post.SlotTemp.push_cvt_rm op r n src <:> Post.SlotTemp.store_pop dst
			      | RP.App ((("sx" | "zx"), [wsrc; wdst]) as op, [rhs]) ->
				  (* what goes wrong if wsrc is not the width of the stack? *)
				  let is = to_stack room rhs in
				  is <:> Post.SlotTemp.store_pop_cvt op wdst dst
                              | RP.App ((("f2f" | "f2i" | "i2f"), [wsrc; wdst]) as op, [rhs; rm]) ->
				  (* what goes wrong if wsrc is not the width of the stack? *)
				  let is = to_stack room rhs in
				  let rcontext = match Post.arg_contexts op with [_; c] -> c
                                    | _ -> impossf "rm context" in
				  let r, is'' = to_warg room (contextmap rcontext) rm in
				  is <:> is'' <:> Post.SlotTemp.store_pop_cvt_rm op r wdst dst
			      | _ -> 
				  let is = to_stack room right in
				  is <:> Post.SlotTemp.store_pop dst
			  else
			    match right with
			      | RP.Fetch (RP.Mem(('m', _, _), ct, addr, assn), w) ->
				  let a, is = address room addr in
				  is <:> Post.load dst a (upassn assn)
			      | RP.Fetch (RP.Reg src, _) -> Post.move ~dst ~src
			      | _ ->
				  let r, is = to_temp room (guess_context right) right in
				  is <:> Post.move dst r)
		| RP.Reg dst -> (* computation *)
                    (match right with
                      | RP.Fetch (RP.Reg src, _) -> Post.move ~dst ~src
                      | _ -> let r, is = to_temp room (guess_context right) right in
                        is <:> Post.move ~dst ~src:r
                    )
                      (* ROUNDING --- narrow register *)
		| RP.Slice (w, lsb, RP.Reg r) ->
                    let t, is = to_warg room icontext right in
                    is <:> Post.hwset (Rg.Slice (w, lsb, r)) t
		| RP.Slice (w, lsb, _) ->
                    impossf "back end advertises slice of non-HW in %s" (D.loc left)
		| RP.Var (_,_,_) | RP.Global(_,_,_) ->
                    impossf "variable passed to code expander"
		      
    and fetch_slot slot w = Dn.exp (A.fetch slot w)
      
    and assign_slot room slot right w =
      match Dn.rtl (A.store slot (Up.exp right) w) with
	| RP.Rtl [(RP.Const (RP.Bool true), RP.Store (left, right, w))] ->
            assign_room room left right w
	| _ -> impossf "stack slot is not a simple store"
	    
    and block b = match b with
      | DG.Rtl r        -> rtl r
      | DG.Seq (b, b')  -> block b <:> block b'
      | DG.If (c, t, f) -> DG.If (expand_cbranch c, block t, block f)
      | DG.While (e, b) -> Impossible.unimp "expand loop"
      | DG.Nop          -> DG.Nop
	  
    and branch' (b, r) = let b', r = rtl_to_branch r in (block b <:> b', r)
    and call'   (b, r) = let b', r = rtl_to_call   r in (block b <:> b', r)
    and jump'   (b, r) = let b', r = rtl_to_jump   r in (block b <:> b', r)
    and cut'    (b, r) = let b', r = rtl_to_cut    r in (block b <:> b', r)
    and rtl r =
      let _ = D.strings ["Expanding "; D.rtl r; "\n"] in
      let is = rtl' r in
      let _ = D.strings ["Expanded "; D.rtl r; " into\n"]; D.pr_block D.brtl is in
      is
	
    and rtl_to_jump r =
      let _ = D.strings ["Expanding "; D.rtl r; "\n"] in
      let is, b = rtl_to_jump' r in
      let _ = D.strings ["Expanded "; D.rtl r; " into\n"];
        D.pr_block D.brtl (is <:> DG.Rtl b) in
      is, b
	
    and rtl_to_cut r =
      let _ = D.strings ["Expanding "; D.rtl r; "\n"] in
      let is, b = rtl_to_cut' r in
      let _ = D.strings ["Expanded "; D.rtl r; " into\n"];
        D.pr_block D.brtl (is <:> DG.Rtl b) in
      is, b
	
    and rtl_to_call r =
      let _ = D.strings ["Expanding "; D.rtl r; "\n"] in
      let is, b = rtl_to_call' r in
      let _ = D.strings ["Expanded "; D.rtl r; " into\n"];
        D.pr_block D.brtl (is <:> DG.Rtl b) in
      is, b
	
    and rtl_to_branch r =
      let _ = D.strings ["Expanding "; D.rtl r; "\n"] in
      let is, b = rtl_to_branch' r in
      let _ = D.strings ["Expanded "; D.rtl r; " into\n"];
        D.pr_block D.brtl (is <:> DG.Rtl b) in
      is, b
	
    and expand_cbranch cb =
      let _ = D.strings ["Expanding conditional branch "; D.cbranch D.exp' cb; "\n"] in
      let is = expand_cbranch' cb in
      let _ =
        D.strings ["Expanded conditional branch "
		  ; " into "
		  ; D.cbranch D.brtl is; "\n"] 
      in is
	
    and noisy_assign l r w =
      let is = assign l r w in
      let r = R.store (Up.loc l) (Up.exp r) w in
      let _ = D.strings ["Shuffling "; D.rtl r; " into\n"];
        D.pr_block D.brtl is in
      is
	
    and to_temp room context e =
      let _ = D.strings ["Targeting "; D.exp e; "...\n"] in
      let t, is = to_temp' room context e in
      let _ = D.strings ["Targeted "; D.exp e; " => "; D.temp t; " by \n"] in
      let _ = D.pr_block D.brtl is in
      t, is
	
    and to_stack room e =
      let _ = D.strings ["Pushing "; D.exp e; "...\n"] in
      let is = to_stack' room e in
      let _ = D.strings ["Pushed "; D.exp e; " by \n"] in
      let _ = D.pr_block D.brtl is in
      is in
    (block, branch', expand_cbranch, call', jump', cut')
  let number = 999
  let expand f proc =
    let modified = ref false in
    let PA.T tgt = proc.Proc.target in
    let machine  = proc, tgt.Target.machine, proc.Proc.exp_of_lbl in
    let update (g, m)    = (if m then modified := true; g) in
    let block_before b g = update (G.block_before machine b g) in
    let block2cfg    b   = update (G.block2cfg    machine b)   in
    let cbranch2cfg c ~ifso ~ifnot g = update (G.cbranch2cfg machine c ifso ifnot g) in
    let (expand_block, expand_branch, expand_cbranch,
         expand_call, expand_jump, expand_cut as expanders) = expand proc in
    let g =
      if Postexpander.Alloc.isValid () then
        f expanders (block_before, block2cfg, cbranch2cfg)
      else
        (Postexpander.remember_allocators proc.Proc.temps proc.Proc.priv;
         Postexpander.remember_expanders expand_block expand_branch expand_cbranch;
         let g = f expanders (block_before, block2cfg, cbranch2cfg) in
         Postexpander.forget_allocators();
         Postexpander.forget_expanders();
         g) in
    g, !modified

  let cfg _ (cfg, proc) =
    let PA.T tgt = proc.Proc.target in
    let m = tgt.Target.machine in
    let expand_cfg (expand_block, expand_branch, expand_cbranch, expand_call,
                    expand_jump, expand_cut)
                   (block_before, block2cfg, cbranch2cfg) =
      let expand_middle n = match n with
        | GR.Instruction  i -> G.unfocus (block2cfg (expand_block (DG.Rtl i)))
        | GR.Stack_adjust _ -> G.single_middle n in
      let expand_last n =
        let to_block (b, i) n = 
          G.unfocus (block_before b (G.entry (G.single_last (G.new_rtll i n)))) in
        match n with
        | GR.Cbranch (i, ifso, ifnot) ->
            let be = (T.boxmach.T.branch.T.project i,
                      DG.Exit true, DG.Exit false) in
            let b  = expand_cbranch (DG.Test (DG.Nop, be)) in
            let g  = cbranch2cfg b ~ifso ~ifnot (G.entry G.empty) in
            G.of_block_list (G.postorder_dfs (G.unfocus g)) (* trim exit node *)
        | GR.Return    _ -> to_block (DG.Nop, m.Mflow.return) n
        | GR.Forbidden _ -> to_block (DG.Nop, m.T.forbidden) n
        | GR.Branch (r, _) | GR.Mbranch (r, _) -> to_block (expand_branch (DG.Nop, r)) n
        | GR.Jump (r, _, _) -> to_block (expand_jump (DG.Nop, r)) n
        | GR.Call {GR.cal_i = r} -> to_block (expand_call (DG.Nop, r)) n
        | GR.Cut (r, _, _) -> to_block (expand_cut (DG.Nop, r)) n
        | GR.Exit -> to_block (DG.Nop, GR.last_instr n) n in
      G.expand expand_middle expand_last cfg in
    let (g, modified) = expand expand_cfg proc in
    (g, proc), modified

  let block   proc b = fst (expand (fun (exp, _, _, _, _, _) _ -> exp b) proc)
  let goto    proc b = fst (expand (fun (_, exp, _, _, _, _) _ -> exp b) proc)
  let cbranch proc b = fst (expand (fun (_, _, exp, _, _, _) _ -> exp b) proc)
  let call    proc b = fst (expand (fun (_, _, _, exp, _, _) _ -> exp b) proc)
  let jump    proc b = fst (expand (fun (_, _, _, _, exp, _) _ -> exp b) proc)
  let cut     proc b = fst (expand (fun (_, _, _, _, _, exp) _ -> exp b) proc)

  let machine = 
    let boxexp f = { T.embed = (fun p e -> f p (DG.Nop, Box.Exp.box e))
                   ; T.project = (fun _ -> assert false) } in
    let cutto =
      { T.embed =
          (fun p ca -> cut p (DG.Nop, Box.ExpList.box [ca.Mflow.new_sp; ca.Mflow.new_pc]))
      ; T.project = (fun box -> match Box.ExpList.unbox box with
                             | [sp; pc] -> {Mflow.new_sp = sp; Mflow.new_pc = pc}
                             | _ -> assert false)} in
    let move p ~src ~dst = Post.move ~dst ~src in
    let spill p r l = 
      let w = Reg.width r in
      match Dn.rtl (l.RU.store (R.fetch (R.reg r) w) w) with
      | RP.Rtl [(RP.Const (RP.Bool true),
                 RP.Store (RP.Mem (_, _, addr, assn), _, _))] ->
          Post.store ~addr:(Up.exp addr) ~src:r (R.aligned assn)
      | _ -> Impossible.impossible "unexpected spill RTL" in
    let reload p l r = 
      let w = Reg.width r in
      match Dn.exp (l.RU.fetch w) with
      | RP.Fetch (RP.Mem (_, _, addr, assn), _) ->
          Post.load ~dst:r ~addr:(Up.exp addr) (R.aligned assn)
      | _ -> Impossible.impossible "unexpected reload exp" 
    in
    { T.bnegate     = Post.bnegate
    ; T.goto        = boxexp goto
    ; T.jump        = boxexp jump
    ; T.call        = boxexp call
    ; T.branch      = { T.embed = (fun p g -> cbranch p (DG.cond g))
                      ; T.project = (fun _ -> assert false) }
    ; T.retgt_br =
        (fun r -> Post.bc_of_guard (DG.Nop, Up.exp (branch_condition r))
          ~ifso:(DG.Exit true) ~ifnot:(DG.Exit false))
    ; T.move   = move
    ; T.spill  = spill
    ; T.reload = reload 
    ; T.cutto       = cutto
    ; Mflow.return  = Post.return
    ; T.forbidden   = Post.forbidden
    }
end
