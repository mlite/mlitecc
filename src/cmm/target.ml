type ('em, 'pr) map' = ('em, 'pr) Ep.pre_map =
    { 
      embed   : 'em; 
      project : 'pr
    }
      
type brtl = Rtl.exp -> Rtl.rtl
type ('a,'b) map  = ('a -> 'b -> brtl Dag.branch, Rtl.rtl -> 'b) map'
type ('a,'b) mapc = ('a -> 'b -> brtl Dag.cbranch, Rtl.rtl -> 'b) map'

type 'a machine = 'a Mflow.machine =
    { bnegate:   Rtl.rtl -> Rtl.rtl
    ; goto:      ('a, Rtl.exp) map
    ; jump:      ('a, Rtl.exp) map
    ; call:      ('a, Rtl.exp) map
    ; branch:    ('a, Rtl.exp) mapc (* condition *)
    ; retgt_br:  Rtl.rtl -> brtl Dag.cbranch
    ; move:      'a -> src:Register.t -> dst:Register.t -> brtl Dag.block
    ; spill:     'a -> Register.t -> Rtlutil.aloc -> brtl Dag.block
    ; reload:    'a -> Rtlutil.aloc -> Register.t -> brtl Dag.block
    ; cutto:     ('a, Mflow.cut_args) map    (* newpc * newsp map*)
    ; return:    Rtl.rtl
    ; forbidden: Rtl.rtl   (* causes a run-time error *)
    }
      
module DG = Dag
module M  = Mflow
module R  = Rtl
module S  = Space
module RP = Rtl.Private
module RU = Rtlutil
  
type 'automaton cc' =      
    { sp:           Rtl.loc         (* stack pointer                      *)
    ; return:       Rtl.rtl         (* machine instr passed to Cfg.return *)
      (* NEEDS TO TAKE ALTS AND INDEX AS PARAMETER *)
    ; proc:         'automaton  (* pass parameter to a procedure      *)
    ; cont:         'automaton  (* pass parameter to a continuation   *)
    ; ret:          'automaton  (* return values                      *)
    ; mutable allocatable:  Register.t list (* regs for reg-allocation            *)
      (* THIS TYPE SHOULD INCLUDE INFORMATION ABOUT ALTERNATE RETURN CONTINUATIONS,
	 IN PARTICULAR, HOW BIG IS EACH SLOT, AND DOES IT HOLD AN INSTRUCTION OR
	 AN ADDRESS? *)
    ; stack_slots:  'automaton (* where private data go *)
    }
      
type capabilities = 
    {
      operators   : Rtl.opr   list;  (* operators that can be used *)
      literals    : Rtl.width list;  (* literals that can be used *)
      litops      : Rtl.opr   list;  (* operators usable on literals only *)
      memory      : Rtl.width list;  (* memory refs that can be used *)
      block_copy  : bool;            (* OK to copy large variables, large refs? *)
      itemps      : Rtl.width list;  (* what integer temporaries can be computed with *)
      ftemps      : Rtl.width list;  (* what floating temporaries can be computed with *)
      iwiden      : bool;            (* use int ops and literals at narrow widths? *)
      fwiden      : bool;            (* use float ops literals at narrow widths? *)
    }
      
type ('proc, 'automaton, 'cc) t = 
    { 
      name: string;
      mutable cc_specs: Automaton.cc_specs;
      cc_spec_to_auto: string -> Automaton.cc_spec -> 'cc;
      vfp : Rtl.exp;   (* the (immutable) virtual frame pointer *)
      (* always equal to Vfp.mk T.pointersize *)
      byteorder:      Rtl.aggregation   ; (* big/little endian, id *)
      wordsize:       int               ; (* bits *)
      pointersize:    int               ; (* bits *)
      alignment:      int               ; (* alignment of word access *)
      memsize:        int               ; (* smallest addressable unit, typically 8 *)
      memspace:       Rtl.space         ; (* redundant with byte order, word size *)
      float:          Float.t           ; (* floating pt name and semantics    *)
      charset:        string            ; (* "latin1"  character encoding      *)
      globals:        'automaton   ; (* Automaton to allocate global vars *)
      max_unaligned_load : Rtl.count;  (* how many cells to load unaligned *)
      spaces:          Space.t list;
      reg_ix_map :     int * int Register.Map.t;
      distinct_addr_sp:  bool;
      data_section:   string;          (* ASM section for global regs *)
      rounding_mode : Rtl.loc;
      named_locs:    Rtl.loc Strutil.Map.t;
      is_instruction: Rtl.rtl -> bool;
      machine : 'proc machine;
      tx_ast : Auxfuns.void Nelab.compunit -> Auxfuns.void Nelab.compunit;
      capabilities: capabilities; 
    }
      
let boxmach =
  let fail _ = assert false in
  let crmap = { embed = (fun () e -> (DG.Nop, Box.Exp.box e))
              ; project = Box.Exp.unbox } in
  let x = (Space.Standard32.x R.LittleEndian [32]).Space.space in
  let bogus  = R.kill (R.reg (x, 0, (R.C 1))) in
  { bnegate = (fun _ -> assert false)
  ; goto = crmap
  ; jump = crmap
  ; call = crmap
  ; cutto = { embed =
      (fun _ ca -> (DG.Nop, Box.ExpList.box [ca.M.new_sp; ca.M.new_pc]))
            ; project = (fun box -> match Box.ExpList.unbox box with
              | [sp; pc] -> {M.new_sp = sp; M.new_pc = pc}
              | _ -> assert false)}
  ; branch = { embed   = (fun _ g -> DG.cond (fun _ -> Box.Guard.box g))
             ; project = (fun box -> Box.Guard.unbox box)}
  ; retgt_br  = fail
  ; move      = fail
  ; spill     = fail
  ; reload    = fail
  ; M.return  = bogus
  ; forbidden = bogus
  }
    
let impossf fmt = Printf.kprintf Impossible.impossible fmt
  
let incapable = 
  { operators = []; literals = []; litops = []; memory = []; itemps = [];
  ftemps = []; block_copy = false; iwiden = false; fwiden = false; }
    
let minimal_capabilities wordsize =
  { literals = [wordsize]
  ; block_copy = false
  ; itemps = [wordsize]
  ; ftemps = []
  ; iwiden = false
  ; fwiden = false
  ; litops = []
  ; memory = [wordsize]
  ; operators = List.map Rtl.Up.opr 
      [
	"sx",      [ 1; wordsize]
	; "zx",      [ 8; wordsize]
	  (* for some reason, -interp always generates 8->32 zx instructions *)
	; "lobits",  [wordsize;  1]
	; "lobits",  [wordsize; wordsize]
	; "add",     [wordsize]
	; "addc",    [wordsize]
	; "and",     [wordsize]
	; "borrow",  [wordsize]
	; "carry",   [wordsize]
	; "com",     [wordsize]
	; "div",     [wordsize]
	; "divu",    [wordsize]
	; "false", []
	; "mod",     [wordsize]
	; "modu",    [wordsize]
	; "mul",     [wordsize]
	; "mulx",    [wordsize]
	; "mulux",   [wordsize]
	; "neg",     [wordsize]
	; "or",      [wordsize]
	; "quot",    [wordsize]
	; "popcnt",  [wordsize]
	; "rem",     [wordsize]
	; "rotl",    [wordsize]
	; "rotr",    [wordsize]
	; "shl",     [wordsize]
	; "shra",    [wordsize]
	; "shrl",    [wordsize]
	; "sub",     [wordsize]
	; "subb",    [wordsize]
	; "true", []
	; "xor",     [wordsize]
	; "eq",      [wordsize]
	; "ge",      [wordsize]
	; "geu",     [wordsize]
	; "gt",      [wordsize]
	; "gtu",     [wordsize]
	; "le",      [wordsize]
	; "leu",     [wordsize]
	; "lt",      [wordsize]
	; "ltu",     [wordsize]
	; "ne",      [wordsize]

	;  "add_overflows",  [wordsize]
	;  "div_overflows",  [wordsize]
	;  "mul_overflows",  [wordsize]
	;  "mulu_overflows", [wordsize]
	;  "quot_overflows", [wordsize]
	;  "sub_overflows",  [wordsize]
	; "not",     []
	; "bool",    []
	; "disjoin", []
	; "conjoin", []
	; "bit",     []
      ];}

let space t s = 
  try List.find (fun x -> RU.Eq.space x.Space.space s) t.spaces
  with Not_found ->
    let (s, _, _) = s in
    impossf "Space '%c' not found in target '%s'\n" s t.name
      
let is_tmp { spaces = spaces } =
  List.fold_right
    (fun s rest ->
      match s.Space.classification with
	| Space.Temp _ -> (fun c -> RU.Eq.space c s.Space.space || rest c)
	| _ -> rest)
    spaces
    (fun _ -> false)
    
let mk_reg_ix_map spaces =
  let space_to_regs space =
    let rec list_to n =
      if n <= 0 then [0]
      else n :: list_to (n-1) in
    List.map (fun i -> (space.S.space, i, R.C 1))
      (list_to (match space.S.indexlimit with
        | Some l -> l
        | None   -> 1 lsl space.S.indexwidth)) 
  in
  let regspaces =
    let keep s = match s.S.classification with S.Reg | S.Fixed -> true | _ -> false in
    List.filter keep spaces 
  in
  Register.reg_int_map (List.concat (List.map space_to_regs regspaces))
    
let space_fits tmp =
  match tmp.Space.classification with
    | Space.Temp {Space.stands_for=ok} -> ok
    | _ -> let (sp, _, _) = tmp.Space.space in
      impossf "space_fits called on non-temp of space '%c'" sp

let fits t space =
  try space_fits (List.find (fun s -> RU.Eq.space s.Space.space space) t.spaces)
  with Not_found -> impossf "space not found in Target.fits"
