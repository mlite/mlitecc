include Common
type location_set =
    { stands_for : Register.t -> bool  (* what registers we stand for *)
    ; set_doc    : string     (* informal description *)    
    }
type classification = 
    | Mem               
    | Reg               
    | Fixed
    | Temp of location_set
type t  =
    { space:            Rtl.space       (* space being described *)
    ; doc:              string          (* informal doc string *)       
    ; indexwidth:       int             (* bits *)
    ; indexlimit:       int option      (* None = 2 ** indexwidth *)
    ; widths:           int list        (* bit widths of supported aggregates *)
    ; classification:   classification  
    }
module type Standard = 
sig
  type generator  = Rtl.aggregation -> Rtl.width list -> t
  type tgenerator = Rtl.aggregation -> Rtl.width      -> t
  val m   :               generator          (* standard 8-bit memory *)
  val r   :  count:int -> generator          (* registers *)
  val t   :               tgenerator         (* register temps *)
  val f   :  count:int -> generator          (* floats *)
  val u   :               tgenerator         (* float temps *)
  val a   :  count:int -> generator          (* address registers *)
  val v   :               tgenerator         (* address temps *)
  val p   :  count:int -> generator          (* predicate registers *)
  val w   :               tgenerator         (* predicate temps *)
  val c   :  count:int -> generator          (* control and special registers *)
  val vf  :               generator          (* virtual frame pointer *)
  val x   :               generator          (* boxed rtls *)
  val s   :  int       -> tgenerator         (* stack-slots temporaries *)
  
  type 'a locations = 
    { pc:       'a
    ; npc:      'a
    ; cc:       'a
    ; fp_mode:  'a  (* FP rounding mode   *)
    ; fp_fcmp:  'a  (* FP %fcmp results   *)
    }

  val locations : c:t -> Rtl.loc locations
        (* apply to c space to get standard locations *)
  val indices : int locations (* standard indices in c space *)
  val vfp : Rtl.exp  (* the virtual frame pointer, $V[0] *)
end
  
let indexwidth n =
  let rec wid = function
    | 0 -> 0
    | n -> 1 + wid (n lsr 1)  
  in wid (n-1)
    
let checked s =
  begin
    let (_, agg, cell) = s.space in
    assert (match s.widths with [] -> false | _ :: _ -> true);
    List.iter (fun w -> assert (Cell.divides cell w)) s.widths;
    (match agg with
    | Rtl.Identity -> assert(s.widths =*= [Cell.to_width cell (Cell.C 1)])
    | _ -> ());
    (match s.indexlimit with
    | Some n -> assert (indexwidth n <= s.indexwidth)
    | _ -> ());
    s
  end
    
let stands_for s agg w =
  (fun ((s', agg', c), _, Register.C ct) ->
    s' =<= s && agg =*= agg' && ct = 1 && Cell.size c = w)
    
module Standard(A:sig val width: int end) = 
struct
  type generator  = Rtl.aggregation -> Rtl.width list -> t
  type tgenerator = Rtl.aggregation -> Rtl.width      -> t
  let m agg ws = checked 
    { space          = ('m', agg, Cell.of_size 8)
    ; doc            = "memory"
    ; indexwidth     = A.width 
    ; indexlimit     = None
    ; widths         = ws
    ; classification = Mem
    } 

  let r ~count agg ws = checked 
    { space          = ('r', agg, Cell.of_size A.width)
    ; doc            = "general-purpose registers"
    ; indexwidth     = indexwidth count
    ; indexlimit     = Some count
    ; widths         = ws
    ; classification = Reg
    }
    
  let f ~count agg ws = checked 
    { space         = ('f', agg, Cell.of_size A.width)
    ; doc           = "floating-point registers"
    ; indexwidth    = indexwidth count
    ; indexlimit    = Some count
    ; widths        = ws
    ; classification = Reg
    }
    
  let a ~count agg ws = checked 
    { space         = ('a', agg, Cell.of_size A.width)
    ; doc           = "address registers"
    ; indexwidth    = indexwidth count
    ; indexlimit    = Some count
    ; widths        = ws
    ; classification = Reg
    } 
  
  let p ~count agg ws = checked 
    { space          = ('p', agg, Cell.of_size 1)
    ; doc            = "predicate registers"
    ; indexwidth     = indexwidth count
    ; indexlimit     = Some count
    ; classification = Reg
    ; widths         = ws
    }
    

    (* CHECK THAT count IS LARGE ENOUGH TO COVER indicies BELOW -- CL*)
  let c ~count agg ws = checked 
    { space         = ('c', agg, Cell.of_size A.width)
    ; doc           = "control and special"
    ; indexwidth    = indexwidth count
    ; indexlimit    = Some count
    ; widths        = ws
    ; classification = Fixed
    } 

  let vf agg ws =
    let doc = "virtual frame pointer" in
    checked { space          = ('V', agg, Cell.of_size A.width)
            ; doc            = doc
            ; indexwidth     = 1
            ; indexlimit     = Some 1
            ; widths         = ws
            ; classification = Fixed
            } 

  let x agg ws = checked 
    { space          = ('\000', agg, Cell.of_size A.width)
    ; doc            = "boxed rtls"
    ; indexwidth     = 1
    ; indexlimit     = Some 1
    ; widths         = ws
    ; classification = Reg
    } 

  let tempspace letter ~forspace doc agg w = checked 
    { space         = (letter, Rtl.Identity, Cell.of_size w);
      doc           = doc;
      indexwidth    = A.width;
      indexlimit    = None;
      widths        = [w];
      classification = Temp  
        { stands_for = stands_for forspace agg w;
          set_doc    = doc;
        };
    } 

  let t = tempspace 't' ~forspace:'r' "general-purpose temporaries"
  let u = tempspace 'u' ~forspace:'f' "floating-point temporaries"
  let v = tempspace 'v' ~forspace:'a' "address temporaries"
  let w = tempspace 'w' ~forspace:'p' "predicate temporaries"
  
  let s align = tempspace (char_of_int align) ~forspace:'m' 
      (string_of_int align ^ " aligned stack-slots temporaries")

  type 'a locations = 
    { pc:       'a
    ; npc:      'a
    ; cc:       'a
    ; fp_mode:  'a  (* FP rounding mode   *)
    ; fp_fcmp:  'a  (* FP condition codes *)
    }
  
  let locations ~c = match c with    
    |   { space       = ('c',bo,_) as space
        ; indexwidth  = iw 
        } ->
            let reg n = Rtl.reg (space, n, Rtl.C 1) in
            { pc      = reg 0
            ; npc     = reg 1
            ; cc      = reg 2 
            ; fp_mode = Rtl.slice 2 0 (reg 4)   
            ; fp_fcmp = Rtl.slice 2 0 (reg 5) 
            }
    |   { space = (s,_,_) } -> Impossible.impossible 
            ( "Standard locations from space " ^ Char.escaped s)
                                 
  
  let indices = { pc = 0; npc = 1; cc = 2; fp_mode = 4 ; fp_fcmp = 5 }
  let vfpcell = Cell.of_size A.width
  let vfp = Rtl.fetch (Rtl.reg (('V',Rtl.Identity,vfpcell), 0, Rtl.C 1)) A.width
end

module Standard32 = Standard(struct let width = 32 end)
module Standard64 = Standard(struct let width = 64 end)

