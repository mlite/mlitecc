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
      
val checked : t -> t
  
module type Standard = sig
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
  
module Standard32 : Standard
module Standard64 : Standard
val stands_for : char -> Register.aggregation -> Register.width -> (Register.t -> bool)
