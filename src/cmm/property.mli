type rep =
  | Live_in      of Register.SetX.t  (* to be extended... *)
  | Vfp          of (Rtl.Private.exp * int)
  | Avail        of Avail.t
  | VarInMap     of Varmap.t
  | VarOutMap    of Varmap.t
  | VarOutMap'   of Varmap.y
  | VarCallInMap of Varmap.t
  | AllocPrefs   of (Register.t Register.Map.t * Register.t list Register.Map.t)
  | Distances of
            (int * (int * Varmap.def_dist Register.Map.t * Varmap.use_dist Register.Map.t))
    
type 'a matcher = { embed : 'a -> rep; project : rep -> 'a option; is : rep -> bool; }
module type S = sig
  type 'a t   (* a property *)

  type list
  val list  : unit -> list    (* fresh, empty property list *)
  val clear : list -> unit   (* remove all properties *)

  val get    : 'a t -> list -> 'a
  val set    : 'a t -> list -> 'a -> unit
  val remove : 'a t -> list -> unit

  val prop : 'a matcher -> 'a t
end
module M : S
