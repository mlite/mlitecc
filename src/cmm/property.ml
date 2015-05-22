let impossf fmt = Printf.kprintf Impossible.impossible fmt
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
module type S = 
sig
  type 'a t   (* a property *)

  type list
  val list  : unit -> list    (* fresh, empty property list *)
  val clear : list -> unit   (* remove all properties *)

  val get    : 'a t -> list -> 'a
  val set    : 'a t -> list -> 'a -> unit
  val remove : 'a t -> list -> unit

  val prop : 'a matcher -> 'a t
end

module M = 
struct
  type 'a plist = 'a list
  type list = { mutable props : rep plist }

  let list () = { props = [] }
  let length t = List.length t.props
  let eq t t' = t == t'
  let clear t = t.props <- []


  let numPeeks: int ref = Reinit.ref 0
  let numLinks: int ref = Reinit.ref 0
  let maxLength: int ref = Reinit.ref 0
     
  let stats () =
    Printf.eprintf "numPeeks = %d; maxLength = %d; avg pos = %4.2f"
      (!numPeeks) (!maxLength) (float (!numLinks) /. float (!numPeeks))


  let get matcher list = 
    let rec loop l n =
      let update () =
        begin
          numLinks := n + !numLinks;
          if !numLinks < n then impossf  "property list numLinks overflow";
          if n > !maxLength then maxLength := n;
        end in
      match l with
      | [] -> (update (); raise Not_found)
      | e :: l ->
          match matcher.project e with
          | Some r -> (update (); r)
          | None -> loop l (n+1) in
    numPeeks := 1 + !numPeeks;
    if !numPeeks < 1 then impossf "propery list numPeeks overflow";
    loop list.props 0 

  let remove matcher list =
    list.props <- List.filter (fun p -> not (matcher.is p)) list.props 
  let add matcher list v = list.props <- matcher.embed v :: list.props
  let set matcher list v = (remove matcher list; add matcher list v)

  type 'a t = 'a matcher
  let prop m = m

end
