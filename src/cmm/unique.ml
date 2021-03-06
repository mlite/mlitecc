module P = Property.M
type uid = int * P.list
let uid =
  let n = Reinit.ref 1 in
  fun () -> let u = !n in (n := u + 1; (u, P.list()))
let distinguished_uid = (0, P.list())

module type MAP = 
sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem   : uid -> 'a t -> bool
  val add   : uid -> 'a -> 'a t -> 'a t
  val find  : uid -> 'a t -> 'a 
  val split : uid -> 'a t -> 'a * 'a t
    (* split k m = (find k m, remove k m) *)
  val splitp: (uid -> 'a -> bool) -> 'a t -> 'a * 'a t
    (* split based on predicate *)
  val union : 'a t -> 'a t -> 'a t          (* keep larger set on the right *)
  val fold  : (uid -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter  : ('a -> unit) -> 'a t -> unit
  val size  : 'a t -> int

  val map   : ('a -> 'b) -> ('a t -> 'b t)
end
  
module type SET = 
sig
  type t
  val empty : t
  val mem : uid -> t -> bool
  val add : uid -> t -> t
end
  
module type ARRAY = 
sig
  type 'a t
  val make : uid list -> 'a -> 'a t
  val get  : 'a t -> uid -> 'a
  val set  : 'a t -> uid -> 'a -> unit
  val update : 'a t -> uid -> ('a -> 'a) -> unit
end

module Ord = 
struct
  type t = uid
  let compare : t -> t -> int = fun (u, _) (u', _) -> compare u u'
end
  
module Set = Set.Make (Ord)
module M = Map.Make (Ord)
  
module Map : MAP = 
struct
  include M
  let split u m = (find u m, remove u m)
  let splitp p m =
    let scan u v (yes, no) = match yes with
    | None when p u v -> (Some v, no)
    | _ -> (yes, add u v no) in
    match fold scan m (None, empty) with
    | (Some v, m) -> v, m
    | (None, _) -> raise Not_found
  let union m m' =
    let add u v m =
      if mem u m then
        raise (Invalid_argument (Printf.sprintf "Unique.Map.union dup key %d" (fst u)))
      else
        add u v m in
    fold add m m'
  let size m = fold (fun _ _ n -> n + 1) m 0
  let iter f m = iter (fun _ v -> f v) m
end

module Array : ARRAY = 
struct
  type 'a t = int Map.t * 'a array
  let make uids init =
    let rec build uids i map = match uids with
    | [] -> map, Array.make i init
    | u :: us -> build us (i+1) (Map.add u i map) in
    build uids 0 Map.empty

  let get (map, a) u   = Array.get a (Map.find u map)
  let set (map, a) u v = Array.set a (Map.find u map) v
  let update (map, a) u f =
    let i = Map.find u map in
    Array.set a i (f (Array.get a i))
end

let eq ((u,_):uid) (u',_) = u = u'

module Prop = 
struct
  type 'a t = 'a P.t
  type list = uid
  let list = uid
  let clear (_, l) = P.clear l
  let get p (_, l) = P.get p l
  let set p (_, l) v = P.set p l v
  let remove p (_, l) = P.remove p l
  let prop = P.prop
end

