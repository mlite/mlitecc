include Common
module type S = 
sig
  type decl
  exception Cycle of decl list
  val sort: decl list -> decl list (* raises Cycle *)
end
  
module type Sortable = 
sig
  type decl
  val defines : decl -> string list
  val uses    : decl -> string list
end

module ID       = struct type t=string let compare=compares end
module IDMap    = Map.Make(ID)

module Make (S: Sortable) = 
struct
  type decl = S.decl
  exception Cycle of decl list 

  let declmap decls =
    let find key  map    = try IDMap.find key map with Not_found -> [] in
    let add  d map key   = IDMap.add key (d :: find key map) map       in
    let add' map d       = List.fold_left (add d) map (S.defines d)    in
    List.fold_left add' IDMap.empty decls
  let succ map x =
    let find map n = try IDMap.find n map with Not_found -> [] in
    List.concat (List.map (find map) (S.uses x))
  let sort decls =
    let m             = declmap decls                               in
    let nv x (v,c)    = x::v ,c                                     in
    let rec sort todo path (visited, cycles) =
      match todo with
        | []    -> visited, cycles
        | x::xs ->
            let vc = if List.mem x path         then visited, x::cycles
            else if List.mem x visited then visited, cycles
            else nv x (sort (succ m x) (x::path) (visited,cycles))
            in
            sort xs path vc
    in
    match sort decls [] ([],[]) with
      | dd, [] -> List.rev dd
      | dd, cc -> raise (Cycle cc)
end
