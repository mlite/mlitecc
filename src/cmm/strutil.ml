include Common
module Compare = struct type t = string let compare=compares end
module Set = Set.Make(Compare)
module Map = Map.Make(Compare)

let assoc2map pairs =
  let f map (key,value) = Map.add key value map 
  in
  List.fold_left f Map.empty pairs
    
let from_list xs = List.fold_left (fun set x -> Set.add x set) Set.empty xs
