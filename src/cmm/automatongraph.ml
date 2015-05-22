module A    = Automaton
let sprintf = Printf.sprintf
let printf  = Printf.printf
type ty = int * string * int (* width * kind * alignment *)
type path =     ty list                         (* leads to a node *)
type node    =  { regs:     Register.Set.t
                ; align:    int
                }

let compare_nodes (x:node) (y:node) =
    ( match compare x.align y.align with
    | 0 -> Register.Set.compare x.regs y.regs
    | x -> x
    )
type edge =     node * ty

let compare_edges (x:edge) (y:edge) =
    ( match Pervasives.compare (snd x) (snd y) with
    | 0 -> compare_nodes (fst x) (fst y)
    | x -> x
    )
module NS = Set.Make (struct type t=node let compare=compare_nodes end)
module ES = Set.Make (struct type t=edge let compare=compare_edges end)
module T  = Map.Make (struct type t=edge let compare=compare_edges end)

type graph =    { nodes:    NS.t            (* all the nodes *)
                ; start:    node            (* start node    *)
                ; edges:    node T.t        (* transition: node*ty => node *)
                }

let graph node = { nodes = NS.add node NS.empty
                 ; start = node
                 ; edges = T.empty
                 }
let mem edge graph = T.mem edge graph.edges     (* is edge in graph? *)
let add (n,t as edge) node graph =              (* add endpoint of edge *)
    assert (NS.mem n graph.nodes);
    { graph with edges = T.add edge node graph.edges
               ; nodes = NS.add node graph.nodes }
module ToString = struct
    let register ((sp,_,_),i,_) = sprintf "$%c%i" sp i
    let ty (width,kind,a) = sprintf "%s::%d@%d" kind width a
    let align n           = sprintf "%i:" n
    let node s            = let regs = Register.Set.elements s.regs in
                            String.concat "" 
                                ((align s.align):: List.map register regs)
    let edge src label dst= sprintf "%s --%s--> %s"
                                (node  src)
                                (ty    label)
                                (node  dst)
    let path p            = String.concat " "  (List.map ty p) 
    let paths ps          = String.concat "\n" (List.map path ps)
    let graph g = 
        let add_edge (src,label) dst strs = edge src label dst :: strs in
        let edges = T.fold add_edge g.edges [] in
            String.concat "\n" edges
end
module ToDot = struct
    let size = function
        | 32 -> ""
        | 64 -> "q"
        | n  -> sprintf ":%d" n
        
    let register ((sp,_,ms),i,c) = sprintf "$%c%i%s" sp i (size (Cell.to_width ms c))
    let ty (width,kind,a) = sprintf "%s::%d@%d" kind width a
    let node s            = let regs = Register.Set.elements s.regs in
                            sprintf "\"%i:%s\"" 
                                        s.align 
                                        (String.concat "" 
                                            (List.map register regs))
    let edge src label dst= printf "%s -> %s [label=\"%s\"]\n"
                                (node  src)
                                (node  dst)
                                (ty    label)
    
    let path p            = String.concat " "  (List.map ty p) 
    let paths ps          = List.iter (fun s -> printf "%s\n" s)
                                                (List.map path ps)
    
    let graph g = 
        ( printf "digraph \"calling convention\" {\n"
        ; printf "// nodes=%d \n" (NS.cardinal g.nodes)
        ; printf "size=\"9,6\"\n"
        ; printf "ratio=fill\n"
        ; let print_edge (src,label) dst () = edge src label dst  in
            T.fold print_edge g.edges () 
        ; printf "}\n"
        )
end
let goto mk path =
    let t    = mk ()                        in
    let ()   = List.iter (fun (w,h,a) -> ignore (A.allocate t w h a)) path in
    let res  = A.freeze t in
        { regs  = res.A.regs_used     
        ; align = res.A.align_state
        }
let registers loc =
    let c   = Rtl.bits (Bits.zero 32) 32 in
    let rtl = A.store loc c 32 in
    let (read, written) = Rtlutil.ReadWrite.sets rtl in
        written
let rec follow (mk:unit->Automaton.t) (dirs: ty list) graph path node ty =
    let path  = path @ [ty]  in
    let node' = goto mk path in
    let edge  = (node,ty)    in
    if mem edge graph then
        graph
    else
        (* assert (Register.Set.subset node.regs node'.regs) *)
        dfs mk dirs (add edge node' graph) path node'

and dfs mk dirs graph path node =   (* call this *)
    List.fold_left 
        (fun graph ty -> follow mk dirs graph path node ty) graph dirs
let _dump g = print_endline (ToString.graph g)

let print ~mk dirs =
    let init    = {regs=Register.Set.empty; align=0}   in
    let g       = graph init            in
    let g       = dfs mk dirs g [] init in
    ToDot.graph g
let next graph edge = T.find edge graph.edges
let outgoing graph node =
    let add_label (src, label) dst labels =
        if compare_nodes src node = 0 then label :: labels else labels
    in    
        T.fold add_label graph.edges []
let rec explore graph (visited:ES.t) (path:path) (paths:path list list) node =
    let labels  = outgoing graph node in
    let paths  = (List.map (fun l -> l::path) labels) :: paths in
    let follow (node, ty as edge) visited paths =
        if ES.mem edge visited then
            (paths, visited)    (* do nothing, return result *)
        else
            explore 
                graph (ES.add edge visited) (ty::path) paths (next graph edge)
    in
        List.fold_left 
            (fun (paths,visited) edge -> follow edge visited paths)
            (paths,visited)
            (List.map (fun ty -> (node,ty)) labels)
let interesting_paths graph = 
    let (paths,_) = explore graph ES.empty [] [] graph.start in
        List.map List.rev (List.concat paths)

let paths ~mk dirs =
    let init    = {regs=Register.Set.empty; align=0}   in
    let g       = graph init           in
    let g       = dfs mk dirs g [] init in
        ToDot.paths (interesting_paths g)

let mapsize m = T.fold (fun _ _ n -> n + 1) m 0

let summary ~what ~mk dirs =
    let init    = {regs=Register.Set.empty; align=0}   in
    let g       = graph init           in
    let g       = dfs mk dirs g [] init in
    Printf.printf "Automaton graph for %s has %d nodes, %d edges, %d paths\n" what
      (NS.cardinal g.nodes) (mapsize g.edges) (List.length (interesting_paths g))



