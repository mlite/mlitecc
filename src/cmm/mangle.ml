
type strset         = Strutil.Set.t
type strmap         = (string,string) Hashtbl.t

type t              = string -> string
type spec           = 
    { preprocess:  string -> string
    ; replace:     char -> char
    ; reserved:    string list
    ; avoid:       string -> string
    }
      
type state          = 
    { mutable used: strset
    ; map:          strmap (* is also mutable *)
    }
      
let mk spec =
  let state    = 
    { used = Strutil.Set.empty
    ; map  = Hashtbl.create 997 (* initial size *)
    } 
  in
  let rec avoid s  =
    if   Strutil.Set.mem s state.used 
    then avoid (spec.avoid s)
    else s 
  in
  let newMangle s =
    let s'   = String.copy (spec.preprocess s) in
    let _    = for i = 0 to (String.length s') - 1 do
      String.set s' i (spec.replace (String.get s' i))
    done in
    let s'   = avoid s' in
    ( state.used <- Strutil.Set.add s'   state.used
    ; Hashtbl.add state.map s s' 
    ; s'
    )
  in         
  let mangle s = 
    try  Hashtbl.find state.map s
    with Not_found -> newMangle s
  in
  mangle
