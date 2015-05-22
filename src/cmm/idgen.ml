type generator = string -> string
let count = Reinit.ref 0
let id kind s =
  ( incr count
  ; Printf.sprintf "%s%s:%c%d" (if kind == 'l' then ".L" else "") s kind !count
  )
    
let label   = id 'l' 
let offset  = id 'o'
let exit    = id 'x'
let cont    = id 'c'
let slot    = id 's'
let block   = id 'b'
module ContEntry = struct
  let cut    = id 'C'
  let unwind = id 'U'
  let return = id 'R'
end
(* add more here as needed *)
