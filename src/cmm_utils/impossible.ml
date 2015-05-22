exception Impossible of string

let impossible msg =
  prerr_endline ("This can't happen: " ^ msg);
  raise (Impossible msg)
let unimp msg = 
  prerr_endline ("Not implemented in qc--: " ^ msg);
  raise (Impossible msg)
