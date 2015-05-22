let debugging =
  try Str.split (Str.regexp ",") (Sys.getenv "QCDEBUG") with _ -> []
let on s = List.mem s debugging

let refs = ref ([] : (string * string) list)
let register ~word ~meaning =
  let (<) : string -> string -> bool = Pervasives.(<) in
  let rec insert word meaning = function
    | [] -> [(word, meaning)]
    | ((w, m) as p) :: ps when word < w -> (word, meaning) :: p :: ps
    | p :: ps -> p :: insert word meaning ps in
  if List.mem_assoc word (!refs) then
    Impossible.impossible ("Cmm_debug.register: multiple registrations of " ^ word)
  else
    refs := insert word meaning (!refs)

let rec ign x = Obj.magic ign
let print_and_flush s = (prerr_string s; flush stderr)
let eprintf s = if on s then Printf.kprintf print_and_flush else ign
(* for reasons known only to INRIA, eprintf and kprintf have different types,
   so the above will not typecheck *)
let eprintf s = if on s then Printf.eprintf else ign
let explain () =
  Printf.eprintf "Words recognized in QCDEBUG:\n";
  List.iter (fun (w, e) -> Printf.eprintf "  %-15s  %s\n" w e) (!refs)
