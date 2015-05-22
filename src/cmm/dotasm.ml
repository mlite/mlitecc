module Asm = Asm

exception Unsupported of string
let unsupported msg = raise (Unsupported msg)

let spec =
  let reserved = [] in
  let id = function
    | 'a'..'z'
    | '0'..'9'
    | 'A'..'Z'
    | '_'      -> true
    | _        -> false in
  let replace = function
    | x when id x -> x
    | _           -> '_' 
  in    
  { Mangle.preprocess = (fun x -> x)  
  ; Mangle.replace    = replace
  ; Mangle.reserved   = reserved
  ; Mangle.avoid      = (fun x -> x ^ "_")
  }
    
class ['proc] asm cfg2dot (fd:out_channel) : ['proc] Asm.assembler =
object (this)
  val mutable _section = "this can't happen"

  (* declarations *)
  method import s = Symbol.unmangled s
  method export s = Symbol.unmangled s
  method local  s = Symbol.unmangled s

  method globals n = ()

  (* sections *)
  method section s = _section <- s
  method current   = _section

  (* definitions *)
  method label s   = ()
  method const s b = ()

  (* locations *)

  method org n    = ()
  method align n  = ()
  method addloc n = ()

  method longjmp_size () =
    Impossible.unimp "longjmp size not set for dot -- needed for alternate returns"

  (* instructions *)
  method cfg_instr (proc : 'proc) =
    let (cfg, proc) = proc in
    let s   = proc.Proc.symbol in
    let mangle  = Mangle.mk spec in
    output_string fd (cfg2dot ~name:(mangle s#mangled_text) cfg)

  method zeroes n = ()
  method value v = ()
  method addr  a = ()
  method comment s = ()
  method emit = ()
end


let asm ~compress ~live fd = new asm (Cfgutil.cfg2dot ~compress ~live) fd
