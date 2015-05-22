class type ['proc] assembler = object
    (* declarations *)
    method import : string -> Symbol.t        (* any name is legal *)
    method export : string -> Symbol.t        (* any name is legal *)
    method local  : string -> Symbol.t        (* any name is legal *)

    (* definitions *)
    method label  : Symbol.t -> unit  (*bind symbol to current location counter*)
    method const  : Symbol.t -> Bits.bits -> unit (*bind symbol to constant*)

    (* section, location counter *)
    method section : string -> unit
    method current : string                 (* req: section called *)

    method org     : int -> unit            (* set location counter       *)
    method align   : int -> unit            (* align location counter     *)
    method addloc  : int -> unit            (* increment location counter *)

    (* size (bytes) of long jump instruction *)
    method longjmp_size : unit -> int

    (* emit instructions - add more methods for different instr types *)
    method cfg_instr   : 'proc -> unit

    (* emit data *)
    method value  : Bits.bits -> unit
    method addr   : Reloc.t -> unit  
    method zeroes : int -> unit             (* n bytes of zeroes *)

    (* announce number of global variables -- used only in interpreter *)
    method globals : int -> unit            (* allocate space for n globals (not bytes) *)

    (* comment *)
    method comment: string -> unit 

    (* emit *)
    method emit: unit                       (* finalize *)
        (* should probably be called progend *)
end

class ['proc] mapped_asm f (asm : 'a assembler) : ['proc] assembler =
object
    (* declarations *)
    method import s = asm#import s
    method export s = asm#export s
    method local  s = asm#local s

    (* sections *)
    method section s = asm#section s
    method current   = asm#current 

    (* definitions *)
    method label s   = asm#label s
    method const s b = asm#const s b

    (* locations *)

    method org n    = asm#org n
    method align n  = asm#align n
    method addloc n = asm#addloc n

    (* instructions *)
    method longjmp_size () = asm#longjmp_size ()
    method cfg_instr proc = asm#cfg_instr (f proc)

    method globals n = asm#globals n
    method zeroes n = asm#zeroes n
    method value v = asm#value v
    method addr  a = asm#addr a
    method comment s = asm#comment s
    method emit = asm#emit
end
let map f asm = new mapped_asm f asm
let reloc_string const =
  let sym (s, _) = s#mangled_text in
  let infix op a b = String.concat "" [a; " "; op; " "; b] in
  Reloc.fold ~const ~sym ~add:(infix "+") ~sub:(infix "-")
