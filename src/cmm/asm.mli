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

val map : ('a -> 'b) -> 'b assembler -> 'a assembler
val reloc_string : (Bits.bits -> string) -> Reloc.t -> string
     (* make string form of relocatable address using mangled text of symbols *)
