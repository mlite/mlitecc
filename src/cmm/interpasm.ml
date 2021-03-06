(* This is not actually a noweb file, but it *is* included in front
   of every generated .ml file.  It turns off evil polymorphic functions *)

   (* permit bare = on integers only *)
let (=|=) (x:int)    (y:int)    = (=) x y
let (=<=) (x:char)   (y:char)   = (=) x y
let (=$=) (x:string) (y:string) = (=) x y
let (=:=) (x:bool)   (y:bool)   = (=) x y
let (=*=) = (=)
let (=) = (=|=)

  (* mnemonics:  |  I  int
                 <  C  char
                 $  S  string
                 :  B  bool
                 *     any type
   *)

let (<>) (x:int) (y:int) = (<>) x y


let comparei (x:int)    (y:int)    = compare x y
let comparec (x:char)   (y:char)   = compare x y
let compares (x:string) (y:string) = compare x y
let compareb (x:bool)   (y:bool)   = compare x y
let compare = comparei

let (<.)  (x:float) (y:float) = (<) x y
let (>.)  (x:float) (y:float) = (>) x y
let (<=.) (x:float) (y:float) = (<=) x y
let (>=.) (x:float) (y:float) = (>=) x y
let (<>.) (x:float) (y:float) = Pervasives.(<>) x y

let (<)  (x:int) (y:int) = (<) x y
let (>)  (x:int) (y:int) = (>) x y
let (<=) (x:int) (y:int) = (<=) x y
let (>=) (x:int) (y:int) = (>=) x y
module G  = Zipcfg
module GR = Zipcfg.Rep
module RP = Rtl.Private
module P  = Proc

type tgt   = Preast2ir.tgt
type proc' = Preast2ir.proc

module type PROC = sig
    type proc
    val emit: proc -> (string -> unit) -> unit
end

module Make (P: PROC) = struct
    type proc = P.proc
    let quoted        s = "\"" ^ s ^ "\""
    let parenthesized s = "("  ^ s ^ ")"
    let braced        s = "{"  ^ s ^ "}"

    let luafunc f args  = "CMM." ^ f ^ parenthesized args ^ "\n"

    (* sz : int , d : string list *)
    let initdata sz d  = luafunc "data" (string_of_int sz ^ "," ^
                                         (braced (String.concat "," 
                                                     (List.map quoted d))))
     
    let hexbits v      = Printf.sprintf "0x%Lx" (Bits.U.to_int64 v)

    exception Unsupported of string
    let unsupported msg = raise (Unsupported msg)
    type reloc = Reloc.t
    let spec =
        let reserved = [] (* add any reserved words here *)
        in
        let id = function
            | 'a'..'z'
            | '0'..'9'
            | 'A'..'Z'
            | '.' 
            | '_' 
            | '$' 
            | '@'      -> true
            | _        -> false in
        let replace = function
            | x when id x -> x
      (*    | _           -> '@'  *)
            | x           -> x
            in    
                { (* Mangle.preprocess = (fun x -> "sym:"^x) *)
                  Mangle.preprocess = (fun x -> x)
                ; Mangle.replace    = replace
                ; Mangle.reserved   = reserved
                ; Mangle.avoid      = (fun x -> x ^ "$")
                }
    class asm  ~(byteorder:Rtl.aggregation)
               ~(memsize:int)
               ~(ptrsize:int)
               (fd:out_channel): [proc] Asm.assembler =
    object (this)
        val mutable _section   = "this can't happen"
        val mutable _exported  = Strutil.Set.empty
        val mutable _imported  = Strutil.Set.empty
        val mutable _body      = [] (* reversed *)

        val         _mangle    = Mangle.mk spec
        val         _fd        = fd

        method private append s = _body <- s :: _body

        (* -- declare symbols -- *)
        method import s =
            ( _imported <- Strutil.Set.add (_mangle s) _imported
            ; Symbol.with_mangler _mangle s
            )
            
        method export s =
            ( _exported <- Strutil.Set.add (_mangle s) _exported
            ; Symbol.with_mangler _mangle s
            )
            
        (* FIX what is local supposed to do ? *)
        method local  s = 
            Symbol.with_mangler _mangle s


        (* -- sections -- *)
        method section s =
            ( this#append (luafunc "end_section" "")
            ; this#append (luafunc "section"     (quoted s))
            ; _section <- s
            )

        method current = _section
                
        (* define symbols *)        
        method label (s: Symbol.t) = 
            this#append (luafunc "define_label" (quoted s#mangled_text))
        
        (* FIX can we ignore constants here? *)
        method const (s: Symbol.t) (b:Bits.bits) = ()

        method globals n = this#append (luafunc "globals" (string_of_int n))

        (* set location counter *)
        method org n =
            unsupported "no location counter in this implementation"

        method align n =
            if n <> 1 then this#append (luafunc "align" (string_of_int n))

        method addloc n = 
            if n <> 0 then this#append (luafunc "skip"  (string_of_int n))

        method longjmp_size () =
          Impossible.unimp "longjmp size not set for Interp -- needed for alternate returns"

        (* instructions *)
        method cfg_instr (proc: proc) = P.emit proc (this#append)

        (* -- emit data -- *)

        (* zeroes emits n zeroes as initialized data *)
        method zeroes (n:int) =
            let rec  z   = function
                | 0 -> []
                | n -> 0 :: z (n-1) in
            if n > 0 then
                this#append (initdata (memsize) 
                                      (List.map string_of_int (z n)))
            else ()
        
        method value (v:Bits.bits) = 
            let sz = Bits.width v in
            let i  = hexbits v    in
                this#append (initdata sz [i])
            
        method addr (a: reloc) = 
            match Reloc.as_simple a with
            | None,   bits -> 
                this#append (initdata (Bits.width bits) [(hexbits bits)])
            | Some s, bits ->
                this#append (luafunc "emit_raddr" 
                                     ((quoted s#mangled_text) ^ "," ^ 
                                      (quoted (hexbits bits))))

        (* we throw comments in as Lua comments... *)
        method comment s = this#append ("-- " ^ s ^ "\n")

        method private imports =
            match Strutil.Set.elements _imported with
            | []    -> ""
            | names -> luafunc "imports" 
                               (braced (String.concat "," 
                                            (List.map quoted
                                                (Strutil.Set.elements _imported))))

        method private exports =
            match Strutil.Set.elements _exported with
            | []    -> ""
            | names -> luafunc "exports" 
                               (braced (String.concat "," 
                                            (List.map quoted
                                                (Strutil.Set.elements _exported))))
        
        (* -- Advertise pointer sizes and such for this assembler -- *)

        method private personality =
              luafunc "memsize"   (string_of_int memsize)
            ^ luafunc "byteorder" ( match byteorder with
                                    | Rtl.BigEndian    -> quoted "NATIVE"
                                    | Rtl.LittleEndian -> quoted "NATIVE"
                                    | _                -> assert false
                                  )
            ^ luafunc "ptrsize"   (string_of_int ptrsize)
        
        method emit = 
            let toplevel = this#imports ^ this#exports ^ this#personality in
            ( output_string _fd toplevel
            ; output_string _fd (String.concat "" (List.rev _body))
            ; output_string _fd (luafunc "end_section" "")
            )

    end    

    let asm = new asm 
end

module New = struct
    type proc = proc'

    (* we ask for more stackdata space than we strictly need; leftover space
       we use for storing continuation information *)
    let emit ((cfg, proc):proc) printer =
        (* this code for emitting stackdata info. stolen from runtimedata.nw *)
        let fail () = Impossible.impossible "found illegal stackdata span value" in
        let rec extract_offset l = match Rtl.Dn.loc l with
          | RP.Mem (_,_, e, _) ->
              (match Rtl.Dn.exp (Simplify.exp (Rtl.Up.exp e)) with
               | RP.Fetch (vfp,_) when Vfp.is_vfp vfp -> 0
               | RP.App(("add", _), [RP.Fetch (vfp,_); RP.Const (RP.Bits n)])
                   when Vfp.is_vfp vfp -> Bits.U.to_int n
               | RP.App(("add", _), [RP.Const (RP.Bits n); RP.Fetch (vfp,_)])
                   when Vfp.is_vfp vfp -> Bits.U.to_int n
               | _ -> fail ())
          | _ -> fail () in

        let stackdata_decls printer =
          (* We have to find a node with spans -- it will have stack data spans.*)
          let spans = G.fold_spans (fun spans _ -> Some spans) cfg None in
          match spans with
          | Some ss ->
            let spans = Runtimedata.stackdata ss in
            ( printer "{ "
            ; List.iter (fun e -> printer (Printf.sprintf "%d," (extract_offset e)))
                        spans
            ; printer " }"
            )
          | None -> printer "{}" in
        ( printer (Printf.sprintf "CMM.procedure ('%s',%d,%d,"
                      (proc.Proc.symbol#mangled_text)
                      proc.Proc.vars
                      (Block.size proc.Proc.stackd+Block.size proc.Proc.conts))
        ; stackdata_decls printer
        ; printer ")\n"
        ; Interpemit.proc (cfg,proc) printer
        ; printer "CMM.end_procedure()\n"
        )
end


module NewAsm = Make(New)
let asm' = NewAsm.asm

