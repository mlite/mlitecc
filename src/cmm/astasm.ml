module T        = Target
module A        = Cmm_ast
module Asm      = Asm

module type PERSONALITY = sig
    val wordsize:       int
    val pointersize:    int
    val memsize:        int
    val byteorder:      Rtl.aggregation
    val float:          string
    val charset:        string
    type proc
    val cfg2ast : proc -> Cmm_ast.proc
end
module type S = sig
    type proc
    val asm: out_channel -> proc Asm.assembler
end    
module Make (P: PERSONALITY): S with type proc = P.proc = struct
    type proc = P.proc
    let pointer     = A.BitsTy(P.pointersize)
    let bits n      = A.BitsTy n
    let int n       = A.Sint( string_of_int n, Some (bits P.wordsize))
    let one         = int 1 (* wordsize *)
    let zero        = A.Sint( "0" , Some (bits P.memsize)) (* memsize *)

    exception Unsupported of string
    let unsupported msg = raise (Unsupported msg)
    type reloc = Reloc.t
    let spec =
        let reserved =
            [ "aborts"; "align"; "aligned"; "also"; "as"; "big"; "byteorder";
            "case"; "const"; "continuation"; "cut"; "cuts"; "else"; "equal";
            "export"; "foreign"; "goto"; "if"; "import"; "invariant"; "jump";
            "little"; "memsize"; "pragma"; "register"; "return"; "returns";
            "section"; "semi"; "span"; "stackdata"; "switch"; "target"; "targets";
            "to"; "typedef"; "unicode"; "unwinds"; "float"; "charset";
            "pointersize"; "wordsize"]
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
            | _           -> '@' 
            in    
                { Mangle.preprocess = (fun x -> "sym:"^x)  
                ; Mangle.replace    = replace
                ; Mangle.reserved   = reserved
                ; Mangle.avoid      = (fun x -> x ^ "$")
                }
    let reladdr (a:reloc) =
      let const b = Rtlutil.ToAST.expr (Rtl.bits b (Bits.width b)) in
      let sym (s,_) = A.Fetch (A.Name(None,s#mangled_text,None)) in
      let binop op l r = A.BinOp(l, op, r) in
      Reloc.fold ~const ~sym ~add:(binop "+") ~sub:(binop "-") a
    type init  = unit

    class asm (fd:out_channel): [proc] Asm.assembler = 
    object (this)
        val         _fd       = fd
        val mutable _section  = "this can't happen"
        val mutable _exported = Strutil.Set.empty
        val mutable _imported = Strutil.Set.empty
        val mutable _toplevel = ([]: (string * A.section list) list) (* rev'ed *)
        val mutable _actions  = ([]: A.section list) (* reversed *)                
        val         _mangle   = Mangle.mk spec

        method globals n = ()  (* probably could do better here... *)

        (* We put every top-level thingie into its own section. This helps
           the pretty printer. For John Dias *)
        method private append (a:A.section) =
            _toplevel <- (_section, [a]) :: _toplevel

        (* declare symbols *)
        method import s =
            ( _imported <- Strutil.Set.add s _imported
            ; Symbol.with_mangler _mangle s
            )
            
        method export s =
            ( _exported <- Strutil.Set.add s _exported
            ; Symbol.with_mangler _mangle s
            )
            
        method local  s = 
            Symbol.with_mangler _mangle s

        (* sections *)
        (* section closes the current section and adds it to toplevel. It is
           dropped in case it is empty. *)
        method section s = 
            ( match _actions with [] -> ()
            | _ :: _ -> _toplevel <- (_section, List.rev _actions) :: _toplevel 
            )    
            ; _section <- s
            ; _actions <- []

        method current = 
            _section
                
        (* define symbols *)        
        method label (s: Symbol.t) = 
            this#append (A.Datum(A.Label s#mangled_text))
        
        method const (s: Symbol.t) (b:Bits.bits) =
            let i = Printf.sprintf "0x%Lx" (Bits.U.to_int64 b) in 
            let bits = A.Uint(i,Some (bits (Bits.width b))) in
            this#append (A.Decl(A.Const(None,s#mangled_text,bits)))

        (* set location counter *)
        method org n =
            unsupported "no location counter in this implementation"

        method align n =
            this#append (A.Datum(A.Align n))

        method  addloc n = 
            let memsize = P.memsize in
            let ty      = bits memsize            in
            let size    = int n                   in
                this#append (A.Datum(A.MemDecl(ty,A.FixSize(size),None)))

        method longjmp_size () =
          Impossible.unimp "longjmp size not set for AST -- needed for alternate returns"

        (* instructions *)
        method cfg_instr (proc:proc) = 
                this#append (A.Procedure(P.cfg2ast proc))

        

        (* emit data *)
        method zeroes (n:int) =
            let ty      = bits P.memsize in
            let size    = int n                   in
            let rec z   = function
                | 0 -> []
                | n -> zero :: z (n-1)            in
            let _init n  = Some(A.InitExprs(z n))  in
            let init n  = None in (* fix for John's problem with SPEC benchmarks *)
                if n > 0 then
                    this#append (A.Datum(A.MemDecl(ty,A.FixSize(size),init n)))
                else
                    ()
        
        method value (v:Bits.bits) = 
            let ty   = bits (Bits.width v)              in
            let i    = Printf.sprintf "0x%Lx" (Bits.U.to_int64 v) in
            let init = A.InitExprs([A.Uint(i, Some ty)]) in
                this#append (A.Datum(A.MemDecl(ty, A.FixSize one, Some init)))
                
        method addr (a: reloc) = 
            let ty   = bits (Reloc.width a) in
            let init = A.InitExprs([reladdr a]) in
                this#append (A.Datum(A.MemDecl(ty, A.FixSize one, Some init)))

        (* the AST has comments only at the statement level. However, we are 
        outside procedures here and thus cannot issue a comment. Should this
        method call unsupported()? *)

        method comment s = ()

        method private imports =
            match Strutil.Set.elements _imported with
            | []    -> None
            | names -> Some (A.Import( Some (A.BitsTy P.pointersize)
                                     , List.map (fun n -> None, n) names
                                     ))

        method private exports =
            match Strutil.Set.elements _exported with
            | []    -> None
            | names -> Some (A.Export( Some pointer
                                     , List.map (fun n -> n, None) names
                                     )) 
        
        (* Advertise pointer sizes and such for this assembler *)

        method private personality =
            [ A.Memsize P.memsize
            ; ( match P.byteorder with
              | Rtl.BigEndian    -> A.ByteorderBig
              | Rtl.LittleEndian -> A.ByteorderLittle
              | _                -> assert false
              )
            ; A.PointerSize P.pointersize
            ; A.WordSize    P.wordsize
            ; A.Charset     P.charset
            ; A.FloatRepr   P.float
            ]
        
        (* emit takes the declarations in toplevel and completes them to a
           C-- program. Imports and exports are announced according to the
           names registered in _imported, _exported *)

        method emit = 
            let target   = A.TopDecl(A.Target this#personality)  in 
            let section  = (_section, List.rev _actions)    in
            let toplevel = List.rev (section :: _toplevel) in
            let sections = List.map (fun (name,sect) -> A.Section(name,sect))
                                    toplevel                in
            let ast      = match this#imports, this#exports with
                | None  , None   -> target :: sections
                | Some i, None   -> target :: A.TopDecl(i) :: sections
                | None  , Some e -> target :: A.TopDecl(e) :: sections
                | Some i, Some e -> target :: A.TopDecl(i) :: A.TopDecl(e) 
                                 :: sections in
            Astpp.emit _fd 72 ast
    end    
    let asm fd = new asm fd
end
