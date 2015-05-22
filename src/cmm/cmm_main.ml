open Format

module E  = Error

let tokenize v = 
  let map = Srcmap.mk ()
  in
  Cmm_scan.token v map
    
module Cmm_Syntax =
  struct
    type token = Cmm_parse.token
    type t = Cmm_ast.program
    let lexer_init = (fun () -> ())
    let tokenize = tokenize
    let parse = Cmm_parse.program
  end

module Cmm_Syntax_Analyzer = 
  Parser_driver.Make(Cmm_Syntax) (Cmm_handle)


let test_scanner = ref false
let test_parser = ref false

module Cmm_Opt:Exec.UserOptType =
  struct
    let banner =
      "Hoare Logic Compiler ... Ning Wang Copyright 2007\n" ^
	"SYNTAX:\thlc [options] files...\n" ^
	"\thlc [options] --\n"

    let user_opts () = []
  end

module Cmm_exec = Exec.Make(Cmm_Opt)(Cmm_Syntax_Analyzer)

let process args out_chan program =
  let fm = Format.formatter_of_out_channel out_chan
  in Cmm_ast_printer.pp_print_t fm program
  

let scan file =
  let fd          = try open_in file
  with Sys_error(msg) -> E.error msg in
  let finally ()  = close_in fd in
  let lexbuf      = Lexing.from_channel fd in
  let map         = Srcmap.mk () in
  let scanner     = Cmm_scan.scan map in
  let location lb = Srcmap.location map (Lexing.lexeme_start lb) in
  let rec loop lb = match scanner lb with
    | Cmm_parse.EOF -> ()
    | tok ->
        let (file,line,col) = location lb in
        let tok = Cmm_scan.tok2str tok 
	in
        ( Printf.printf "%-16s %3d %2d %s\n" file line col tok
        ; loop lb
        )
  in
  ( Srcmap.sync map 0 (file,1,1)
  ; loop lexbuf
  ; finally ()
  )


let parse (file:string) = 
  let fd = 
    try open_in file 
    with Sys_error(msg) -> E.error msg 
  in
  let finally ()  = close_in fd in
  let lexbuf      = Lexing.from_channel fd in
  let map         = Srcmap.mk () in
  let scanner     = Cmm_scan.scan map in
  try
    ( Srcmap.sync map 0 (file,1,1)
    ; (map, Cmm_parse.program scanner lexbuf) 
    )
  with
    | Parsing.Parse_error ->
        ( finally()
        ; E.errorPointPrt (map, Lexing.lexeme_start lexbuf) "parse error"
        ; E.error "parse error - compilation aborted"
        )
    | E.ErrorExn msg ->
        ( finally()
        ; E.errorPointPrt (map, Lexing.lexeme_start lexbuf) msg
        ; E.error "parse error - compilation aborted"
        )
    | e ->
        ( finally()
        ; raise e
        )
	
let banner =
  "SYNTAX:\tctoxml [options] files...\n" ^
  "\tctoxml [options] --\n"


let outfile = ref ""

let user_opts =
  [ ("-scan", Arg.Set test_scanner, "test scanner")
  ; ("-parse", Arg.Set test_parser, "test parser")
  ; ("-o", Arg.Set_string outfile, "output file");
  ]


module PA = Preast2ir


module Personality = struct
  type proc = Ast2ir.proc
  let target      = Interp.target'
  let memsize     = 8
  let wordsize    = 32
  let pointersize = wordsize
  let byteorder   = Rtl.LittleEndian
  let charset     = "latin1"
  let float       = "ieee754"
    
  let cfg2ast (g, proc) =
    Cfgutil.cfg2ast Rtlutil.ToAST.rtl g proc.Proc.symbol#mangled_text
end
module AstAsm = Astasm.Make(Personality)



let _ =
  let files = ref []
  in
  Arg.parse user_opts
    (fun file -> files := file::!files) banner;
  
  if List.length !files <> 1 then
    Arg.usage user_opts banner
  else
    let c_file = (List.hd !files) (** db file **)
    in
    if !test_scanner then
      let _ = scan c_file
      in ()
    else
      begin
	let (map, program) = parse c_file
	in
	if !test_parser then
	  begin
	    let fm = Format.formatter_of_out_channel stdout
	    in Cmm_ast_printer.pp_print_t fm program
	  end;
	let nast = Nast.program program
	and asm = (Interpasm.asm' 
	  ~byteorder:Rtl.LittleEndian ~memsize:8 ~ptrsize:32) stdout
	and validate = (fun _ -> None)
	and PA.T target = Interp.target'
	in
	let asm = AstAsm.asm stdout
	in
	let asm = (Dotasm.asm ~compress:true ~live:false) stdout
	in
	let abort () = Error.error "compilation aborted because of errors" in
	let (env, compunit) = 
	  match Nelab.program ~swap:false validate map asm nast with
	    | Error.Ok x -> x 
	    | Error.Error -> abort ()
	in 
	let () = Ast2ir.translate (PA.T target) (Fenv.clean env) 
	  ~optimizer:(fun proc -> asm#cfg_instr proc)
	  ~defineglobals:false compunit
	in
	asm#emit;
      end
      
	   
    
