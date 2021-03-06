(* 
   MLite C Compiler -- Ning Wang <email@ningwang.org> 2006-2010
   
   The name `Mlite C Compiler' belongs to us, but the code is available free for
   any use in any field of endeavor.  You may redistribute Mlite C Compiler in
   whole or in part.  We ask that, as a matter of courtesy, you acknowledge its
   source and include this LICENSE file.  You may modify Mlite C Compiler and
   create derived works, with which you may do as you like, but the result may
   not be called Mlite C Compiler without written consent.
   
   The software is placed in the public domain.  It is not protected by copyright,
   and it is not protected by a ``copyleft'' agreement like the one used by the
   Free Software Foundation.
*)

(* This code is provided for the public domain without copyright.

   To build:

   ocamlfind ocamlc -g -package pcre -linkpkg -o make_suite \
     make_suite.ml *)

let output_tests ch tests =
  let os = output_string ch in
  os "(* This file is autogenerated.  Do not modify *)\n";
  os "open OUnit\n";
  os "\n";
  os ("let suite = \"Test Suite\" >::: [\n");
  let print_testentry s =
    Printf.fprintf ch "  \"%s\" >:: %s;\n" s s;
  in
    List.iter print_testentry tests;
  os "];;\n";
  os "\n";
  os "run_test_tt_main suite"

let module_of_filename fn =
  let fn = Filename.chop_suffix fn ".ml" in
  let fn_parts = Pcre.split ~pat:"/" fn in
  let module_parts = List.map String.capitalize fn_parts in
  String.concat "." module_parts

let with_file_in fn f =
  let ch = open_in fn in
  let r = f ch in
  close_in ch;
  r

let find_tests filenames =
  let file tests fn =
    let mn = module_of_filename fn in
    let rec line tests ch =
      try
        let l = input_line ch in
        match Pcre.extract ~pat:"^let (test_[A-Za-z0-9-_]+)" l with
          | [| _; tn |] ->
              let test = mn ^ "." ^ tn in
              line (tests @ [test]) ch
          | _ ->
              line tests ch
      with
        | Not_found -> line tests ch
        | End_of_file -> close_in ch; tests in
    with_file_in fn (line tests) in
  List.fold_left file [] filenames

let doit () =
  let output_file = ref None in
  let files = ref [] in
  Arg.parse
    [("-o",
      Arg.String (fun s -> output_file := Some s),
      "Name of output file (stdout if none given)")]
    (fun fn -> files := !files @ [fn])
    "make_suite [-o outfile] infile [infile …]";
  let tests = find_tests !files in
  let ch =
    match !output_file with
        Some fn -> open_out fn
      | None -> stdout in
  output_tests ch tests;;

doit ()
