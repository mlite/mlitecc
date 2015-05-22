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

open Ocamlbuild_plugin
open Command
module PN = Pathname

let split path =
  let rec aux path =
    if path = Filename.current_dir_name then []
    else (Filename.basename path) :: aux (Filename.dirname path)
  in List.rev (aux path)

(* I tried to use Pcre here but I don’t see how to specify libs for
  myocamlbuild.ml *)

let ends_with ew s =
  let sl = String.length s in
  let ewl = String.length ew in
  sl >= ewl && String.sub s (sl - ewl) ewl = ew

let starts_with sw s =
  let sl = String.length s in
  let swl = String.length sw in
  sl >= swl && String.sub s 0 swl = sw

let rec find pred sofar fn =
  if starts_with "_build" fn || ends_with ".svn" fn
  then sofar
  else begin
    if PN.is_directory fn
    then
      let fns = PN.readdir fn in
      let fns = Array.map (PN.concat fn) fns in
      Array.fold_left (find pred) sofar fns
    else if pred fn then fn::sofar else sofar
  end

(* relative to _build; maybe should install tools somewhere? *)
let make_suite = A"../../tools/make_suite"

let make_tests files =
  rule "make tests"
    ~prod:"tests.ml"
    ~deps:files
    begin fun _ _ ->
        Cmd (S ([make_suite; A"-o"; Px"tests.ml"] @
               (List.map (fun s -> P s) files)));
    end

dispatch begin function
  | After_rules ->
      let is_test_fn fn =
        starts_with "test_" (Filename.basename fn) ||
          List.mem "test" (split (Filename.dirname fn))
      in
      let files = find is_test_fn [] "." in
      make_tests files;
  | _ -> ()
end
