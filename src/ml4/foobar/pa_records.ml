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

let make_record_expr loc l =
  let fields =
    List.map (fun ((loc, name, mut, t), default) -> 
		(<:patt< $lid:name$ >>, <:expr< $lid:name$ >>)) l in
  <:expr< { $list:fields$ } >>

let expand_record loc type_name l =
  let type_def = 
    let fields = List.map fst l in
    <:str_item< type $lid:type_name$ = { $list:fields$ } >> in
  let expr_def =
    let record_expr = make_record_expr loc l in
    let f =
      List.fold_right
	(fun ((loc, name, mut, t), default) e ->
	   match default with
	       None ->
		 <:expr< fun ~ $name$ -> $e$ >>
	     | Some x ->
		 <:expr< fun ? ($lid:name$ = $x$) -> $e$ >>)
	l
        <:expr< fun () -> $record_expr$ >> in
    <:str_item< value rec $lid: "create_" ^ type_name$ = $f$ >> in
  <:str_item< declare $type_def$; $expr_def$; end >>

EXTEND
  GLOBAL: Pcaml.str_item;

  Pcaml.str_item: LEVEL "top" [
    [ "record"; type_name = LIDENT; "="; 
      "{"; l = LIST1 field_decl SEP ";"; "}" -> expand_record loc type_name l ]
  ];

  field_decl: [
    [ mut = OPT "mutable";
      name = LIDENT; ":"; t = Pcaml.ctyp; 
      default = OPT [ "="; e = Pcaml.expr LEVEL "simple" -> e ] -> 
	((loc, name, (mut <> None), t), default) ]
  ];
END;;
