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

include Opaque


type size_tag = SIZE
and stack_addr_tag = STACK_ADDR
and contour_id_tag = CONTOUR_ID
and tmp_tag = TMP
and gnode_id_tag = GNODE_ID
and semantic_coord_id_tag = SEMANTIC_COORD_ID_TAG
and graph_coord_id_tag = GRAPH_COORD_ID_TAG
and offset_tag = OFFSET_TAG
and typ_id_tag = TYPE_ID_TAG



(**
 **
 **  token category      token category id 
 **  meta token            0
 **  const token           1
 **  sta_proc token        2
 **  stack token           3
 **  heap token            4
 **  dll_proc token        5
 **
 **)

and token_category_tag = TOKEN_CATEGORY_TAG
and abs_set_tag = ABS_SET_TAG
and const_tag = CONST_TAG
and meta_tok_tag = META_TOK_TAG
and sta_proc_addr_tag = STA_PROC_ADDR
and heap_addr_tag = HEAP_ADDR
and dll_proc_addr_tag = DLL_PROC_ADDR
and generic_addr_tag = GENERIC_ADDR

type size = size_tag int_t
and stack_addr = stack_addr_tag int_t
and offset = offset_tag int_t
and contour_id = contour_id_tag int_t
and tmp = tmp_tag int_t
and gnode_id = gnode_id_tag int_t
and semcoord_id = semantic_coord_id_tag int_t
and graphcoord_id = graph_coord_id_tag int_t
and type_id = typ_id_tag int_t
and heap_addr = heap_addr_tag int_t
and tok_cat_id = token_category_tag int_t
and abs_set = abs_set_tag int_t
and const_id = const_tag int_t
and meta_tok_id = meta_tok_tag int_t
and sta_proc_addr = sta_proc_addr_tag int_t
and dll_proc_addr = dll_proc_addr_tag int_t
and generic_addr = generic_addr_tag int_t

let int_t = int_t
let t_int = t_int
