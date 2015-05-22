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

open Collection
open Tent
open Csize
	
module T = Tent
module TO = Tent_op

let fields_of v = 
  List.map (fun f -> (f.field_sname_opt, f.field_teid, f.field_offset))
    (TO.named_field_infos v)
    
let rec alloc: te -> init_val:'a -> 'a Typ_mem.t = 
  fun typ ~init_val -> 
    let typ = TO.unqualified_typ typ
    in
    let type_class = TO.te_kind_of typ
    in
    match type_class with
      | T.Struct field_table ->
	  begin
	    ignore (TO.static_sizeof typ);
	    let fields = fields_of field_table
	    in
	    let a = Array.create (List.length fields) (-1L, None, Typ_mem.Null)
	    and index = ref 0
	    in
	    List.iter
	      (fun (f, typ_id, offset) ->
		Array.set a !index (offset, f, alloc (fst typ, typ_id) ~init_val);
		incr index
	      ) fields;
	    Typ_mem.Struct (typ, a, ref init_val)
	  end
      | T.Union field_table -> 
	  begin
	    ignore (TO.static_sizeof typ);
	    let fields = fields_of field_table
	    in
	    let a = Array.create (List.length fields) (0L, None, Typ_mem.Null)
	    and index = ref 0
	    in
	    List.iter
	      (fun (f, typ_id, offset) ->
		Array.set a !index (offset, f, alloc (fst typ, typ_id) ~init_val);
		incr index
	      ) fields;
	    Typ_mem.Union (typ, a, ref init_val)
	  end

      | T.Array v ->
	  begin
	    match v.m_array_cardi with
	      | T.ARRAY_FIXED _ -> 
		  begin
		    ignore (TO.static_sizeof typ);
		    let array_cardi = ((TO.array_cardi typ))
		    and elmt_typ = TO.elmt_of typ
		    in
		    let a = 
		      Array.create (int_of_csize array_cardi) (Typ_mem.Null)
		    in
		    for i = 0 to (int_of_csize (array_cardi -$ 1L)) do
		      Array.set a i (alloc elmt_typ ~init_val)
		    done;
		    Typ_mem.Array (typ, a)
		  end
	      | _ -> 
		  camlp4_macro_exception 
		    "incomplete array is used in static initializer\n"
	  end
      | T.Bits _ ->
	  Typ_mem.Bits (typ, ref init_val)
      | _ -> 
	  Typ_mem.Scalar (typ, ref init_val)


let alloc_top: te -> init_val:'a -> 'a Typ_mem.t = 
  fun typ ~init_val -> 
    let typ = TO.unqualified_typ typ
    in
    let type_class = TO.te_kind_of typ
    in
    match type_class with
      | T.Struct field_table ->
	  begin
	    ignore (TO.static_sizeof typ);
	    let fields = fields_of field_table
	    in
	    let a = Array.create (List.length fields) (-1L, None, Typ_mem.Null)
	    and index = ref 0
	    in
	    List.iter
	      (fun (f, typ_id, offset) ->
		Array.set a !index (offset, f, alloc (fst typ, typ_id) ~init_val);
		incr index
	      ) fields;
	    Typ_mem.Struct (typ, a, ref init_val)
	  end
      | T.Union field_table -> 
	  begin
	    ignore (TO.static_sizeof typ);
	    let fields = fields_of field_table
	    in
	    let a = Array.create (List.length fields) (0L, None, Typ_mem.Null)
	    and index = ref 0
	    in
	    List.iter
	      (fun (f, typ_id, offset) ->
		Array.set a !index (offset, f, alloc (fst typ, typ_id) ~init_val);
		incr index
	      ) fields;
	    Typ_mem.Union (typ, a, ref init_val)
	  end
	    
      | T.Array v ->
	  begin
	    match v.m_array_cardi with
	      | T.ARRAY_FIXED array_cardi ->
		  begin
		    ignore (TO.static_sizeof typ);
		    let elmt_typ = TO.elmt_of typ
		    in
		    let a = Array.create (int_of_csize array_cardi) (Typ_mem.Null)
		    in
		    for i = 0 to (int_of_csize (array_cardi -$ 1L)) do
		      Array.set a i (alloc elmt_typ ~init_val)
		    done;
		    Typ_mem.Array (typ, a)
		  end
	      | T.ARRAY_VARIABLE ->
		  Typ_mem.Xarray (typ, init_val, ref [], ref 0)
	  end
      | T.Bits _ ->
	  assert false
      | _ -> 
	  Typ_mem.Scalar (typ, ref init_val)
	

let rec rec_has_values: ('a -> bool) -> 'a Typ_mem.t -> bool =
  fun has_value expr ->
    match expr with
      | Typ_mem.Null -> false
      | Typ_mem.Scalar (c_type, c_val) ->
	  has_value !c_val
      | Typ_mem.Bits (c_type, c_val) ->
	  has_value !c_val
      | Typ_mem.Union (typ, a, ref_e) 
      | Typ_mem.Struct (typ, a, ref_e) -> 
	  begin
	    if has_value !ref_e then
	      true
	    else
	      begin
		let has = ref false
		in
		Array.iter
		  (fun (offset, f, v) ->
		    has := !has or rec_has_values has_value v
		  ) a;
		!has
	      end
	  end
      | Typ_mem.Array (typ, a) ->
	  begin
	    let has = ref false
	    in
	    Array.iter
	      (fun v ->
		has := !has or rec_has_values has_value v
	      ) a;
	    !has
	  end
      | Typ_mem.Xarray (typ, init_val, l, s) ->
	  List.exists 
	    (function (i,v) -> rec_has_values has_value v) !l



let rec compile: ('a -> 'b) -> 'a Typ_mem.t -> 'b Typ_mem.t =
  fun fn typ_mem ->
    match typ_mem with
      | Typ_mem.Null -> Typ_mem.Null
      | Typ_mem.Scalar (typ, c_init_expr) ->
	  Typ_mem.Scalar (typ, ref (fn !c_init_expr))
      | Typ_mem.Bits (typ, c_init_expr) ->
	  Typ_mem.Bits (typ, ref (fn !c_init_expr))
      | Typ_mem.Struct (typ, a, ref_e) ->
	  let na = Array.create (Array.length a) (-1L, None, Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i (offset, s, e) ->
	      Array.set na i (offset, s, compile fn e)
	    ) a;
	  Typ_mem.Struct (typ, na, ref (fn !ref_e))
      | Typ_mem.Union (typ, a, ref_e) ->
	  let na = Array.create (Array.length a) (0L, None, Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i (offset, s, e) ->
	      Array.set na i (offset, s, compile fn e)
	    ) a;
	  Typ_mem.Union (typ, na, ref (fn !ref_e))
      | Typ_mem.Array (typ, a) ->
	  let na = Array.create (Array.length a) (Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i e ->
	      Array.set na i (compile fn e)
	    ) a;
	  Typ_mem.Array (typ, na)
      | Typ_mem.Xarray (typ, init_val, l, s) ->
	  let na = Array.create !s (Typ_mem.Null)
	  and elmt_typ = TO.elmt_of typ
	  in
	  let at = TO.create_array_type elmt_typ (Int64.of_int !s)
	  in
	  let _ = 
	    List.iter
	      (fun (i, e) ->
		Array.set na i (compile fn e)) !l
	  in
	  Typ_mem.Array (at, na)
	  



let rec compile_ex: (te -> te) -> ('a -> 'b) -> 'a Typ_mem.t -> 'b Typ_mem.t =
  fun lnk_typ fn typ_mem ->
    match typ_mem with
      | Typ_mem.Null -> Typ_mem.Null
      | Typ_mem.Scalar (typ, c_init_expr) ->
	  Typ_mem.Scalar (lnk_typ typ, ref (fn !c_init_expr))
	    
      | Typ_mem.Bits (typ, c_init_expr) ->
	  Typ_mem.Bits (lnk_typ typ, ref (fn !c_init_expr))
	    
      | Typ_mem.Struct (typ, a, ref_e) ->
	  let na = Array.create (Array.length a) (-1L, None, Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i (offset, s, e) ->
	      Array.set na i (offset, s, compile_ex lnk_typ fn e)
	    ) a;
	  Typ_mem.Struct (lnk_typ typ, na, ref (fn !ref_e))
	    
      | Typ_mem.Union (typ, a, ref_e) ->
	  let na = Array.create (Array.length a) (0L, None, Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i (offset, s, e) ->
	      Array.set na i (offset, s, compile_ex lnk_typ fn e)
	    ) a;
	  Typ_mem.Union (lnk_typ typ, na, ref (fn !ref_e))
	    
      | Typ_mem.Array (typ, a) ->
	  let na = Array.create (Array.length a) (Typ_mem.Null)
	  in
	  Array.iteri 
	    (fun i (e) ->
	      Array.set na i (compile_ex lnk_typ fn e)
	    ) a;
	  Typ_mem.Array (lnk_typ typ, na)
	    
      | Typ_mem.Xarray (typ, init_val, l, s) ->
	  let na = Array.create !s (Typ_mem.Null)
	  and elmt_typ = TO.elmt_of typ
	  in
	  let at = TO.create_array_type elmt_typ (Int64.of_int !s)
	  in
	  let _ = 
	    List.iter
	      (fun (i, e) ->
		Array.set na i (compile_ex lnk_typ fn e)) !l
	  in Typ_mem.Array (lnk_typ at, na)




let sizeof = function
  | Typ_mem.Null -> assert false
  | Typ_mem.Bits (t, a) -> assert false
  | Typ_mem.Scalar (t, s) -> (TO.static_sizeof t)
  | Typ_mem.Struct (t, _, _) -> (TO.static_sizeof t)
  | Typ_mem.Union (t, _, _) -> (TO.static_sizeof t)
  | Typ_mem.Array (t, a) -> (TO.static_sizeof t)
  | Typ_mem.Xarray (t, a, l, n) -> 
      assert false


let rec fold_offset: (Csize.csize -> 'a Typ_mem.t -> unit) -> Csize.csize 
  -> 'a Typ_mem.t -> unit =
  fun f x typ_mem ->
    match typ_mem with
      | Typ_mem.Null -> ()
      | Typ_mem.Scalar (typ, c_init_expr) -> 
	  f x typ_mem
	    
      | Typ_mem.Bits (typ, c_init_expr) -> 
	  f x typ_mem
	    
      | Typ_mem.Struct (typ, a, ref_e) ->
	  Array.iter
	    (fun (offset, s, e) -> 
	      let addr = x +$ offset 
	      in fold_offset f addr e
	    ) a
	    
      | Typ_mem.Union (typ, a, ref_e) ->
	  Array.iter
	    (fun (offset, s, e) ->
	      let addr = x +$ offset
	      in (fold_offset f addr e)
	    ) a
	       
      | Typ_mem.Array (typ, a) ->
	  let elmt_typ = TO.elmt_of typ
	  in
	  let elmt_size = TO.static_sizeof elmt_typ
	  in
	  Array.iteri
	    (fun i e ->
	      let addr = x +$ ((csize_of_int i) *$ elmt_size)
	      in (fold_offset f addr e)
	    ) a
	    
      | Typ_mem.Xarray (typ, init_val, l, s) -> assert false
	  
