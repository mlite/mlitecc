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

open Typ_mem
open Format

let indent = 2

let pp_print_c_space: formatter -> unit = 
  fun fm -> pp_print_break fm 1 indent

let pp_print_c_comma: formatter -> unit = 
  fun fm -> 
    pp_print_string fm ","; 
    pp_print_cut fm ()




let rec rec_pp_print_t: formatter -> (formatter -> 'a -> unit) -> ('a -> bool) -> 'a t -> unit =
  fun fm pp_print_val has_value expr ->
    match expr with
      | Null -> pp_print_int fm 0;
      | Bits (c_type, c_val) ->
	  begin
	    pp_print_val fm !c_val
	  end
      | Scalar (c_type, c_val) ->
	  begin
	    pp_print_val fm !c_val
	  end
      | Union (typ, a, ref_e) ->
	  begin
	    let has_initialized = ref false
	    in
	    if has_value !ref_e then
	      pp_print_val fm !ref_e
	    else
	      begin
		pp_print_string fm "{";
		pp_open_hvbox fm indent;
		begin
		  pp_print_space fm ();
		  Mlite_printer.pp_print_array fm 
		    (fun fm i (offset, f_opt, typ_mem) ->
		      if (not !has_initialized) then
			begin
			  match f_opt with
			    | Some f ->
				begin
				  has_initialized := true;
				  fprintf fm ".%s" f;
				  pp_print_space fm ();
				  pp_print_string fm "=";
				  pp_print_space fm ();
				  rec_pp_print_t fm pp_print_val has_value typ_mem;
				  pp_print_c_comma fm
				end
			    | None -> ()
			end
		    ) 
		    (fun fm -> ())
		    a;
		end;
		pp_close_box fm ();
		pp_print_space fm ();
		pp_print_string fm "}"
	      end
	  end
      | Struct (typ, a, ref_e) -> 
	  begin
	    if has_value !ref_e then
	      pp_print_val fm !ref_e
	    else
	      begin
		pp_print_string fm "{";
		pp_open_hvbox fm indent;
		begin
		  pp_print_space fm ();
		  Mlite_printer.pp_print_array fm 
		    (fun fm i (offset, f_opt, typ_mem) ->
		      match f_opt with
			| Some f ->
			    begin
			      fprintf fm ".%s" f;
			      pp_print_space fm ();
			      pp_print_string fm "=";
			      pp_print_space fm ();
			      rec_pp_print_t fm pp_print_val has_value typ_mem;
			      pp_print_c_comma fm
			    end
			| None -> ()
		    ) 
		    (fun fm -> ())
		    a;
		end;
		pp_close_box fm ();
		pp_print_space fm ();
		pp_print_string fm "}"
	      end
	  end
      | Array (typ, a) ->
	  begin
	    pp_print_string fm "{";
	    pp_print_space fm ();
	    Mlite_printer.pp_print_array fm 
	      (fun fm i typ_mem ->
		fprintf fm "[%d]" i;
		pp_print_cut fm ();
		pp_print_string fm "=";
		pp_print_cut fm ();
		rec_pp_print_t fm pp_print_val has_value typ_mem;
	      ) 
	      pp_print_c_comma
	      a;
	    pp_print_space fm ();
	    pp_print_string fm "}"
	  end
      | Xarray (typ,ie,l, s) ->
	  begin
	    pp_print_string fm "{";
	    pp_print_space fm ();
	    fprintf fm "/* %d */" !s;
	    pp_print_space fm ();
	    pp_print_space fm ();
	    let index = ref 0
	    in
	    Mlite_printer.pp_print_list fm 
	      (fun fm (i, typ_mem) ->
		fprintf fm "[%d]" i;
		pp_print_cut fm ();
		pp_print_string fm "=";
		pp_print_cut fm ();
		rec_pp_print_t fm pp_print_val has_value typ_mem;
		incr index;
	      ) 
	      pp_print_c_comma
	      !l;
	    pp_print_space fm ();
	    pp_print_string fm "}"
	  end
	  
let pp_print_t: formatter -> (formatter -> 'a -> unit) -> has_value:('a -> bool) -> 'a t -> unit =
  fun fm f ~has_value t ->
    pp_open_box fm 0;
    begin
      rec_pp_print_t fm f has_value t
    end;
    pp_close_box fm ()
