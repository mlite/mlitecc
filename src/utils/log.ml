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

(* $Id: log.ml,v 1.5 2005/07/21 15:55:41 wangn Exp $ *)
open Collection

type log_unit = {
    unit_enable: bool;
    unit_suffix: string;
    unit_items: item list;
  }
and item = {
    item_tag: string;
    item_enable: bool;
  }
and config = {
    tag_enable: bool;
    chan_enable: bool;
    log_chan: out_channel;
  }
and t = {
    mutable count: int;
    chan_table: out_channel StringHashtbl.t;
    config_table: config StringHashtbl.t;
  }
      
    

let default = {
  count = 0;
  chan_table = StringHashtbl.create 17;
  config_table = StringHashtbl.create 211;
}
    
let init: string * log_unit list -> unit = 
  fun (basename, log_units) ->
    List.iter
      (fun log_unit ->
	let log_chan = 
	  if log_unit.unit_suffix = "" then
	    stdout
	  else
	    begin
	      try
		StringHashtbl.find default.chan_table log_unit.unit_suffix 
	      with
		Not_found ->
		  begin
		    let chan = open_out (basename ^ log_unit.unit_suffix)
		    in
		    StringHashtbl.add default.chan_table log_unit.unit_suffix chan;
		    chan
		  end
	    end
	in
	begin
	  List.iter
	    (fun item -> 
	      try
		begin
		  let _ = StringHashtbl.find default.config_table item.item_tag 
		  in
		  Printf.fprintf stderr "duplicated log_item '%s'\n" item.item_tag;
		  assert false
		end
	      with
		Not_found ->
		  let config = {
		    tag_enable = item.item_enable;
		    chan_enable = log_unit.unit_enable;
		    log_chan = log_chan;
		  } 
		  in
		  StringHashtbl.add default.config_table item.item_tag config
	    ) log_unit.unit_items
	end
      ) log_units


let close: unit -> unit = 
  fun () ->
    StringHashtbl.iter 
      (fun key chan ->
	close_out chan
      ) default.chan_table


let get: string -> bool * out_channel = 
  fun tag ->
    try 
      let config = StringHashtbl.find default.config_table tag in
      let enable = config.tag_enable & config.chan_enable 
      in
      if enable then 
        begin 
          Printf.fprintf config.log_chan 
	    "\n---SERIAL NO %d---\n" default.count;
          default.count <- default.count + 1
        end;
      (enable, config.log_chan)
    with
      Not_found -> 
	Printf.fprintf stderr "error: logging tag \"%s\" hasn't been set\n" tag;
	assert false



(** old functions **)

let (is_stdio, log_chan) = (ref false, ref None)

    
let open_log use_stdio file = 
  if file <> "" then 
    log_chan := Some (open_out file)
  else if use_stdio then 
    begin
      log_chan := Some stdout;
      is_stdio := true
    end
      
let close_log () = 
  match !log_chan with
  | Some chan' -> if not !is_stdio then close_out chan'
  | None -> ()

let flush_log () =
  match !log_chan with
  | Some chan' -> flush chan' 
  | None -> ()

	
let get_log_chan () =
  !log_chan
    
let printf = Printf.printf
(*
let printf fmt = 
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  match !log_chan with 
  | Some chan ->
      begin
	let rec doprn i =
	  if i >= len then Obj.magic () 
	  else
	    match String.unsafe_get fmt i with
	    | '%' -> Printf.scan_format fmt i cont_s cont_a cont_t cont_f
	    |  c  -> output_char chan c; doprn (succ i)
	and cont_s s i =
	  output_string chan s; doprn i
	and cont_a printer arg i =
	  printer chan arg; doprn i
	and cont_t printer i =
	  printer chan; doprn i
	and cont_f i =
	  flush chan; doprn i     
	in doprn 0
      end
  | None -> 
      begin
	let rec doprn i =
	  if i >= len then Obj.magic () else
	  match String.unsafe_get fmt i with
	  | '%' -> Printf.scan_format fmt i cont_s cont_a cont_t cont_f
	  |  c  -> doprn (succ i)
	and cont_s s i =
	  doprn i
	and cont_a printer arg i =
	  doprn i
	and cont_t printer i =
	  doprn i
	and cont_f i =
	  doprn i
	in doprn 0
      end




let printf fmt = 
  let fmt = string_of_format fmt in
  let len = String.length fmt in
  match !log_chan with 
  | Some chan ->
      begin
	let rec doprn i =
	  if i >= len then Obj.magic () 
	  else
	    match String.unsafe_get fmt i with
	    | '%' -> Printf.scan_format fmt i cont_s cont_a cont_t cont_f
	    |  c  -> output_char chan c; doprn (succ i)
	and cont_s s i =
	  output_string chan s; doprn i
	and cont_a printer arg i =
	  printer chan arg; doprn i
	and cont_t printer i =
	  printer chan; doprn i
	and cont_f i =
	  flush chan; doprn i
	in doprn 0
      end
  | None -> 
      begin
	let rec doprn i =
	  if i >= len then Obj.magic () else
	  match String.unsafe_get fmt i with
	  | '%' -> Printf.scan_format fmt i cont_s cont_a cont_t cont_f
	  |  c  -> doprn (succ i)
	and cont_s s i =
	  doprn i
	and cont_a printer arg i =
	  doprn i
	and cont_t printer i =
	  doprn i
	and cont_f i =
	  doprn i
	in doprn 0
      end
*)
