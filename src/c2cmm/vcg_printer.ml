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

open Format

let pp_print_vcg_edge:formatter -> src:int -> dest:int -> unit = 
  fun fm ~src ~dest ->
    pp_open_vbox fm 0;
    begin
      pp_print_string fm "edge:";
      pp_print_space fm ();
      pp_print_string fm "{";
      pp_open_vbox fm 2;
      begin
	pp_print_space fm ();
	
	fprintf fm "sourcename:\"%d\"" src;
	pp_print_space fm ();
	fprintf fm "targetname:\"%d\"" dest;
	pp_print_space fm ();
	fprintf fm "color: darkgreen";
	pp_print_space fm ();
	fprintf fm "textcolor: red";
	pp_print_space fm ();
	fprintf fm "arrowsize: 13";
      end;
      pp_close_box fm ();
      pp_print_space fm ();
      pp_print_string fm  "}";
    end;
    pp_close_box fm ()

let pp_print_vcg_node: formatter -> (int -> 'a -> Vcg.vcg_node_att) option -> 
  int -> 'a -> int -> unit  = 
    fun fm get_vcg_att_opt nid t vo ->
      pp_open_vbox fm 0;
      begin
	pp_print_string fm "node:";
	pp_print_space fm ();
	pp_print_string fm "{";
	pp_open_vbox fm 2;
	begin
	  pp_print_space fm ();
	  let vcg_node_att = 
	    match get_vcg_att_opt with
	    | Some f -> f nid t
	    | None -> Vcg.default_vcg_node_att
	  in
	  fprintf fm "title: \"%d\"" nid;
	  pp_print_space fm ();
	  
	  fprintf fm "label: \"[%d]%s\"" 
	    nid (String.escaped vcg_node_att.Vcg.label);
	  pp_print_space fm ();
	  
	  fprintf fm "shape: %s"  vcg_node_att.Vcg.shape;
	  pp_print_space fm ();
	  
	  fprintf fm "color: %s"  vcg_node_att.Vcg.bg_color;
	  pp_print_space fm ();
	  
	  fprintf fm "textcolor: %s" vcg_node_att.Vcg.fg_color;
	  pp_print_space fm ();
	  
	  fprintf fm "vertical_order: %d" vo;
	end;
	pp_close_box fm ();
	pp_print_space fm ();
	pp_print_string fm "}";
      end;
      pp_close_box fm ()
	
let to_vcg: string -> 'a array -> succs:('a -> int list) -> rank_opt:(int -> int) option ->
  get_vcg_node_att_opt:(int -> 'a -> Vcg.vcg_node_att) option -> unit = 
    fun file t ~succs ~rank_opt ~get_vcg_node_att_opt ->
      let vo = match rank_opt with
      | Some f -> f
      | None -> (fun i -> i)
      and out = open_out file 
      in 
      let fm = formatter_of_out_channel out
      in
      pp_open_vbox fm 0;
      pp_print_string fm "graph:";
      pp_print_space fm ();
      pp_print_string fm "{";
      
      pp_open_vbox fm 2;
      pp_print_space fm ();
      
      fprintf fm "title: \"%s\"" file;
      pp_print_space fm ();
      pp_print_string fm "layoutalgorithm: minindegree";
      pp_print_space fm ();
      pp_print_string fm "display_edge_labels:yes";
      pp_print_space fm ();
      pp_print_string fm "manhatten_edges: yes";
      pp_print_space fm ();
      pp_print_string fm "arrowmode: fixed";
      pp_print_space fm ();
      pp_print_string fm "port_sharing: yes";
      pp_print_space fm ();
      (* (stretch/shrink = scaling of the graph in percentage *)
      pp_print_string fm "stretch: 4";
      pp_print_space fm ();
      pp_print_string fm "shrink:  5";
      pp_print_space fm ();
      pp_print_string fm "splines: yes";
      pp_print_space fm ();
      Array.iteri 
	(fun nid node -> 
	  pp_print_vcg_node fm get_vcg_node_att_opt nid node (vo nid);
	  pp_print_space fm ();
	  List.iter (fun dest -> pp_print_vcg_edge fm ~src:nid ~dest) 
	    (succs node);
	  pp_print_space fm ();
	) 
	t;
      pp_close_box fm ();
      pp_print_space fm ();
      pp_print_string fm "}";
      
      pp_close_box fm ();
      pp_print_flush fm ();
      close_out out

(** compute the rank of nodes in dominator tree 
   rank(root) = 0
   rank(node) = 1 + rank(parent(node))
 **)
(*
   let node_rank: int option array -> int array = 
   fun doms -> 
   let length = Array.length doms in 
   let ranks = Array.make length (-1) 
   and max = ref 0 in 
   let rec comp_rank i r =
   if r = (-1) then 
   begin 
   if i = 0 then 
   Array.set ranks 0 0
   else
   begin
   match Array.get doms i with
   | None -> ()
   | Some idom -> 
   let idom_rank = Array.get ranks idom
   in 
   let mr = 
   if idom_rank = (-1) then 
   begin
   comp_rank idom idom_rank;
   (1 + (Array.get ranks idom))
   end
   else
   (1 + idom_rank)
   in 
   if mr > !max then 
   max := mr;
   Array.set ranks i mr
   end
   end
   in
   let _ = Array.iteri comp_rank ranks in 
   let _ = Array.set ranks (length - 1) (!max + 1) in 
   ranks
 *)


let to_idom_vcg: string -> 'a array -> succs:('a -> int list) -> rank_opt:(int -> int) option ->
  get_vcg_node_att_opt:(int -> 'a -> Vcg.vcg_node_att) option -> 
    get_idom_opt:('a -> int option) ->
    unit = 
    fun file t ~succs ~rank_opt ~get_vcg_node_att_opt ~get_idom_opt ->
      let vo = match rank_opt with
      | Some f -> f
      | None -> (fun i -> i)
      and out = open_out file 
      in 
      let fm = formatter_of_out_channel out
      in
      pp_open_vbox fm 0;
      pp_print_string fm "graph:";
      pp_print_space fm ();
      pp_print_string fm "{";
      
      pp_open_vbox fm 2;
      pp_print_space fm ();
      
      fprintf fm "title: \"%s\"" file;
      pp_print_space fm ();
      pp_print_string fm "layoutalgorithm: minindegree";
      pp_print_space fm ();
      pp_print_string fm "display_edge_labels:yes";
      pp_print_space fm ();
      pp_print_string fm "manhatten_edges: yes";
      pp_print_space fm ();
      pp_print_string fm "arrowmode: fixed";
      pp_print_space fm ();
      pp_print_string fm "port_sharing: yes";
      pp_print_space fm ();
      (* (stretch/shrink = scaling of the graph in percentage *)
      pp_print_string fm "stretch: 4";
      pp_print_space fm ();
      pp_print_string fm "shrink:  5";
      pp_print_space fm ();
      pp_print_string fm "splines: yes";
      pp_print_space fm ();
      Array.iteri 
	(fun nid node -> 
	  pp_print_vcg_node fm get_vcg_node_att_opt nid node (vo nid);
	  pp_print_space fm ();
	  List.iter (fun dest -> pp_print_vcg_edge fm ~src:nid ~dest) 
	    (succs node);
	  pp_print_space fm ();
	  pp_print_space fm ();
	  if nid <> 0 then 
	    begin
	      let idom_opt = get_idom_opt node
	      in
	      match idom_opt with
	      | Some idom ->
		  begin
		    pp_print_string fm "edge:";
		    pp_print_space fm ();
		    pp_print_string fm "{";
		    pp_open_vbox fm 2;
		    begin
		      pp_print_space fm ();
		      pp_print_string fm "linestyle: dotted";
		      pp_print_space fm ();
		      pp_print_string fm "color: lightred";
		      pp_print_space fm ();
		      fprintf fm "sourcename:\"%d\"" idom;
		      pp_print_space fm ();
		      fprintf fm "targetname:\"%d\"" nid;
		    end;
		    pp_close_box fm ();
		    pp_print_space fm ();
		    pp_print_string fm "}";
		    pp_print_space fm ();
		  end
	      | None -> ()
	    end
	) 
	t;
      pp_close_box fm ();
      pp_print_space fm ();
      pp_print_string fm "}";
      
      pp_close_box fm ();
      pp_print_flush fm ();
      close_out out
