type 'a answer = Dataflow of 'a | Rewrite of Zipcfg.graph
type txlimit = int
type 'a fact = 
    {
      fact_name : string;                     (* documentation *)
      init_info : 'a;                         (* lattice bottom element *)
      add_info  : 'a -> 'a -> 'a;             (* lattice join (least upper bound) *)
      changed   : old:'a -> new':'a -> bool;  (* is new one bigger? *)
      prop      : 'a Unique.Prop.t;           (* access to mutable state by uid *)
    }
      
type 'a fact' = 
    {
      fact_name' : string;                     (* documentation *)
      init_info' : 'a;                         (* lattice bottom element *)
      add_info'  : 'a -> 'a -> 'a;             (* lattice join (least upper bound) *)
      changed'   : old:'a -> new':'a -> bool;  (* is new one bigger? *)
      get'       : Zipcfg.uid -> 'a;
      set'       : Zipcfg.uid -> 'a -> unit;
    }
      
module B : 
sig
  type ('i, 'o) computation =
      { 
	name      : string;
	last_in   : Zipcfg.Rep.last -> 'o;
	middle_in : 'i -> Zipcfg.Rep.middle -> 'o;
	first_in  : 'i -> Zipcfg.Rep.first -> 'o;
      } 
	
  type 'a analysis       = 'a fact  * ('a, 'a)                   computation
  type 'a analysis'      = 'a fact' * ('a, 'a)                   computation
  type 'a transformation =           ('a, Zipcfg.graph option)   computation
  type 'a pass           = 'a fact  * ('a, txlimit -> 'a answer) computation
  type 'a pass'          = 'a fact' * ('a, txlimit -> 'a answer) computation
  val anal  : 'a analysis  -> 'a pass
  val anal' : 'a analysis' -> 'a pass'
  val a_t   : 'a analysis  -> 'a transformation -> 'a pass
  val a_t'  : 'a analysis' -> 'a transformation -> 'a pass'
  val debug' : ('a -> string) -> 'a pass' -> 'a pass'
  val debug  : ('a -> string) -> 'a pass  -> 'a pass
  val run_anal  : 'a analysis  -> Zipcfg.graph -> unit
  val run_anal' : 'a analysis' -> Zipcfg.graph -> unit
  val rewrite  : 'a pass  -> Zipcfg.graph -> Zipcfg.graph * bool
  val rewrite' : 'a pass' -> Zipcfg.graph -> Zipcfg.graph * bool
end
  
module F : 
sig
  type 'a edge_fact_setter = (Zipcfg.uid -> 'a -> unit) -> unit

  type ('i, 'om, 'ol) computation =
      { 
	name       : string;
	middle_out : 'i -> Zipcfg.Rep.middle -> 'om;
	last_outs  : 'i -> Zipcfg.Rep.last   -> 'ol;
      } 
	
  type 'a analysis  = 'a fact  * ('a, 'a,                'a edge_fact_setter) computation
  type 'a analysis' = 'a fact' * ('a, 'a,                'a edge_fact_setter) computation
  type 'a transformation =     ('a, Zipcfg.graph option, Zipcfg.graph option) computation
  type 'a pass =
      'a fact *
        ('a, txlimit -> 'a answer, txlimit -> 'a edge_fact_setter answer) 
	computation
	
  type 'a pass' =
      'a fact' *
        ('a, txlimit -> 'a answer, txlimit -> 'a edge_fact_setter answer)
	computation
	
  val anal  : 'a analysis  -> 'a pass
  val anal' : 'a analysis' -> 'a pass'
  val a_t   : 'a analysis  -> 'a transformation -> 'a pass
  val a_t'  : 'a analysis' -> 'a transformation -> 'a pass'
  val debug' : ('a -> string) -> 'a pass' -> 'a pass'
  val debug  : ('a -> string) -> 'a pass  -> 'a pass
  val run_anal  : 'a analysis  -> entry_fact:'a -> Zipcfg.graph -> unit
  val run_anal' : 'a analysis' -> entry_fact:'a -> Zipcfg.graph -> unit
  val rewrite  : 'a pass  -> entry_fact:'a -> Zipcfg.graph -> Zipcfg.graph * bool
  val rewrite' : 'a pass' -> entry_fact:'a -> Zipcfg.graph -> Zipcfg.graph * bool
  val rewrite_solved  : 'a pass  -> entry_fact:'a -> Zipcfg.graph -> Zipcfg.graph * bool
  val rewrite_solved' : 'a pass' -> entry_fact:'a -> Zipcfg.graph -> Zipcfg.graph * bool
  val modify_solved'  : 'a pass' -> entry_fact:'a -> Zipcfg.graph -> Zipcfg.graph * bool
end
  
val limit_fun  : ('a -> 'b -> 'c option) -> ('a -> 'b -> txlimit -> 'c option)
