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


type qn_namespace = 
  | QN_CLANG
  | QN_BUILTIN
  | QN_CSTD
  | QN_DEFAULT
  | QN_CUSTOM of string
  | QN_TMP
      
and qn_scope = 
  | QN_SCOPE_FILE of string
  | QN_SCOPE_FUN of string
  | QN_SCOPE_BLOCK of int
  | QN_SCOPE_UTYPE of string 

and qn_class =
  | QN_DATA_REGI
  | QN_TYPE_NAME 
  | QN_CODE_ADDR
  | QN_DATA_ADDR
  | QN_ENUM_CNST of int
  | QN_STRN_CNST
  | QN_PARAM_NAME of int 
      (* The 'int' argument is the teid of the parameter.  Formal paramter
	 represents a special entity, it has not fixed nested scopes. In order
	 to avoid name collision, we have to use its type to disambiguate
	 different formal paramters with same sname. This inevitablely
	 introduces another level of recursion when we want to merge all type
	 entities.
      *)
  
and qn_span = 
  | QN_AUTO
  | QN_STATIC
  (*| QN_EXTERN*)
      
and qn_init = 
  | QN_INIT of string 
  | QN_INT of int
  | QN_NULL
      
and t =
    {
      qn_namespace: qn_namespace;
      qn_span: qn_span;
      (* QN_PARAM_NAME might be stacknized to QN_DATA_ADDR on local stack *)
      qn_class: qn_class; 
      qn_scopes: qn_scope list;
      mutable qn_init: qn_init;
      qn_sname: string;
    }

and qname = t

let default_qual_name =
  {
    qn_namespace = QN_CLANG;
    qn_span = QN_AUTO;
    qn_class = QN_TYPE_NAME;
    qn_scopes = [];
    qn_init = QN_NULL;
    qn_sname = "int";
  }

let null = 
  {
    qn_namespace = QN_CLANG;
    qn_span = QN_AUTO;
    qn_class = QN_TYPE_NAME;
    qn_scopes = [];
    qn_init = QN_NULL;
    qn_sname = "";
  }


let eq t0 t1 = 
  match t0.qn_class, t1.qn_class with
    | QN_CODE_ADDR, QN_CODE_ADDR  
    | QN_DATA_ADDR, QN_DATA_ADDR ->
	begin
	  match t0.qn_scopes, t1.qn_scopes with
	    | [], [] -> 
		t0.qn_sname = t1.qn_sname
	    
	    | [QN_SCOPE_FILE f0], [QN_SCOPE_FILE f1] ->
		begin
		  match t0.qn_span, t1.qn_span with
		    | QN_STATIC, QN_STATIC ->
			f0 = f1 & t0.qn_sname = t1.qn_sname
		    | QN_STATIC, _ -> false
		    | _, QN_STATIC -> false
		    | _, _ -> 
			(t0.qn_sname = t1.qn_sname)
		end
		  
	    | l0, l1 ->
		(t0.qn_sname = t1.qn_sname) & (l0 = l1)
	end
	  
    | QN_TYPE_NAME, QN_TYPE_NAME ->
	begin
	  match t0.qn_scopes, t1.qn_scopes with
	    | [], [] -> 
		t0.qn_sname = t1.qn_sname
	    
	    | [QN_SCOPE_FILE f0], [QN_SCOPE_FILE f1] ->
		(t0.qn_sname = t1.qn_sname)
		  
	    | l0, l1 ->
		(t0.qn_sname = t1.qn_sname) & (l0 = l1)
	end
    | QN_PARAM_NAME i0, QN_PARAM_NAME i1 -> 
	i0 = i1 & t0.qn_sname = t1.qn_sname
  
    | QN_ENUM_CNST i0, QN_ENUM_CNST i1 -> 
	begin
	  let result = i0 = i1 & t0.qn_sname = t1.qn_sname
	  in
	  match t0.qn_scopes, t1.qn_scopes with
	    | [], [] -> result
	    | [QN_SCOPE_FILE f0], [QN_SCOPE_FILE f1] -> result
	    | l0, l1 -> result & (l0 = l1)
	end
	  
    | QN_STRN_CNST, QN_STRN_CNST -> t0 = t1

    | _, _ -> false
  
  
module HashQualName =
struct
  type t = qname
  let equal (s1 : t) (s2 : t) = eq s1 s2
  let hash (s : t) = Hashtbl.hash s.qn_sname
end
  
module QualNameHashtbl = Hashtbl.Make(HashQualName)
