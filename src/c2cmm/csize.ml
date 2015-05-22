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

open Int64
type csize = int64
    
let max_size = max_int
let min_size = 0L
let string_of_csize v = to_string v
let csize_of_string v = of_string v
let csize_of_int v = of_int v
let int_of_csize v = to_int v  

let ( +$ ) : csize -> csize -> csize = 
  fun v1 v2 -> add v1 v2 

let ( -$ ) : csize -> csize -> csize = 
  fun v1 v2 -> sub v1 v2 

let ( *$ ) : csize -> csize -> csize = 
  fun v1 v2 -> mul v1 v2 
    
let ( /$ ) : csize -> csize -> csize = 
  fun v1 v2 -> div v1 v2 
    
let ( <$ ) : csize -> csize -> csize = 
  fun v1 v2 -> shift_left v1 (to_int v2)
      
let ( >$ ) : csize -> csize -> csize = 
  fun v1 v2 -> shift_right v1 (to_int v2)
      
let ( %$ ) : csize -> csize -> csize = 
  fun v1 v2 -> rem v1 v2
    
let ( ^$ ) : csize -> csize -> csize = 
  fun v1 v2 -> logxor v1 v2
    
let ( &$ ) : csize -> csize -> csize = 
  fun v1 v2 -> logand v1 v2
    
let ( |$ ) : csize -> csize -> csize = 
  fun v1 v2 -> logor v1 v2
    
let ( ~$ ) : csize -> csize = 
  fun v1 -> lognot v1
    
let neg v = neg v
