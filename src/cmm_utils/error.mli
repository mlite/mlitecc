# 33 "error.nw"
type 'a error       = Error                     (* bad result  *)
                    | Ok        of 'a           (* good result *)

exception ErrorExn  of string
val error :         string -> 'a (* ErrorExn *)
val errorf : ('a, unit, string, 'b) format4 -> 'a
# 73 "error.nw"
val combine : 'a error error -> 'a error
val ematch  : 'a error -> ('a -> 'b) -> 'b error
val ematch2 : 'a error -> 'b error -> ('a -> 'b -> 'c) -> 'c error
val ematch3 : 'a error -> 'b error -> 'c error ->
              ('a -> 'b -> 'c -> 'd) -> 'd error
val ematch4 : 'a error -> 'b error -> 'c error -> 'd error -> 
              ('a -> 'b -> 'c -> 'd -> 'e) -> 'e error
val ematch5 : 'a error -> 'b error -> 'c error -> 'd error -> 'e error ->
              ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f error
val ematch6 : 'a error -> 'b error -> 'c error -> 'd error -> 'e error -> 'f error ->
              ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'g error
val ematchPair  : 'a error * 'b error -> ('a * 'b -> 'c) -> 'c error
val ematchTriple: 'a error * 'b error * 'c error -> 
                   ('a * 'b * 'c -> 'd) -> 'd error
val ematchQuad  : 'a error * 'b error * 'c error * 'd error -> 
                   ('a * 'b * 'c * 'd -> 'e) -> 'e error
val seq         : 'a error -> ('a -> 'b error) -> 'b error
val seq2        : 'a error -> 'b error -> ('a -> 'b -> 'c error) -> 'c error
val seqPair     : 'a error * 'b error -> ('a * 'b -> 'c error) -> 'c error
val seq'        : 'b -> 'a error -> ('a -> 'b) -> 'b 
# 107 "error.nw"
module Raise :
  sig
    val option : 'a error option                -> 'a option error
    val list   : 'a error list                  -> 'a list error
    val pair   : 'a error * 'b error            -> ('a * 'b) error
# 116 "error.nw"
    val left   : 'a error * 'b                  -> ('a * 'b) error
    val right  : 'a * 'b error                  -> ('a * 'b) error
    
    val triple : 'a error * 'b error * 'c error -> ('a * 'b * 'c) error
    val quad   : 'a error * 'b error * 'c error * 'd error 
                  -> ('a * 'b * 'c * 'd) error
  end
# 133 "error.nw"
module Lower :
  sig
    val pair   : ('a * 'b)           error -> 'a error * 'b error
    val triple : ('a * 'b * 'c)      error -> 'a error * 'b error * 'c error
    val quad   : ('a * 'b * 'c * 'd) error -> 'a error 
                                            * 'b error 
                                            * 'c error 
                                            * 'd error
  end
# 155 "error.nw"
module Implode :
  sig
    val singleton : 'a error                                  -> unit error
    val pair :      'a error * 'b error                       -> unit error
    val triple :    'a error * 'b error * 'c error            -> unit error
    val quad :      'a error * 'b error * 'c error * 'd error -> unit error
    val list :      'a error list                             -> unit error
    val map :       ('a -> 'b error) -> 'a list               -> unit error
  end
# 179 "error.nw"
val catch   :       (string -> unit) 
                    -> ('a -> 'b error) -> 'a -> 'b error
val catch'  : 'b -> (string -> unit) 
                    -> ('a -> 'b      ) -> 'a -> 'b 
# 196 "error.nw"
val warningPrt : string -> unit
val errorPrt   : string -> unit
# 207 "error.nw"
val errorPointPrt  : Srcmap.point -> string -> unit
val errorRegionPrt : Srcmap.region -> string -> unit
val errorPosPrt    : Srcmap.pos -> string -> unit
val errorRegPrt    : Srcmap.rgn -> string -> unit
