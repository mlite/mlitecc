type doc

val empty : doc

val (^^) : doc -> doc -> doc

val text : string -> doc

val break : doc

val breakWith : string -> doc

val nest : int -> doc -> doc

val hgrp : doc -> doc
    
val vgrp : doc -> doc

val agrp : doc -> doc

val fgrp : doc -> doc

val ppToString : int -> doc -> string
val ppToFile : out_channel -> int -> doc -> unit
val list      : doc -> ('a -> doc) -> 'a list -> doc 
val commalist :        ('a -> doc) -> 'a list -> doc

val (^/)      : doc -> doc -> doc

val block     : ('a -> doc) -> 'a list -> doc
