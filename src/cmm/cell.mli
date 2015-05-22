type t
type count = C of int   (* a number of cells *)
type width = int        (* a number of bits *)

val to_width : t -> count -> width
val to_count : t -> width -> count
val divides  : t -> width -> bool   (* width is an even number of cells *)
val size     : t -> width (* number of bits in one cell *)

val of_size : int -> t
