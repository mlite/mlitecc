val lst2str : ('a -> string) -> 'a list -> string -> string
val array2str : (int -> 'a -> string) -> 'a array -> string -> int * string -> string
val list_to_str : (int -> 'a -> string) -> 'a list -> string -> int * string -> string
val indent  : int -> string
val std_indent : string
val trim_suffix: string -> string
val trim_ll_suffix: string -> string
val trim_llu_suffix: string -> string
val trim_plus_sign: string -> string
val extract_str: string option -> string
val extract_int: string option -> int
val extract_n_string: string option array -> int -> string
val extract_n_int: string option array -> int -> int
type sfm = { buffer: Buffer.t; formatter:Format.formatter; }
val create_sfm: int -> sfm
val flush_sfm: sfm -> string  
val pp_print_str_pp_print_pos: Format.formatter -> string -> int -> unit
val delete_spaces: string -> string
