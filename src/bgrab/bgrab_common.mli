exception Error
exception True of (string * int)
exception File of string
type argument_type =
    Exec of string
  | Switch of string list
  | Library of string
  | CrtObj of string
  | Objects of (string list ref)
  | CFile of string
  | CPPFile of string
  | IFile of string
  | SFile of string

val mlite_bgrab_ccx : string
val mlite_bgrab_as : string
val mlite_bgrab_ar : string
val mlite_bgrab_ld : string

val is_switcher : string -> bool
val start_with_char : char -> string -> bool
val split_complex_arg : String.t list -> string -> string * string
val end_with_substr : String.t list -> string -> bool
val load_env : string -> string


val bgrab_capture: bool
val gnu_cc1_real : string
val gnu_cc1plus_real : string
val gnu_as_real : string
val gnu_ar_real : string
val gnu_ld_real : string


val make_cmd : string list -> string
val make_argument_type_cmd : argument_type list -> string
val exec_argv : string -> string array -> 'a
val get_temp_filename : bool -> string -> string -> string
val get_basename : string -> string -> string
val test_only : bool ref
val exec_cmd : string -> int
val print_msg : string -> string -> unit

(*
val start : string -> string -> ('a -> 'b) -> 'a -> 'b
*)

val create_ccx_input_zip: string -> string -> argument_type list -> string -> string
val unzip_ccx_input_zip: ccx:string -> input_file:string -> (argument_type list * string)
val create_ccx_output_zip: ccx:string -> input_zip:string -> output:string -> string

val create_as_input_zip: oexec_cmd:string -> argv:argument_type list -> input_file:string -> string

val unzip_as_input_zip: string -> (argument_type list * string)

val create_as_output_zip: input_zip:string -> output:string -> string

val create_ar_output_zip: oexec_cmd:string -> exec_cmd:string -> 
  argv:argument_type list -> output:string ->
  input:argument_type list -> string 

val create_ld_output_zip: 
  oexec_cmd:string -> exec_cmd:string -> 
  argv: argument_type list -> 
  output:string -> string 
  
  
val clean_up_absolute_build_dir: unit -> unit
  
val get_zip_file_name: string -> string -> string

val split_str: string -> char -> string list

val pass_to: string -> string array -> int
