class type t = object
    method mangled_text:    string
    method original_text:   string
end 
val unmangled    : string -> t
val with_mangler : (string -> string) -> string -> t
