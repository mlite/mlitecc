type t = string -> string
type spec   = 
    { preprocess:  string -> string
    ; replace:     char -> char
    ; reserved:    string list
    ; avoid:       string -> string
    }
val mk:         spec -> t     (* create a mangler *)
