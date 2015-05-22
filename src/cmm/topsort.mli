module type S = sig
    type decl
    exception Cycle of decl list
    val sort: decl list -> decl list (* raises Cycle *)
end
module type Sortable = sig
    type decl
    val defines : decl -> string list
    val uses    : decl -> string list
end

module Make (S: Sortable) : (S with type decl = S.decl)
