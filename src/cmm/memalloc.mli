type t          (* immutable *)

type growth = Up | Down

val at :            start:Rtl.exp -> growth -> t       (* provide start address *)
val relative :      anchor:Rtl.exp -> dbg:string -> growth -> t (* unknown addr relative to anchor *)
val allocate :      t -> size:int -> t      (* increase block *)
val align :         t -> int -> t           (* align cursor *)
val alignment :     t -> int                (* max alignment ever requested *)
val current :       t -> Rtl.exp            (* obtain cursor *)
val freeze :        t -> Block.t            (* return allocated, aligned block *)
val num_allocated : t -> int
