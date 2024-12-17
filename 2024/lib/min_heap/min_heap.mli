type 'a heap

val create: size:int -> init:'a -> 'a heap

val min_elt: 'a heap -> 'a option

val add: 'a heap -> 'a -> score:int -> unit

val pop_min_elt: 'a heap -> (int * 'a) option

val length: 'a heap -> int

val print: 'a heap -> unit

val empty: 'a heap -> unit
