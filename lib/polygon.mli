type 'a t

val start: 'a -> 'a t
val extend: 'a t -> 'a -> 'a t
val rev: 'a t -> 'a t

val fold2: 'a t -> ('a * 'a -> 'b -> 'b) -> 'b -> 'b
val fold3: 'a t -> ('a * 'a * 'a -> 'b -> 'b) -> 'b -> 'b

val is_degenerate: 'a t -> bool

val least_point: ('a -> 'a -> int) -> 'a t -> 'a
val least_triple: ('a -> 'a -> int) -> 'a t -> 'a * 'a * 'a

val to_path: Gg.p2 t -> Vg.path
