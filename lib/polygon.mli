type point = Gg.p2

type t

val start: point -> t
val extend: t -> point -> t
val rev: t -> t

val fold2: t -> (point * point -> 'a -> 'a) -> 'a -> 'a
val fold3: t -> (point * point * point -> 'a -> 'a) -> 'a -> 'a

val is_degenerate: t -> bool
val is_simple: t -> bool

val least: (point -> point -> int) -> t -> point * point * point

val to_path: t -> Vg.path
