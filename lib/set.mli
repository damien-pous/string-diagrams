(** finite sets of values *)

type 'a t

val empty: 'a t
val single: 'a -> 'a t
val union: 'a t -> 'a t -> 'a t
val add: 'a -> 'a t -> 'a t
val memq: 'a -> 'a t -> bool
