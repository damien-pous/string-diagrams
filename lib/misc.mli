val failwith: ('a, Format.formatter, unit, 'b) format4 -> 'a
val assertk: bool -> (unit -> unit) -> unit

val pp_print_sep:
  string -> Format.formatter -> unit -> unit

val pp_print_list:
  string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

val big: ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a

val sqr: float -> float

val comp: ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

val memo: ('a -> 'b) -> 'a -> 'b
val memo': int -> ('a -> 'b) -> (unit -> unit) * ('a -> 'b)

(** iter n f x = f^n(x) *)
val iter: int -> ('a -> 'a) -> 'a -> 'a

(** starts at 1 *)
val fold: (int -> 'a -> 'a) -> int -> 'a -> 'a
val forall: int -> (int -> bool) -> bool
val exists: int -> (int -> bool) -> bool

val unique_assq: ('a * 'b) list -> bool


val float_of_string: string -> float
val p2_of_string: string -> Gg.P2.t
val string_of_p2: Gg.P2.t -> string

