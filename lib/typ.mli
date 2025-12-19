type t
type ts = t list

val name: string -> t
val flex1: unit -> t
val flex: int -> ts

val unify1: msg:string -> t -> t -> unit
val unify: msg:string -> ts -> ts -> unit

val eq1: t -> t -> bool
val eq: ts -> ts -> bool

val get: t -> string option

val pp1: Format.formatter -> t -> unit
val pp: Format.formatter -> ts -> unit
