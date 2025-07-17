open Types

type kv
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: formatter -> kvl -> unit

val var: int -> int -> kvl -> positionned
val box: size -> kvl -> positionned
