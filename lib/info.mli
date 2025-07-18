open Types

type kv
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: formatter -> kvl -> unit

val get_size: kvl -> size

val var: int -> int -> kvl -> positionned
val box: size -> kvl -> positionned
