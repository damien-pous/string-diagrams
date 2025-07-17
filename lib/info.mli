open Types

type kv
type kvl = kv list

val kv: string -> string -> kv
val add_name: name -> kvl -> kvl

val get_color: name -> kvl -> color
val get_size: int -> int -> kvl -> size

val pp_kvl: formatter -> kvl -> unit

val kvl_to_printable: kvl -> printable
val kvl_to_positionned: kvl -> positionned

val pos0: unit -> positionned

