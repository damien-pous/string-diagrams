open Types

type kv
type kvl = kv list

val kv: string -> string -> kv

val kvl_to_printable: (kvl, printable) mapper
val kvl_to_positionned: (kvl, positionned) mapper

val positionned_node: point -> positionned
val positionned_port: int -> point -> positionned

val same_label_kvl: kvl -> kvl -> bool
val same_label: #printable -> #printable -> bool

val escape: string -> string
