open Types

type kv
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: formatter -> kvl -> unit

val merge: kvl -> kvl -> kvl    (* gives precedence to the first argument *)

type 'a env = (name*(kvl*int*int*'a option)) list
val envmap: ('a -> 'b) -> 'a env -> 'b env

val gen: size -> kvl -> positionned
val gen_at: point -> size -> kvl -> positionned
