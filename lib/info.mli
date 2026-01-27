open Misc
open Gg
  
type kv 
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: Format.formatter -> kvl -> unit

val mem: string -> kvl -> bool
val get: string -> kvl -> string
val get_opt: string -> kvl -> string option
val get_color: string -> kvl -> Gg.color

val pos: P2.t -> kvl -> kvl

val merge: kvl -> kvl -> kvl    (* gives precedence to the first argument *)

class gen: kvl ->
  object
    val kvl: kvl ref
    method has: string -> bool
    method get: string -> string option
    method set: string -> string -> unit
    method unset: string -> unit
  end
