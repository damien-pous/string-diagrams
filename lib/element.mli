open Types

type kv
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: formatter -> kvl -> unit

val pos: point -> kvl -> kvl

val merge: kvl -> kvl -> kvl    (* gives precedence to the first argument *)

type 'a env = (name*(kvl*typs*typs*'a option)) list
val envmap: ('a -> 'b) -> 'a env -> 'b env

class proxy: #element -> element

class rectangular: typs -> typs -> ?pos:point -> size:size -> name:name -> kvl -> element
class circular: typs -> typs -> ?pos:point -> radius:float -> name:name -> kvl -> element
class polygonial: (typ*point) list -> (typ*point) list -> polygon -> element

val mk: typs -> typs -> name:name -> kvl -> element
