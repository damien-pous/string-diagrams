open Types

type kv
val kv: string -> string -> kv

type kvl = kv list
val pp_kvl: formatter -> kvl -> unit

val pos: point -> kvl -> kvl

val merge: kvl -> kvl -> kvl    (* gives precedence to the first argument *)

val radius: kvl -> float option

type 'a env = (name*(kvl*typs*typs*'a option)) list
val envmap: ('a -> 'b) -> 'a env -> 'b env

class rectangle_area: ?pos:point -> size -> ?name:string -> kvl -> area
class circular_area: ?pos:point -> float -> ?name:string -> kvl -> area
class polygon_area: polygon -> ?name:string -> kvl -> area

class proxy: area -> area
