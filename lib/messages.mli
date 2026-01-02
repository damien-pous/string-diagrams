open Types

val debug: string -> canvas
val debug_msg: string -> ('a, formatter, unit) format -> 'a

val temporary: msg_canvas

val error: ('a, formatter, unit, 'b) format4 -> 'a
val warning: ('a, formatter, unit, 'b) format4 -> 'a

val catch: ('a -> 'b) -> 'a -> 'b -> (unit -> unit) -> 'b
