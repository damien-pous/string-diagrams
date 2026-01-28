open Types

val set_output: (string -> unit) -> (unit -> unit) -> unit
val clear: unit -> unit

val debug: string -> canvas
val debug_msg: string -> ('a, formatter, unit) format -> 'a

val message: ('a, formatter, unit) format -> 'a
val warning: ('a, formatter, unit) format -> 'a

val temporary: canvas

val abort: ('a, formatter, unit, 'b) format4 -> 'a
val user_error: ('a, formatter, unit, 'b) format4 -> 'a
val program_error: ('a, formatter, unit, 'b) format4 -> 'a

val catch: ('a -> 'b) -> 'a -> 'b -> (unit -> unit) -> 'b
