open Types

val hyps: 'a env -> (name * ('a * 'a)) list

val map: ('a -> 'b) -> 'a env -> 'b env
val emap: ('b env -> 'a -> 'b) -> 'a env -> 'b env
