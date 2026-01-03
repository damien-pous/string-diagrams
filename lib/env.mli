open Types

val hyps: 'a env -> 'a equation list

val map: ('a -> 'b) -> 'a env -> 'b env
val emap: ('b env -> 'a -> 'b) -> 'a env -> 'b env
