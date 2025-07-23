open Types
open Info

type env = graph Info.env

(* graph algebra *)
val empty: int -> int -> graph
val emp: graph
val idm: graph
val seq: graph -> graph -> graph
val tns: graph -> graph -> graph
val var: int -> int -> name -> kvl -> graph
val box: graph -> kvl -> graph

(* graphs from raw terms *)
val env: kvl Raw.env -> env
val of_raw: env -> kvl Raw.term -> graph
val envgraph: kvl Raw.envterm -> env * graph

(* terms from graph
   !! for now, forgetting isolated components *)
val to_term: graph -> Term.term

(* textual pretty printing *)
val pp: pp_mode -> formatter -> graph -> unit
val pp_env: pp_mode -> formatter -> env -> unit
val pp_envgraph: pp_mode -> formatter -> env*graph -> unit

(* graph isomorphism
   !! for now, forgetting isolated components *)
val iso: graph -> graph -> bool
val iso_env: env -> env -> bool
val iso_envgraph: env*graph -> env*graph -> bool
