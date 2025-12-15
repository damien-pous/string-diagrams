open Types
open Info
open Graph_type

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
val equations: kvl Raw.equations -> env * (equation list) * equation

(* textual pretty printing *)
val pp: pp_mode -> formatter -> graph -> unit
val pp_env: pp_mode -> formatter -> env -> unit
val pp_envgraph: pp_mode -> formatter -> env*graph -> unit

(* graph isomorphism
   !! for now, forgetting isolated components *)
val iso: graph -> graph -> bool
val iso_env: env -> env -> bool
val iso_envgraph: env*graph -> env*graph -> bool

(* capturing a subgraph inside a box *)
val create_box: graph -> polygon -> unit

(* finding elements by their position *)
val find: graph -> point -> [ `I of iport | `O of oport | `N of node | `None ]


val iter_positionned_edges: graph -> (iport*point -> oport*point -> unit) -> unit
val iter_iports: graph -> (iport -> unit) -> unit
val iter_oports: graph -> (oport -> unit) -> unit
