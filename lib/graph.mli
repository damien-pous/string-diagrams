open Types
open Element
open Graph_type

(* graph algebra *)
val empty: typs -> typs -> graph
val emp: unit -> graph
val idm: typs -> graph
val seq: graph -> graph -> graph
val tns: graph -> graph -> graph
val var: typs -> typs -> name -> kvl -> graph
val box: graph -> (* kvl ->  *)graph

val var_node: typs -> typs -> name -> kvl -> node

(* graphs from raw terms *)
val env: kvl Raw.env -> env
val of_raw: env -> kvl Raw.term -> graph
val envgraph: kvl Raw.envterm -> env * graph
val equations: kvl Raw.equations -> equations

(* textual pretty printing *)
val pp: pp_mode -> formatter -> graph -> unit
val pp_env: pp_mode -> formatter -> env -> unit
val pp_envgraph: pp_mode -> formatter -> env*graph -> unit
val pp_equations: pp_mode -> formatter -> equations -> unit

(* sharing-free copy (by serialisation for now) *)
val copy: env -> graph -> graph

(* graph isomorphism
   !! for now, forgetting isolated components *)
val iso: graph -> graph -> bool
val iso_env: env -> env -> bool
val iso_envgraph: env*graph -> env*graph -> bool

(* capturing a subgraph inside a box *)
val create_box: graph -> polygon -> graph

(* finding elements by their position *)
val find: graph -> point -> [ `N of node | `None ]
val find_ports: graph -> point -> [ `I of iport | `O of oport | `N of node | `None ]


val iter_positionned_edges: graph -> (iport*point -> oport*point -> unit) -> unit
val iter_iports: graph -> (iport -> unit) -> unit
val iter_oports: graph -> (oport -> unit) -> unit

val icolor: graph -> iport -> color
val ocolor: graph -> oport -> color
