open Graph_type

(** improve the current placement, using elastic dynamics
    not recursive on inner graphs,
    returns true if the graph is stable,
 *)
val improve: graph -> bool
val contract: graph -> bool

(** fix or unfix nodes for previous function *)
val fix: node -> unit
val unfix: node -> unit

(** group all nodes of the graph to its center *)
val group: graph -> unit
