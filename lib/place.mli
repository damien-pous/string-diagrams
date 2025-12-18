open Graph_type

(** improve the current placement, using elastic dynamics *)
val improve_placement: float -> graph -> unit
val improve_placement_depth: ?force:bool -> float -> graph -> unit
val improve_placement_depth': ?force:bool -> float -> graph -> unit

(** fix or unfix nodes for previous function *)
val fix: node -> unit
val unfix: node -> unit
