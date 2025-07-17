open Graph

(** improve the current placement, using elastic dynamics *)
val improve_placement: float -> graph -> unit

(** fix or unfix nodes for previous function *)
val fix: node -> unit
val unfix: node -> unit

(** scale the bounding box of the give graph *)
val scale: float -> graph -> unit
