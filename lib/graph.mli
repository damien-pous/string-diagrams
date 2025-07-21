open Types
open Info

type port = private Outer of int | Inner of node * int
and iport = port
and oport = port
and edge = private { src: iport; tgt: oport }
and kind = private Var of int*int*name | Box of graph
and node = private { ninfo: positionned; kind: kind }
and graph = private
    { info: positionned;
      sources: int;
      targets: int;
      nodes: node mset;
      edges: edge mset }

type env = graph Info.env
val env: kvl Raw.env -> env
val of_raw: env -> kvl Raw.term -> graph
val envgraph: kvl Raw.envterm -> env * graph

(* !! for now, forgetting isolated components *)
val to_term: graph -> Term.term

val pp: pp_mode -> formatter -> graph -> unit
val pp_env: pp_mode -> formatter -> env -> unit
val pp_envgraph: pp_mode -> formatter -> env*graph -> unit

val empty: int -> int -> graph
val emp: graph
val idm: graph
val seq: graph -> graph -> graph
val tns: graph -> graph -> graph

val gsize: graph -> size
val gbox: graph -> box

val nsources: node -> int
val ntargets: node -> int
val nbox: node -> box

val is_empty: graph -> bool

val out_edge: graph -> iport -> edge option
val out_free: graph -> iport -> bool
val next: graph -> iport -> oport option
val nexts: graph -> iport -> node set * oport set

val inp_edge: graph -> oport -> edge option
val inp_free: graph -> oport -> bool
val prev: graph -> oport -> iport option
val prevs: graph -> oport -> node set * iport set

val reaches: graph -> iport -> oport -> bool

val rem_edge: graph -> edge -> graph
val rem_node: graph -> node -> graph

val add_edge: graph -> iport -> oport -> graph * edge
val add_var: graph -> positionned -> int -> int -> name -> graph * node
val add_box: graph -> positionned -> graph -> graph * node

val subst: graph -> node -> graph -> graph
val unbox: graph -> node -> graph

val nshift: node -> vector -> unit
val nmove: node -> point -> unit
val gscale: float -> graph -> unit
val nscale: float -> node -> unit

val npos: node -> point
val ipos: graph -> iport -> point
val opos: graph -> oport -> point

val iter_inner_iports: (iport -> unit) -> graph -> unit
val iter_inner_oports: (oport -> unit) -> graph -> unit

val draw_on: canvas -> graph -> unit
val draw: graph -> image

(* !! for now, only correct on connected graphs *)
val iso: graph -> graph -> bool
val iso_env: env -> env -> bool
val iso_envgraph: env*graph -> env*graph -> bool

(* !! only returns (toplevel) nodes for now *)
val find: point -> graph -> [ `I of iport | `O of oport | `N of node | `None ]
