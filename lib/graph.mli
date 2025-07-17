open Types
open Info

type sort = {
      sources: int;
      targets: int;
      mutable size: size }
type port = private Outer of int | Inner of node * int
and iport = port
and oport = port
and edge = private { src: iport; tgt: oport }
and kind = private Var of sort*name | Box of graph
and node = private { info: positionned; kind: kind }
and graph = private {
    gsort: sort;
    nodes: node mset;
    edges: edge mset }

type env
val env: kvl Raw.env -> env
val of_raw: env -> kvl Raw.term -> graph
val envgraph: kvl Raw.envterm -> env * graph

val pp: pp_mode -> formatter -> graph -> unit
val pp_env: pp_mode -> formatter -> env -> unit
val pp_envgraph: pp_mode -> formatter -> env*graph -> unit

val empty: int -> int -> graph
val emp: graph
val idm: graph
val seq: graph -> graph -> graph
val tns: graph -> graph -> graph

val gsources: graph -> int
val gtargets: graph -> int
val gsize: graph -> size
val gbox: graph -> box

val nsources: node -> int
val ntargets: node -> int
val nsize: node -> size
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
val add_var: graph -> positionned -> sort -> name -> graph * node
val add_box: graph -> positionned -> graph -> graph * node

val subst: graph -> node -> graph -> graph
val unbox: graph -> node -> graph

val nshift: node -> vector -> unit
val nmove: node -> point -> unit


val npos: node -> point
val ipos: graph -> iport -> point
val opos: graph -> oport -> point

val iter_inner_iports: (iport -> unit) -> graph -> unit
val iter_inner_oports: (oport -> unit) -> graph -> unit

val draw_on: canvas -> graph -> unit
val draw: graph -> image


(* !! only safe for graphs where all nodes are reacheable from the sources, for now *)
val iso: graph -> graph -> bool

(* !! only returns (toplevel) nodes for now *)
val find: point -> graph -> [ `I of iport | `O of oport | `N of node | `None ]
