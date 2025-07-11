open Types

type 'a port = private Outer of int | Inner of 'a node * int
and 'a iport = private 'a port
and 'a oport = private 'a port
and 'a edge = private { src: 'a iport; tgt: 'a oport }
and 'a kind = private Var of (int*int*label) | Box of 'a graph
and 'a node = private { info: 'a; kind: 'a kind }
and 'a graph = private {
    sources: int;
    targets: int;
    nodes: 'a node mset;
    edges: 'a edge mset }

val map: ('a -> 'b) -> 'a graph -> 'b graph

val ksrc: 'a kind -> int
val ktgt: 'a kind -> int

val nsrc: 'a node -> int
val ntgt: 'a node -> int

val is_empty: 'a graph -> bool

val out_edge: 'a graph -> 'a iport -> 'a edge option
val out_free: 'a graph -> 'a iport -> bool
val next: 'a graph -> 'a iport -> 'a oport option
val nexts: 'a graph -> 'a iport -> 'a node set * 'a oport set

val inp_edge: 'a graph -> 'a oport -> 'a edge option
val inp_free: 'a graph -> 'a oport -> bool
val prev: 'a graph -> 'a oport -> 'a iport option
val prevs: 'a graph -> 'a oport -> 'a node set * 'a iport set

val reaches: 'a graph -> 'a iport -> 'a oport -> bool

val empty: int -> int -> 'a graph
val idmap: int -> 'a graph
val tensor: 'a graph -> 'a graph -> 'a graph
val seq: 'a graph -> 'a graph -> 'a graph
val var: 'a -> int -> int -> label -> 'a graph
val box: 'a -> 'a graph -> 'a graph

val rem_edge: 'a graph -> 'a edge -> 'a graph
val rem_node: 'a graph -> 'a node -> 'a graph

val add_edge: 'a graph -> 'a iport -> 'a oport -> 'a graph * 'a edge
val add_node: 'a graph -> 'a -> 'a kind -> 'a graph * 'a node

val subst: 'a graph -> 'a node -> 'a graph -> 'a graph
val unbox: 'a graph -> 'a node -> 'a graph

val iter_inner_iports: ('a iport -> unit) -> 'a graph -> unit
val iter_inner_oports: ('a oport -> unit) -> 'a graph -> unit

(*
(* isomorphim check, using the given function to compare edge infos *)
val iso: ('a -> 'a -> bool) -> 'a graph -> 'a graph -> bool

val draw_on: canvas -> ?iprops:bool -> #positionned graph -> unit
val draw: ?iprops:bool -> #positionned graph -> image

val bbox: #positionned graph -> box

val find: ('a -> bool) -> 'a graph -> [`P of 'a port | `E of 'a edge | `N]
 *)
