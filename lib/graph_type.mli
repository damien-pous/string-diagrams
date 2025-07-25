open Types

type 'graph nkind = Var of int*int*name | Box of 'graph

class type gbox =
  object
    inherit positionned
    method sources: int
    method targets: int
    method src: int -> port
    method tgt: int -> port
    method draw_box_on: canvas -> unit
  end
and node =
  object
    inherit gbox
    method kind: graph nkind    
    method draw_on: canvas -> unit
  end
and port =
  object
    method pos: point
    method kind: node pkind
  end
and graph =
  object
    inherit gbox
    method nodes: node mset
    method edges: (port*port) mset

    method next: port -> port
    method next_opt: port -> port option
    method nexts: port -> node set * port set
    
    method prev: port -> port
    method prev_opt: port -> port option
    method prevs: port -> node set * port set

    method reaches: port -> port -> bool

    method rem_edge: port*port -> unit
    method rem_node: node -> unit

    method add_edge: port*port -> unit
    (* method new_var_box: int -> int -> name -> node *)
    (* method new_graph_box: graph -> node *)

    method add_box: polygon -> unit

    method subst: node -> graph -> unit
    method unbox: node -> unit

    method find: point -> [ `I of port | `O of port | `N of node | `None ]

    method draw_on: canvas -> unit
    method draw: image
  end

type env = graph Info.env
