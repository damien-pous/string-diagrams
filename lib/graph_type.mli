open Types

class type gbox =
  object
    inherit positionned
    method sources: int
    method targets: int
    method src: int -> port
    method tgt: int -> port
  end
and pregraph =
  object
    inherit gbox
    method nodes: node mset
    method edges: node _edge mset

    (* input and output ports, internally to the graph *)
    method iport: node pkind -> port
    method oport: node pkind -> port

    method next: port -> port
    method next_opt: port -> port option
    method nexts: port -> node set * port set
    
    method prev: port -> port
    method prev_opt: port -> port option
    method prevs: port -> node set * port set

    method reaches: port -> port -> bool

    method rem_edge: node _edge -> unit
    method rem_node: node -> unit

    method add_edge: port -> port -> unit
    (* method new_var_box: int -> int -> name -> node *)
    (* method new_graph_box: graph -> node *)

    method subst: node -> graph -> unit
    method unbox: node -> unit
  end
and node =
  object
    inherit gbox
    method kind: graph _nkind    
  end
and port =
  object
    method pos: point
    method kind: node pkind
  end
and graph =
  object
    inherit pregraph
    
    method is_empty: bool

    method ipos: node pkind -> point
    method opos: node pkind -> point
    method find: point -> [ `I of port | `O of port | `N of node | `None ]

    method draw: image
    method draw_on: canvas -> unit

  end

type port_ = node pkind
type edge = node _edge
type nkind = graph _nkind
type env = graph Info.env
