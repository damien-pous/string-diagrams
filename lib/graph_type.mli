open Types
open Term

type 'graph nkind = Var of int*int*name | Box of 'graph

class type interface =
  object
    method sources: int
    method targets: int
  end

class type boundary =
  object
    inherit area
    inherit interface
    method src: int -> port
    method tgt: int -> port
    method nsrc: node -> int -> port
    method ntgt: node -> int -> port
  end
and node =
  object
    inherit boundary
    method kind: graph nkind    
    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method term: term
  end
and port =
  object
    method pos: point
    method kind: node pkind
  end
and graph =
  object
    inherit boundary
    method nodes: node mset
    method edges: (port*port) mset
    method update: node mset -> (node pkind*node pkind) mset -> unit

    method next: port -> port
    method next_opt: port -> port option
    method nexts: port -> node set * port set
    
    method prev: port -> port
    method prev_opt: port -> port option
    method prevs: port -> node set * port set

    method reaches: port -> port -> bool

    method rem_edge: port*port -> unit
    method add_edge: port*port -> unit
    method rem_node: node -> unit

    method subst: node -> graph -> unit
    method unbox: node -> unit

    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method term: term
  end

type env = graph Info.env
