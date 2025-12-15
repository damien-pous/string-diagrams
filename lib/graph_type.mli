open Types
open Term

type 'graph nkind = Var of int*int*name | Box of 'graph

class type interface =
  object
    method sources: int         (* number of sources *)
    method targets: int         (* number of targets *)
  end

class type boundary =
  object
    inherit area
    inherit interface
    method spos: int -> point   (* position of the i-th source *)
    method tpos: int -> point   (* position of the i-th target *)
  end

class type node =
  object
    inherit boundary    
    method kind: graph nkind    
    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method term: term
  end
and graph =
  object
    inherit boundary
    (* note below: [node iport / node oport] should be abbreviated as [iport / oport] *)

    method nodes: node mset
    method edges: (node iport*node oport) mset    
    method update: node mset -> (node iport*node oport) mset -> unit

    (* positions relative to the interior of the graph:
       outer sources and targets of inner nodes are input ports,
       outer targets and sources of inner nodes are output ports,
     *)
    method ipos: node iport -> point
    method opos: node oport -> point

    method ifree: node iport -> bool
    method next: node iport -> node oport
    method next_opt: node iport -> node oport option
    method nexts: node iport -> node mset * node oport mset

    method ofree: node oport -> bool
    method prev: node oport -> node iport
    method prev_opt: node oport -> node iport option
    method prevs: node oport -> node mset * node iport mset

    (* distance to the targets of the graph *)
    method depth: node -> int

    method rem_edge: node iport*node oport -> unit
    method add_edge: node iport*node oport -> unit
    method rem_node: node -> unit
    method add_node: int -> int -> name -> Info.kvl -> unit

    (* note: the graph in argument to [subst] should be copied beforehand
       (to avoid aliasing problems) *)
    method subst: node -> graph -> unit
    method unbox: node -> unit

    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method term: term
  end
type iport = node Types.iport
type oport = node Types.oport

type env = graph Info.env

type equation = graph * graph
