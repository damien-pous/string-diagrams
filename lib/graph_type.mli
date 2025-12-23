open Types
open Term

class type graph =
  object
    inherit element
    (* note below: [node iport / node oport] should be abbreviated as [iport / oport] *)

    method nodes: node mset
    method edges: (node iport*node oport) mset    
    method update: node mset -> (node iport*node oport) mset -> unit

    method inner_graphs: graph mset

    (* positions relative to the interior of the graph:
       outer sources and targets of inner nodes are input ports,
       outer targets and sources of inner nodes are output ports,
     *)
    method ipos: node iport -> point
    method opos: node oport -> point

    method idir: node iport -> vector
    method odir: node oport -> vector
    
    method ityp: node iport -> typ
    method otyp: node oport -> typ
    
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
    method add_node: node -> unit

    (* the graph in argument to [subst/replace] should be copied beforehand
       (to avoid aliasing problems) *)
    method subst: node -> graph -> unit
    method replace: graph -> unit
    method unbox: node -> unit

    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method draw_edge: canvas -> node iport*node oport -> unit
    method term: term

    (* is the graph stable (elastic dynamic) *)
    method stable: bool
    method set_stable: bool -> unit
  end
and node =
  object
    inherit element
    method kind: graph kind
    method draw: canvas -> unit
    method term: term
  end

type iport = node Types.iport
type oport = node Types.oport
type kind = graph Types.kind

type env = graph Element.env

type equation = graph * graph
type equations = env * equation list * equation
