open Types
open Term

class type graph =
  object
    inherit element
    (* note below: [(node,int) iport / (node,int) oport] should be abbreviated as [iport / oport] *)

    method nodes: node mset
    method edges: ((node,int) iport*(node,int) oport) mset    
    method update: node mset -> ((node,int) iport*(node,int) oport) mset -> unit

    method inner_graphs: graph mset

    (* positions relative to the interior of the graph:
       outer sources and targets of inner nodes are input ports,
       outer targets and sources of inner nodes are output ports,
     *)
    method ipos: (node,int) iport -> point
    method opos: (node,int) oport -> point

    method fakeipos: (node,float) iport -> point
    method fakeopos: (node,float) oport -> point

    method idir: (node,int) iport -> vector
    method odir: (node,int) oport -> vector
    
    method ityp: (node,int) iport -> typ
    method otyp: (node,int) oport -> typ
    
    method ifree: (node,int) iport -> bool
    method next: (node,int) iport -> (node,int) oport
    method next_opt: (node,int) iport -> (node,int) oport option
    method nexts: (node,int) iport -> node mset * (node,int) oport mset

    method ofree: (node,int) oport -> bool
    method prev: (node,int) oport -> (node,int) iport
    method prev_opt: (node,int) oport -> (node,int) iport option
    method prevs: (node,int) oport -> node mset * (node,int) iport mset

    (* distance to the targets of the graph *)
    method depth: node -> int
    method ceil: node -> (node,float) iport

    method rem_edge: (node,int) iport*(node,int) oport -> unit
    method add_edge: (node,int) iport*(node,int) oport -> unit
    method rem_node: node -> unit
    method add_node: node -> unit

    (* the graph in argument to [subst/replace] should be copied beforehand
       (to avoid aliasing problems) *)
    method subst: node -> graph -> unit
    method replace: graph -> unit
    method unbox: node -> unit

    method pp: pp_mode -> formatter -> unit
    method draw: canvas -> unit
    method draw_edge: canvas -> (node,int) iport*(node,int) oport -> unit
    method term: term

    (* improve placement of inner nodes (elastic dynamic)
       if force is true, recheck for stability
       returns true if the graph was stable (= needs not be redrawn)
     *)
    method improve: force:bool -> bool
    method on_stabilize: (unit -> bool) -> unit
  end
and node =
  object
    inherit element
    method kind: graph kind
    method draw: canvas -> unit
    method term: term
  end

type iport = (node,int) Types.iport
type oport = (node,int) Types.oport
type fakeiport = (node,float) Types.iport
type fakeoport = (node,float) Types.oport
type kind = graph Types.kind

type env = graph Element.env

type equation = graph * graph
type equations = env * equation list * equation
