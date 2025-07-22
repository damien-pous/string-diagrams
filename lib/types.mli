type 'a seq = 'a Seq.t      (* sequences (with index starting at 1) *)
type 'a set = 'a Set.t      (* sets *)
type 'a mset = 'a MSet.t    (* multisets *)
type perm = Perm.t          (* finite support permutations *)
type inj = Inj.t            (* finite support injections *)
type iseq = ISeq.t          (* increasing sequences *)

type point= Gg.p2               (* 2D points *)
type vector = Gg.v2             (* 2D vectors *)
type box = Gg.box2              (* 2D boxes *)
type size = Gg.size2            (* 2D sizes *)
type line = { point: point; dir: vector }          (* directed lines *)
type circle = { center: point; radius: float }     (* circles *)

type color = Gg.color           (* colors *)
type font = Vg.font             (* fonts *)
type image = Vg.image           (* images *)
type path = Vg.path             (* paths *)

type formatter = Format.formatter

class type canvas =
  object
    method clear: unit
    method get: image
    method path: ?color:color -> ?fill:color -> path -> unit
    method box: ?color:color -> ?fill:color -> box -> unit 
    method circle: ?color:color -> ?fill:color -> circle -> unit
    method pentagon: ?color:color -> ?fill:color -> circle -> unit
    method point: ?color:color -> point -> unit
    method segment: ?color:color -> point -> point -> unit 
    method line: ?color:color -> line -> unit 
    method text: point -> string -> unit 
  end

class type arena =
  object
    method canvas: canvas
    method view: box
    method ensure: box -> unit
    method zoom: float -> unit
    method move: float*float -> unit
    method resize: float*float -> unit
    method pointer: point
    method refresh: unit
  end

type pp_mode = Full | Sparse | Term

type name = string              (* box/variable names *)
type 'node pkind = Outer of int | Inner of 'node * int
type 'node _edge = { src: 'node pkind; tgt: 'node pkind }
type 'graph _nkind = Var of int*int*name | Box of 'graph

(* raw parsed terms/environments *)
module Raw: sig
  type port = string pkind
  type 'a term =
    | Emp
    | Idm
    | Var of name * 'a
    | Seq of 'a term * 'a term
    | Tns of 'a term * 'a term
    | Typ of 'a term * int * int
    | Box of 'a term * 'a
    | Gph of 'a elem list * 'a
  and 'a elem =
    | Node of string * 'a term * 'a
    | Edge of port * port
  (* environments *)
  type 'a env = (name*('a*(int*int) option*'a term option)) list
  type 'a envterm = 'a env * 'a term
end

class type printable =
  object
    method get: string -> string option
    method set: string -> string -> unit
    method unset: string -> unit
    method pp: pp_mode -> formatter -> unit
    method pp_empty: pp_mode -> bool
  end

class type positionned =
  object
    inherit printable
    method pos: point
    method size: size
    method width: float
    method height: float
    method box: box
    method safebox: box
    method color: color
    method set_color: color -> unit
    method move: point -> unit
    method shift: vector -> unit
    method scale: float -> unit
    method placed: bool (* was the element placed before? *)
  end

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

    (* method rem_edge: node _edge -> unit *)
    (* method rem_node: node -> unit *)

    (* method new_edge: port -> port -> node _edge *)
    (* (\* method new_var_box: int -> int -> name -> node *\) *)
    (* (\* method new_graph_box: graph -> node *\) *)

    (* method subst: node -> graph -> unit *)
    (* method unbox: node -> unit *)
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

    method ipos: node pkind -> point
    method opos: node pkind -> point
    method find: point -> [ `I of port | `O of port | `N of node | `None ]

    method draw: image
    method draw_on: canvas -> unit

  end
type port_ = node pkind
type edge = node _edge
type nkind = graph _nkind
