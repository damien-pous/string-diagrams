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
type line = { point: point; dir: vector }           (* directed lines *)
type polygon = point Polygon.t                      (* polygons *)
type circle = { center: point; radius: float }      (* circles *)

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
    method polygon: ?color:color -> ?fill:color -> polygon -> unit
    method point: ?color:color -> point -> unit
    method segment: ?color:color -> point -> point -> unit 
    method line: ?color:color -> line -> unit 
    method text: point -> string -> unit 
  end

class type msg_canvas =
  object
    inherit canvas
    method msg: 'a. ('a, formatter, unit) format -> 'a
    method messages: string
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

(* input/output ports *)
type 'node iport = Outer of int | Inner of 'node * int
type 'node oport = 'node iport

(* raw parsed terms/environments *)
module Raw: sig
  type port = string iport
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
    method pp_infos: pp_mode -> formatter -> unit
  end

class type area =
  object
    inherit printable
    method pos: point
    method size: size
    method width: float
    method height: float
    method box: box
    method safebox: box
    method color: color
    
    method contains: point -> bool

    method move: point -> unit
    method shift: vector -> unit
    method private on_shift: vector -> unit
    method scale: float -> unit

    method draw_boundary: canvas -> unit
  end
