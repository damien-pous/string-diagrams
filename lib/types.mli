type 'a seq = 'a Seq.t      (* sequences (with index starting at 1) *)
type 'a mset = 'a MSet.t    (* (multi)sets *)
type 'a set = 'a Set.t      (* sets *)
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
    method path: ?color:color -> path -> unit
    method shape: ?border: float -> ?color:color -> ?fill:color -> path -> unit
    method box: ?color:color -> ?fill:color -> box -> unit 
    method circle: ?color:color -> ?fill:color -> circle -> unit
    method pentagon: ?color:color -> ?fill:color -> circle -> unit
    method polygon: ?border: float -> ?color:color -> ?fill:color -> polygon -> unit
    method point: ?color:color -> point -> unit
    method segment: ?color:color -> point -> point -> unit 
    method curve: ?color:color -> point -> point -> point -> point -> unit 
    method line: ?color:color -> line -> unit 
    method text: point -> string -> unit 
  end

class type msg_canvas =
  object
    inherit canvas
    method msg: 'a. ('a, formatter, unit) format -> 'a
    method messages: string
    method clear_all: unit
  end
    
class type arena =
  object
    method canvas: canvas
    method view: box
    method ensure: box -> unit
    method fit: box -> unit
    method zoom: float -> unit
    method move: float*float -> unit
    method resize: float*float -> unit
    method pointer: point
    method refresh: unit
    method clipboard: string
    method set_clipboard: string -> unit
  end

type pp_mode = Full | Sparse | Term | TermIfPossible

type name = string              (* box/variable names *)

type typ = Typ.t                (* object types *)
type typs = Typ.ts

(* input/output ports
   'v is intended to be int for real ports
      and float for intermediate fake ones
 *)
type ('node,'v) iport = Source of 'v | InnerTarget of 'node * 'v
type ('node,'v) oport = Target of 'v | InnerSource of 'node * 'v

type 'graph kind = Box of 'graph | Var of name


(* raw parsed terms/environments *)
module Raw: sig
  type 'a term =
    | Emp
    | Idm of typs
    | Var of name * 'a
    | Seq of 'a term * 'a term
    | Tns of 'a term * 'a term
    | Typ of 'a term * typs * typs
    | Box of 'a term * 'a
    | Gph of 'a elem list * 'a
  and 'a elem =
    | Node of string * 'a term * 'a
    | Edge of (string,int) iport * (string,int) oport
  (* environments *)
  type 'a env = (name*('a*(typs*typs) option*'a term option)) list
  type 'a envterm = 'a env * 'a term
  type 'a equations = 'a env * ('a term * 'a term) list * bool
end

class type paddable =
  object
    method width: float
    method height: float
    method shift: vector -> unit
  end

class type element = object
  method get: string -> string option
  method set: string -> string -> unit
  method unset: string -> unit
  method pp_kvl: pp_mode -> formatter -> unit

  method sources: typs
  method targets: typs
  method nsources: int
  method ntargets: int
  method fnsources: float
  method fntargets: float
  method styp: int -> typ
  method ttyp: int -> typ  
  
  inherit paddable
  method pos: point
  method spos: int -> point
  method tpos: int -> point
  method fakespos: float -> point
  method faketpos: float -> point
  method sdir: int -> vector
  method tdir: int -> vector
  method size: size
  method color: color
  method box: box
  method safebox: box  
  method contains: point -> bool  
  method move: point -> unit
  method scale: float -> unit
  method rebox: box -> unit  
  method draw: canvas -> unit
end
