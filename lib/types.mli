type 'a seq = 'a Seq.t      (* sequences (with index starting at 1) *)
type 'a mset = 'a MSet.t    (* (multi)sets *)
type 'a set = 'a Set.t      (* sets *)
type perm = Perm.t          (* finite support permutations *)
type inj = Inj.t            (* finite support injections *)
type iseq = ISeq.t          (* increasing sequences *)

type kv = Info.kv           (* placement/size/color informations *)
type kvl = Info.kvl

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
    method segment: ?color:color -> point*point -> unit 
    method curve: ?color:color -> point*point*point*point -> unit 
    method line: ?color:color -> line -> unit 
    method text: ?color:color -> point -> string -> unit 
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

class type ['a] writer =
  object
    method private read: string -> 'a
    method private write: string -> 'a -> unit
    method private write_svg: (image*box) list -> string -> unit
    method private write_pdf: (image*box) list -> string -> unit
  end

type pp_mode = Full | Sparse | Term | TermIfPossible | Rocq

(* names *)
type name = string

(* object types *)
type typ = Typ.t
type typs = Typ.ts

(* arrow types *)
type typ1 = typs * typs

(* equations *)
type 't equation = 't * 't

(* declarations *)
type 't decl =
  | T1
  | T2 of typ1 * 't option
  | TE of 't equation

(* environments *)
type 't env = (name * (kvl * 't decl)) list

(* terms in environment *)
type 't eterm = 't env * 't

(* goals (horn sentences) + current script *)
type 't goal = ('t env * 't equation) * string


(* input/output ports
   'v is intended to be int for real ports
      and float for intermediate fake ones
 *)
type ('node,'v) iport = Source of 'v | InnerTarget of 'node * 'v
type ('node,'v) oport = Target of 'v | InnerSource of 'node * 'v

type 'graph kind = Box of 'graph | Var of name


(* raw parsed terms/environments *)
module Raw: sig
  type term =
    | One
    | Idm
    | Wld
    | Var of name * kvl
    | Seq of term * term
    | Dot of term * term
    | Tns of term * term
    | Typ of term * term
    | Arr of term * term
    | Exp of term * int
    | Eqn of term * term
    | Box of term * kvl
    | Gph of elem list * kvl
    | Let of name * kvl * decl * term
  and elem =
    | Node of string * term * kvl
    | Edge of (string,int) iport * (string,int) oport
  and decl = term option * term option
end

class type paddable =
  object
    method width: float
    method height: float
    method shift: vector -> unit
  end

class type element = object
  method has: string -> bool
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
  method improve_shape: unit
  method setdirs: (point*vector) list -> (point*vector) list -> unit
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
