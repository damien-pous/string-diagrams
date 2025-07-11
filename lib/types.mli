type 'a seq = 'a Seq.t      (* sequences (with index starting at 1) *)
type 'a set = 'a Set.t      (* sets *)
type 'a mset = 'a MSet.t    (* multisets *)
type perm = Perm.t          (* finite support permutations *)
type inj = Inj.t            (* finite support injections *)
type iseq = ISeq.t          (* increasing sequences *)

type point= Gg.p2               (* 2D points *)
type vector = Gg.v2             (* 2D vectors *)
type box = Gg.box2              (* 2D boxes *)
type line = { point: point; dir: vector }          (* directed lines *)
type circle = { center: point; radius: float }     (* circles *)

type color = Gg.color           (* colors *)
type font = Vg.font             (* fonts *)
type image = Vg.image           (* images *)
type path = Vg.path             (* paths *)

type formatter = Format.formatter
type pp_mode = Full | Sparse

type label = string             (* box names *)

type 'a rterm =                 (* raw terms *)
  | Id of int
  | Seq of 'a rterm * 'a rterm
  | Tns of 'a rterm * 'a rterm
  | Box of label*'a
type 'a env = (label*(int*'a*int)) list (* environments *)
  

(* 'functions' used to map decorations in terms or graphs *)
type ('a,'b) mapper =
  { fs: int -> 'a -> 'b;        (* sources; first argument is the arity *)
    ft: int -> 'a -> 'b;        (* targets; first argument is the arity *)
    fi: 'a -> 'b }              (* inner boxes *)

class type printable =
  object
    method label: string
    method get: string -> string option
    method set: string -> string -> unit
    method unset: string -> unit
    method pp: pp_mode -> formatter -> unit
    method pp_empty: pp_mode -> bool
    method kind: [`E|`I|`S]
  end

val same_label: #printable -> #printable -> bool

class type positionned =
  object
    inherit printable
    method pos: point
    method radius: float
    method circle: circle
    method color: color
    method set_color: color -> unit
    method move: point -> unit
    method scale: float -> unit
    method placed: bool (* was the element placed before? *)
  end

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

module type BASE = sig
  type 'a t
  val nsrc: 'a t -> int
  val ntgt: 'a t -> int
  val size: 'a t -> int
  val map: ('a,'b) mapper -> 'a t -> 'b t
end

module type ALGEBRA = sig
  include BASE
  val id: int -> 'a t
  val seq: 'a t -> 'a t -> 'a t
  val tns: 'a t -> 'a t -> 'a t
  val box: int -> 'a -> int -> 'a t
end

(* initial algebras *)
module type IALGEBRA = sig
  include ALGEBRA
  module I(M: ALGEBRA): sig val eval: 'a t -> 'a M.t end
  val pp: pp_mode -> formatter -> #printable t -> unit
end

(* extension with source&target decorations *)
module type SALGEBRA = sig
  type 'a u 
  module U: IALGEBRA with type 'a t = 'a u
  include BASE
  val decorate: 'a seq -> 'a u -> 'a seq -> 'a t
end

(* initial with source&target algebras *)
module type ISALGEBRA = sig
  include SALGEBRA
  module SI(M: SALGEBRA): sig val eval: 'a t -> 'a M.t end  
  val pp: pp_mode -> formatter -> #printable t -> unit
end
