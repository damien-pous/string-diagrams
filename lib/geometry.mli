open Types

(* debug area *)
val set_debug: canvas -> unit
val unset_debug: unit -> unit

(* directed lines *)
val line: point -> point -> line
val line': point -> vector -> line

(* circles *)
val circle: point -> float -> circle

(* random point in [-s;s]x[-s;s] *)
val random2: float -> point

(* distance between two points *)
val dist: point -> point -> float

(* barycenter *)
val center: point list -> point

(* center of incircle *)
val mid3: point -> point -> point -> point

(* directed angle between two vectors *)
val angle: vector -> vector -> float

(* intersection of two lines *)
val line_inter: line -> line -> point

(* (directed) distance between a line and a point *)
val line_dist : line -> point -> float

(* orientations *)
type lr = L | E | R
val swap: lr -> lr

(* side of a point w.r.t. a line *)
val side: line -> point -> lr

(* orientation of three points [orient A B C = side AB C] *)
val orient: point -> point -> point -> lr

(* orient a polygon in the clockwise direction *)
val clockwise: polygon -> polygon

(* membership of a point in a polygon *)
val mem_poly: point -> polygon -> bool

(* enclosing box for a polygon *)
val poly_box: polygon -> box

(* (potential, oriented) intersection of two segments
   (allowing A,C,D, excluding B)
   when [AB] and [CD] cross, the orientation is [side AB C] *)
val intersection: point*point -> point*point -> (point*lr) option
  
(* scale a box around its center *)
val scale_box: float -> box -> box
