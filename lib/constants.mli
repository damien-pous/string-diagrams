open Misc
open Gg
open Vg

(* are we running the editor *)
val editor: bool

val labels: bool ref
val toggle_labels: unit -> unit

(* number of points in an inch: 72(.27) *)
val inch: float
(* number of points in a mm: inch / 25.4 *)
val mm: float

(* below: always in points *)
val fontsize: float
val font: font
val msg_font: string

val pathlinewidth: float
val shapelinewidth: float

val spacing: float

val empty_size: int -> int -> size2
val var_size: int -> int -> size2
val idm_size: int -> size2
val estimate_size: int -> int -> int -> size2 (* sources, targets, number of nodes *)

val point_radius: float
val circle_radius: float
val cross_radius: float
val triangle_radius: float

val expand: size2 -> size2

val iport_color: color
val oport_color: color

val red: color
val gray: color
val black: color
val alpha: float -> color -> color

val color: string -> color      (* "blue" -> blue, etc *)
val id_color: string -> color   (* "a" -> blue, etc *)
