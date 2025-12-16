open Gg
open Vg

(* number of points in an inch: 72(.27) *)
val inch: float
(* number of points in a mm: inch / 25.4 *)
val mm: float

(* below: always in points *)
val fontsize: float
val font: font

val linewidth: float

val spacing: float

val empty_size: int -> int -> size2
val var_size: int -> int -> size2
val idm_size: size2
val estimate_size: int -> int -> int -> size2 (* sources, targets, number of nodes *)

val expand: size2 -> size2

val pradius: float

val iport_color: color
val oport_color: color

val red: color
val gray: color
val black: color
val alpha: float -> color -> color
val color: string -> color
val color': ?color:string -> string -> color
