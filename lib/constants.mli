open Gg
open Vg

(* number of points in an inch: 72(.27) *)
val inch: float
(* number of points in a mm: inch / 25.4 *)
val mm: float

(* below: always in points *)
val fontsize : float
val font : font

val linewidth : float

val idsize: int -> size2
val varsize: int -> int -> size2
val expand: size2 -> size2

val pradius : float


val gray : color
val color : string -> color
val color' : ?color:color -> string -> color
