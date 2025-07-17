open Diagrams.Types

val create:
  width:int ->
  height:int ->
  ?window:< misc : #GDraw.misc_ops; .. > ->
  GMisc.drawing_area ->
  unit -> 
  arena
