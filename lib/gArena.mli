open Diagrams.Types

val create:
  width:int ->
  height:int ->
  ?window:#GWindow.window ->
  #GMisc.drawing_area ->
  unit -> 
  arena
