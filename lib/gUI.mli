open Diagrams
open Types
open Graph_type

val create:
  window: #GWindow.window ->
  entry: #GText.view ->
  messages: #GText.view ->
  file: string -> 
  unit ->
  state ui_io
