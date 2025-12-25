open Types
open Graph_type

class virtual mk: arena ->
  object
    method private virtual help: string -> unit

    method private virtual read: string -> equations
    method private virtual write: string -> equations -> unit
    
    (* Boolean indicates whether shift is pressed *)
    method on_button_press: bool -> unit
    method on_button_release: unit
    method on_motion: unit
    method on_key_press: string -> unit
    method on_tic: unit

    method undo: unit -> unit
    method redo: unit -> unit

    method load': equations -> unit
    method load: string -> unit
    method save: string -> unit
  end
