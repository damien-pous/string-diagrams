open Types
open Graph_type

type state = env * graph

class virtual locate: arena ->
  object
    method virtual entry: string
    method virtual set_entry: string -> unit
    method virtual entry_warning: string -> unit
    method virtual help: string -> unit

    method private virtual read: string -> state
    method private virtual write: string -> state -> unit
    method private virtual export: string -> state -> unit
    
    method on_button_press: unit
    method on_button_release: unit
    method on_motion: unit
    method on_key_press: string -> unit
    method on_entry_changed: unit

    method undo: unit -> unit
    method redo: unit -> unit

    method load_from: string -> unit
    method save_to: string -> unit

    method init: string -> unit
  end
