open Types
open Graph_type

class virtual mk: arena ->
  object
    method private virtual help: string -> unit

    method private virtual read: string -> goal
    method private virtual write: string -> goal -> unit

    method private virtual open_dialog: unit
    method private virtual saveas_dialog: unit
    method private virtual do_save: unit
    method private virtual quit: unit
    method virtual fullscreen: unit
    
    (* Boolean indicates whether control is pressed *)
    method on_key_press: bool -> string -> unit
    (* Boolean indicates whether shift is pressed *)
    method on_button_press: bool -> unit
    method on_button_release: unit
    method on_motion: unit
    method on_tic: unit

    method undo: unit -> unit
    method redo: unit -> unit

    method load': goal -> unit
    method load: string -> unit
    method save: string -> unit
  end
