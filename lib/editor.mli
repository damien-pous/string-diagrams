open Types
open Graph_type

type state = env * graph

class virtual mk: arena ->
  object
    inherit [state] program
    method virtual entry: string
    method virtual set_entry: string -> unit
    method virtual entry_warning: string -> unit
    method on_entry_changed: unit
  end
