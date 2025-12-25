type 'a t

val create: 'a -> 'a t
val present: 'a t -> 'a
val save: ?cmp: ('a -> 'a -> bool) -> 'a t -> 'a -> unit
val clear: 'a t -> unit
val undo: 'a t -> 'a option
val redo: 'a t -> 'a option


class virtual ['a] mk:
      ('a -> string) ->           (* serialize *)
      (string -> 'a) ->           (* deserialize *)
  object
    method private virtual state: 'a
    method private virtual set_state: 'a -> unit

    method private virtual read: string -> 'a
    method private virtual write: string -> 'a -> unit

    method private checkpoint: unit
    method private abort: unit
    
    method undo: unit -> unit
    method redo: unit -> unit

    method load: string -> unit
    method load': 'a -> unit
    method save: string -> unit
  end
