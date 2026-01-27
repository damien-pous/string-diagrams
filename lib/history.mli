type 'a t

val create: 'a -> 'a t
val present: 'a t -> 'a
val save: ?cmp: ('a -> 'a -> bool) -> 'a t -> 'a -> unit
val clear: 'a t -> unit
val undo: 'a t -> 'a option
val redo: 'a t -> 'a option


class virtual mk:
  object
    method private virtual on_reset: unit

    method private checkpoint: unit
    method private clear_history: unit
    method private abort: unit
    
    method undo: unit -> unit
    method redo: unit -> unit
  end
