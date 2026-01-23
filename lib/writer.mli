open Types

class virtual ['a] virt:
  object
    method private virtual read: string -> 'a
    method private virtual write: string -> 'a -> unit
    method private virtual write_svg: (image*box) list -> string -> unit
    method private virtual write_pdf: (image*box) list -> string -> unit
  end
                
class fake: string -> ['a] writer
