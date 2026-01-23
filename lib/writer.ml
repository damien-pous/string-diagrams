open Types
open Messages

class virtual ['a] virt =
  object
    method private virtual read: string -> 'a
    method private virtual write: string -> 'a -> unit
    method private virtual write_svg: (image*box) list -> string -> unit
    method private virtual write_pdf: (image*box) list -> string -> unit
  end

class fake s: ['a] writer =
  object
    method private read _ =
      warning "cannot read files%s" s
    method private write _ _ =
      warning "cannot read files%s" s
    method private write_svg _ _ =
      warning "cannot write SVG files%s" s
    method private write_pdf _ _ =
      warning "cannot write PDF files%s" s      
  end
