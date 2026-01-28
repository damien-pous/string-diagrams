open Types
open Messages

class fake msg: ['a] writer =
  object
    method private read =
      warning "cannot read files%s" msg
    method private write _ =
      warning "cannot write files%s" msg
    method private write_svg _ =
      warning "cannot write SVG files%s" msg
    method private write_pdf _ =
      warning "cannot write PDF files%s" msg
  end
