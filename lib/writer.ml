open Types
open Messages

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
