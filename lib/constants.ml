open Gg

let inch = 72.27
let mm = inch /. 25.4

(* in points *)
let fontsize = 11.
(* let font = Vg.Font.{name="Latin Modern Roman"; slant=`Italic; weight=`W100; size=fontsize } *)
let font = Vg.Font.{name="Helvetica"; slant=`Normal; weight=`W100; size=fontsize }

(* in inches *)
let pathlinewidth = 2.0
let shapelinewidth = 0.0
let point_radius = 2.0
let circle_radius = 5.0
let cross_radius = 5.0
let triangle_radius = 8.0

let spacing = fontsize *. 3.
let expand s = V2.(s + v spacing spacing)
let size n = Size2.v (float_of_int n *. spacing) spacing
let idm_size = size
let var_size n m = size (max n m)
let empty_size n m =
  if n+m=0 then Size2.v (spacing /. 2.) spacing
  else var_size n m
let estimate_size n m k =
  let nm = max n m in
  Size2.v (float_of_int nm *. spacing) (float_of_int nm *. spacing /. float_of_int (k+1))

let black = Color.black
let red = Color.v 0.8 0.1 0.1 1.
let gray = Color.gray 0.5
let alpha s c = Color.with_a c s
let xcolor = function
  | "red"    -> red
  | "green"  -> Color.v 0.1 0.8 0.1 1.
  | "blue"   -> Color.v 0.1 0.1 0.8 1.
  | "yellow" -> Color.v 0.8 0.8 0.0 1.
  | "lblue"  -> Color.v 0.4 0.8 0.8 1.
  | "orange" -> Color.v 1.0 0.4 0.0 1.
  | "violet" -> Color.v 0.4 0.1 0.4 1.
  | "turquoise" -> Color.v 0.0 0.4 0.4 1.
  | "rose"   -> Color.v 1.0 0.4 1.0 1.
  | "purple" -> Color.v 0.8 0.0 0.4 1.
  | "brown"  -> Color.v 0.7 0.3 0.0 1.
  | "cacadoie" -> Color.v 0.3 0.6 0.0 1.
  | "white"  -> Color.white
  | "void"  -> Color.void
  | "black"  -> Color.black
  | "gray"   -> gray
  | "lhs"    -> Color.v 1.0 1.0 0. 0.5
  | "rhs"    -> Color.v 1.0 0.5 0. 0.6
  | "done"   -> Color.v 0.0 1.0 0.3 0.4
  | "tgray"  -> Color.gray ~a:0.2 0.0
  | _        -> gray

let color' ?color name =
  match color with
  | Some c -> xcolor c 
  | None -> xcolor
              (if name = "" then "void"
               else match name.[0] with
               | 'a' -> "yellow"
               | 'b' -> "orange"
               | 'c' | 'A' -> "red"
               | 'd' | 'C' -> "violet"
               | 'e' -> "green"
               | 'f' -> "lblue"
               | 'g' | 'B' -> "blue"
               | 'h' -> "turquoise"
               | 'i' -> "purple"
               | 'j' -> "rose"
               | 'k' -> "cacadoie"
               | _   -> "gray")
let color = xcolor

let iport_color = color "violet"
let oport_color = color "turquoise"
