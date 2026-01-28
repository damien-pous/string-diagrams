open Misc
open Types
open Gg
open Vg

let area w = `O { P.o with P.width = w }
let path_area = area Constants.pathlinewidth
(* let shape_area = area Constants.shapelinewidth *)

let pentagon c r =
  let r = V2.smul r V2.oy in
  let p i = V2.add c (V2.ltr (M2.rot2 (i *. Float.pi /. 2.5)) r) in
  P.empty |>
    P.sub (p 0.) |>
    P.line (p 1.) |>
    P.line (p 2.) |>
    P.line (p 3.) |>
    P.line (p 4.) |>
    P.close

class basic: canvas =
  object(self)
    val mutable image = I.void
    method clear = image <- I.void
    method get = image
    method private blend i = image <- (image |> I.blend i)
    method path ?(color=Color.black) p =
      self#blend (I.const color |> I.cut ~area:path_area p)
    method shape ?(border=Constants.shapelinewidth) ?(color=Color.black) ?fill p =
      Option.iter (fun fill -> self#blend (I.const fill |> I.cut p)) fill;
      if !Constants.contours then
        self#blend (I.const color |> I.cut ~area:(area border) p)
    method circle ?color ?fill c =
      self#shape ?color ?fill (P.empty |> P.circle c.center c.radius)
    method pentagon ?color ?fill c =
      self#shape ?color ?fill (pentagon c.center c.radius)
    method polygon ?border ?color ?fill p =
      self#shape ?border ?color ?fill (Polygon.to_path p)
    method box ?color ?fill b =
      self#shape ?color ?fill (P.empty |> P.rect b)
    method point ?color p =
      let fill = color in
      self#shape ?color ?fill (P.empty |> P.circle p Constants.point_radius)
    method segment ?color (x,y) =
      self#path ?color (P.empty |> P.sub x |> P.line y)
    method curve ?color (x,p,q,y) =
      self#path ?color (P.empty |> P.sub x |> P.ccurve p q y)
      
    method line ?color l =
      let d = V2.smul 1000. l.dir in
      self#point ?color l.point;
      self#segment ?color V2.(l.point-d, l.point+d)
    method text ?(color=Color.black) p text =
      let p = V2.sub p
                (V2.v (float_of_int (String.length text) *. Constants.fontsize/.3.)
                   (Constants.fontsize/.3.)) in
      self#blend (I.move p (I.const color |> I.cut_glyphs ~text Constants.font []))
  end

class void: canvas =
  object
    method clear = ()
    method get = I.void
    method path ?color _ = ignore (color) 
    method shape ?border ?color ?fill _ = ignore (border,color,fill) 
    method circle ?color ?fill _ = ignore (color,fill) 
    method pentagon ?color ?fill _ = ignore (color,fill) 
    method polygon ?border ?color ?fill _ = ignore (border,color,fill) 
    method box ?color ?fill _ = ignore (color,fill) 
    method point ?color _ = ignore color 
    method segment ?color _ = ignore color 
    method curve ?color _ = ignore color 
    method line ?color _ = ignore color 
    method text ?color _ _ = ignore color 
  end
