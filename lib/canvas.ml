open Types
open Gg
open Vg

let area = `O { P.o with P.width = Constants.linewidth }

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
    method path ?(color=Color.black) ?fill p =
      Option.iter (fun fill -> self#blend (I.const fill |> I.cut p)) fill;
      self#blend (I.const color |> I.cut ~area p)
    method circle ?color ?fill c =
      self#path ?color ?fill (P.empty |> P.circle c.center c.radius)
    method pentagon ?color ?fill c =
      self#path ?color ?fill (pentagon c.center c.radius)
    method polygon ?color ?fill p =
      self#path ?color ?fill (Polygon.to_path p)
    method box ?color ?fill b =
      self#path ?color ?fill (P.empty |> P.rect b)
    method point ?color p =
      let fill = color in
      self#path ?color ?fill (P.empty |> P.circle p Constants.pradius)
    method segment ?color x y =
      self#path ?color (P.empty |> P.sub x |> P.line y)
    method curve ?color x' y' =
      let d = V2.(norm (x'-y')) /. 3. in
      self#path ?color (P.empty |> P.sub x' |> P.ccurve V2.(x' - smul d oy) V2.(y' + smul d oy) y')
    method line ?color l =
      let d = V2.smul 1000. l.dir in
      self#point ?color l.point;
      self#segment ?color (V2.sub l.point d) (V2.add l.point d)
    method text p text =
      let p = V2.sub p
                (V2.v (float_of_int (String.length text) *. Constants.fontsize/.3.)
                   (Constants.fontsize/.3.)) in
      self#blend (I.move p (I.const Color.black |> I.cut_glyphs ~text Constants.font []))
  end

class void: canvas =
  object
    method clear = ()
    method get = I.void
    method path ?color ?fill _ = ignore (color,fill) 
    method circle ?color ?fill _ = ignore (color,fill) 
    method pentagon ?color ?fill _ = ignore (color,fill) 
    method polygon ?color ?fill _ = ignore (color,fill) 
    method box ?color ?fill _ = ignore (color,fill) 
    method point ?color _ = ignore color 
    method segment ?color _ _ = ignore color 
    method curve ?color _ _ = ignore color 
    method line ?color _ = ignore color 
    method text _ _ = ()
  end
