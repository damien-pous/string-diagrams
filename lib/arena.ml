open Gg

class virtual generic =
  let canvas = new Canvas.basic in
  object(self)

    method virtual private dpointer: float*float
    method virtual private dsize: float*float
    method refresh = ()

    val mutable view = Box2.unit
    method view = view

    method canvas = canvas

    val mutable clipboard = ""
    method clipboard = clipboard
    method set_clipboard s = clipboard <- s
    
    method private point_of_dpoint (x,y) =
      let w,h = self#dsize in
      (* p is (x,y) in [0;1]x[0;1] *)
      let p = P2.v
                (x /. w)
                (1. -. y /. h)
      in
      V2.add (Box2.o view) (V2.mul p (Box2.size view))

    method private vector_of_dvector v =
      V2.sub (self#point_of_dpoint v) (self#point_of_dpoint (0.,0.))
    
    method private dpoint_of_point p =
      let w,h = self#dsize in
      let p = V2.div (V2.sub p (Box2.o view)) (Box2.size view) in
      let x',y' = V2.x p, V2.y p in
      (w *. x', h *. (1.-.y'))
    
    method pointer = self#point_of_dpoint self#dpointer

    method fit b =
      let bw,bh = Box2.w b, Box2.h b in
      let w,h = self#dsize in
      if bw*.h <= bh*.w
      then view <- Box2.v_mid (Box2.mid b) (V2.smul 1.1 (V2.v (bh*.w/.h) bh))
      else view <- Box2.v_mid (Box2.mid b) (V2.smul 1.1 (V2.v bw (bw*.h/.w)));
      self#refresh

    method ensure b =
      if not (Box2.subset b self#view) then
        self#fit b
    
    method move v =
      view <- Box2.move (self#vector_of_dvector v) view;
      self#refresh

    method resize (w,h) =
      view <- Box2.v_mid (Box2.mid view)
                (V2.sub (self#point_of_dpoint (w,0.)) (self#point_of_dpoint (0.,h)));
      (* note: self#refresh to be called by concrete subclasses *)
    
    method zoom s =
      let c = self#pointer in
      let v = V2.sub c (Box2.o view) in
      view <- Box2.v (V2.sub c (V2.smul s v)) (V2.smul s (Box2.size view));
      self#refresh
    
  end
