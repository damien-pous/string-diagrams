open Diagrams
open Gg
open Vg

class arena ~width ~height ?window da () =
  object(self)
    inherit Arena.generic

    val clipboard = GData.clipboard Gdk.Atom.clipboard
    method! clipboard = Option.value clipboard#text ~default:"" 
    method! set_clipboard = clipboard#set_text
    
    val mutable backing = GDraw.pixmap ~width ~height ?window ()
    
    method private dsize =
      let w,h = backing#size in
      float_of_int w, float_of_int h
    
    method private dpointer =
      let x,y = da#misc#pointer in
      float_of_int x, float_of_int y      
    
    method! refresh =
      let cr = Cairo_gtk.create backing#pixmap in  
      let vgr = Vgr.create (Vgr_cairo.target cr) `Other in 
      let w,h = self#dsize in
      let size = Size2.v w h in
      let image = I.blend self#canvas#get (I.const Color.white) in
      let image = I.blend Messages.temporary#get image in
      let _ = Vgr.render vgr (`Image (size, self#view, image)) in
      let _ = Vgr.render vgr (`End) in
      da#misc#draw None

    method private expose ev =
      let area = GdkEvent.Expose.area ev in
      let x = Gdk.Rectangle.x area in
      let y = Gdk.Rectangle.y area in
      let width = Gdk.Rectangle.width area in
      let height = Gdk.Rectangle.width area in
      let drawing =
        da#misc#realize ();
        new GDraw.drawable da#misc#window
      in
      drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height backing#pixmap;
      false
    
    method private configure ev =
      let width = GdkEvent.Configure.width ev in
      let height = GdkEvent.Configure.height ev in
      (* Format.eprintf "%ix%i -> " width height; *)
      self#resize (float_of_int width, float_of_int height);
      backing <- GDraw.pixmap ~width ~height ?window ();
      self#refresh;
      false

    method private scroll ev =
      let state = GdkEvent.Scroll.state ev in
      Gdk.Convert.test_modifier `CONTROL state &&
      match GdkEvent.Scroll.direction ev with
      | `UP -> self#zoom 0.9; true
      | `DOWN -> self#zoom 1.1; true
      | _ -> false

    val mutable mode = None
    method private button_release ev =
      ignore ev;
      (* let state = GdkEvent.Button.state ev in *)
      (* Gdk.Convert.test_modifier `CONTROL state && *)
      (mode <- None; false)
    method private button_press ev =
      let state = GdkEvent.Button.state ev in
      Gdk.Convert.test_modifier `CONTROL state &&
      (mode <- Some da#misc#pointer; true)
    method private motion_notify ev =
      ignore ev;
      (* let state = GdkEvent.Motion.state ev in *)
      (* Gdk.Convert.test_modifier `CONTROL state && *)
      match mode with
      | Some(x0,y0) ->
         let (x,y) as p = da#misc#pointer in
         self#move (float_of_int (x0-x), float_of_int (y0-y)); mode <- Some p; true
      | _ -> false
    
    initializer
    let _ = da#event#connect#expose ~callback:self#expose in
    let _ = da#event#connect#configure ~callback:self#configure in
    let _ = da#event#connect#scroll ~callback:self#scroll in
    let _ = da#event#connect#button_press ~callback:self#button_press in
    let _ = da#event#connect#button_release ~callback:self#button_release in
    let _ = da#event#connect#motion_notify ~callback:self#motion_notify in
    GtkBase.Widget.add_events da#as_widget [ `BUTTON_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ]      
  end

let create = new arena
