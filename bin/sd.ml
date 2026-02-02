open Diagrams
open Diagrams_cairo
open Diagrams_gtk

open GMain

let width = 1900
let height = 800

let file = 
  (match Sys.argv with
   | [|_|] -> "default"
   | [|_;file|] when File.exists file -> file
   | _ -> Format.eprintf "usage: sd [file]\n"; exit 1)

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:file ~position:`CENTER ()
let paned = GPack.paned `VERTICAL ~packing:window#add ()
let vbox1 = GPack.vbox ~homogeneous:false ~packing:(paned#pack1 ~resize:true ~shrink:true) ()
let vbox2 = GPack.vbox ~homogeneous:false ~packing:(paned#pack2 ~resize:true ~shrink:true) ()

(* let menubar = GMenu.menu_bar ~packing:vbox#pack () *)
(* let factory = new GMenu.factory menubar *)
(* let accel_group = factory#accel_group *)
(* let file_factory = new GMenu.factory (factory#add_submenu "File") ~accel_group *)
(* let edit_factory = new GMenu.factory (factory#add_submenu "Edit") ~accel_group *)
(* let view_factory = new GMenu.factory (factory#add_submenu "View") ~accel_group *)

let da = GMisc.drawing_area ~width:width ~height ~packing:(vbox1#pack ~expand:true) ()
let arena = GArena.create ~width:width ~height ~window da ()
let messages = GText.view ~editable:false ~cursor_visible:false ~packing:(vbox1#pack) ()
let _ = messages#misc#modify_font_by_name Constants.msg_font

let scroll = GBin.scrolled_window
               ~hpolicy:`AUTOMATIC
               ~vpolicy:`AUTOMATIC
               ~shadow_type:`IN
               ~placement:`BOTTOM_RIGHT
               ~packing:(vbox2#pack ~expand:true) ()
let entry = GText.view ~width:width ~height:0 ~editable:true ~accepts_tab:false
              ~packing:scroll#add ()
let _ = entry#misc#modify_font_by_name Constants.msg_font

let ui = GUI.create ~window ~entry ~messages ~file ()
let self = Program.create arena ui

let on_button_press ev =
  let state = GdkEvent.Button.state ev in
  not (Gdk.Convert.test_modifier `CONTROL state) &&
    (self#on_button_press ~shft:(Gdk.Convert.test_modifier `SHIFT state) ~ctrl:false; true)

let on_motion _ =
  self#on_motion; true

let on_button_release _ =
  self#on_button_release; true

let on_key_press ev =
  if not (List.mem (GdkEvent.Key.keyval ev) [GdkKeysyms._Control_L; GdkKeysyms._Control_R]) then
    let state = GdkEvent.Key.state ev in  
    let ctrl = List.mem `CONTROL state in
    let shft = List.mem `SHIFT state in
    let s =
      let kv = GdkEvent.Key.keyval ev in
      if kv = GdkKeysyms._Left then "ArrowLeft"
      else if kv = GdkKeysyms._Right then "ArrowRight"
      else if kv = GdkKeysyms._Escape then "Escape"
      else if kv = GdkKeysyms._e then "e"
      else if kv = GdkKeysyms._f then "f"
      else if kv = GdkKeysyms._o then "o"
      else if kv = GdkKeysyms._q then "q"
      else if kv = GdkKeysyms._r then "r"
      else if kv = GdkKeysyms._s then "s"
      else if kv = GdkKeysyms._z then "z"
      else if kv = GdkKeysyms._v then "v"
      else GdkEvent.Key.string ev
    in
    try self#on_key_press ~ctrl ~shft s; false with Program.Skip_key -> true
  else true

(* TODO: capture this in editor *)
let refresh() = arena#refresh
let atomic ?(keep=false) m f x d =
  if false then print_endline m;
  if keep then Messages.temporary#clear else Messages.clear();
  Messages.catch f x d refresh
let atomic_unit ?keep m f x = atomic ?keep m f x ()
let atomic_true ?keep m f x = atomic ?keep m f x true
(* let _ = file_factory#add_item "Open" ~key:GdkKeysyms._O ~callback:(atomic_unit load) *)
(* let _ = file_factory#add_item "Save" ~key:GdkKeysyms._S ~callback:(atomic_unit save) *)
(* let _ = file_factory#add_item "Save as" ~key:GdkKeysyms._E ~callback:(atomic_unit save_as) *)
(* let _ = file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit *)
(* let _ = edit_factory#add_item "Undo" ~key:GdkKeysyms._Z ~callback:(atomic_unit self#undo) *)
(* let _ = edit_factory#add_item "Redo" ~key:GdkKeysyms._R ~callback:(atomic_unit self#redo) *)
(* let _ = view_factory#add_item "Toggle fullscreen" ~key:GdkKeysyms._F ~callback:fullscreen *)

let _ = GtkBase.Widget.add_events da#as_widget
          [ `KEY_PRESS; `POINTER_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ]
let _ = da#misc#set_can_focus true
let _ = da#event#connect#motion_notify ~callback:(atomic_true ~keep:true "m" on_motion)
let _ = da#event#connect#button_press ~callback:(atomic_true "bp" on_button_press)
let _ = da#event#connect#button_release ~callback:(atomic_true "br" on_button_release)
let _ = da#event#connect#key_press ~callback:(atomic_true "kp" on_key_press)
let _ = entry#buffer#connect#changed ~callback:(atomic_unit ~keep:true "ec" ui#on_entry_changed)
let _ = Glib.Timeout.add ~ms:25 ~callback:(Printexc.print (fun _ -> self#on_tic; true))
let _ = window#connect#destroy ~callback:Main.quit
(* let _ = window#add_accel_group accel_group *)
let _ = ui#fullscreen
let _ = window#show ()
let _ = atomic_unit "lf" (fun () -> self#load_file) ()
let _ = Main.main ()

