open Diagrams
open Diagrams_cairo
open Diagrams_gtk

open GMain

let width = 1900
let height = 1000

let file = ref
             (match Sys.argv with
              | [|_|] -> "default"
              | [|_;file|] when File.SDP.exists file -> file
              | _ -> Format.eprintf "usage: prove [file]\n"; exit 1)

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:!file ~position:`CENTER ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()

(* let menubar = GMenu.menu_bar ~packing:vbox#pack () *)
(* let factory = new GMenu.factory menubar *)
(* let accel_group = factory#accel_group *)
(* let file_factory = new GMenu.factory (factory#add_submenu "File") ~accel_group *)
(* let edit_factory = new GMenu.factory (factory#add_submenu "Edit") ~accel_group *)
(* let view_factory = new GMenu.factory (factory#add_submenu "View") ~accel_group *)

let da = GMisc.drawing_area ~width ~height ~packing:(vbox#pack ~expand:true) ()
let arena = GArena.create ~width ~height ~window da ()
let general_msg = GText.view ~editable:false ~cursor_visible:false ~packing:(vbox#pack) ()
let _ = general_msg#misc#modify_font_by_name Constants.msg_font

let dialog title action stock stock' filter =
  let dlg = GWindow.file_chooser_dialog
    ~action ~title
    ~parent:window
    ~position:`CENTER_ON_PARENT
    ~destroy_with_parent:true ()
  in
  dlg#add_button_stock `CANCEL `CANCEL;
  dlg#add_select_button_stock stock stock';
  dlg#add_filter filter;
  ignore(dlg#set_current_folder ".");
  fun k -> Messages.catch (fun () ->
               if dlg#run() = stock' then (
                 match dlg#filename with
                 | Some f ->
                    let f = Filename.chop_extension f in
                    window#set_title (Filename.basename f);
                    file := f;
                    k f
                 | None -> ()
               )) () () dlg#misc#hide

class gprover =
  object(self)
    inherit Prover.mk arena
    inherit File.SDP.writer
    method help = print_endline
    val open_dialog =
      dialog "Open graph file" `OPEN `OPEN `OPEN 
        (GFile.filter ~name: "SDP file" ~patterns:["*.sdp"] ())
    method private open_dialog = open_dialog self#load
    val saveas_dialog=
      dialog "Save graphs as" `SAVE `SAVE `SAVE 
        (GFile.filter ~name: "SD file" ~patterns:["*.sd"] ())
    method private saveas_dialog = saveas_dialog self#save
    method private do_save =
      self#save !file
    method private quit = Main.quit()
    val mutable fs = false
    method fullscreen =
      if fs then window#unfullscreen() else window#fullscreen();
      fs <- not fs    
  end
let self = new gprover

let on_button_press ev =
  let state = GdkEvent.Button.state ev in
  not (Gdk.Convert.test_modifier `CONTROL state) &&
    (self#on_button_press (Gdk.Convert.test_modifier `SHIFT state); true)

let on_motion _ =
  self#on_motion; true

let on_button_release _ =
  self#on_button_release; true

let on_key_press ev =
  (if not (List.mem (GdkEvent.Key.keyval ev) [GdkKeysyms._Control_L; GdkKeysyms._Control_R]) then
     let state = GdkEvent.Key.state ev in  
     let ctrl = List.mem `CONTROL state in
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
       else GdkEvent.Key.string ev
     in
     self#on_key_press ctrl s); true

(* TODO: capture this in editor *)
let refresh() = arena#refresh; general_msg#buffer#set_text Messages.temporary#messages
let atomic ?(clearall=true) f x d =
  if clearall then Messages.temporary#clear_all else Messages.temporary#clear;  
  Messages.catch f x d refresh
let atomic_unit ?clearall f x = atomic ?clearall f x ()
let atomic_true ?clearall f x = atomic ?clearall f x true
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
let _ = da#event#connect#motion_notify ~callback:(atomic_true ~clearall:false on_motion)
let _ = da#event#connect#button_press ~callback:(atomic_true on_button_press)
let _ = da#event#connect#button_release ~callback:(atomic_true on_button_release)
let _ = da#event#connect#key_press ~callback:(atomic_true on_key_press)
let _ = Glib.Timeout.add ~ms:25 ~callback:(Printexc.print (fun _ -> self#on_tic; true))
let _ = window#connect#destroy ~callback:Main.quit
(* let _ = window#add_accel_group accel_group *)
let _ = self#fullscreen
let _ = window#show ()
let _ = atomic_unit self#load !file
let _ = Main.main ()

