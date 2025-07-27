open Diagrams
open Diagrams_cairo
open Diagrams_gtk

open GMain

let width = 600
let height = 600

let file = ref
             (match Sys.argv with
              | [|_|] -> "default"
              | [|_;file|] when File.exists file -> file
              | _ -> Format.eprintf "usage: hgui [file]\n"; exit 1)

let _ = GtkMain.Main.init ()
let window = GWindow.window ~title:!file ~position:`CENTER ()
let vbox = GPack.vbox ~homogeneous:false ~packing:window#add ()

let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory menubar
let accel_group = factory#accel_group
let file_factory = new GMenu.factory (factory#add_submenu "File") ~accel_group
let edit_factory = new GMenu.factory (factory#add_submenu "Edit") ~accel_group
let view_factory = new GMenu.factory (factory#add_submenu "View") ~accel_group

let paned = GPack.paned `HORIZONTAL ~packing:(vbox#pack ~expand:true) ()
let vbox1 = GPack.vbox ~homogeneous:false ~packing:paned#pack1 ()
let vbox2 = GPack.vbox ~homogeneous:false ~packing:paned#pack2 ()


let da = GMisc.drawing_area ~width ~height ~packing:(vbox1#pack ~expand:true) ()
let arena = GArena.create ~width ~height ~window da ()
let general_msg = GText.view ~editable:false ~cursor_visible:false ~packing:(vbox1#pack) ()

let entry = GText.view ~editable:true ~accepts_tab:false ~packing:(vbox2#pack ~expand:true) ()
let entry_msg = GMisc.label ~packing:(vbox2#pack) ()

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
  fun k () ->
  if dlg#run() = stock' then (
    match dlg#filename with
    | Some f ->
       let f = Filename.chop_extension f in
       window#set_title (Filename.basename f);
       file := f;
       k f
    | None -> ()
  ); dlg#misc#hide()

class glocate =
  object
    inherit Locate.locate arena as parent
    val mutable blocked_entry = false
    method entry = entry#buffer#get_text()
    method set_entry s =
      blocked_entry <- true;
      Messages.debug_msg "entry" "set_entry to `%s'" s;
      entry#buffer#set_text s;
      blocked_entry <- false;
    method! on_entry_changed =
      if not blocked_entry then parent#on_entry_changed
    method entry_warning = entry_msg#set_text
    method help = print_endline
    method private read = File.read
    method private write = File.write
    method private export = File.export
  end
let self = new glocate


let load =
  dialog "Open graph file" `OPEN `OPEN `OPEN 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ())
    self#load_from

let save () =
  self#save_to !file

let save_as =
  dialog "Save graphs as" `SAVE `SAVE `SAVE 
    (GFile.filter ~name: "HG file" ~patterns:["*.hg"] ())
    self#save_to

let on_button_press ev =
  let state = GdkEvent.Button.state ev in
  not (Gdk.Convert.test_modifier `CONTROL state) &&
    (self#on_button_press; true)

let on_motion _ =
  self#on_motion; true

let on_button_release _ =
  self#on_button_release; true

let on_entry_changed _ =
  self#on_entry_changed

let on_key_press ev =
  (if List.mem (GdkEvent.Key.keyval ev) [GdkKeysyms._Tab]
   then entry#misc#grab_focus()
   else if not (List.mem (GdkEvent.Key.keyval ev) [GdkKeysyms._Control_L; GdkKeysyms._Control_R]) then
     let s =
       let kv = GdkEvent.Key.keyval ev in
       if kv = GdkKeysyms._Left then "ArrowLeft"
       else if kv = GdkKeysyms._Right then "ArrowRight"
       else if kv = GdkKeysyms._Escape then "Escape"
       else GdkEvent.Key.string ev
     in
     self#on_key_press s
  );
  true

let fullscreen =
  let fs = ref false in
  fun _ ->
  if !fs then window#unfullscreen() else window#fullscreen();
  fs := not !fs    

(* TODO: capture this in locate *)
let refresh() = arena#refresh; general_msg#buffer#set_text Messages.temporary#messages
let atomic f x d =
  Messages.temporary#clear;  
  Messages.catch f x d refresh
let atomic_unit f x = atomic f x ()
let atomic_true f x = atomic f x true

let _ = file_factory#add_item "Open" ~key:GdkKeysyms._O ~callback:(atomic_unit load)
let _ = file_factory#add_item "Save" ~key:GdkKeysyms._S ~callback:(atomic_unit save)
let _ = file_factory#add_item "Save as" ~key:GdkKeysyms._E ~callback:(atomic_unit save_as)
let _ = file_factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:Main.quit
let _ = edit_factory#add_item "Undo" ~key:GdkKeysyms._Z ~callback:(atomic_unit self#undo)
let _ = edit_factory#add_item "Redo" ~key:GdkKeysyms._R ~callback:(atomic_unit self#redo)
let _ = view_factory#add_item "Fullscreen" ~key:GdkKeysyms._F ~callback:fullscreen

let _ = GtkBase.Widget.add_events da#as_widget
          [ `KEY_PRESS; `POINTER_MOTION; `BUTTON_PRESS; `BUTTON_RELEASE ]
let _ = da#misc#set_can_focus true
let _ = da#event#connect#motion_notify ~callback:(atomic_true on_motion)
let _ = da#event#connect#button_press ~callback:(atomic_true on_button_press)
let _ = da#event#connect#button_release ~callback:(atomic_true on_button_release)
let _ = da#event#connect#key_press ~callback:(atomic_true on_key_press)
let _ = entry#buffer#connect#changed ~callback:(atomic_unit on_entry_changed)
let _ = window#connect#destroy ~callback:Main.quit
let _ = window#add_accel_group accel_group
let _ = atomic_unit self#load_from !file

let _ = window#show ()
let _ = Main.main ()

