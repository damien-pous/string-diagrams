open Diagrams
open Diagrams_cairo
open Types
open Graph_type

let create ~(window:#GWindow.window) ~(entry:#GText.view) ~(messages:#GText.view) ~file (): state ui_io =
  let file = ref file in
  let set_file f = window#set_title (Filename.basename f); file := f in
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
                   | Some f -> set_file (Filename.chop_extension f); k ()
                   | None -> ()
               )) () () dlg#misc#hide
  in
  let open_dialog =
    dialog "Open diagram file" `OPEN `OPEN `OPEN 
      (GFile.filter ~name: "SD file" ~patterns:["*.sd"] ())
  in
  let saveas_dialog=
    dialog "Save diagram file as" `SAVE `SAVE `SAVE 
      (GFile.filter ~name: "SD file" ~patterns:["*.sd"] ())
  in
  
  object(self)
    inherit File.io
        
    val clipboard = GData.clipboard Gdk.Atom.clipboard
    method clipboard = Option.value clipboard#text ~default:"" 
    method set_clipboard = clipboard#set_text

    method file = !file
    method set_file = set_file
    method open_dialog = open_dialog 
    method saveas_dialog = saveas_dialog 

    method quit = GMain.Main.quit()

    val mutable fs = false
    method fullscreen =
      if fs then window#unfullscreen() else window#fullscreen();
      fs <- not fs
    
    val mutable blocked_entry = false
    method entry = entry#buffer#get_text()
    method set_entry s =
      blocked_entry <- true;
      Messages.debug_msg "entry" "set_entry to `%s'" s;
      entry#buffer#set_text s;
      entry#scroll_mark_onscreen (`NAME "end");
      blocked_entry <- false;
    val mutable on_entry_changed = fun () -> ()
    method set_on_entry_changed k = on_entry_changed <- k
    method on_entry_changed () =
      if not blocked_entry then (Messages.clear(); on_entry_changed ()) 

    method private add_message s =      
      let s = if true then s else String.concat "\n" [messages#buffer#get_text(); s] in
      messages#buffer#set_text s
    method private clear_messages () =
      messages#buffer#set_text ""

    initializer
    let _ = entry#buffer#create_mark ~name:"end" ~left_gravity:false entry#buffer#end_iter in
    Messages.set_output self#add_message self#clear_messages;
    ()
      
  end
