open Diagrams
open Messages
open Js_of_ocaml
open Gg
open Vg


let initial_term =
  "m: M⊗M -> M
n: N⊗N -> N
x: N⊗M -> M⊗N
mn: M⊗N⊗M⊗N -> M⊗N := M·x·N ; m·n
mA: m·M ; m ≡ M·m ; m
nA: n·N ; n ≡ N·n ; n
mx: N·m ; x ≡ x·M ; M·x ; m·N
nx: n·M ; x ≡ N·x ; x·N ; M·n
------
M·x·N⊗M⊗N ; M⊗M·n·M⊗N ; m·x·N ; m·n ≡ M⊗N⊗M·x·N ; M⊗N·m·N⊗N ; M·x·n ; m·n"

module Html = Dom_html
	
let app p mk =
  let e = mk Html.document in
  Dom.appendChild p e;
  e

let println par s =
  Dom.appendChild par (Html.document##createTextNode (Js.string s));
  ignore(app par Html.createBr)

let clear par =
  let rec aux () =
    match Js.Opt.to_option par##.firstChild with
      | Some c -> Dom.removeChild par c; aux()
      | None -> ()
  in aux ()

let _print par s =
  clear par;
  let l = String.split_on_char '\n' s in
  List.iter (println par) l

let set_text t s =
  t##.value := Js.string s;
  t##.rows := String.fold_right (fun c n -> if c='\n' then n+1 else n) s 1

let get s = 
  Js.Opt.get (Html.document##getElementById(Js.string s))
    (fun () -> assert false)

let add_listener elt evt f =
  ignore(Html.addEventListener elt evt (Html.handler f) Js._true)
  

class arena (canvasdiv: Html.divElement Js.t) (canvas: Html.canvasElement Js.t) =
  object(self)
    inherit Arena.generic
    
    method private dsize =
      float_of_int canvasdiv##.clientWidth,
      float_of_int canvasdiv##.clientHeight

    val mutable dpointer = (0,0)
    method private dpointer =
      let x,y = dpointer in
      float_of_int x,
      float_of_int y
    
    method! refresh =
      (* let w',h' = canvasdiv##.clientWidth, canvasdiv##.clientHeight in *)
      (* Format.eprintf "A: (%i,%i), (%i,%i)@." *)
      (*   canvasdiv##.clientWidth canvasdiv##.clientHeight *)
      (*   canvas##.clientWidth canvas##.clientHeight; *)
      canvas##.width := canvasdiv##.clientWidth;
      canvas##.height := canvasdiv##.clientHeight - 4;
      (* Format.eprintf "B: (%i,%i), (%i,%i)@." *)
      (*   canvasdiv##.clientWidth canvasdiv##.clientHeight *)
      (*   canvas##.clientWidth canvas##.clientHeight; *)
      let w,h = self#dsize in
      let size = V2.(v w h) in
      let vgr = Vgr.create (Vgr_htmlc.target ~resize:false canvas) `Other in 
      let image = I.blend self#canvas#get (I.const Color.white) in
      let image = I.blend Messages.temporary#get image in
      ignore (Vgr.render vgr (`Image (size, self#view, image)));
      ignore (Vgr.render vgr `End);
      (* Format.eprintf "C: (%i,%i), (%i,%i)@." *)
      (*   canvasdiv##.clientWidth canvasdiv##.clientHeight *)
      (*   canvas##.clientWidth canvas##.clientHeight; *)
      (* canvas##.width := w'; *)
      (* canvas##.height := h'; *)

    method private scroll ev =
      if Js.to_bool ev##.ctrlKey then
        ((if ev##.wheelDelta > 0 then self#zoom 0.95
          else self#zoom 1.05); Js._false)
      else Js._true

    val mutable mode = None
    method private mouseup ev =
      if Js.to_bool ev##.ctrlKey then
        (mode <- None; Js._false)
      else Js._true
    method private mousedown ev =
      if Js.to_bool ev##.ctrlKey then
        (mode <- Some dpointer; Js._false)
      else Js._true
    method private mousemove (ev: Html.mouseEvent Js.t)  =
      dpointer <- int_of_float (Js.float_of_number ev##.offsetX),
                  int_of_float (Js.float_of_number ev##.offsetY); 
      match Js.to_bool ev##.ctrlKey, mode with
      | true,Some(x0,y0) ->
         let x,y = dpointer in
         self#move (float_of_int (x0-x), float_of_int (y0-y)); mode <- Some dpointer;
         Js._false
      | _ -> Js._true

    val mutable dsize = (0,0)
    method private checksize _ =
      let s = canvasdiv##.clientWidth, canvasdiv##.clientHeight in
      if s <> dsize then (dsize <- s; self#refresh);
      Js._true      
    
    initializer
      canvas##.onwheel := Html.handler self#scroll;
      add_listener canvas Html.Event.mousedown self#mousedown;
      add_listener canvas Html.Event.mousemove self#mousemove;
      add_listener canvas Html.Event.mouseup self#mouseup;
      add_listener canvasdiv Html.Event.mousemove self#checksize;
      ()

  end

let onload _ =
  let canvasdiv = get "canvas" in
  let canvas = app canvasdiv Html.createCanvas in
  let entry = app (get "entry") (Html.createTextarea ~name:(Js.string "entrytext")) in
  entry##.style##.width := Js.string "99.7%";
  (* entry##.style##.clip := Js.string "vertical"; *)
  let messages = app (get "messages") (Html.createTextarea ~name:(Js.string "messagestext")) in
  messages##.rows := 1;
  messages##.style##.width := Js.string "100%";
  messages##.style##.border := Js.string "none";
  (* messages##.style##setProperty (Js.string "resize") (Js.string "none"); *)
  Messages.set_output (set_text messages) (fun () -> set_text messages "");
  let arena = new arena canvasdiv canvas in
  let examples = get "examples" in
  let ui: _ Types.ui_io =
    object
      val clipboard = Brr.(Brr_io.Clipboard.of_navigator G.navigator)
      method clipboard =
        user_error "cannot read clipboard from the applet, please paste directly in the text box above"
        (* Fut.await (Brr_io.Clipboard.read_text clipboard) *)
        (*   (function Ok v -> k (Jstr.to_string v) *)
        (*           | Error _ -> print_endline "error when retrieving clipboard") *)
      method set_clipboard s =
        ignore(Brr_io.Clipboard.write_text clipboard (Jstr.v s))

      method file = abort "no file access from the applet"
      method read = abort "no file access from the applet (read)"
      method write _ = abort "no file access from the applet (write)"
      method write_pdf _ = abort "no file access from the applet (write_pdf)"
      method write_svg _ = abort "no file access from the applet (write_svg)"
      method set_file _ = abort "no file access from the applet (set_file)"
      method open_dialog _ = abort "no file access from the applet (open_dialog)"
      method saveas_dialog _ = abort "no file access from the applet (saveas_dialog)"
      method quit = abort "cannot quit the applet"

      method fullscreen =
        ignore Brr.(    
          Fut.of_promise ~ok:ignore @@
            Jv.call (El.to_jv (Document.body G.document)) "requestFullscreen" [||])

      method entry = Js.to_string (entry##.value)
      method set_entry = set_text entry 

      val mutable on_entry_changed = fun () -> ()
      method set_on_entry_changed k = on_entry_changed <- k
      method on_entry_changed = on_entry_changed
    end
  in
  let self = Program.create arena ui in
  let () = List.iter (fun (n,e,g) ->
               let ex = app examples Html.createLi in
               Dom.appendChild ex (Html.document##createTextNode (Js.string n));
               add_listener ex Html.Event.click (fun _ -> self#load_string (e^"\n------\n"^g); Js._true)
             ) Examples.list
  in
  
  let onkeypress ev =
    (* return true to leave the key to the browser *)
    if Brr.El.has_focus (Brr.El.of_jv (Jv.Id.to_jv entry)) (* safer cast? *)
       || Js.to_bool ev##.altKey then Js._true
    else try 
        let s = Js.to_string (Js.Optdef.get ev##.key (fun _ -> raise Program.Skip_key)) in
        let ctrl = Js.to_bool ev##.ctrlKey in
        let shft = Js.to_bool ev##.shiftKey in
        self#on_key_press ~ctrl ~shft s; Js._false
      with Program.Skip_key -> Js._true
  in
  let onkeyup _ev =
    ui#on_entry_changed();
    Js._true
  in
  let onbuttonpress ev =
    let ctrl = Js.to_bool ev##.ctrlKey in
    let shft = Js.to_bool ev##.shiftKey in
    self#on_button_press ~ctrl ~shft
  in
  let catch ?(keep=false) f ev =
    if keep then Messages.temporary#clear else Messages.clear();  
    Messages.catch f ev Js._false (fun () -> arena#refresh)
  in
  let catch' ?keep f ev =
    if not (Js.to_bool ev##.ctrlKey) then
      (catch ?keep (fun ev -> f ev; Js._false) ev)
    else Js._true
  in
  
  add_listener Html.window Html.Event.keydown (catch onkeypress);
  add_listener entry Html.Event.keyup (catch onkeyup);
  (* for mouse events, ctrl-ones are already caught by the arena *)
  add_listener canvas Html.Event.mousedown (catch' onbuttonpress);
  add_listener canvas Html.Event.mousemove (catch' ~keep:true (fun _ -> self#on_motion));
  add_listener canvas Html.Event.mouseup (catch' (fun _ -> self#on_button_release));      
  ignore (Html.window##setInterval
            (Js.wrap_callback (Printexc.print (fun _ -> self#on_tic)))
            (Js.float 25.));
  self#load_string initial_term;
  Js._false

let _ =
  Html.window##.onload := Html.handler onload;


