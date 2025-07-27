open Types
open Graph
open Graph_type
open Messages

type state = env * graph

let state_of_string s =
  let l = Lexing.from_string s in
  let et = Parser.envterm Lexer.token l in
  envgraph et

let string_of_state =
  Format.asprintf "%a" (Graph.pp_envgraph Full)

let entry_of_state =
  Format.asprintf "%a" (Graph.pp_envgraph Term)

class virtual locate (arena: arena) =
  object(self)
    
    method virtual entry: string
    method virtual set_entry: string -> unit
    method virtual entry_warning: string -> unit
    method virtual help: string -> unit

    method private virtual read: string -> state
    method private virtual write: string -> state -> unit
    method private virtual export: string -> state -> unit
        
    val hist = History.create ("","")
    val mutable env = env []
    val mutable graph = Graph.emp
    val mutable mode = `Normal

    method private checkpoint =
      debug_msg "checkpoint" "%a" (pp_envgraph Term) (env,graph);
      History.save hist (string_of_state (env,graph),self#entry)

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      graph#draw arena#canvas;
      if rebox then arena#ensure graph#box;
      self#refresh

    method private refresh =
      (match mode with
       | `Select p ->
          temporary#polygon ~fill:(Gg.Color.gray ~a:0.2 0.) p;
          let p = Geometry.clockwise p in
          MSet.iter (fun (i,o) ->              
              let s,t = graph#ipos i, graph#opos o in
              Polygon.fold2 p (fun ij () ->
                  match Geometry.intersection ij (s,t) with
                  | Some(x,d) ->
                     let color = match d with
                       | R -> Constants.iport_color
                       | _ -> Constants.oport_color
                     in
                     temporary#point ~color x
                  | None -> ()
                ) ()
            ) graph#edges;
          MSet.iter (fun n ->
              if Geometry.mem_poly n#pos p then
                temporary#box ~fill:(Gg.Color.gray ~a:0.5 0.) n#box
            ) graph#nodes
       | _ -> ()
      );
      arena#refresh
    
    method private set_state ?rebox ?s (e,g) =
      env <- e;
      graph <- g;
      let s = match s with
        | Some s -> s
        | None -> entry_of_state (e,g)
      in
      self#set_entry s;
      self#redraw ?rebox ()
    
    method undo () =
      match History.undo hist with
      | Some (eg,s) -> self#set_state ~s (state_of_string eg)
      | None -> temporary#msg "no more undos"

    method redo () =
      match History.redo hist with
      | Some (eg,s) -> self#set_state ~s (state_of_string eg)
      | None -> temporary#msg "no more redos"

    method on_entry_changed =
      debug_msg "entry" "on entry changed with `%s'" self#entry;
      mode <- `Normal;
      match state_of_string self#entry with
      | e,g ->
         self#entry_warning "";
         if not (Graph.iso_envgraph (e,g) (env,graph)) then (
           temporary#msg "graph changed";
           env <- e;
           graph <- g;
           self#checkpoint;
           self#redraw ~rebox:true ()
         )
      | exception (Failure s) -> self#entry_warning s
      | exception Parser.Error -> self#entry_warning "Parsing error"
      | exception e -> self#entry_warning (Printexc.to_string e)

    method private drawing_changed =
      self#checkpoint;
      self#redraw()

    method private graph_changed =
      self#set_entry (entry_of_state (env,graph));
      self#drawing_changed
    
    method private catch =
      Graph.find graph arena#pointer

    method private remove =
      match self#catch with
      | `N n -> graph#rem_node n; self#graph_changed 
      | _ -> temporary#msg "no node to remove here"

    method private add_node f =
      try let l,n,m,_ = List.assoc f env in
          graph#add_node n m f (Info.pos arena#pointer l); self#graph_changed
      with Not_found -> error "unknown node name: %s" f
    
    method private unbox =
      match self#catch with
      | `N n -> graph#unbox n; self#graph_changed
      | _ -> temporary#msg "no node to unbox here"

    method private scale s =
      match self#catch with
      | `N n -> n#scale s; self#drawing_changed
      | `None -> graph#scale s; self#drawing_changed
      | _ -> temporary#msg "nothing to scale here"

    method private improve_placement =
      Place.improve_placement 0.05 graph; self#drawing_changed

    method private block b =
      let f = if b then Place.fix else Place.unfix in
      match self#catch with
      | `N n -> f n; self#checkpoint
      | _ -> temporary#msg "no node to %s here" (if b then "fix" else "release")

    method on_button_press =
      match mode with 
      | `Normal ->
         (match self#catch with
          | `N x -> mode <- `Move_node (x,Gg.V2.sub arena#pointer x#pos)
          | `I _ -> temporary#msg "not yet implemented"
          | `O _ -> temporary#msg "not yet implemented"
          | `None -> mode <- let p = arena#pointer in `Select(Polygon.start p)
         )
      | `New_node -> temporary#msg "aborted node creation"; mode <- `Normal
      | _ -> ()
    
    method on_button_release =
      match mode with
      | `Move_node _ ->
         mode <- `Normal; self#checkpoint
      | `Select p ->
         mode <- `Normal;
         Graph.create_box graph p; self#graph_changed
      | `Normal -> ()
      | `New_node -> ()

    method on_motion =
      match mode with
      | `Move_node (n,u) ->
         n#move (Gg.V2.sub arena#pointer u);
         self#redraw()
      | `Select p ->
         let q = arena#pointer in
         mode <-`Select(Polygon.extend p q);
         self#refresh         
      | _ -> self#refresh

    method on_key_press s =
      match mode with
      | `Normal | `Move_node _ ->
         (match s with
          | "h" -> self#help 
                     "** keys **
d:      remove node
n:      create node (give name afterward)
u:      unbox node
i:      improve placement
-/+:    shrink/enlarge element
f/F:    fix/Free element (for later placement optimisations)
->/<-:    undo/redo
r:      refresh picture
h:      print this help message"
          | "i" -> self#improve_placement
          | "n" -> temporary#msg "type node name"; mode <- `New_node
          | "f" -> self#block true
          | "F" -> self#block false
          | "d" -> self#remove
          | "u" -> self#unbox
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "r" -> self#redraw()
          | "ArrowLeft" -> self#undo()
          | "ArrowRight" -> self#redo()
          | "" -> ()
          | s -> temporary#msg "skipping key '%s'" s)
      | `New_node ->
         if s = "Escape" then (mode <- `Normal; temporary#msg "aborted node creation")
         else (mode <- `Normal; self#add_node s)
      | `Select _ ->
         mode <- `Normal; temporary#msg "aborted box creation"

    method init s =
      self#set_state ~rebox:true ~s (state_of_string s);
      self#checkpoint;
      History.clear hist

    method load_from file =
      self#set_state ~rebox:true (self#read file);
      self#checkpoint;
      History.clear hist

    method save_to file =
      self#write file (env,graph);
      self#export file (env,graph)
    
  end
