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
  Format.asprintf "%a" (Graph.pp_envgraph TermIfPossible)

class virtual mk (arena: arena) =
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
    val mutable graph = Graph.emp()
    val mutable mode = `Normal

    method private checkpoint =
      debug_msg "checkpoint" "%a" (pp_envgraph TermIfPossible) (env,graph);
      History.save hist (string_of_state (env,graph),self#entry)      

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      graph#draw arena#canvas;
      if rebox then arena#ensure graph#box;
      self#refresh

    method private highlight_iport i =
      temporary#circle ~fill:(Graph.icolor graph i)
        {center = graph#ipos i; radius = 1.5*.Constants.pradius}
    method private highlight_oport o =
      temporary#circle ~fill:(Graph.ocolor graph o)
        {center = graph#opos o; radius = 1.5*.Constants.pradius}
    
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
       | `New_node ->
          temporary#box ~color:Constants.red (Gg.Box2.v_mid arena#pointer (Constants.empty_size 0 0))
       | `New_edge_i i ->
          let p = arena#pointer in
          let q = graph#ipos i in
          let color = Graph.icolor graph i in
          if Geometry.mem_point p q then
            temporary#point q;
          Graph.iter_oports graph
            (fun o -> if graph#ofree o && Typ.eq1 (graph#ityp i) (graph#otyp o) then (
                        let q = graph#opos o in
                        temporary#point ~color q;
                        if Geometry.mem_point p q then
                          self#highlight_oport o
                      )
            );
          temporary#segment ~color q p;
       | `New_edge_o o ->
          let p = arena#pointer in
          let q = graph#opos o in
          let color = Graph.ocolor graph o in
          if Geometry.mem_point p q then
            temporary#point q;
          Graph.iter_iports graph
            (fun i -> if graph#ifree i && Typ.eq1 (graph#ityp i) (graph#otyp o)  then (
                        let q = graph#ipos i in
                        temporary#point ~color q;
                        if Geometry.mem_point p q then
                          self#highlight_iport i
                      )
            );
          temporary#segment ~color p q
       | `Move_node _ -> ()
       | `Normal -> ();
          match self#catch with
          | `I i -> self#highlight_iport i
          | `O o -> self#highlight_oport o
          | `N _ | `None -> ()
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
      mode <- `Normal;
      self#redraw ?rebox ()

    method private abort =
      let (eg,s) = History.present hist in
      self#set_state ~s (state_of_string eg)
    
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
      Graph.find_ports graph arena#pointer

    method private remove =
      match self#catch with
      | `N n -> graph#rem_node n; self#graph_changed 
      | _ -> temporary#msg "no node to remove here"

    method private add_node f =
      try let l,n,m,_ = List.assoc f env in
          graph#add_node (Graph.var_node n m f (Element.pos arena#pointer l)); self#graph_changed
      with Not_found -> error "unknown node name: %s" f
    
    method private unfold =
      match self#catch with
      | `N n ->
         (match n#kind with
          | Box _ -> graph#unbox n; self#graph_changed
          | Var f ->
             match List.assoc f env with
             | (_,_,_,Some g) -> graph#subst n (Graph.copy g); self#graph_changed
             | _ -> temporary#msg "this box is atomic"
         )
      | _ -> temporary#msg "no node to unfold/unbox here"

    method private scale s =
      match self#catch with
      | `N n -> n#scale s; self#drawing_changed
      | `None -> graph#scale s; self#drawing_changed
      | _ -> temporary#msg "nothing to scale here"

    method private improve_placement =
      if not (graph#improve ~force:true) then self#drawing_changed

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
          | `I i -> (match graph#next_opt i with
                     | None -> mode <- `New_edge_i i
                     | Some o -> graph#rem_edge (i,o); mode <- `New_edge_o o; self#redraw())
          | `O o -> (match graph#prev_opt o with
                     | None -> mode <- `New_edge_o o
                     | Some i -> graph#rem_edge (i,o); mode <- `New_edge_i i; self#redraw())
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
         ignore(Graph.create_box graph p); self#graph_changed
      | `New_edge_i i ->
         (match self#catch with
          | `O o when graph#ofree o && Typ.eq1 (graph#ityp i) (graph#otyp o) ->
             mode <- `Normal; graph#add_edge (i,o); self#graph_changed
          | _ -> mode <- `Normal; self#graph_changed)
      | `New_edge_o o ->
         (match self#catch with
          | `I i when graph#ifree i && Typ.eq1 (graph#ityp i) (graph#otyp o) ->
             mode <- `Normal; graph#add_edge (i,o); self#graph_changed
          | _ -> mode <- `Normal; self#graph_changed)
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
      if s = "Escape" then self#abort
      else match mode with
      | `Normal | `Move_node _ ->
         (match s with
          | "h" -> self#help 
                     "** keys **
d:      remove node
n:      create node (give name afterward)
u:      unbox or unfold node
i:      improve placement
-/+:    shrink/enlarge element
f/F:    fix/Free element (for later placement optimisations)
->/<-:    undo/redo
ESC:    abort current action
=       fit screen
r:      refresh picture
h:      print this help message"
          | "i" -> self#improve_placement
          | "n" -> temporary#msg "type node name"; mode <- `New_node; self#refresh
          | "f" -> self#block true
          | "F" -> self#block false
          | "d" -> self#remove
          | "u" -> self#unfold
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "=" -> arena#fit graph#box; self#refresh
          | "r" -> self#redraw()
          | "ArrowLeft" -> self#undo()
          | "ArrowRight" -> self#redo()
          | "" -> ()
          | s -> temporary#msg "skipping key '%s'" s)
      | `New_node -> mode <- `Normal; self#add_node s
      | _ -> temporary#msg "ignored key `%s' during ongoing action" s

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
