open Types
open Graph

type state = env * graph

let state_of_string s =
  let l = Lexing.from_string s in
  let et = Parser.envterm Lexer.token l in
  envgraph et

class virtual locate (arena: Types.arena) =
  object(self)
    
    method virtual entry: string
    method virtual set_entry: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual entry_warning: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual info: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual warning: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a
    method virtual help: 'a. ('a, Format.formatter, unit, unit) format4 -> 'a

    method private virtual read: string -> state
    method private virtual write: string -> state -> unit
    method private virtual export: string -> state -> unit
        
    val hist = History.create ""
    val mutable env = env []
    val mutable graph = Graph.emp
    val mutable active = `None
    val mutable mode = `Normal

    method private checkpoint =
      Format.kasprintf (fun s -> History.save hist s) "%a"
        (pp_envgraph Full) (env,graph)

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      graph#draw_on arena#canvas;
      if rebox then arena#ensure graph#box;
      arena#refresh

    method private display_graph_infos = 
    (* self#info "%t" pp_graph_infos *)
      self#info ""
    
    method private set_state ?rebox (e,g) =
      (* print_endline "set_graph"; *)
      env <- e;
      graph <- g;
      self#redraw ?rebox ();
      self#display_graph_infos;
      if (match state_of_string self#entry with
          | _,g' -> not (Graph.iso g g')
          | exception _ -> true)
      then
        self#set_entry "%a" (pp_envgraph Term) (e,g)

    method undo () =
      match History.undo hist with
      | Some s -> self#set_state (state_of_string s)
      | None -> self#info "no more undos"

    method redo () =
      match History.redo hist with
      | Some s -> self#set_state (state_of_string s)
      | None -> self#info "no more redos"

    method private on_graph f =
      self#set_state (env,f graph);
      if mode = `Normal then self#checkpoint

    method on_entry_changed =
      active <- `None;
      match state_of_string self#entry with
      | e,g ->
         if not (Graph.iso g graph) then (
           (* print_endline "text_changed.really"; *)
           env <- e;
           graph <- g;
           active <- `None;
           self#redraw ~rebox:true ();
           self#display_graph_infos;
           self#checkpoint)
         else
           self#display_graph_infos
      | exception (Failure s) -> self#entry_warning "%s" s
      | exception Parser.Error -> self#entry_warning "Parsing error"
      | exception e -> self#entry_warning "%s" (Printexc.to_string e)

    method private catch =
      graph#find arena#pointer

    (* method private ivertex = *)
    (*   let v = Info.positionned_ivertex arena#pointer in *)
    (*   self#on_graph (Graph.add_ivertex v); *)
    (*   v *)

    (* method private lift = *)
    (*   self#on_graph (Graph.lft (Info.positionned_source (Graph.arity graph+1) arena#pointer)) *)

    method private remove =
      match self#catch with
      | `N n -> graph#rem_node n; self#checkpoint; self#redraw()
      | _ -> ()

    method private unbox =
      match self#catch with
      | `N n -> graph#unbox n; self#checkpoint; self#redraw()
      | _ -> ()
    
    (* method private promote = *)
    (*   match self#catch with *)
    (*   | `V (Inn v) -> self#on_graph (Graph.promote v) *)
    (*   | `V (Src _) -> self#general_warning "cannot promote a source" *)
    (*   | `E _ -> self#general_warning "cannot promote an edge" *)
    (*   | `None -> () *)

    (* method private forget = *)
    (*   match self#catch with *)
    (*   | `V (Src i) -> self#on_graph (Graph.forget i) *)
    (*   | `V (Inn _) -> self#general_warning "cannot forget an inner vertex (use r to remove it)" *)
    (*   | `E _ -> self#general_warning "cannot forget an edge (use r to remove it)" *)
    (*   | `None -> () *)

    (* method private edge l s = *)
    (*   let e = Info.positionned_edge (Seq.size l) s in *)
    (*   let e,g = Graph.add_edge e l graph in *)
    (*   Place.center_edge g e; *)
    (*   self#set_graph g; *)
    (*   self#checkpoint *)

    method private scale s =
      match self#catch with
      | `N n -> n#scale s; self#checkpoint; self#redraw()
      | `None -> graph#scale s; self#checkpoint; self#redraw()
      | _ -> ()

    method private improve_placement =
      Place.improve_placement 0.05 graph; self#checkpoint; self#redraw()

    method private block b =
      let f = if b then Place.fix else Place.unfix in
      match self#catch with
      | `N n -> f n; self#checkpoint
      | _ -> ()

    method on_button_press =
      match mode, self#catch with
      | `Normal, `N x -> active <- `N (x,Gg.V2.sub arena#pointer x#pos)
      (* | `InsertEdge l, `V v -> mode <- `InsertEdge (Seq.snoc l v) *)
      (* | `InsertEdge l, `N -> *)
      (*    mode <- `InsertEdge (Seq.snoc l (Inn self#ivertex)) *)
      | _ -> ()
    
    method on_button_release =
      (match active with `N _ -> self#checkpoint | `None -> ());
      active <- `None

    method on_motion =
      match active with
      | `N (n,u) ->
         n#move (Gg.V2.sub arena#pointer u);
         self#redraw()
      | `None -> ()    

    method on_key_press s =
      match mode with
      | `Normal ->
         (match s with
          | "h" -> self#help 
                     "** keys **
d:      remove node
u:      unbox node
i:      improve placement
-/+:    shrink/enlarge element
f/F:    fix/Free element (for later placement optimisations)
Z/R:    undo/redo
r:      refresh picture
h:      print this help message"
          | "i" -> self#improve_placement
          | "f" -> self#block true
          | "F" -> self#block false
          | "d" -> self#remove
          | "u" -> self#unbox
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "r" -> arena#refresh
          | "Z" -> self#undo()
          | "R" -> self#redo()
          | s -> self#warning "skipping key '%s'@." s)

    method private on_term f =
      match state_of_string self#entry with
      | t -> self#set_entry "%a" (pp_envgraph Term) (f t)
      | exception _ -> self#entry_warning "current term is not valid"

    method private init_from s =
      self#set_state ~rebox:true s;
      self#checkpoint;
      History.clear hist

    method load_from file = self#init_from (self#read file)

    method init s = self#init_from (state_of_string s)      

    method save_to file =
      self#write file (env,graph);
      self#export file (env,graph)

  end
