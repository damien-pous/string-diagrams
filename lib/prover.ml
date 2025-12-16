open Types
open Graph
open Graph_type
open Messages

let state_of_string s =
  let l = Lexing.from_string s in
  let x = Parser.equations Lexer.token l in
  Graph.equations x

let string_of_state =
  Format.asprintf "%a" (pp_equations Full)

class virtual mk (arena: arena) =
  object(self)

    method virtual help: string -> unit
    
    method private virtual read: string -> equations
    method private virtual write: string -> equations -> unit
        
    val hist = History.create ""
    val mutable state = env [], [], (Graph.emp,Graph.emp)
    val mutable mode = `Normal

    method private checkpoint =
      debug_msg "checkpoint" "%a" (pp_equations TermIfPossible) state;
      History.save hist (string_of_state state)      

    method private env = let (e,_,_) = state in e
    method private hyps = let (_,h,_) = state in h
    method private left = let (_,_,(l,_)) = state in l
    method private right = let (_,_,(_,r)) = state in r
    method private iter_graphs f =
      List.iter (fun (l,r) -> f l; f r) self#hyps;
      f self#left; f self#right
    method private fold_graphs f a =
      List.fold_right (fun (l,r) a -> f l (f r a)) self#hyps
      (f self#left (f self#right a))
      
    method private box =
      Gg.Box2.(
          List.fold_right (fun (l,r) -> union (union l#box r#box)) self#hyps
            (union self#left#box self#right#box))

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      self#iter_graphs (fun g -> g#draw arena#canvas);
      if rebox then arena#ensure self#box;
      self#refresh
    
    method private refresh =
      (match mode with
       | `Select (g,p) ->
          temporary#polygon ~fill:(Gg.Color.gray ~a:0.2 0.) p;
          let p = Geometry.clockwise p in
          MSet.iter (fun (i,o) ->              
              let s,t = g#ipos i, g#opos o in
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
            ) g#edges;
          MSet.iter (fun n ->
              if Geometry.mem_poly n#pos p then
                temporary#box ~fill:(Gg.Color.gray ~a:0.5 0.) n#box
            ) g#nodes
       | `Move_node _ -> ()
       | `Normal -> ();
      );
      arena#refresh
    
    method private set_state ?rebox s =
      state <- s;
      mode <- `Normal;
      self#redraw ?rebox ()

    method private abort =
      let s = History.present hist in
      self#set_state (state_of_string s)
    
    method undo () =
      match History.undo hist with
      | Some s -> self#set_state (state_of_string s)
      | None -> temporary#msg "no more undos"

    method redo () =
      match History.redo hist with
      | Some s -> self#set_state (state_of_string s)
      | None -> temporary#msg "no more redos"

    method private changed =
      self#checkpoint;
      self#redraw()
    
    method private catch_graph =
      self#fold_graphs (fun g -> function
          | None -> if Gg.Box2.mem arena#pointer g#box then Some g else None
          | x -> x) None      
      
    method private catch =
      match self#catch_graph with
      | None -> `None
      | Some g ->
         match Graph.find g arena#pointer with
         | `N n -> `N(g,n)
         | _ -> `G g
    
    method private unfold =
      match self#catch with
      | `N (g,n) ->
         (match n#kind with
          | Box _ -> g#unbox n; self#changed
          | Var(_,_,f) ->
             match List.assoc f self#env with
             | (_,_,_,Some g) -> g#subst n (Graph.copy self#env g); self#changed
             | _ -> temporary#msg "this box is atomic"
         )
      | _ -> temporary#msg "no node to unfold/unbox here"

    method private scale s =
      match self#catch with
      | `N (_,n) -> n#scale s; self#changed
      | `G g -> g#scale s; self#changed
      | _ -> temporary#msg "nothing to scale here"

    method private improve_placement s =
      self#iter_graphs (Place.improve_placement_depth s); self#changed

    method private block b =
      let f = if b then Place.fix else Place.unfix in
      match self#catch with
      | `N(_,n) -> f n; self#checkpoint
      | `G _ -> temporary#msg "graphs are always fixed"
      | _ -> temporary#msg "no node to %s here" (if b then "fix" else "release")

    method on_button_press ctrl =
      match mode with 
      | `Normal ->
         (match self#catch with
          | `N(_,x) -> mode <- `Move_node ((x:>area),Gg.V2.sub arena#pointer x#pos)
          | `G g when ctrl -> mode <- `Move_node ((g:>area),Gg.V2.sub arena#pointer g#pos)
          | `G g -> mode <- let p = arena#pointer in `Select(g,Polygon.start p)
          | `None -> ()
         )
      | _ -> ()
    
    method on_button_release =
      match mode with
      | `Move_node _ ->
         mode <- `Normal; self#checkpoint
      | `Select (g,p) ->
         mode <- `Normal;
         Graph.create_box g p; self#changed
      | `Normal -> ()

    method on_motion =
      match mode with
      | `Move_node (n,u) ->
         n#move (Gg.V2.sub arena#pointer u);
         self#redraw()
      | `Select (g,p) ->
         let q = arena#pointer in
         mode <-`Select(g,Polygon.extend p q);
         self#refresh         
      | _ -> self#refresh

    method on_key_press s =
      if s = "Escape" then self#abort
      else match mode with
      | `Normal | `Move_node _ ->
         (match s with
          | "h" -> self#help 
                     "** keys **
u:      unbox or unfold node
i/I:    improve placement
-/+:    shrink/enlarge element
f/F:    fix/Free element (for later placement optimisations)
->/<-:    undo/redo
ESC:    abort current action
r:      refresh picture
h:      print this help message"
          | "i" -> self#improve_placement 0.05
          | "I" -> self#improve_placement 0.2
          | "f" -> self#block true
          | "F" -> self#block false
          | "u" -> self#unfold
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "r" -> self#redraw()
          | "ArrowLeft" -> self#undo()
          | "ArrowRight" -> self#redo()
          | "" -> ()
          | s -> temporary#msg "skipping key '%s'" s)
      | _ -> temporary#msg "ignored key `%s' during ongoing action" s

    method init s =
      self#set_state ~rebox:true (state_of_string s);
      self#checkpoint;
      History.clear hist

    method load_from file =
      self#set_state ~rebox:true (self#read file);
      self#checkpoint;
      History.clear hist

    method save_to file =
      self#write file state
    
  end
