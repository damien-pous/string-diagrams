open Types
open Graph
open Graph_type
open Messages

let state_of_string s =
  (Marshal.from_string s 0: equations)
  (* let l = Lexing.from_string s in *)
  (* let x = Parser.equations Lexer.token l in *)
  (* Graph.equations x *)

let string_of_state (s: equations) =
  Marshal.to_string s [Marshal.Closures] 
  (* Format.asprintf "%a" (pp_equations Full) *)

let changed = ref false

class virtual mk (arena: arena) =
  object(self)

    inherit [equations]History.mk string_of_state state_of_string as parent
    method private virtual help: string -> unit
        
    val mutable state = env [], [], (Graph.emp(),Graph.emp())
    val mutable mode = `Normal

    method private state = state    
    method private env = let (e,_,_) = state in e
    method private hyps = let (_,h,_) = state in h
    method private lhs = let (_,_,(l,_)) = state in l
    method private rhs = let (_,_,(_,r)) = state in r
    method private iter_graphs f =
      List.iter (fun (l,r) -> f l; f r) self#hyps;
      f self#lhs; f self#rhs
    method private fold_graphs: 'a. (graph -> 'a -> 'a) -> 'a -> 'a = fun f a ->
      List.fold_right (fun (l,r) a -> f l (f r a)) self#hyps
      (f self#lhs (f self#rhs a))
      
    method private box =
      Gg.Box2.(
          List.fold_right (fun (l,r) -> union (union l#box r#box)) self#hyps
            (union self#lhs#box self#rhs#box))

    method private redraw =
      arena#canvas#clear;
      self#iter_graphs (fun g -> g#draw arena#canvas);
      List.iter (fun (l,r) -> arena#canvas#text (Gg.P2.mid l#pos r#pos) "=") self#hyps;
      arena#canvas#text (Gg.P2.mid self#lhs#pos self#rhs#pos)
        (if self#lhs#get "fill" = None then "=?=" else "=");
      self#refresh

    method !load' f =
      (* not optimal: parent#load already calls #redraw, via #set_state *)
      parent#load' f;
      arena#fit self#box;
      self#redraw
    
    method private refresh =
      (match mode with
       | `Select (g,p) ->
          temporary#polygon ~fill:(Constants.color "tgray") p;
          if false then 
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
    
    method private set_state s =
      state <- s;
      mode <- `Normal;
      self#redraw

    method private changed_nocheckpoint =
      if Graph.iso self#lhs self#rhs then
        (self#lhs#set "fill" "done"; self#rhs#set "fill" "done")
      else
        (self#lhs#unset "fill"; self#rhs#unset "fill");        
      self#perturbate;
    (* note that #perturbate always calls #redraw *)
      
    method private changed =
      self#changed_nocheckpoint;
      self#checkpoint;
      
    method private catch =
      let p = arena#pointer in
      let g =
        self#fold_graphs (fun g -> function
            | None -> if g#contains p then Some g else None
            | x -> x) None
      in
      match g with
      | None -> `None
      | Some g ->
         match Graph.find g p with
         | `N n -> `N(g,n)
         | _ -> `G g
    
    method private unfold =
      match self#catch with
      | `N (g,n) ->
         (match n#kind with
          | Box _ ->
             g#unbox n;
             List.iter (fun (l,r) -> l#unset "fill"; r#unset "fill") self#hyps;
             self#changed
          | Var f ->
             match List.assoc f self#env with
             | (_,_,_,Some h) -> g#subst n (Graph.copy h); self#changed
             | _ -> temporary#msg "this box is atomic"
         )
      | _ -> temporary#msg "no node to unfold/unbox here"

    method private scale s =
      match self#catch with
      | `N (_,n) -> n#scale s; self#changed
      | `G g -> g#scale s; self#changed
      | _ -> temporary#msg "nothing to scale here"

    method private improve_placement force =      
      if not (self#fold_graphs (fun g s -> g#improve ~force && s) true) || force then        
        self#redraw
    method private perturbate =
      self#improve_placement true
    
    method private release =
      match self#catch with
      | `N(_,n) -> Place.unfix n; self#perturbate
      | _ -> temporary#msg "no node to release here"

    method private create_box g p: unit =
      let n,h = Graph.create_box g p in
      g#set "place" "locked";
      List.iter (fun (l,r) ->
          if Graph.iso h l then
            (h#set "fill" "lhs"; l#set "fill" "lhs"; r#set "fill" "rhs")
          else if Graph.iso h r then
            (h#set "fill" "rhs"; l#set "fill" "lhs"; r#set "fill" "rhs")
        ) self#hyps;
      if h#get "fill" = Some "tgray" then h#set "place" "locked"
      else (
        h#set "place" "contract";
        let hyps = self#hyps in (* needed to leave [self] out of the closure below *)
        h#on_stabilize
          (fun () ->
            (try
               List.iter (fun (l,r) ->
                   if Graph.iso h l then
                     (h#replace (Graph.copy r);
                      h#unset "place";
                      h#on_stabilize (fun () ->
                          g#unbox n;
                          g#unset "place";
                          l#unset "fill";
                          r#unset "fill";
                          (* here we would like to call
                             self#changed_nocheckpoint;
                             but Marshal cannot deal with references to self *)
                          changed := true;
                          false);
                      h#set "fill" "rhs";
                      Place.group h; 
                      raise Not_found)
                   else if Graph.iso h r then
                     (h#replace (Graph.copy l);
                      h#unset "place";
                      h#on_stabilize (fun () ->
                          g#unbox n;
                          g#unset "place";
                          l#unset "fill";
                          r#unset "fill";
                          (* here we would like to call
                             self#changed_nocheckpoint;
                             but Marshal cannot deal with references to self *)
                          changed := true;
                          false);
                      h#set "fill" "lhs";
                      Place.group h; 
                      raise Not_found)
                 ) hyps;
               false
             with Not_found -> false);
        );
      );
      self#changed

    method private rewrite unbox =
      match self#catch with
      | `N(g,n) ->
         (match n#kind with
          | Box h -> 
             if h#get "fill" = Some "tgray" then error "no match so far";
             (try
             List.iter (fun (l,r) ->
                 if Graph.iso h l then
                   (if unbox then
                      (g#subst n (Graph.copy r); l#unset "fill"; r#unset "fill")
                    else
                      (h#replace (Graph.copy r); h#set "fill" "rhs");
                    raise Not_found)
                 else if Graph.iso h r then
                   (if unbox then
                      (g#subst n (Graph.copy l); l#unset "fill"; r#unset "fill")
                    else
                      (h#replace (Graph.copy l); h#set "fill" "lhs");
                    raise Not_found)
               ) self#hyps
             with Not_found -> ());
             self#changed
          | _ -> temporary#msg "no box to rewrite here")
      | _ -> temporary#msg "no box to rewrite here"

    val mutable play = true
    method on_tic =
      if play then
        match mode with
        | `Normal | `Move_node _ ->
           self#improve_placement false;
           if !changed then (changed := false; self#changed_nocheckpoint)           
        | `Select _ -> ()

    method on_button_press ctrl =
      match mode with 
      | `Normal ->
         (match self#catch with
          | `N(g,x) -> Place.fix x; mode <- `Move_node (g,(x:>element),Gg.V2.sub arena#pointer x#pos)
          | `G g when ctrl -> mode <- `Move_node (g,(g:>element),Gg.V2.sub arena#pointer g#pos)
          | `G g -> mode <- let p = arena#pointer in `Select(g,Polygon.start p)
          | `None -> ()
         )
      | _ -> ()
    
    method on_button_release =
      match mode with
      | `Move_node _ ->
         mode <- `Normal; self#checkpoint
      | `Select (g,p) ->
         mode <- `Normal; self#create_box g p
      | `Normal -> ()

    method on_motion =
      match mode with
      | `Move_node (_,n,u) ->
         n#move (Gg.V2.sub arena#pointer u);
         self#perturbate
      | `Select (g,p) ->
         let q = arena#pointer in
         mode <-`Select(g,Polygon.extend p q);
         self#refresh         
      | _ -> ();
         (* (match self#catch with `N(g,n) -> temporary#msg "depth %i" (g#depth n) | _ -> ()); *)
         (* self#refresh *)

    method private load_from_clipboard =
      let l = Lexing.from_string arena#clipboard in
      let x = Parser.equations Lexer.token l in
      self#load' (Graph.equations x)

    method private graph_to_clipboard =
      match self#catch with
      | `N(g,_) | `G g ->
         let s = Format.asprintf "%a" (Graph.pp Term) g in
         arena#set_clipboard s;
         temporary#msg "graph copied to clipboard: %s" s
      | _ -> temporary#msg "no graph to export here"

    method on_key_press s =
      if s = "Escape" then self#abort
      else match mode with
      | `Normal | `Move_node _ ->
         (match s with
          | "h" -> self#help 
                     "** keys **
r/R     rewrite box
u       unbox or unfold node
-/+     shrink/enlarge element
f       release fixed element
l       load state from clipboard
e       export graph term to clipboard
->/<-   undo/redo
SPACE   pause/start
ESC     abort current action
=       fit screen
h       print this help message"
          | "f" -> self#release
          | "u" -> self#unfold
          | "l" -> self#load_from_clipboard
          | "e" -> self#graph_to_clipboard
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "=" -> arena#fit self#box; self#redraw
          | "r" -> self#rewrite false
          | "R" -> self#rewrite true
          | "ArrowLeft" -> self#undo()
          | "ArrowRight" -> self#redo()
          | " " -> play <- not play
          | "!" -> self#redraw
          | "?" -> self#refresh
          | "" -> ()
          | s -> temporary#msg "skipping key '%s'" s)
      | _ -> temporary#msg "ignored key `%s' during ongoing action" s
    
  end
