open Misc
open Types
open Graph_type
open Messages

let initial_state: state = ([], Trm (Graph.idm (Typ.flex 1)))

let state_of_string s =
  let l = Lexing.from_string s in
  let x = Parser.rawterm Lexer.token l in
  Graph.state x

let string_of_state s =
  Format.asprintf "%a" (Graph.pp_state Full) s

let copy_state: state -> state =
  if can_marshal_closures then marshal_copy
  else fun s -> state_of_string (string_of_state s)

exception Found3 of string*graph*node*graph

let changed = ref false

class virtual mk (arena: arena): [state] program =
  object(self)

    inherit [state] History.mk copy_state initial_state as parent

    method private virtual write_svg: _
    method private virtual write_pdf: _
    method private virtual help: _
    method private virtual open_dialog: _
    method private virtual saveas_dialog: _
    method private virtual save_dialog: _
    method private virtual quit: _
    method virtual fullscreen: _
    method virtual entry: _
    method virtual set_entry: _
    method virtual entry_warning: _
    
    val mutable mode = `Normal

    val mutable state = initial_state
    method private state = state
    method private term_or_equation = snd state
    method private env = fst state
    method private hyps = Env.hyps self#env
    method private graph =
      match self#term_or_equation with
      | Trm g -> g
      | Eqn _ -> error "single graph expected and equation found"    
    method private lhs =
      match self#term_or_equation with
      | Eqn ((g,_),_) -> g
      | Trm _ -> error "equation expected and single graph found (lhs)"
    method private rhs =
      match self#term_or_equation with
      | Eqn ((_,g),_) -> g
      | Trm _ -> error "equation expected and single graph found (rhs)"
    method private script =
      match self#term_or_equation with
      | Eqn (_,s) -> s
      | Trm _ -> error "equation expected and single graph found (script)"
    method private add_script: 'a. ('a, formatter, unit) format -> 'a =
      match self#term_or_equation with
      | Eqn (e,s) ->
         Format.kasprintf (fun s' -> state <- self#env,Eqn(e,s^"\n"^s'))
      | Trm _ -> error "equation expected and single graph found (add_script)"
      
    method private iter_graphs f =
      List.iter (fun (_,(l,r)) -> f l; f r) self#hyps;
      match self#term_or_equation with
      | Eqn ((l,r),_) -> f l; f r
      | Trm g -> f g
    method private fold_graphs: 'a. (graph -> 'a -> 'a) -> 'a -> 'a = fun f a ->
      List.fold_right (fun (_,(l,r)) a -> f l (f r a)) self#hyps
        (match self#term_or_equation with
         | Eqn ((l,r),_) -> f l (f r a)
         | Trm g -> f g a)
    
    method private box =
      Gg.Box2.(self#fold_graphs (fun g -> union g#box) empty)

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      self#iter_graphs (fun g -> g#draw arena#canvas);
      List.iter (fun (_,(l,r)) -> arena#canvas#text (Gg.P2.mid l#pos r#pos) "=") self#hyps;
      arena#canvas#text (Gg.P2.mid self#lhs#pos self#rhs#pos)
        (if self#lhs#get "fill" = None then "=?=" else "=");
      if rebox then arena#ensure self#box;
      self#refresh

    method private refresh =
      (match mode with
       | `Select (g,p) ->
          temporary#polygon ~fill:(Constants.color "tgray") p;
          if false then 
            let p = Geometry.clockwise p in
            MSet.iter (fun e ->
              let c = g#edge_curve e in
              Polygon.fold2 p (fun ij () ->
                  match Geometry.cintersection ij c with
                  | Some(x,d,_) ->
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
      self#redraw()

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

    method on_entry_changed =
      debug_msg "entry" "on entry changed with `%s'" self#entry;
      mode <- `Normal;
      match state_of_string self#entry with
      | state' ->
         self#entry_warning "";
         if not (Graph.iso_state state state') then (
           debug_msg "entry" "on entry and state changed";
           state <- state';
           self#checkpoint;
           self#redraw ~rebox:true ()
         )
      | exception (Failure s) -> self#entry_warning s
      | exception Parser.Error -> self#entry_warning "Parsing error"
      | exception e -> self#entry_warning (Printexc.to_string e)
      
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
             g#unset "place";
             List.iter (fun (_,(l,r)) -> l#unset "fill"; r#unset "fill") self#hyps;
             self#changed
          | Var f ->
             match List.assoc f self#env with
             | _,T2(_,Some h) -> g#subst n (Graph.copy h); self#changed
             | _,T2(_,None) -> warning "this box is abstract"
             | _ -> assert false
         )
      | _ -> warning "no node to unfold/unbox here"

    method private scale s =
      match self#catch with
      | `N (_,n) -> n#scale s; self#changed
      | `G g -> g#scale s; self#changed
      | _ -> warning "nothing to scale here"

    method private improve_placement force =      
      if not (self#fold_graphs (fun g s -> g#improve ~force && s) true) || force then        
        self#redraw()
    method private perturbate =
      self#improve_placement true
    
    method private release =
      match self#catch with
      | `N(_,n) -> Place.unfix n; self#perturbate
      | _ -> warning "no node to release here"
    
    method private create_box g p =
      let _,h = Graph.create_box g p in
      g#set "place" "locked";
      h#set "place" "locked";
      List.iter (fun (_,(l,r)) ->
          if Graph.iso h l then
            (h#set "fill" "lhs"; l#set "fill" "lhs"; r#set "fill" "rhs")
          else if Graph.iso h r then
            (h#set "fill" "lhs"; r#set "fill" "lhs"; l#set "fill" "rhs")
        ) self#hyps;
      self#changed

    method private rewrite i =
      let x,l,r =
        let j = ref i in
        match List.fold_left (fun acc (x,(l,r)) ->
                  if l#get "fill" = Some "lhs" then (
                    decr j;                  
                    if !j=0 then Some (x,l,r) else
                      (l#unset "fill"; r#unset "fill"; acc)
                  ) else if r#get "fill" = Some "lhs" then (
                    decr j;                  
                    if !j=0 then Some ("-"^x,r,l) else
                      (l#unset "fill"; r#unset "fill"; acc)
                  ) else acc
              ) None self#hyps
        with
        | Some(x,l,r) -> x,l,r
        | None -> error "no such matching hypothesis (%i)" i
      in
      let i,g,n,h =
        let k (i,g) =
          if g#get "place" = Some "locked" then
            MSet.iter (fun n ->
                match n#kind with
                | Box h -> if h#get "fill" = Some "lhs" then
                             raise (Found3(i,g,n,h))
                | _ -> ()) g#nodes
        in
        try List.iter k ["",self#lhs; "2: ",self#rhs];
            error "could not find the pattern to rewrite"
        with Found3(i,g,n,h) -> i,g,n,h
      in
      self#add_script "  transitivity (%a). %smcat.\n  rewrite %s.\n" (Graph.pp Rocq) g i x;
      temporary#msg "rewrite %s" x;
      h#set "place" "contract";
      h#on_stabilize
        (fun () ->
          h#replace (Graph.copy r);
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
          false);
      self#changed      

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

    method !load v =
      parent#load v;
      arena#fit self#box
             
    method load_string s =
      let l = Lexing.from_string s in
      let x = Parser.rocqgoal Lexer.token l in
      self#load (Graph.state x)
      
    method private load_from_clipboard =
      self#load_string arena#clipboard

    method private graph_to_clipboard =
      let k,g = 
        match self#catch with
        | `G g -> "graph",g
        | `N(g,n) ->
           (match n#kind with
            | Box h -> "node",h
            | _ -> "graph",g)
        | _ -> warning "no graph/node to export here"
      in
      let s = Format.asprintf "%a" (Graph.pp Rocq) g in
      arena#set_clipboard s;
      temporary#msg "%s copied to clipboard: %s" k s

    method private graph_to_pdf =
      let k,g = 
        match self#catch with
        | `G g -> "graph",g
        | `N(g,n) ->
           (match n#kind with
            | Box h -> "node",h
            | _ -> "graph",g)
        | _ -> warning "no graph/node to export here"
      in
      self#write_pdf [Graph.image g] "g.pdf";
      temporary#msg "%s exported to g.pdf" k

    method private script_to_clipboard =
      arena#set_clipboard self#script;
      temporary#msg "Rocq script copied to clipboard"    

    method on_key_press ctrl s =
      if ctrl then
        (match s with
         | "s" -> self#save_dialog
         | "e" -> self#saveas_dialog
         | "o" -> self#open_dialog
         | "f" -> self#fullscreen
         | "z" -> self#undo()
         | "r" -> self#redo()
         | "q" -> self#quit
         | s -> warning "skipping key control %s" s)
      else if s = "Escape" then self#abort
      else match mode with
      | `Normal | `Move_node _ ->
         (match s with
          | "h" -> self#help 
                     "** keys **
1..n    rewrite box using matching hypothesis
u       unbox or unfold node
t:      toggle node labels
-/+     shrink/enlarge element
f       release fixed element
l       load state from clipboard
e       export graph term to clipboard
p       export graph as pdf (file g.pdf)
E       export Rocq script to clipboard
->/<-   undo/redo
SPACE   pause/start
ESC     abort current action
=       fit screen
r       redraw picture
h       print this help message"
          | "f" -> self#release
          | "u" -> self#unfold
          | "t" -> Constants.toggle_labels(); self#redraw()
          | "p" -> self#graph_to_pdf
          | "l" -> self#load_from_clipboard
          | "e" -> self#graph_to_clipboard
          | "E" -> self#script_to_clipboard
          | "-" -> self#scale (1. /. 1.1)
          | "+" -> self#scale 1.1
          | "=" -> self#redraw ~rebox:true ()
          | "1" -> self#rewrite 1
          | "2" -> self#rewrite 2
          | "3" -> self#rewrite 3
          | "4" -> self#rewrite 4
          | "5" -> self#rewrite 5
          | "6" -> self#rewrite 6
          | "7" -> self#rewrite 7
          | "8" -> self#rewrite 8
          | "9" -> self#rewrite 9
          | "ArrowLeft" -> self#undo()
          | "ArrowRight" -> self#redo()
          | " " -> play <- not play
          | "r" -> self#redraw()
          | "!" -> self#refresh
          | "q" -> self#quit
          | "" -> ()
          | s -> warning "skipping key '%s'" s)
      | _ -> warning "ignored key `%s' during ongoing action" s
    
  end
