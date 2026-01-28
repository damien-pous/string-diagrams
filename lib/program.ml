open Misc
open Types
open Graph_type
open Messages

let initial_state: state = ([], Trm (Graph.idm (Typ.flex 1)))

let state_of_string s =
  let l = Lexing.from_string s in
  let x = Parser.rawterm Lexer.token l in
  Graph.state x

class virtual mk (arena: arena): [state] program =
  object(self)

    inherit History.mk

    method private virtual read: _
    method private virtual write: _
    method private virtual write_svg: _
    method private virtual write_pdf: _
    method private virtual help: _
    method private virtual open_dialog: _
    method private virtual saveas_dialog: _
    method private virtual quit: _
    method virtual fullscreen: _
    method virtual entry: _
    method virtual set_entry: _
    method virtual entry_warning: _
    
    val mutable mode = `Normal

    val state = ref initial_state
    method private state = !state
    method private term_or_equation = snd !state
    method private env = fst !state
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
         Format.kasprintf (fun s' -> state := self#env,Eqn(e,s^"\n"^s'))
      | Trm _ -> Format.kasprintf (fun _ -> ())
      
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
      let b = Gg.Box2.(self#fold_graphs (fun g -> union g#box) empty) in      
      if !Constants.labels then
        Gg.Box2.(v_mid (mid b)
                   (Gg.V2.smul ((h b +. 3. *. Constants.fontsize) /. h b) (size b)))
      else b

    method private redraw ?(rebox=false) () =
      arena#canvas#clear;
      self#iter_graphs (fun g -> g#draw arena#canvas);
      List.iter (fun (_,(l,r)) -> arena#canvas#text (Gg.P2.mid l#pos r#pos) "=") self#hyps;
      (match self#term_or_equation with
       | Eqn((l,r),_) -> 
          arena#canvas#text (Gg.P2.mid l#pos r#pos)
            (if l#get "fill" = None then "=?=" else "=")
       | _ -> ());
      if rebox then arena#fit self#box;
      self#refresh

    method private highlight_iport g i =
      temporary#circle ~fill:(Graph.icolor g i)
        {center = g#ipos i; radius = 1.5*.Constants.point_radius}
    method private highlight_oport g o =
      temporary#circle ~fill:(Graph.ocolor g o)
        {center = g#opos o; radius = 1.5*.Constants.point_radius}

    val highlight_ports = false
    
    method private refresh =
      (match mode with
       | `Select (g,p) ->
          temporary#polygon ~fill:(Constants.color "tgray") p;
          if highlight_ports then 
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
       | `New_node _ ->
          temporary#box ~color:Constants.red (Gg.Box2.v_mid arena#pointer (Constants.empty_size 0 0))
       | `New_edge_i (g,i) ->
          let p = arena#pointer in
          let q = g#ipos i in
          let color = Graph.icolor g i in
          if Geometry.mem_point p q then
            temporary#point q;
          Graph.iter_oports g
            (fun o -> if g#ofree o && Typ.eq1 (g#ityp i) (g#otyp o) then (
                        let q = g#opos o in
                        temporary#point ~color q;
                        if Geometry.mem_point p q then
                          self#highlight_oport g o
                      )
            );
          temporary#segment ~color (q,p);
       | `New_edge_o (g,o) ->
          let p = arena#pointer in
          let q = g#opos o in
          let color = Graph.ocolor g o in
          if Geometry.mem_point p q then
            temporary#point q;
          Graph.iter_iports g
            (fun i -> if g#ifree i && Typ.eq1 (g#ityp i) (g#otyp o)  then (
                        let q = g#ipos i in
                        temporary#point ~color q;
                        if Geometry.mem_point p q then
                          self#highlight_iport g i
                      )
            );
          temporary#segment ~color (p,q)
       | `Move_node _ -> ()
       | `Normal ->
          if !Constants.edit_mode || highlight_ports then
            match self#catch_ports() with
            | `I (g,i) -> self#highlight_iport g i
            | `O (g,o) -> self#highlight_oport g o
            | `N _ | `G _ | `None -> ()
      );
      arena#refresh

    method private update_entry =
      Format.kasprintf self#set_entry "%a" (Graph.pp_state TermIfPossible) !state
    
    method private on_reset =
      mode <- `Normal;
      self#update_entry;
      self#redraw()

    method private changed_nocheckpoint =
      (match self#term_or_equation with
      | Eqn((l,r),_) -> 
         if Graph.iso l r then
           (l#set "fill" "done"; r#set "fill" "done")
         else
           (l#unset "fill"; r#unset "fill")
      | Trm _ -> ()); 
      self#update_entry;
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
         if not (Graph.iso_state !state state') then (
           debug_msg "entry" "on entry and state changed";
           state := state';
           self#checkpoint;
           self#redraw ~rebox:true ();
         )
      | exception (Failure s) -> self#entry_warning s
      | exception Parser.Error -> self#entry_warning "Parsing error"
      | exception e -> self#entry_warning (Printexc.to_string e)
      
    method private catch_ports = self#catch ~ports:true
    method private catch ?(ports=false) () =
      let p = arena#pointer in
      let g =
        self#fold_graphs (fun g -> function
            | None -> if g#contains p then Some g else None
            | x -> x) None
      in
      match g with
      | None -> `None
      | Some g ->
         if ports && !Constants.edit_mode then
           match Graph.find_ports g p with
           | `N n -> `N(g,n)
           | `I i -> `I(g,i)
           | `O o -> `O(g,o)
           | _ -> `G g
         else
           match Graph.find g p with
           | `N n -> `N(g,n)
           | _ -> `G g

    method private remove_node =
      match self#catch() with
      | `N (g,n) -> g#rem_node n; self#changed 
      | _ -> warning "no node to remove here"

    method private new_node =
      match self#catch() with
      | `G g -> temporary#msg "type node name"; mode <- `New_node g; self#refresh
      | _ -> warning "cannot create a node here"

    method private add_node g f =
      match List.assoc f self#env with
      | l,T2((n,m),_) -> 
         g#add_node (Graph.var_node n m f (Info.pos arena#pointer l)); self#changed
      | _ -> error "not a node name: %s" f
      | exception Not_found -> error "unknown node name: %s" f
    
    method private unfold =
      match self#catch() with
      | `N (g,n) ->
         (match n#kind with
          | Box _ ->
             g#unbox n;
             g#unset "place";
             List.iter (fun (_,(l,r)) -> l#unset "fill"; r#unset "fill") self#hyps;
             self#changed
          | Var f ->
             match List.assoc f self#env with
             | _,T2(_,Some h) -> g#subst n (Graph.copy self#env h); self#changed
             | _,T2(_,None) -> warning "this box is abstract"
             | _ -> assert false
         )
      | _ -> warning "no node to unfold/unbox here"

    method private scale s =
      match self#catch() with
      | `N (_,n) -> n#scale s; self#changed
      | `G g -> g#scale s; self#changed
      | _ -> warning "nothing to scale here"

    method private improve_placement force =      
      if not (self#fold_graphs (fun g s -> g#improve ~force && s) true) || force then        
        self#redraw()
    method private perturbate =
      self#improve_placement true
    
    method private release =
      match self#catch() with
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
      match self#catch() with
      | `N(g,n) ->
         (match n#kind with
          | Var _ -> warning "atomic boxes cannot be rewritten"
          | Box h ->
             let x,l,r =
               let j = Stdlib.ref i in
               match List.fold_left (fun acc (x,(l,r)) ->
                         if Graph.iso h l then (
                           decr j;                  
                           if j.contents=0 then Some (x,l,r) else
                             (l#unset "fill"; r#unset "fill"; acc)
                         ) else if Graph.iso h r then (
                           decr j;                  
                           if j.contents=0 then Some ("-"^x,r,l) else
                             (l#unset "fill"; r#unset "fill"; acc)
                         ) else acc
                       ) None self#hyps
               with
               | Some(x,l,r) -> x,l,r
               | None -> error "no such matching hypothesis (%i)" i
             in
             let i = match self#term_or_equation with
               | Eqn((_,g'),_) when g==g' -> "2: "
               | _ -> ""
             in
             let t =
               try Format.asprintf "%a" (Graph.pp Rocq) g
               with _ -> "[not a term]"
             in
             self#add_script "  transitivity (%s). %smcat.\n  rewrite %s.\n" t i x;
             temporary#msg "rewrite %s" x;
             h#set "place" "contract";
             h#on_stabilize
               (fun () ->
                 h#replace (Graph.copy self#env r);
                 h#unset "place";
                 self#update_entry;
                 h#on_stabilize (fun () ->
                     g#unbox n;
                     g#unset "place";
                     l#unset "fill";
                     r#unset "fill";
                     self#changed_nocheckpoint;
                     false);
                 h#set "fill" "rhs";
                 Place.group h;
                 false);
             self#changed)
      | _ -> warning "no box to rewrite here"

    val mutable play = true
    method on_tic =
      if play then
        match mode with
        | `Normal | `Move_node _ ->
           self#improve_placement false;
        | _ -> ()

    method on_button_press ctrl =
      match mode with 
      | `Normal ->
         (match self#catch_ports() with
          | `I(g,i) -> (match g#next_opt i with
                        | None -> mode <- `New_edge_i (g,i)
                        | Some o -> g#rem_edge (i,o); mode <- `New_edge_o (g,o); self#redraw())
          | `O(g,o) -> (match g#prev_opt o with
                        | None -> mode <- `New_edge_o (g,o)
                        | Some i -> g#rem_edge (i,o); mode <- `New_edge_i (g,i); self#redraw())
          | `N(g,x) -> Place.fix x; mode <- `Move_node (g,(x:>element),Gg.V2.sub arena#pointer x#pos)
          | `G g when ctrl -> mode <- `Move_node (g,(g:>element),Gg.V2.sub arena#pointer g#pos)
          | `G g -> mode <- let p = arena#pointer in `Select(g,Polygon.start p)
          | `None -> ())
      | _ -> ()
    
    method on_button_release =
      match mode with
      | `Move_node _ ->
         mode <- `Normal; self#checkpoint
      | `Select (g,p) ->
         mode <- `Normal; self#create_box g p
      | `New_edge_i (g,i) ->
         (match self#catch_ports() with
          | `O (g',o) when g==g' && g#ofree o && Typ.eq1 (g#ityp i) (g#otyp o) ->
             mode <- `Normal; g#add_edge (i,o); self#changed
          | _ -> mode <- `Normal; self#changed)
      | `New_edge_o (g,o) ->
         (match self#catch_ports() with
          | `I (g',i) when g==g' && g#ifree i && Typ.eq1 (g#ityp i) (g#otyp o) ->
             mode <- `Normal; g#add_edge (i,o); self#changed
          | _ -> mode <- `Normal; self#changed)
      | _ -> ()

    method on_motion =
      match mode with
      | `Move_node (_,n,u) ->
         n#move (Gg.V2.sub arena#pointer u);
         self#perturbate
      | `Select (g,p) ->
         let q = arena#pointer in
         mode <-`Select(g,Polygon.extend p q);
         self#refresh         
      | _ ->
         if !Constants.edit_mode then self#refresh
         (* (match self#catch with `N(g,n) -> temporary#msg "depth %i" (g#depth n) | _ -> ()); *)
         (* self#refresh *)

    method load v =
      mode <- `Normal;
      state := v;
      self#clear_history;
      self#update_entry;
      arena#fit self#box;
      self#redraw()

    method load_file =
      self#load self#read
             
    method load_string s =
      let l = Lexing.from_string s in
      let x = Parser.rawterm Lexer.token l in
      self#load (Graph.state x)

    method save =
      self#write !state
      
    method private load_from_clipboard =
      self#load_string arena#clipboard

    method private graph_to_clipboard =
      let k,g = 
        match self#catch() with
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

    method private pdf =
      let k,i = 
        match self#catch() with
        | `G g -> "graph",Graph.image g
        | `N(g,n) ->
           (match n#kind with
            | Box h -> "node",Graph.image h
            | _ -> "graph",Graph.image g)
        | _ -> "picture", (arena#canvas#get, self#box)
      in
      self#write_pdf [i];
      temporary#msg "%s exported to pdf" k

    method private script_to_clipboard =
      arena#set_clipboard self#script;
      temporary#msg "Rocq script copied to clipboard"    

    method on_key_press ctrl s =
      if ctrl then
        (match s with
         | "s" -> self#save
         | "e" -> self#saveas_dialog
         | "o" -> self#open_dialog
         | "v" -> self#load_from_clipboard
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
t       toggle node labels
-/+     shrink/enlarge element
f       release fixed element
l       toggle labels printing
c       toggle contours printing
=       fit screen
r       redraw picture
e       toggle link edition mode
d       remove node
n       create node (give name afterward)
t       export term to clipboard
p       export diagram as pdf
R       export Rocq script to clipboard
->/<-   undo/redo
SPACE   pause/start
ESC     abort current action
Ctrl-O  open file
Ctrl-S  save file
Ctrl-E  save as file
Ctrl-V  load from clipboard
Ctrl-F  toggle fullscreen
h       print this help message"
          | "f" -> self#release
          | "u" -> self#unfold
          | "d" -> self#remove_node
          | "n" -> self#new_node
          | "l" -> toggle Constants.labels; self#redraw ~rebox:true ()
          | "c" -> toggle Constants.contours; self#redraw()
          | "e" -> toggle Constants.edit_mode;
                   temporary#msg "edit mode: %b" !Constants.edit_mode; self#redraw()
          | "p" -> self#pdf
          | "t" -> self#graph_to_clipboard
          | "R" -> self#script_to_clipboard
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
      | `New_node g -> mode <- `Normal; self#add_node g s
      | _ -> warning "ignored key `%s' during ongoing action" s
    
  end
