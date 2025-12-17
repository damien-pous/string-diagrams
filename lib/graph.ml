open Types
open Graph_type
open Misc
open Messages
open Gg

(** * misc *)

exception Incomplete_graph
exception Not_a_graph_term of string
let not_a_graph_term fmt = Format.kasprintf (fun s -> raise (Not_a_graph_term s)) fmt
let tns_ctx u v = Term.(tns u (tns idm v))

let src = fst
let tgt = snd

let iter_positionned_edges g f =
  MSet.iter (fun (i,o) -> f (i,g#ipos i) (o,g#opos o)) g#edges
let iter_iports g f =
  fold (fun i () -> f (Source i)) g#sources ();  
  MSet.iter (fun n -> fold (fun i () -> f (InnerTarget(n,i))) n#targets ()) g#nodes
let iter_oports g f =
  fold (fun i () -> f (Target i)) g#targets ();  
  MSet.iter (fun n -> fold (fun i () -> f (InnerSource(n,i))) n#sources ()) g#nodes

(** checking isomorphism
    !! for now, only correct on connected graphs *)
let rec iso (g: graph) (h: graph) =
  g#sources = h#sources &&
  g#targets = h#targets &&
  MSet.size g#nodes = MSet.size h#nodes &&
  MSet.size g#edges = MSet.size h#edges &&
  let bisim = Hashtbl.create (2 * (MSet.size g#edges + MSet.size h#edges)) in
  let same_kind h k = match h,k with
    | Var(_,_,f), Var(_,_,g) -> f=g 
    | Box g, Box h -> iso g h
    | _ -> false
  in      
  let rec iso_dn x y =
    match g#next_opt x, h#next_opt y with
    | None, None -> true
    | Some (Target i), Some (Target j) when i=j -> true
    | Some (InnerSource(n,i)), Some (InnerSource(m,j)) when i=j ->
       Hashtbl.mem bisim (n,m) ||
         let _ = Hashtbl.add bisim (n,m) () in
         same_kind n#kind m#kind &&
         forall n#sources (fun i -> iso_up (InnerSource(n,i)) (InnerSource(m,i))) &&
         forall n#targets (fun i -> iso_dn (InnerTarget(n,i)) (InnerTarget(m,i)))
    | _ -> false
  and iso_up x y =
    match g#prev_opt x, h#prev_opt y with
    | None, None -> true
    | Some (Source i), Some (Source j) when i=j -> true
    | Some (InnerTarget(n,i)), Some (InnerTarget(m,j)) when i=j ->
       Hashtbl.mem bisim (n,m) ||
         let _ = Hashtbl.add bisim (n,m) () in
         same_kind n#kind m#kind &&
         forall n#sources (fun i -> iso_up (InnerSource(n,i)) (InnerSource(m,i))) &&
         forall n#targets (fun i -> iso_dn (InnerTarget(n,i)) (InnerTarget(m,i)))
    | _ -> false
  in
  forall g#targets (fun i -> iso_up (Target i) (Target i)) &&
    forall g#sources (fun i -> iso_dn (Source i) (Source i))
let rec iso_env e f =
  match e,f with
  | [],[] -> true
  | (x,(n,m,_,g))::e, (x',(n',m',_,h))::f when x=x' && n=n' && m=m' ->
     iso_env e f && (match g,h with
                     | None, None -> true
                     | Some g, Some h -> iso g h
                     | _ -> false)
  | _ -> false
let iso_envgraph (e,g) (f,h) =
  iso_env e f && iso g h

(** extracting terms from connected graphs
    !! buggy 
 *)
let buggy_connected_to_term = 
let rec to_term (g: graph) =
  let rec add_up n j l =
    if j>n#sources then l
    else InnerSource(n,j)::add_up n (j+1) l
  in
  let rec add_dn n j l =
    if j>n#targets then l
    else InnerTarget(n,j)::add_dn n (j+1) l
  in
  let rec eat_up n j j' = function
    | [] -> tns_ctx (closed_dn n j (j'-1)) (closed_dn n (j'+1) n#targets), []
    | i::q as l -> match g#prev i  with
                    | InnerTarget(n',j'') when n=n' ->
                       if not (j''>j') then not_a_graph_term "needs symmetry";
                       let u,q = eat_up n (j'+1) j'' q in
                       tns_ctx (closed_dn n j (j'-1)) u,q
                    | _ -> tns_ctx (closed_dn n j (j'-1)) (closed_dn n (j'+1) n#targets), l
  and eat_dn n j j' = function
    | [] -> tns_ctx (closed_up n j (j'-1)) (closed_up n (j'+1) n#sources), []
    | i::q as l -> match g#next i  with
                   | InnerSource(n',j'') when n=n' ->
                      if not (j''>j') then not_a_graph_term "needs symmetry";
                      let u,q = eat_dn n (j'+1) j'' q in
                      tns_ctx (closed_up n j (j'-1)) u,q
                   | _ -> tns_ctx (closed_up n j (j'-1)) (closed_up n (j'+1) n#sources), l
  and bottom_line = function
    | [] -> Term.emp,[]
    | i::q -> match g#prev i with
              | InnerTarget(n,j) ->
                 (* Format.eprintf "here %i@." j; *)
                 let u,q = eat_up n 1 j q in
                 (* Format.eprintf "eat up (%i,%i): %a@." j (List.length q) Term.pp u; *)
                 let t,k = bottom_line q in
                 Term.(tns (seq n#term u) t), add_up n 1 k
              | Source _ ->
                 let t,k = bottom_line q in Term.(tns idm t), i::k
  and top_line = function
    | [] -> Term.emp,[]
    | i::q -> match g#next i with
              | InnerSource(n,j) ->
                 let u,q = eat_dn n 1 j q in
                 let t,k = top_line q in
                 let v = match n#kind with
                   | Var(n,m,f) -> Term.var n m f
                   | Box g -> Term.box (to_term g)
                 in
                 Term.(tns (seq u v) t), add_dn n 1 k
              | Target _ ->
                 let t,k = top_line q in Term.(tns idm t), i::k
  and up l =
    let u,l = bottom_line l in
    (* Format.eprintf "bottom line was %a@." Term.pp u; *)
    if Term.is_id u then u,l
    else let v,l = up l in Term.seq v u,l
  and dn l =
    let u,l = top_line l in
    if Term.is_id u then u,l
    else let v,l = dn l in Term.seq u v,l
  and closed_dn n j k =
    if k<j then Term.emp else
    let l = List.init (1+k-j) (fun i -> InnerTarget(n,j+i)) in
    let u,l = dn l in
    (* Format.eprintf "closing down (%i-%i): %a@." j k Term.pp u; *)
    if l<>[] then not_a_graph_term "closing down reached a target";
    u
  and closed_up n j k =
    if k<j then Term.emp else
    let l = List.init (1+k-j) (fun i -> InnerSource(n,j+i)) in
    let u,l = up l in
    if l<>[] then not_a_graph_term "closing up reached a source";
    u
  in
  let closed_outer_dn j k =
    if k<j then Term.emp else
    let l = List.init (1+k-j) (fun i -> Source(j+i)) in
    let u,l = dn l in
    if l<>[] then not_a_graph_term "closing outer down reached a target";
    u
  in
  let rec complete i n = function
    | [] ->
       (* Format.eprintf "complete %i %i []@." i n; *)
       closed_outer_dn i n
    | p :: q -> match g#prev p with
                | Source j ->
                   (* Format.eprintf "complete %i %i (%i::...)@." i n j; *)
                   tns_ctx (closed_outer_dn i (j-1)) (complete (j+1) n q)
                | _ -> assert false
  in
  let targets = List.init g#targets (fun i -> Target (i+1)) in
  let u,l = up targets in
  (* Format.eprintf "before completion: %a (remains %i)@." Term.pp u (List.length l); *)
  let v = complete 1 g#sources l in
  (* Format.eprintf "completion: %a@." Term.pp v; *)
  Term.seq v u

in to_term


(** extracting terms from graphs without empty target nodes
    (building on the depth of each node)
 *)
let depth_to_term (g: graph) =
  if MSet.exists (fun n -> n#targets = 0) g#nodes then
    failwith "empty target nodes are not supported yet";
  let rec add n j l =
    if j>n#sources then l
    else InnerSource(n,j)::add n (j+1) l
  in
  let rec eat n j l =
    if j=n#targets+1 then l
    else match l with
         | [] -> not_a_graph_term "missing target"
         | i::q -> match g#prev i  with
                   | InnerTarget(n',j') when n=n' && j=j' -> eat n (j+1) q
                   | _ -> not_a_graph_term "probably needs symmetry"
  in
  let rec slice d = function
    | [] -> Term.emp,[]
    | j::q ->
         match g#prev j with
         | InnerTarget(n,i) when g#depth n = d ->
            if i<>1 then not_a_graph_term "did not reach first target first";
            let u,l = slice d (eat n 2 q) in
            Term.tns n#term u, (add n 1 l)            
         | _ -> let u,q = slice d q in Term.(tns idm u),j::q       
  in
  let rec up d l =
    let v,l = slice d l in
    if Term.is_id v then
      if l = (List.init g#sources (fun i -> g#next (Source(i+1)))) then v
      else not_a_graph_term "sources improperly linked"
    else 
      let u = up (d+1) l in
      Term.(seq u v)
  in up 1 (List.init g#targets (fun i -> Target(i+1)))


let _ = buggy_connected_to_term, depth_to_term
let to_term = depth_to_term


(** pretty printing graphs *)
let pp mode f g = g#pp mode f
let pp_env mode f (e: env) =
  let rec pp_env f = function
    | [] -> ()
    | (x,(l,n,m,None))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i in\n" x Info.pp_kvl l n m
    | (x,(l,n,m,Some g))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i := %t in\n" x Info.pp_kvl l n m (g#pp mode)
  in pp_env f e
let pp_envgraph mode f (e,g) = pp_env mode f e; pp mode f g
let pp_equation mode f (u,v) = 
  Format.fprintf f "%a = %a" (pp mode) u (pp mode) v
let pp_equations mode f (e,h,g) =
  Format.fprintf f "%a[%a]" 
  (pp_env mode) e
  (pp_print_list " -> \n" (pp_equation mode)) (h@[g])

(** * generic graph/node constructors *)

let top_pos b i n =
  let p = Gg.Box2.tl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)
let bot_pos b i n =
  let p = Gg.Box2.bl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)

class iface n m: interface =
  object
    method sources = n
    method targets = m
  end

(* shared boundary *)
class proxy (b: boundary) =
  object
    inherit Info.proxy (b:>area)
    method sources = b#sources
    method targets = b#targets
    method spos = b#spos
    method tpos = b#tpos
  end

(* rectangular boundary *)
class rectangle_boundary n m ?pos size ?name kvl =
  object(self)
    inherit iface n m 
    inherit Info.rectangle_area ?pos size ?name kvl
    method spos i = top_pos self#box i n 
    method tpos i = bot_pos self#box i m 
  end

(* polygonial boundary *)
class polygon_boundary poly spos tpos ?name kvl =
  let n = List.length spos in
  let m = List.length tpos in
  object
    inherit iface n m
    inherit Info.polygon_area poly ?name kvl as area
    val mutable spos = spos
    val mutable tpos = tpos
    method spos i = List.nth spos (i-1)
    method tpos i = List.nth tpos (i-1)
    method! private on_shift d =
      area#on_shift d;
      spos <- List.map (V2.add d) spos;
      tpos <- List.map (V2.add d) tpos;
  end

(* variable node *)
let var_node n m f l =
  object(self)
    inherit rectangle_boundary n m (Constants.var_size n m) ~name:f l
    method draw (draw: canvas) =
      draw#box ~fill:self#color self#box;
      draw#text self#pos f
    method kind = Var(n,m,f)
    method pp mode ff =
      Format.fprintf ff "%s%t" f (self#pp_infos mode)
    method term = Term.var n m f
  end

(* box node *)
let box_node g (_: Info.kvl) =
  (* TODO: use infos, via proxy? *)
  object
    inherit proxy (g:>boundary)
    method kind = Box g
    method pp mode = g#pp mode 
    method draw c = g#draw c
    method term = Term.box g#term
  end    

(* generic pregraph *)
class virtual gen_graph nodes edges =
  object(self)

    method virtual pp_infos: pp_mode -> formatter -> unit
    method virtual sources: int
    method virtual targets: int
    method virtual spos: int -> point
    method virtual tpos: int -> point
    method virtual draw_boundary: canvas -> unit
    method private virtual shift_boundary: vector -> unit

    val mutable depth = Hashtbl.create (MSet.size nodes)
    val mutable edges: (iport*oport) mset = edges
    val mutable nodes = nodes

    (* warning: to be called whenever the graph changes *)
    method private reset_depth = Hashtbl.clear depth
    
    method edges = edges
    method nodes = nodes
    method update nodes' edges' =
      (* TODO: sanity checks *)
      nodes <- nodes';
      edges <- edges';
      self#reset_depth;

    (* distance to the targets of the graph (TODO: acyclicity check) *)
    method depth n =
      try Hashtbl.find depth n
      with Not_found ->
            let d = 1 + fold (fun i ->
                            max (match self#next (InnerTarget(n,i)) with
                                 | Target _ -> 0
                                 | InnerSource(m,_) -> self#depth m)
                          ) n#targets 0
            in Hashtbl.add depth n d; d

    method ipos = function
      | Source i -> self#spos i
      | InnerTarget(n,i) -> n#tpos i
    method opos = function
      | Target i -> self#tpos i
      | InnerSource(n,i) -> n#spos i

    
    (* memoise? *)
    method private out_edge p = MSet.find (fun e -> src e = p) edges
    method ifree p = self#out_edge p = None
    method next_opt p = Option.map tgt (self#out_edge p)
    method next p = match self#next_opt p with Some q -> q | None -> raise Incomplete_graph
    method nexts p =
      let rec dfs p (nodes,ports) =
        match self#next_opt p with
        | None -> nodes, ports
        | Some (InnerSource(n,_) as q) when not (MSet.mem n nodes) ->
           fold (fun i -> dfs (InnerTarget(n,i))) n#targets (MSet.add n nodes, MSet.add q ports)
        | Some q -> nodes, MSet.add q ports
      in dfs p (MSet.empty,MSet.empty)

    (* memoise? *)
    method private inp_edge p = MSet.find (fun e -> tgt e = p) edges
    method ofree p = self#inp_edge p = None
    method prev_opt p = Option.map src (self#inp_edge p)
    method prev p = match self#prev_opt p with Some q -> q | None -> raise Incomplete_graph
    method prevs p =
      let rec dfs p (nodes,ports) =
        match self#prev_opt p with
        | None -> nodes, ports
        | Some (InnerTarget(n,_) as q) when not (MSet.mem n nodes) ->
           fold (fun i -> dfs (InnerSource(n,i))) n#sources (MSet.add n nodes, MSet.add q ports)
        | Some q -> nodes, MSet.add q ports
      in dfs p (MSet.empty,MSet.empty)

    method rem_edge e =      
      assert (MSet.mem e edges);
      edges <- MSet.rem e edges;
      self#reset_depth;
      
    method rem_node n =
      assert (MSet.mem n nodes);
      nodes <- MSet.rem n nodes;
      edges <- MSet.filter
                 (function
                  | InnerTarget(m,_),InnerSource(m',_) -> m<>n && m'<>n
                  | InnerTarget(m,_),_ | _,InnerSource(m,_) -> m<>n
                  | _ -> true) edges;
      self#reset_depth;
      
    
    method add_edge (src,tgt) =
      assert (self#ifree src && self#ofree tgt);
      (* assert (not (self#reaches tgt src));       *)
      edges <- MSet.add (src,tgt) edges;
      self#reset_depth;      

    method add_node n m f l =
      nodes <- MSet.add (var_node n m f l) nodes;
      (* self#reset_depth;       *)
    
    method subst n h =
      assert (n#sources = h#sources && n#targets = h#targets);
      let remap_src = function
        | Source i -> self#prev_opt (InnerSource(n,i))
        | p -> Some p
      in
      let remap_tgt = function
        | Target j -> self#next_opt (InnerTarget(n,j))
        | p -> Some p
      in
      let new_edges =
        MSet.omap (fun (s,t) ->
            match remap_src s, remap_tgt t with
            | Some s, Some t -> Some (s,t)
            | _ -> None
          ) h#edges
      in
      self#rem_node n;
      h#move n#pos;   (* TODO: resize h, or create it to fit the current box? *)
      nodes <- MSet.union nodes h#nodes;
      edges <- MSet.union edges new_edges;
      self#reset_depth;
    
    method unbox n =
      match n#kind with
      | Var(_,_,_) -> assert false
      | Box g -> self#subst n g

    (* textual pretty printing *)
    method private pp_iport f = function
      | Source i -> Format.fprintf f "%i" i
      | InnerTarget(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n self#nodes) i
    method private pp_oport f = function
      | Target i -> Format.fprintf f "%i" i
      | InnerSource(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n self#nodes) i
    method private pp_kind mode f = function
      | Var(_,_,x) -> Format.fprintf f "%s" x
      | Box g -> g#pp mode f
    method private pp_ mode f =
      let first = ref true in
      Format.fprintf f "{";
      MSet.iteri (fun i n ->
          if not !first then Format.fprintf f ",\n "; first := false;
          Format.fprintf f "n%i%t: %a" i (n#pp_infos mode) (self#pp_kind mode) n#kind;
        ) self#nodes;
      MSet.iter (fun (s,t) ->
          if not !first then Format.fprintf f ",\n "; first := false;
          Format.fprintf f "%a -> %a" self#pp_iport s self#pp_oport t;
        ) self#edges;
      Format.fprintf f "}%t: %i -> %i" (self#pp_infos mode) self#sources self#targets
    method pp mode f =
      match mode with
      | Term -> Term.pp f self#term
      | TermIfPossible ->
         (try Term.pp f self#term
          with Not_a_graph_term _ | Incomplete_graph -> self#pp_ Sparse f)
      | _ -> self#pp_ mode f

    method term = to_term (self:>graph)

    method draw (draw: canvas) =
      let draw_node n =
        n#draw draw;
        iter_iports (self:>graph) (fun p ->
            if self#ifree p then
              draw#point ~color:Constants.gray(* iport_color *) (self#ipos p)
          );
        iter_oports (self:>graph) (fun p ->
            if self#ofree p then
              draw#point ~color:Constants.gray(* oport_color *) (self#opos p)
          )
      in
      let draw_edge (i,o) = draw#segment (self#ipos i) (self#opos o) in
      self#draw_boundary draw;
      MSet.iter draw_node nodes;
      MSet.iter draw_edge edges

    method shift d =
      self#shift_boundary d;
      MSet.iter (fun n -> n#shift d) nodes

  end

let polygon_graph spos tpos nodes edges poly l =
  object
    inherit polygon_boundary poly spos tpos l as boundary
    method private shift_boundary = boundary#shift
    inherit! gen_graph nodes edges
  end

let rectangle_graph n m nodes edges ?pos size l =
  object 
    inherit rectangle_boundary n m ?pos size l as boundary
    method private shift_boundary = boundary#shift
    inherit! gen_graph nodes edges
  end 


(** * algebra of graphs *)

(* empty graph of type n->m *)
let empty n m =
  rectangle_graph
    n m
    MSet.empty
    MSet.empty
    (Constants.empty_size n m)
    []
let emp = empty 0 0

(* identity graph (of type 1->1) *)
let idm =
  rectangle_graph
    1 1
    MSet.empty
    (MSet.single (Source 1, Target 1))
    (Constants.idm_size)
    []

let shift_edges n m =
  let shift_iport = function
    | Source i -> Source (n+i)
    | p -> p
  in
  let shift_oport = function
    | Target i -> Target (m+i)
    | p -> p
  in
  MSet.map (fun (s,t) -> shift_iport s, shift_oport t)

(* tensor product *)
let tns (g: graph) (h: graph) =
  let size = Size2.v (g#width +. h#width) (max g#height h#height) in
  g#move (P2.v (-. h#width /. 2.) 0.);
  h#move (P2.v (g#width /. 2.) 0.);
  rectangle_graph
    (g#sources + h#sources) (g#targets + h#targets)
    (MSet.union g#nodes h#nodes)
    (MSet.union g#edges (shift_edges g#sources g#targets h#edges))
    size
    [] 

(* sequential composition *)
let seq (g: graph) (h: graph) =
  assert (g#targets = h#sources);
  let size = Size2.v (max g#width h#width) (g#height +. h#height) in
  g#move (P2.v 0. (h#height /. 2.));
  h#move (P2.v 0. (-. g#height /. 2.));
  rectangle_graph 
    g#sources h#targets
    (MSet.union g#nodes h#nodes)
    (MSet.union
       (MSet.omap (function
            | s,Target i -> Option.map
                             (fun o -> s,o)
                             (h#next_opt (Source i))
            | e -> Some e
          ) g#edges)
       (MSet.omap (function
            | Source _,_ -> None
            | e -> Some e)
          h#edges))
    size
    []

(* generic box graph *)
let gen_box_graph b n m =
  rectangle_graph
    n m
    (MSet.single b)
    (MSet.union
       (MSet.init n (fun i -> (Source i, InnerSource(b,i))))
       (MSet.init m (fun j -> (InnerTarget(b,j), Target j))))
    (Constants.expand b#size)
    [] 

(* graph reduced to a variable node *)
let var n m f l = gen_box_graph (var_node n m f l) n m

(* graph reduced to a box node *)
let box g l = gen_box_graph (box_node g l) g#sources g#targets



(** * graphs from generalised terms *)

let of_gterm u =
  let rec build = function
    | GTerm.Emp -> emp
    | GTerm.Idm -> idm
    | GTerm.Var(n,m,f,l) -> var n m f l
    | GTerm.Seq(u,v) -> seq (build u) (build v)
    | GTerm.Tns(u,v) -> tns (build u) (build v)
    | GTerm.Box(u,l) -> box (build u) l
    | GTerm.Gph(n,m,nodes,edges,l) -> gph n m nodes edges l
  and gph n m nodes edges l =
    let t = Hashtbl.create (List.length nodes) in
    let nodes = 
      MSet.mapl (fun (n,(k,l)) ->
          let node = match k with
            | GTerm.VNode(n,m,f) -> var_node n m f l
            | GTerm.GNode(t) -> box_node (build t) l
          in
          Hashtbl.add t n node;
          node
        ) nodes
    in
    let iport = function
      | Source i -> if 1<=i && i<=n then Source i
                   else failwith "invalid outer source: %i" i
      | InnerTarget(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#targets then InnerTarget(n,i)
             else failwith "invalid inner source: %s.%i" j i
         with Not_found -> failwith "unknown source node: %i" n
    in
    let oport = function
      | Target i -> if 1<=i && i<=m then Target i
                   else failwith "invalid outer target: %i" i
      | InnerSource(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#sources then InnerSource(n,i)
             else failwith "invalid inner target: %s.%i" j i
         with Not_found -> failwith "unknown target node: %i" n
    in
    let edges = MSet.mapl (fun (i,o) -> iport i,oport o) edges in
    let box = MSet.fold (fun n -> Box2.union n#box) Box2.empty nodes in
    let pos,size,l =
      if Box2.is_empty box then
        (* TODO: compute and use depth *)
        None,Constants.estimate_size n m (MSet.size nodes), l
      else
        let size = Constants.expand (Box2.size box) in
        Some (Box2.mid box), size, l
    in
    rectangle_graph n m nodes edges ?pos size l
  in build u
  

let env e = Info.envmap of_gterm (GTerm.env e)
let of_raw e t = of_gterm (GTerm.of_raw e t)
let envgraph et =
  let (e,t) = GTerm.envterm et in
  Info.envmap of_gterm e, of_gterm t


let of_equation ~placed (u,v) =
  let g = of_gterm u in
  let h = of_gterm v in
  if not placed then (
    let b = Box2.union g#box h#box in
    g#rebox b; h#rebox b);
  g,h

let equations ehg =
  let (e,h,g,placed) = GTerm.equations ehg in
  let h = List.map (of_equation ~placed) h in
  let l,r = of_equation ~placed g in
  if not placed then (
    l#scale 2.0; r#scale 2.0;
    let ph = List.map (fun (l,r) -> Pad.hpad Constants.spacing [l;r]) h in
    let ph = Pad.hpad (3.*.Constants.spacing) ph in
    let plr = Pad.hpad Constants.spacing [l;r] in
    ignore (Pad.vpad (2.*.Constants.spacing) [plr;ph]));
  Info.envmap of_gterm e, h, (l,r)

(* copy a graph by serialisation (is there a nicer way?) *)
let copy e g =
  let s = Format.kasprintf (fun s -> s) "%a" (pp Full) g in
  try
    let l = Lexing.from_string s in
    let t = Parser.justterm Lexer.token l in
    let t = GTerm.of_raw e t in
    of_gterm t
  with e -> Format.eprintf "error copying graph@."; raise e


exception Found_iport of iport
exception Found_oport of oport
let find g p =
  try
    iter_iports g (fun i -> if Geometry.mem_point p (g#ipos i) then raise (Found_iport i));
    iter_oports g (fun o -> if Geometry.mem_point p (g#opos o) then raise (Found_oport o));
    match MSet.find (fun n -> Box2.mem p n#box) g#nodes with
    | Some x -> `N x
    | None -> `None
  with
  | Found_iport i -> `I i
  | Found_oport o -> `O o

let create_box (g: graph) p =
  let debug_msg fmt = debug_msg "box" fmt in
  let edge_error (i,o) msg =
    temporary#segment ~color:Color.red (g#ipos i) (g#opos o);
    error msg
  in              
  let p = Geometry.clockwise p in
  let cuts = Polygon.fold2 p (fun ij acc ->
                 MSet.fold (fun e acc ->
                     let s,t = g#ipos (src e), g#opos (tgt e) in
                     match Geometry.intersection ij (s,t) with
                     | Some(x,(L|E)) -> `S (e,x) :: acc
                     | Some(x,R) -> `T (e,x) :: acc
                     | None -> acc
                   ) acc g#edges
               ) []
  in
  let cuts = List.rev cuts in
  let _ =
    let rec check = function
      | [] -> ()
      | `S(e,_)::q -> check q; check_s e q
      | `T(e,_)::q -> check q; check_t e q
    and check_s e = function 
      | [] -> ()
      | `S(e',_)::_ when e=e' -> edge_error e "edge cut twice as a source"
      | `T(e',_)::q when e=e' -> check_st e q
      | _::q -> check_s e q
    and check_t e = function 
      | [] -> ()
      | `T(e',_)::_ when e=e' -> edge_error e "edge cut twice as a target"
      | `S(e',_)::q when e=e' -> check_st e q
      | _::q -> check_t e q
    and check_st e = function 
      | [] -> ()
      | (`S(e',_)::_ | `T(e',_)::_) when e=e' -> edge_error e "edge cut more than twice"
      | _::q -> check_st e q
    in check cuts
  in 
  let rec group = function
    | [] -> []
    | `S a :: q ->
       (match group q with
        | [] -> [`S [a]]
        | `S l::q -> `S(a::l)::q
        | l -> `S [a] :: l)
    | `T a :: q ->
       (match group q with
        | [] -> [`T [a]]
        | `T l::q -> `T(a::l)::q
        | l -> `T [a] :: l)
  in
  let src,tgt = match group cuts with
    | [] -> if true then error "ignored empty box creation" else [],[]
    | [`S s] -> if true then error "empty target boxes unsupported yet" else s,[]
    | [`T t] -> [],t
    | [`S s;`T t]
      | [`T t;`S s] -> s,t
    | [`S s;`T t;`S s'] -> s'@s,t
    | [`T t;`S s;`T t'] -> s,t'@t
    | _ -> error "too many alternations of sources and targets"
  in
  debug_msg "%i -> %i" (List.length src) (List.length tgt);
  assert(unique_assq src && unique_assq tgt); (* TODO: with proper edge-comparison function *)
  let tgt = List.rev tgt in
  (* List.iteri (fun i (_,p) -> debug_msg "src.%i: %a@." (i+1) V2.pp p) src; *)
  (* List.iteri (fun i (_,p) -> debug_msg "tgt.%i: %a@." (i+1) V2.pp p) tgt; *)
  let nodes_out,nodes_in =
    MSet.partition (fun n -> Geometry.mem_poly n#pos p) g#nodes
  in
  debug_msg "%i nodes out, %i nodes in" (MSet.size nodes_out) (MSet.size nodes_in);
  let ikind = function
    | InnerTarget(n,_) when MSet.mem n nodes_in -> `In
    | _ -> `Out
  in
  let okind = function
    | InnerSource(n,_) when MSet.mem n nodes_in -> `In
    | _ -> `Out
  in
  let index e l =
    let rec index k = function
      | [] -> None
      | (e',_)::_ when e=e' -> Some k
      | _::q -> index (k+1) q
    in index 1 l
  in  
  let edges_out,edges_in =
    MSet.fold (fun (i,o as e) (edges_out,edges_in) ->
        let error = edge_error e in
        match ikind i, okind o, index e src, index e tgt with
        | `Out, `Out, Some s, Some t ->
           (fun b -> MSet.add (i, InnerSource(b,s)) (MSet.add (InnerTarget(b,t), o) (edges_out b))),
           MSet.add (Source s, Target t) edges_in
        | `Out, `Out, None, None ->
           (fun b -> MSet.add e (edges_out b)),
           edges_in
        | `Out, `Out, _, _ -> error "traversing edge cut only once"
        | `In, `In, None, None ->
           edges_out,
           MSet.add e edges_in
        | `In, `In, _, _ -> error "edges between selected nodes cannot be cut"
        | `Out, `In, Some s, None ->
           (fun b -> MSet.add (i, InnerSource(b,s)) (edges_out b)),
           MSet.add (Source s, o) edges_in
        | `Out, `In, _, _ -> error "out to in edge incorrectly cut"
        | `In, `Out, None, Some t ->
           (fun b -> MSet.add (InnerTarget(b,t), o) (edges_out b)),
           MSet.add (i, Target t) edges_in
        | `In, `Out, _, _ -> error "in to out edge incorrectly cut"
      ) ((fun _ -> MSet.empty),MSet.empty) g#edges
  in
  debug_msg "%i edges in" (MSet.size edges_in);
  let h = polygon_graph (List.map snd src) (List.map snd tgt) nodes_in edges_in p [] in
  let b = box_node h [] in
  debug_msg "%t" (b#pp Full);
  g#update (MSet.add b nodes_out) (edges_out b)
