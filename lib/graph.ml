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
let okind = function Some s -> Some s#kind | None -> None

let src = fst
let tgt = snd

(** checking isomorphism
    !! for now, only correct on connected graphs *)
let rec iso (g: graph) (h: graph) =    (* or pregraph? *)
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
  let rec iso dn x y =
    let step = if dn then (fun g -> g#next_opt) else (fun g -> g#prev_opt) in
    match okind (step g x), okind (step h y) with
    | None, None -> true
    | Some (Outer i), Some (Outer j) when i=j -> true
    | Some (Inner (n,i)), Some (Inner (m,j)) when i=j ->
       Hashtbl.mem bisim (n,m) ||
         let _ = Hashtbl.add bisim (n,m) () in
         same_kind n#kind m#kind &&
         forall n#sources (fun i -> iso_up (n#src i) (m#src i)) &&
         forall n#targets (fun i -> iso_dn (n#tgt i) (m#tgt i))
    | _ -> false
  and iso_dn x y = iso true x y
  and iso_up x y = iso false x y
  in
  forall g#sources (fun i -> iso_dn (g#src i) (h#src i)) &&
  forall g#targets (fun i -> iso_up (g#tgt i) (h#tgt i))
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

(** extracting terms from graphs
    !! for now, only correct on connected graphs *)
let rec to_term (g: graph) =    (* or pregraph? *)
  let rec add_up n j l =
    if j>n#sources then l
    else n#src j::add_up n (j+1) l
  in
  let rec add_dn n j l =
    if j>n#targets then l
    else n#tgt j::add_dn n (j+1) l
  in
  let rec eat_up n j j' = function
    | [] -> tns_ctx (closed_dn n j (j'-1)) (closed_dn n (j'+1) n#targets), []
    | i::q as l -> match (g#prev i)#kind  with
                    | Inner(n',j'') when n==n' ->
                       if not (j''>j') then not_a_graph_term "needs symmetry";
                       let u,q = eat_up n (j'+1) j'' q in
                       tns_ctx (closed_dn n j (j'-1)) u,q
                    | _ -> tns_ctx (closed_dn n j (j'-1)) (closed_dn n (j'+1) n#targets), l
  and eat_dn n j j' = function
    | [] -> tns_ctx (closed_up n j (j'-1)) (closed_up n (j'+1) n#sources), []
    | i::q as l -> match (g#next i)#kind  with
                   | Inner(n',j'') when n==n' ->
                      if not (j''>j') then not_a_graph_term "needs symmetry";
                      let u,q = eat_dn n (j'+1) j'' q in
                      tns_ctx (closed_up n j (j'-1)) u,q
                   | _ -> tns_ctx (closed_up n j (j'-1)) (closed_up n (j'+1) n#sources), l
  and bottom_line = function
    | [] -> Term.emp,[]
    | i::q -> match (g#prev i)#kind with
              | Inner(n,j) ->
                 (* Format.eprintf "here %i@." j; *)
                 let u,q = eat_up n 1 j q in
                 (* Format.eprintf "eat up (%i,%i): %a@." j (List.length q) Term.pp u; *)
                 let t,k = bottom_line q in
                 Term.(tns (seq n#term u) t), add_up n 1 k
              | Outer _ ->
                 let t,k = bottom_line q in Term.(tns idm t), i::k
  and top_line = function
    | [] -> Term.emp,[]
    | i::q -> match (g#next i)#kind with
              | Inner(n,j) ->
                 let u,q = eat_dn n 1 j q in
                 let t,k = top_line q in
                 let v = match n#kind with
                   | Var(n,m,f) -> Term.var n m f
                   | Box g -> Term.box (to_term g)
                 in
                 Term.(tns (seq u v) t), add_dn n 1 k
              | Outer _ ->
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
    let l = List.init (1+k-j) (fun i -> n#tgt (j+i)) in
    let u,l = dn l in
    (* Format.eprintf "closing down (%i-%i): %a@." j k Term.pp u; *)
    if l<>[] then not_a_graph_term "closing down reached a target";
    u
  and closed_up n j k =
    if k<j then Term.emp else 
    let l = List.init (1+k-j) (fun i -> n#src (j+i)) in
    let u,l = up l in
    if l<>[] then not_a_graph_term "closing up reached a source";
    u
  in
  let closed_outer_dn j k =
    if k<j then Term.emp else 
    let l = List.init (1+k-j) (fun i -> g#src (j+i)) in
    let u,l = dn l in
    if l<>[] then not_a_graph_term "closing outer down reached a target";
    u
  in  
  let rec complete i n = function
    | [] ->
       (* Format.eprintf "complete %i %i []@." i n; *)
       closed_outer_dn i n
    | p :: q -> match (g#prev p)#kind with
                | Outer j ->
                   (* Format.eprintf "complete %i %i (%i::...)@." i n j; *)
                   tns_ctx (closed_outer_dn i (j-1)) (complete (j+1) n q)
                | _ -> assert false
  in
  let targets = List.init g#targets (fun i -> g#tgt (i+1)) in
  let u,l = up targets in
  (* Format.eprintf "before completion: %a (remains %i)@." Term.pp u (List.length l); *)
  let v = complete 1 g#sources l in
  (* Format.eprintf "completion: %a@." Term.pp v; *)
  Term.seq v u

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

let port p k i =
  object
    method pos = p i
    method kind = k i
  end

class iface n m: interface =
  object
    method sources = n
    method targets = m
  end

(* generic boundary *)
class virtual gen_boundary n m =
  object(self)
    inherit iface n m 
    method private virtual ipos: int -> point
    method private virtual opos: int -> point
    method private src_ k i =
      if 0<i && i<=n then port self#ipos k i
      else failwith "invalid source: %i (%i->%i)" i n m
    method private tgt_ k i =
      if 0<i && i<=m then port self#opos k i
      else failwith "invalid target: %i (%i->%i)" i n m
    method src = self#src_ (fun i -> Outer i)
    method tgt = self#tgt_ (fun i -> Outer i)
    method nsrc (n: node) = self#src_ (fun i -> Inner(n,i))
    method ntgt (n: node) = self#tgt_ (fun i -> Inner(n,i))
  end

(* shared boundary *)
class proxy (b: boundary) =
  object
    inherit Info.proxy (b:>area)
    method sources = b#sources
    method targets = b#targets
    method src = b#src
    method tgt = b#tgt
    method nsrc = b#nsrc
    method ntgt = b#ntgt
  end

(* rectangular boundary *)
class rectangle_boundary n m ?pos size kvl =
  object(self)
    inherit gen_boundary n m 
    inherit Info.rectangle_area ?pos size kvl
    method private ipos i = top_pos self#box i n 
    method private opos i = bot_pos self#box i m 
  end

(* polygonial boundary *)
class polygon_boundary poly ipos opos kvl =
  let n = List.length ipos in
  let m = List.length opos in
  object
    inherit gen_boundary n m
    inherit Info.polygon_area poly kvl as area
    val mutable ipos = ipos
    val mutable opos = opos
    method private ipos i = List.nth ipos (i-1)
    method private opos i = List.nth opos (i-1)
    method! private on_shift d =
      area#on_shift d;
      ipos <- List.map (V2.add d) ipos;
      opos <- List.map (V2.add d) opos;
  end

(* variable node *)
let var_node n m f l =
  object(self)
    inherit rectangle_boundary n m (Constants.var_size n m) l
    method! src = self#nsrc (self:>node)
    method! tgt = self#ntgt (self:>node)
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
  object(self)
    inherit proxy (g:>boundary)
    method! src = self#nsrc (self:>node)
    method! tgt = self#ntgt (self:>node)
    method kind = Box g
    method pp mode = g#pp mode 
    method draw c = g#draw c
    method term = Term.box g#term
  end    

(* generic pregraph *)
class virtual gen_graph nodes edges_ =
  object(self)

    method virtual pp_infos: pp_mode -> formatter -> unit
    method virtual sources: int
    method virtual targets: int
    method virtual src: int -> port
    method virtual tgt: int -> port
    method virtual nsrc: node -> int -> port
    method virtual ntgt: node -> int -> port
    method virtual draw_boundary: canvas -> unit
    method private virtual shift_boundary: vector -> unit

    val mutable edges: (port * port) mset = MSet.empty
    val mutable nodes = nodes
    method edges = edges
    method nodes = nodes

    (* memoise? *)
    method private out_edge p = let pk = p#kind in MSet.find (fun (s,_) -> s#kind = pk) edges
    method private out_free p = self#out_edge p = None
    method next_opt p = Option.map tgt (self#out_edge p)
    method next p = match self#next_opt p with Some q -> q | None -> raise Incomplete_graph
    method nexts p =
      let rec dfs p (nodes,ports) =
        match self#next_opt p with
        | None -> nodes, ports
        | Some q -> match q#kind with
                    | Inner(n,_) when not (Set.memq n nodes) ->
                       fold (fun i -> dfs (n#tgt i)) n#targets (Set.add n nodes, Set.add q ports)
                    | _ -> nodes, Set.add q ports
      in dfs p (Set.empty,Set.empty)

    (* memoise? *)
    method private inp_edge p = let pk = p#kind in MSet.find (fun (_,t) -> t#kind = pk) edges
    method private inp_free p = self#inp_edge p = None
    method prev_opt p = Option.map src (self#inp_edge p)
    method prev p = match self#prev_opt p with Some q -> q | None -> raise Incomplete_graph
    method prevs p =
      let rec dfs p (nodes,ports) =
        match self#prev_opt p with
        | None -> nodes, ports
        | Some q -> match q#kind with
                    | Inner(n,_) when not (Set.memq n nodes) ->
                       fold (fun i -> dfs (n#src i)) n#sources (Set.add n nodes, Set.add q ports)
                    | _ -> nodes, Set.add q ports
      in dfs p (Set.empty,Set.empty)

    method reaches p q = Set.memq q (snd (self#nexts p)) 

    method rem_edge e =      
      assert (MSet.memq e edges); (* TODO: use appropriate equality *)
      edges <- MSet.remq e edges
    method rem_node n =
      assert (MSet.memq n nodes);
      nodes <- MSet.remq n nodes;
      edges <- MSet.filter
                 (fun (s,t) -> match s#kind,t#kind with
                           | Inner(m,_),Inner(m',_) -> m!=n && m'!=n
                           | Inner(m,_),_ | _,Inner(m,_) -> m!=n
                           | _ -> true) edges
    
    method add_edge (src,tgt) =
      assert (self#out_free src && self#inp_free tgt);
      assert (not (self#reaches tgt src));      
      edges <- MSet.add (src,tgt) edges
    
    method subst n h =
      assert (n#sources = h#sources && n#targets = h#targets);
      let remap_src p = match p#kind with
        | Inner _ -> Some p
        | Outer i -> self#prev_opt (n#src i)
      in
      let remap_tgt p = match p#kind with
        | Inner _ -> Some p
        | Outer j -> self#next_opt (n#tgt j)
      in
      let new_edges =
        MSet.omap (fun (s,t) ->
            match remap_src s, remap_tgt t with
            | Some s, Some t -> Some (s,t)
            | _ -> None
          ) h#edges
      in
      self#rem_node n;
      h#move n#pos;   (* TOTHINK: resize, and probably copy h first *)
      nodes <- MSet.union nodes h#nodes;
      edges <- MSet.union edges new_edges
    
    method unbox n =
      match n#kind with
      | Var(_,_,_) -> assert false
      | Box g -> self#subst n g

    (* textual pretty printing *)
    method private pp_port f p =
      match p#kind with
      | Outer i -> Format.fprintf f "%i" i
      | Inner(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n self#nodes) i
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
          Format.fprintf f "%a -> %a" self#pp_port s self#pp_port t;
        ) self#edges;
      Format.fprintf f "}%t: %i -> %i" (self#pp_infos mode) self#sources self#targets
    method pp mode f =
      match mode with
      | Term -> Term.pp f self#term
      | _ -> self#pp_ mode f

    method term = to_term (self:>graph)

    method draw (draw: canvas) =
      let draw_node n = n#draw draw in
      let draw_edge (i,o) = draw#segment i#pos o#pos in
      self#draw_boundary draw;
      MSet.iter draw_node nodes;
      MSet.iter draw_edge edges

    method shift d =
      self#shift_boundary d;
      MSet.iter (fun n -> n#shift d) nodes

    method private iport = function
      | Outer i -> self#src i
      | Inner(n,i) -> n#tgt i
    method private oport = function
      | Outer i -> self#tgt i
      | Inner(n,i) -> n#src i
    method private set_edges edges_ =
      edges <- MSet.map (fun (i,o) -> self#iport i, self#oport o) edges_
    
    method update nodes' (edges_: (node pkind*node pkind) mset) =
      (* TODO: sanity checks *)
      nodes <- nodes';
      self#set_edges edges_

    initializer
      self#set_edges edges_
  end

let polygon_graph ipos opos nodes edges poly l =
  object
    inherit polygon_boundary poly ipos opos l as boundary
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
    (MSet.single (Outer 1, Outer 1))
    (Constants.idm_size)
    []

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p
let shift_edges n m =
  MSet.map (fun (s,t) -> shift_port n s#kind, shift_port m t#kind)
let reloc_edges =
  MSet.map (fun (s,t) -> s#kind, t#kind)

(* tensor product *)
let tns (g: graph) (h: graph) =
  let size = Size2.v (g#width +. h#width) (max g#height h#height) in
  g#move (P2.v (-. h#width /. 2.) 0.);
  h#move (P2.v (g#width /. 2.) 0.);
  rectangle_graph
    (g#sources + h#sources) (g#targets + h#targets)
    (MSet.union g#nodes h#nodes)
    (MSet.union (reloc_edges g#edges) (shift_edges g#sources g#targets h#edges))
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
       (MSet.omap (fun (s,t) ->
            match t#kind with
            | Outer i -> Option.map
                           (fun o -> s#kind,o#kind)
                           (h#next_opt (h#src i))
            | tk -> Some (s#kind,tk)
          ) g#edges)
       (MSet.omap (fun (s,t) ->
            match s#kind with Outer _ -> None | sk -> Some (sk,t#kind))
          h#edges))
    size
    []

(* generic box graph *)
let gen_box_graph b n m =
  rectangle_graph
    n m
    (MSet.single b)
    (MSet.union
       (MSet.init n (fun i -> (Outer i, Inner(b,i))))
       (MSet.init m (fun j -> (Inner(b,j), Outer j))))
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
      | Outer i -> if 1<=i && i<=n then Outer i
                   else failwith "invalid outer source: %i" i
      | Inner(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#targets then Inner(n,i)
             else failwith "invalid inner source: %s.%i" j i
         with Not_found -> failwith "unknown source node: %i" n
    in
    let oport = function
      | Outer i -> if 1<=i && i<=m then Outer i
                   else failwith "invalid outer target: %i" i
      | Inner(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#sources then Inner(n,i)
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

let find g p =
  (* Format.eprintf "find at %a@." V2.pp p; *)
  match MSet.find (fun n -> Box2.mem p n#box) g#nodes with
  | Some x -> `N x
  | None -> `None

let create_box g p =
  let debug_msg fmt = debug_msg "box" fmt in
  let p = Geometry.clockwise p in
  let cuts = Polygon.fold2 p (fun ij acc ->
                 MSet.fold (fun (s,t as e) acc ->
                     let s,t = s#pos, t#pos in
                     match Geometry.intersection ij (s,t) with
                     | Some(x,(L|E)) -> `S (e,x) :: acc
                     | Some(x,R) -> `T (e,x) :: acc
                     | None -> acc
                   ) acc g#edges
               ) []
  in
  let cuts = List.rev cuts in
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
    | [] -> [],[]
    | [`S s] -> s,[]
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
  let kind = function
    | Inner(n,_) when MSet.memq n nodes_in -> `In
    | _ -> `Out
  in
  let index e l =
    let rec index k = function
      | [] -> None
      | ((s,t),_)::_ when e=(s#kind,t#kind) -> Some k
      | _::q -> index (k+1) q
    in index 1 l
  in  
  let edges_out,edges_in =
    MSet.fold (fun (i,o) (edges_out,edges_in) ->
        let error msg =
          temporary#segment ~color:Color.red i#pos o#pos;
          error msg
        in              
        let ik,ok = i#kind, o#kind in
        match kind ik, kind ok, index (ik,ok) src, index (ik,ok) tgt with
        | `Out, `Out, Some s, Some t ->
           (fun b -> MSet.add (ik, Inner(b,s)) (MSet.add (Inner(b,t), ok) (edges_out b))),
           MSet.add (Outer s, Outer t) edges_in
        | `Out, `Out, None, None ->
           (fun b -> MSet.add (ik,ok) (edges_out b)),
           edges_in
        | `Out, `Out, _, _ -> error "traversing edge cut only once"
        | `In, `In, None, None ->
           edges_out,
           MSet.add (ik,ok) edges_in
        | `In, `In, _, _ -> error "edges between selected nodes cannot be cut"
        | `Out, `In, Some s, None ->
           (fun b -> MSet.add (ik, Inner(b,s)) (edges_out b)),
           MSet.add (Outer s, ok) edges_in
        | `Out, `In, _, _ -> error "out to in edge incorrectly cut"
        | `In, `Out, None, Some t ->
           (fun b -> MSet.add (Inner(b,t), ok) (edges_out b)),
           MSet.add (ik, Outer t) edges_in
        | `In, `Out, _, _ -> error "in to out edge incorrectly cut"
      ) ((fun _ -> MSet.empty),MSet.empty) g#edges
  in
  debug_msg "%i edges in" (MSet.size edges_in);
  let h = polygon_graph (List.map snd src) (List.map snd tgt) nodes_in edges_in p [] in
  let b = box_node h [] in
  debug_msg "%t" (b#pp Full);
  g#update (MSet.add b nodes_out) (edges_out b)
