open Types
open Graph_type
open Misc
open Gg

(** * misc *)

exception Incomplete_graph
exception Not_a_graph_term of string
let not_a_graph_term fmt = Format.kasprintf (fun s -> raise (Not_a_graph_term s)) fmt
let tns_ctx u v = Term.(tns u (tns idm v))
let okind = function Some s -> Some s#kind | None -> None

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
                 let v = match n#kind with
                   | Var(n,m,f) -> Term.var n m f
                   | Box g -> Term.box (to_term g)
                 in
                 Term.(tns (seq v u) t), add_up n 1 k
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

let src_port g k i =
  let kind = k i in
  object
    method kind = kind
    method pos = top_pos g#box i g#sources
  end
let tgt_port g k i =
  let kind = k i in
  object
    method kind = kind
    method pos = bot_pos g#box i g#targets
  end

(* generic gbox *)
class gen_gbox n m ?(pos=P2.o) size kvl =
  object
    inherit Info.positioner pos size kvl
    val mutable sources: port seq = Seq.empty
    val mutable targets: port seq = Seq.empty
    method src i = if 0<i && i<=n then Seq.get sources i
                   else failwith "invalid source: %i (%i->%i)" i n m
    method tgt i = if 0<i && i<=m then Seq.get targets i
                   else failwith "invalid target: %i (%i->%i)" i n m
    method sources: int = n
    method targets: int = m
  end

(* generic inner node *)
let gen_node kind ?pos size kvl =
  let n = match kind with Var(n,_,_) -> n | Box g -> g#sources in
  let m = match kind with Var(_,m,_) -> m | Box g -> g#targets in
  object(self)
    inherit gen_gbox n m ?pos size kvl as parent
    method kind = kind
    method! shift d =
      parent#shift d;
      match kind with
      | Box g -> g#shift d
      | _ -> ()
    method! scale s =
      parent#scale s;
      match kind with
      | Box g -> g#scale s
      | _ -> ()
    initializer
      sources <- Seq.init n (src_port self (fun i -> Inner(self,i)));
      targets <- Seq.init m (tgt_port self (fun i -> Inner(self,i)))
  end

(* variable node *)
let var_node n m f l = gen_node (Var(n,m,f)) (Constants.var_size n m) l

(* box node *)
let box_node g l = gen_node (Box g) g#size l

(* generic pregraph *)
class gen_pregraph n m nodes edges ?pos size kvl: pregraph =
  object(self)
    inherit gen_gbox n m ?pos size kvl as parent
    val mutable edges = edges
    val mutable nodes = nodes
    method edges = edges
    method nodes = nodes

    method iport = function
      | Outer i -> self#src i
      | Inner(n,i) -> n#tgt i
    method oport = function
      | Outer i -> self#tgt i
      | Inner(n,i) -> n#src i

    (* memoise? *)
    method private out_edge p = MSet.find (fun e -> e.src = p#kind) edges
    method private out_free p = self#out_edge p = None
    method next_opt p = Option.map (fun e -> self#oport e.tgt) (self#out_edge p)
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
    method private inp_edge p = MSet.find (fun e -> e.tgt = p#kind) edges
    method private inp_free p = self#inp_edge p = None
    method prev_opt p = Option.map (fun e -> self#iport e.src) (self#inp_edge p)
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
      assert (MSet.memq e edges);
      edges <- MSet.remq e edges
    method rem_node n =
      assert (MSet.memq n nodes);
      nodes <- MSet.remq n nodes;
      edges <- MSet.filter
                 (fun e -> match e.src,e.tgt with
                           | Inner(m,_),Inner(m',_) -> m!=n && m'!=n
                           | Inner(m,_),_ | _,Inner(m,_) -> m!=n
                           | _ -> true) edges
    
    method add_edge src tgt =
      assert (self#out_free src && self#inp_free tgt);
      assert (not (self#reaches tgt src));      
      edges <- MSet.add { src=src#kind; tgt=tgt#kind } edges
    
    (* let add_node g n = *)
    (*   { g with nodes = MSet.add n g.nodes },n *)
    
    (* let add_var g ninfo n m f = add_node g { ninfo ; kind = Var(n,m,f) } *)
    (* let add_box g ninfo h = add_node g { ninfo ; kind = Box h } *)
    
    method subst n h =
      assert (n#sources = h#sources && n#targets = h#targets);
      let remap_src p = match p with
        | Inner _ -> Some p
        | Outer i -> okind (self#prev_opt (n#src i))
      in
      let remap_tgt p = match p with
        | Inner _ -> Some p
        | Outer j -> okind (self#next_opt (n#tgt j))
      in
      let new_edges =
        MSet.omap (fun e ->
            match remap_src e.src, remap_tgt e.tgt with
            | Some src, Some tgt -> Some {src=src; tgt=tgt}
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
    
    method! shift d =
      parent#shift d;
      MSet.iter (fun n -> n#shift d) nodes

    initializer
      sources <- Seq.init n (src_port self (fun i -> Outer i));
      targets <- Seq.init m (tgt_port self (fun i -> Outer i))
  end

(* generic graph *)
let gen_graph n m nodes edges ?pos size kvl: graph =
  object(self)
    inherit gen_pregraph n m nodes edges ?pos size kvl as parent
    
    method is_empty =
      MSet.is_empty self#nodes && MSet.is_empty self#edges

    method ipos p = (self#iport p)#pos
    method opos p = (self#oport p)#pos

    method find p =
      (* Format.eprintf "find at %a@." V2.pp p; *)
      match MSet.find (fun n -> Box2.mem p n#box) self#nodes with
      | Some x -> `N x
      | None -> `None

    (* graphical rendering *)
    method draw_on (draw: canvas) =
      let draw_node n =
        match n#kind with
        | Var(_,_,f) ->
           draw#box ~fill:n#color n#box;
           draw#text n#pos f
        | Box g -> g#draw_on draw
      in
      let draw_edge e = 
        draw#segment (self#ipos e.src) (self#opos e.tgt)
      in
      draw#box self#box;
      MSet.iter draw_node self#nodes;
      MSet.iter draw_edge self#edges
    method draw =
      let c = new Canvas.basic in
      self#draw_on c;
      c#get

    (* textual pretty printing *)
    method private pp_port f = function
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
          Format.fprintf f "n%i%t: %a" i (n#pp mode) (self#pp_kind mode) n#kind;
        ) self#nodes;
      MSet.iter (fun e ->
          if not !first then Format.fprintf f ",\n "; first := false;
          Format.fprintf f "%a -> %a" self#pp_port e.src self#pp_port e.tgt;
        ) self#edges;
      Format.fprintf f "}%t: %i -> %i" (parent#pp mode) self#sources self#targets
    method! pp mode f =
      match mode with
      | Full -> self#pp_ mode f
      | Term -> Term.pp f (to_term self)
      | Sparse ->
         try Term.pp f (to_term self)
         with Not_a_graph_term _ | Incomplete_graph -> self#pp_ mode f
    
  end


(** * algebra of graphs *)

(* empty graph of type n->m *)
let empty n m =
  gen_graph
    n m
    MSet.empty
    MSet.empty
    (Constants.empty_size n m)
    []
let emp = empty 0 0

(* identity graph (of type 1->1) *)
let idm =
  gen_graph
    1 1
    MSet.empty
    (MSet.single { src = Outer 1; tgt = Outer 1 })
    (Constants.idm_size)
    [] 

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p
let shift_edges n m =
  MSet.map (fun e -> {src=shift_port n e.src; tgt=shift_port m e.tgt})

(* tensor product *)
let tns (g: graph) (h: graph) =
  let size = Size2.v (g#width +. h#width) (max g#height h#height) in
  g#move (P2.v (-. h#width /. 2.) 0.);
  h#move (P2.v (g#width /. 2.) 0.);
  gen_graph
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
  gen_graph 
    g#sources h#targets
    (MSet.union g#nodes h#nodes)
    (MSet.union
       (MSet.omap (fun e ->
            match e.tgt with
            | Outer i -> Option.map
                           (fun o -> {e with tgt = o#kind})
                           (h#next_opt (h#src i))
            | _ -> Some e
          ) g#edges)
       (MSet.filter
          (fun e -> match e.src with Outer _ -> false | _ -> true)
          h#edges))
    size
    []

(* generic box graph *)
let gen_box_graph b n m =
  gen_graph
    n m
    (MSet.single b)
    (MSet.union
       (MSet.init n (fun i -> {src=Outer i; tgt=Inner(b,i)}))
       (MSet.init m (fun j -> {src=Inner(b,j); tgt=Outer j})))
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
    let edges = MSet.mapl (fun (i,o) -> {src=iport i;tgt=oport o}) edges in
    let box = MSet.fold (fun n -> Box2.union n#box) Box2.empty nodes in
    let pos,size,l =
      if Box2.is_empty box then
        (* TODO: compute and use depth *)
        None,Constants.estimate_size n m (MSet.size nodes), l
      else
        let size = Constants.expand (Box2.size box) in
        Some (Box2.mid box), size, l
    in
    gen_graph n m nodes edges ?pos size l
  in build u       

let env e = Info.envmap of_gterm (GTerm.env e)
let of_raw e t = of_gterm (GTerm.of_raw e t)
let envgraph et =
  let (e,t) = GTerm.envterm et in
  Info.envmap of_gterm e, of_gterm t

