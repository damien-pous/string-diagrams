open Types
open Misc
open Gg

type port = Outer of int | Inner of node * int
and iport = port
and oport = port
and edge = { src: iport; tgt: oport }
and kind = Var of int*int*name | Box of graph
and node = { ninfo: positionned; kind: kind }
and graph =
  { info: positionned;
    sources: int;
    targets: int;
    nodes: node mset;
    edges: edge mset }

let ksources = function Var(n,_,_) -> n | Box g -> g.sources
let ktargets = function Var(_,m,_) -> m | Box g -> g.targets

let nsources n = ksources n.kind 
let ntargets n = ktargets n.kind

let is_empty g = MSet.is_empty g.nodes && MSet.is_empty g.edges

let out_edge g p = MSet.find (fun e -> e.src = p) g.edges
let out_free g p = out_edge g p = None 
let next g p = Option.map (fun e -> e.tgt) (out_edge g p)
let nexts g p =
  let rec dfs p (nodes,ports) =
    match next g p with
    | None -> nodes, ports
    | Some (Inner(n,_) as q) when not (Set.memq n nodes) ->
       fold (fun i -> dfs (Inner(n,i))) (ntargets n) (Set.add n nodes, Set.add q ports)
    | Some q -> nodes, Set.add q ports
  in dfs p (Set.empty,Set.empty)

let inp_edge g p = MSet.find (fun e -> e.tgt = p) g.edges
let inp_free g p = inp_edge g p = None 
let prev g p = Option.map (fun e -> e.src) (inp_edge g p)
let prevs g p =
  let rec dfs p (nodes,ports) =
    match prev g p with
    | None -> nodes, ports
    | Some (Inner(n,_) as q) when not (Set.memq n nodes) ->
       fold (fun i -> dfs (Inner(n,i))) (ntargets n) (Set.add n nodes, Set.add q ports)
    | Some q -> nodes, Set.add q ports
  in dfs p (Set.empty,Set.empty)

let reaches g p q = Set.memq q (snd (nexts g p)) (* optimisable *)


let gpos g = g.info#pos 
let gsize g = g.info#size 
let width g = Size2.w (gsize g) 
let height g = Size2.h (gsize g)
let gbox g = g.info#box 

let npos n = n.ninfo#pos 
let nsize n = n.ninfo#size 
let nbox n = n.ninfo#box 


let empty n m =
  { sources = n; targets = m;
    nodes = MSet.empty;
    edges = MSet.empty;
    info = Info.gen (Constants.empty_size n m) [] }
let emp = empty 0 0

let idm =
  { sources = 1; targets = 1; 
    nodes = MSet.empty;
    edges = MSet.single { src = Outer 1; tgt = Outer 1 };
    info = Info.gen (Constants.idm_size) [] }

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p

let shift_edges n m =
  MSet.map (fun e -> {src=shift_port n e.src; tgt=shift_port m e.tgt})

let rec gshift g d =
  g.info#shift d;
  MSet.iter (fun n -> nshift n d) g.nodes
and nshift n d =
  n.ninfo#shift d;
  match n.kind with
  | Box g -> gshift g d
  | _ -> ()
let gmove g p = gshift g V2.(p - gpos g)
let nmove n p = nshift n V2.(p - npos n)

let gscale k g = g.info#scale k
let nscale k n = n.ninfo#scale k


let tns g h =
  let size = Size2.v (width g +. width h) (max (height g) (height h)) in
  gmove g (P2.v (-. width h /. 2.) 0.);
  gmove h (P2.v (width g /. 2.) 0.);
  { sources = g.sources + h.sources;
    targets = g.targets + h.targets;
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union g.edges (shift_edges g.sources g.targets h.edges);
    info = Info.gen size [] }

let seq g h =
  assert (g.targets = h.sources);
  let size = Size2.v (max (width g) (width h)) (height g +. height h) in
  gmove g (P2.v 0. (height h /. 2.));
  gmove h (P2.v 0. (-. height g /. 2.));
  { sources = g.sources;
    targets = h.targets;
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union
              (MSet.omap (fun e ->
                   match e.tgt with
                   | Outer i -> Option.map
                                  (fun o -> {e with tgt = o})
                                  (next h (Outer i))
                   | _ -> Some e
                 ) g.edges)
              (MSet.filter
                 (fun e -> match e.src with Outer _ -> false | _ -> true)
                 h.edges);
    info = Info.gen size [] }

let gen_box b n m =
  { sources = n;
    targets = m;
    nodes = MSet.single b;
    edges = MSet.union
              (MSet.init n (fun i -> {src=Outer i; tgt=Inner(b,i)}))
              (MSet.init m (fun j -> {src=Inner(b,j); tgt=Outer j}));
    info = Info.gen (Constants.expand (nsize b)) [] }

let var_node n m f l = { ninfo = Info.gen (Constants.var_size n m) l; kind = Var(n,m,f) }
let box_node g l = { ninfo = Info.gen (gsize g) l; kind = Box g }

let var n m f l = gen_box (var_node n m f l) n m
let box g l = gen_box (box_node g l) g.sources g.targets

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
      | Raw.Outer i -> if 1<=i && i<=n then Outer i
                       else failwith "invalid outer source: %i" i
      | Raw.Inner(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=ntargets n then Inner(n,i)
             else failwith "invalid inner source: %s.%i" j i
         with Not_found -> failwith "unknown source node: %i" n
    in
    let oport = function
      | Raw.Outer i -> if 1<=i && i<=m then Outer i
                       else failwith "invalid outer target: %i" i
      | Raw.Inner(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=nsources n then Inner(n,i)
             else failwith "invalid inner target: %s.%i" j i
         with Not_found -> failwith "unknown target node: %i" n
    in
    let edges = MSet.mapl (fun (i,o) -> {src=iport i;tgt=oport o}) edges in
    let box = MSet.fold (fun n -> Box2.union (nbox n)) Box2.empty nodes in
    let info =
      if Box2.is_empty box then
        (* TODO: compute and use depth *)
        Info.gen (Constants.estimate_size n m (MSet.size nodes)) l
      else
        let size = Constants.expand (Box2.size box) in
        Info.gen_at (Box2.mid box) size l
    in
    { sources = n; targets = m; nodes; edges; info }
  in build u       

type env = graph Info.env

let env e = Info.envmap of_gterm (GTerm.env e)
let of_raw e t = of_gterm (GTerm.of_raw e t)
let envgraph et =
  let (e,t) = GTerm.envterm et in
  Info.envmap of_gterm e, of_gterm t

let rem_edge g e =
  assert (MSet.memq e g.edges);
  { g with edges = MSet.remq e g.edges }

let rem_node g n =
  assert (MSet.memq n g.nodes);
  { g with
    nodes = MSet.remq n g.nodes;
    edges = MSet.filter
              (fun e -> match e.src,e.tgt with
                        | Inner(m,_),_ | _,Inner(m,_) -> m!=n
                        | _ -> true) g.edges }

let add_edge g src tgt =
  assert (out_free g src && inp_free g tgt);
  assert (not (reaches g tgt src));
  let e = { src; tgt } in
  { g with edges = MSet.add e g.edges },e

let add_node g n =
  { g with nodes = MSet.add n g.nodes },n

let add_var g ninfo n m f = add_node g { ninfo ; kind = Var(n,m,f) }
let add_box g ninfo h = add_node g { ninfo ; kind = Box h }

let subst g n h =
  assert (nsources n = h.sources && ntargets n = h.targets);
  let g_n = rem_node g n in
  let remap_src p = match p with
    | Inner _ -> Some p
    | Outer _ -> prev g p
  in
  let remap_tgt p = match p with
    | Inner _ -> Some p
    | Outer _ -> next g p
  in
  gmove h (npos n);             (* TOTHINK: resize, and probably copy h first *)
  { g with nodes = MSet.union g_n.nodes h.nodes;
           edges = MSet.union g_n.edges
                     (MSet.omap (fun e ->
                          match remap_src e.src, remap_tgt e.tgt with
                          | Some src, Some tgt -> Some {src; tgt}
                          | _ -> None
                        ) h.edges) }

let unbox g n =
  match n.kind with
  | Var(_,_,_) -> assert false
  | Box h -> subst g n h


let iter_inner_iports f g =
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (nsources n) ()) g.nodes
let iter_inner_oports f g =
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (ntargets n) ()) g.nodes


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

let ipos g = function
  | Outer i -> top_pos (gbox g) i g.sources
  | Inner(n,i) -> bot_pos (nbox n) i (ntargets n)
let opos g = function
  | Outer i -> bot_pos (gbox g) i g.targets
  | Inner(n,i) -> top_pos (nbox n) i (nsources n)

let rec draw_on (draw: canvas) g =
  let draw_node n =
    match n.kind with
    | Var(_,_,f) ->
       draw#box ~fill:n.ninfo#color (nbox n);
       draw#text (npos n) f
    | Box g -> draw_on draw g
  in
  let draw_edge e = 
    draw#segment (ipos g e.src) (opos g e.tgt)
  in
  draw#box (gbox g);
  MSet.iter draw_node g.nodes;
  MSet.iter draw_edge g.edges

let draw g =
  let c = new Canvas.basic in
  draw_on c g;
  c#get

(** checking isomorphism
    !! for now, only correct on graphs where each node is either reachable or coreachable
       (or both), in a directed sense (no change of direction allowed) *)
let rec iso g h =
  let rec iso_iports x y =
    match next g x, next h y with
    | None, None -> true
    | Some (Outer i), Some (Outer j) -> i=j
    | Some (Inner (n,i)), Some (Inner (m,j)) when i=j ->
       (match n.kind, m.kind with
        | Var(_,_,f), Var(_,_,g) -> f=g
        | Box g, Box h -> iso g h
        | _ -> false) &&
         forall (ntargets n) (fun i -> iso_iports (Inner(n,i)) (Inner(m,i)))
    | _ -> false
  in
  let rec iso_oports x y =
    match prev g x, prev h y with
    | None, None -> true
    | Some (Outer i), Some (Outer j) -> i=j
    | Some (Inner (n,i)), Some (Inner (m,j)) when i=j ->
       (match n.kind, m.kind with
        | Var(_,_,f), Var(_,_,g) -> f=g
        | Box g, Box h -> iso g h
        | _ -> false) &&
         forall (nsources n) (fun i -> iso_oports (Inner(n,i)) (Inner(m,i)))
    | _ -> false
  in
  g.sources = h.sources &&
  g.targets = h.targets &&
  MSet.size g.nodes = MSet.size h.nodes &&
  MSet.size g.edges = MSet.size h.edges &&
  forall g.sources (fun i -> iso_iports (Outer i) (Outer i)) &&
  forall g.targets (fun i -> iso_oports (Outer i) (Outer i))
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

let find p g =
  (* Format.eprintf "find at %a@." V2.pp p; *)
  match MSet.find (fun n -> Box2.mem p (nbox n)) g.nodes with
  | Some x -> `N x
  | None -> `None


(** reconstructing terms from graphs
    !! for now, only for graphs where each node is coreachable
    (i.e., empty target nodes are not allowed)
 *)
exception Not_a_graph_term of string
let not_a_graph_term fmt = Format.kasprintf (fun s -> raise (Not_a_graph_term s)) fmt
let rec to_term g =
  let rec eat j n l =
    if j>ntargets n then l
    else match l with
         | [] -> not_a_graph_term "did not reach all outputs of a node"
         | i::q -> match prev g i  with
                     | None -> not_a_graph_term "incomplete graph"
                     | Some (Inner(n',j')) when n==n' && j=j' -> eat (j+1) n q
                     | _ -> not_a_graph_term "incorrectly linked graph"
  in
  let rec add j n l =
    if j>nsources n then l
    else Inner(n,j)::add (j+1) n l
  in
  let rec bottom_line = function
    | [] -> Term.emp,[]
    | i::q -> match prev g i with
              | None -> not_a_graph_term "incomplete graph"
              | Some (Inner(n,1)) ->
                 let q = eat 2 n q in
                 let t,k = bottom_line q in
                 let u = match n.kind with
                   | Var(n,m,f) -> Term.var n m f
                   | Box g -> Term.box (to_term g)
                 in
                 Term.(tns u t), add 1 n k
              | Some _ ->
                 let t,k = bottom_line q in Term.(tns idm t), i::k
  in
  let rec lines l =
    let u,l = bottom_line l in
    if Term.is_id u then
      if List.map (prev g) l = List.init g.sources (fun i -> Some (Outer(i+1)))
      then u
      else not_a_graph_term "not properly linking sources"
    else Term.seq (lines l) u
  in
  let targets = List.init g.targets (fun i -> Outer(i+1)) in
  lines targets
  


(** pretty printing *)
let pp mode f g =
  let pp_port g f = function
    | Outer i -> Format.fprintf f "%i" i
    | Inner(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n g.nodes) i
  in
  let rec pp tab f g =
    let first = ref true in
    Format.fprintf f "%s{"
      tab;
    MSet.iteri (fun i n ->
        if not !first then Format.fprintf f ",\n "; first := false;
        Format.fprintf f "%s n%i%t: %a"
          tab i (n.ninfo#pp mode) (pp_kind tab) n.kind;
      ) g.nodes;
    MSet.iter (fun e ->
        if not !first then Format.fprintf f ",\n "; first := false;
        Format.fprintf f "%s %a -> %a"
          tab (pp_port g) e.src (pp_port g) e.tgt;
      ) g.edges;
    Format.fprintf f "%s}%t: %i -> %i" 
      tab (g.info#pp mode) g.sources g.targets
  and pp_kind tab f = function
    | Var(_,_,x) -> Format.fprintf f "%s" x
    | Box g -> pp (tab^"  ") f g
  in
  match mode with
  | Full -> pp "" f g
  | Term -> Term.pp f (to_term g)
  | Sparse -> 
    try Term.pp f (to_term g)
    with Not_a_graph_term _ -> pp "" f g

let pp_env mode f (e: env) =
  let rec pp_env f = function
    | [] -> ()
    | (x,(l,n,m,None))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i in\n" x Info.pp_kvl l n m
    | (x,(l,n,m,Some g))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i := %a in\n" x Info.pp_kvl l n m (pp mode) g
  in pp_env f e

let pp_envgraph mode f (e,g) = pp_env mode f e; pp mode f g
    

(* STOPPED HERE *)

(*
let filter_edges f g =
  { g with edges = MSet.filter f g.edges }

let rem_last_source g =
  let k = g.arity in
  if k = 0 then failwith "no source to remove";
  { g with
    arity = k - 1;
    edges = nomap
              (function
               | Src i when i = k -> None
               | v -> Some v)
              g.edges }
let rem_source i g =
  let k = g.arity in
  if i>k then failwith "rem_source: not a valid source"
  else if i=k then rem_last_source g
  else rem_last_source (prm (Perm.of_cycle [i;k]) g)

let promote x g =
  let arity = g.arity+1 in
  { arity; ivertices = MSet.remq x g.ivertices;
    edges = nmap
              (function
               | Inn y when x == y -> src arity
               | v -> v)
              g.edges }

 *)
