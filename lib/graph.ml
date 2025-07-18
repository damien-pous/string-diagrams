open Types
open Misc
open Info
open Gg

type port = Outer of int | Inner of node * int
and iport = port
and oport = port
and edge = { src: iport; tgt: oport }
and kind = Var of int*int*name | Box of graph
and node = { info: positionned; kind: kind }
and graph = {
    sources: int;
    targets: int;
    mutable size: size;
    nodes: node mset;
    edges: edge mset }

let gsources g = g.sources 
let gtargets g = g.targets 
let gsize g = g.size 

let ksources = function Var(n,_,_) -> n | Box g -> gsources g
let ktargets = function Var(_,m,_) -> m | Box g -> gtargets g

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

let empty n m =
  { sources = n; targets = m;
    size = Constants.idsize (max n m);
    nodes = MSet.empty;
    edges = MSet.empty }
let emp = empty 0 0

let idm =
  { sources = 1; targets = 1; 
    size = Constants.idsize 1;
    nodes = MSet.empty;
    edges = MSet.single { src = Outer 1; tgt = Outer 1 } }

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p

let shift_edges n m =
  MSet.map (fun e -> {src=shift_port n e.src; tgt=shift_port m e.tgt})

let width g = Size2.w g.size 
let height g = Size2.h g.size
let center g = V2.(g.size/2.)

let rec gshift g d =
  MSet.iter (fun n -> nshift n d) g.nodes
and nshift n d =
  n.info#shift d;
  match n.kind with
  | Box g -> gshift g d
  | _ -> ()
let nmove n p = nshift n V2.(p - n.info#pos)

let gscale k g = g.size <- V2.smul k g.size


let tns g h =
  let size = Size2.v (width g +. width h) (max (height g) (height h)) in
  let c = V2.(size/2.) in
  gshift g (V2.(center g-c));
  gshift h (V2.(center h-c+v (width g) 0.));
  { sources = g.sources + h.sources;
    targets = g.targets + h.targets;
    size;
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union g.edges (shift_edges (gsources g) (gtargets g) h.edges) }

let seq g h =
  assert (gtargets g = gsources h);
  let size = Size2.v (max (width g) (width h)) (height g +. height h) in
  let c = V2.(size/2.) in
  gshift g (V2.(center g-c+v 0. (height h)));
  gshift h (V2.(center h-c));
  { sources = g.sources;
    targets = h.targets;
    size;
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
                 h.edges) }

let gbox b n m =
  { sources = n;
    targets = m;
    size = Constants.expand b.info#size;
    nodes = MSet.single b;
    edges = MSet.union
              (MSet.init n (fun i -> {src=Outer i; tgt=Inner(b,i)}))
              (MSet.init m (fun j -> {src=Inner(b,j); tgt=Outer j})) }

let var_node n m f l = { info = Info.var n m l; kind = Var(n,m,f) }
let box_node g l = { info = Info.box g.size l; kind = Box g }

let var n m f l = gbox (var_node n m f l) n m
let box g = gbox (box_node g []) g.sources g.targets

type env = (name*(int*int*kvl*graph option)) list  

let of_raw e u =
  let sym f = 
    try List.assoc f e
    with Not_found -> failwith "unknown symbol: %s" f
  in
  let rec build = function
    | Raw.Emp -> emp
    | Raw.Idm -> idm
    | Raw.Var f -> let (n,m,l,_) = sym f in var n m f l
    | Raw.Seq(u,v) -> seq (build u) (build v)
    | Raw.Tns(u,v) -> tns (build u) (build v)
    | Raw.Box(u) -> box (build u)
    | Raw.Gph(n,m,size,nodes,edges) -> gph n m size nodes edges
  and gph n m size nodes edges =
    let t = Hashtbl.create 10 in
    let nodes = 
      MSet.mapl (fun (n,l,u) ->
          if Hashtbl.mem t n then failwith "duplicate node: %s" n;
          let node = match u with
            | Raw.Var f -> let (n,m,_,_) = sym f in var_node n m f l
            | _ -> let g = build u in box_node g l
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
    { sources = n; targets = m; size; nodes; edges }
  in build u       

let env e =
  let rec env = function
    | [] -> []
    | (f,(l,t,b))::e ->
       let e = env e in
       match b,t with
       | None,None -> failwith "variables must be given either a type or a body (%s)" f
       | Some b,Some(n,m) ->
          let b = of_raw e b in
          assert(gsources b = n && gtargets b = m);
          (f,(n,m,l,Some b))::e
       | Some b,None ->
          let b = of_raw e b in
          (f,(gsources b,gtargets b,l,Some b))::e
       | None,Some(n,m) -> 
          (f,(n,m,l,None))::e
  in
  env (List.rev e)

let envgraph (e,t) = let e = env e in e,of_raw e t

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

let add_var g info n m f = add_node g { info ; kind = Var(n,m,f) }
let add_box g info h = add_node g { info ; kind = Box h }

let subst g n h =
  assert (nsources n = gsources h && ntargets n = gtargets h);
  let g_n = rem_node g n in
  let remap_src p = match p with
    | Inner _ -> Some p
    | Outer _ -> prev g p
  in
  let remap_tgt p = match p with
    | Inner _ -> Some p
    | Outer _ -> next g p
  in
  gshift h n.info#pos;
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

let gbox g = Box2.v_mid P2.o (gsize g)
let nbox n = n.info#box

let npos n = n.info#pos
let ipos g = function
  | Outer i -> top_pos (gbox g) i (gsources g)
  | Inner(n,i) -> bot_pos (nbox n) i (ntargets n)
let opos g = function
  | Outer i -> bot_pos (gbox g) i (gtargets g)
  | Inner(n,i) -> top_pos (nbox n) i (nsources n)

let rec draw_on (draw: canvas) g =
  let draw_node n =
    match n.kind with
    | Var(_,_,f) ->
       draw#box (nbox n);
       draw#text n.info#pos f
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

(* checking isomorphism
   (only safe for graphs where all nodes are reacheable from the sources, for now) *)
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
  gsources g = gsources h &&
    gtargets g = gtargets h &&
      MSet.size g.nodes = MSet.size h.nodes &&
        MSet.size g.edges = MSet.size h.edges &&
          forall (gsources g) (fun i -> iso_iports (Outer i) (Outer i))
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
  match MSet.find (fun n -> Box2.mem p (nbox n)) g.nodes with
  | Some x -> `N x
  | None -> `None

let pp mode f g =
  let pp_port g f = function
    | Outer i -> Format.fprintf f "%i" i
    | Inner(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n g.nodes) i
  in
  let rec pp tab f g =
    Format.fprintf f "%s{"
      tab;
    MSet.iteri (fun i n ->
        Format.fprintf f "%s  n%i%t: %a,\n"
          tab i (n.info#pp mode) (pp_kind tab) n.kind;
      ) g.nodes;
    MSet.iter (fun e ->
        Format.fprintf f "%s  %a -> %a,\n"
          tab (pp_port g) e.src (pp_port g) e.tgt;
      ) g.edges;
    Format.fprintf f "%s  size=%g,%g }: %i -> %i\n" 
      tab (width g) (height g) g.sources g.targets
  and pp_kind tab f = function
    | Var(_,_,x) -> Format.fprintf f "%s" x
    | Box g -> pp (tab^"  ") f g
  in pp "" f g

let pp_env mode f =
  let rec pp_env f = function
    | [] -> ()
    | (x,(n,m,l,None))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i in\n" x Info.pp_kvl l n m
    | (x,(n,m,l,Some g))::q ->
       pp_env f q;
       Format.fprintf f "let %s%a: %i -> %i = %a in\n" x Info.pp_kvl l n m (pp mode) g
  in pp_env f

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
