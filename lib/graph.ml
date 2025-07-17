open Types
open Misc
open Info
open Gg

type sort = {
      sources: int;
      targets: int;
      mutable size: size }

type port = Outer of int | Inner of node * int
and iport = port
and oport = port
and edge = { src: iport; tgt: oport }
and kind = Var of sort*name | Box of graph
and node = { info: positionned; kind: kind }
and graph = {
    gsort: sort;
    nodes: node mset;
    edges: edge mset }

let gsources g = g.gsort.sources 
let gtargets g = g.gsort.targets 
let gsize g = g.gsort.size 

let ksources = function Var(s,_) -> s.sources | Box g -> gsources g
let ktargets = function Var(s,_) -> s.targets | Box g -> gtargets g

let nsources n = ksources n.kind 
let ntargets n = ktargets n.kind
let nsize n = match n.kind with
  | Var(s,_) -> s.size
  | Box g -> gsize g

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
  { gsort = { sources = n; targets = m; size = Constants.idsize (max n m) };
    nodes = MSet.empty;
    edges = MSet.empty }
let emp = empty 0 0

let idm =
  { gsort = { sources = 1; targets = 1; size = Constants.idsize 1 };
    nodes = MSet.empty;
    edges = MSet.single { src = Outer 1; tgt = Outer 1 } }

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p

let shift_edges n m =
  MSet.map (fun e -> {src=shift_port n e.src; tgt=shift_port m e.tgt})

let width g = Size2.w g.gsort.size 
let height g = Size2.h g.gsort.size
let center g = V2.(g.gsort.size/2.)

let rec gshift g d =
  MSet.iter (fun n -> nshift n d) g.nodes
and nshift n d =
  n.info#shift d;
  match n.kind with
  | Box g -> gshift g d
  | _ -> ()
let nmove n p = nshift n V2.(p - n.info#pos)


let tns g h =
  let size = Size2.v (width g +. width h) (max (height g) (height h)) in
  let c = V2.(size/2.) in
  gshift g (V2.(center g-c));
  gshift h (V2.(center h-c+v (width g) 0.));
  { gsort = { sources = gsources g + gsources h;
              targets = gtargets g + gtargets h;
              size };
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union g.edges (shift_edges (gsources g) (gtargets g) h.edges) }

let seq g h =
  assert (gtargets g = gsources h);
  let size = Size2.v (max (width g) (width h)) (height g +. height h) in
  let c = V2.(size/2.) in
  gshift g (V2.(center g-c+v 0. (height h)));
  gshift h (V2.(center h-c));
  { gsort = { sources = gsources g;
              targets = gtargets h;
              size };
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union
              (MSet.omap (fun e ->
                   match e.tgt with
                   | Outer i -> Option.map
                                  (fun o -> {e with tgt = o})
                                  (next g (Outer i))
                   | _ -> Some e
                 ) g.edges)
              (MSet.filter
                 (fun e -> match e.src with Outer _ -> false | _ -> true)
                 h.edges) }

let gbox b s =
  { gsort = { s with size = Constants.expand s.size };
    nodes = MSet.single b;
    edges = MSet.union
              (MSet.init s.sources (fun i -> {src=Outer i; tgt=Inner(b,i)}))
              (MSet.init s.targets (fun j -> {src=Inner(b,j); tgt=Outer j})) }

let var n m f =
  let s = { sources = n ; targets = m ;
            size = Constants.varsize n m } in
  gbox { info = Info.pos0 () ; kind = Var(s,f) } s

let box g =
  gbox { info = Info.pos0 (); kind = Box g } g.gsort

type env = (name*(int*int*kvl*graph option)) list  

let of_raw e u =
  let rec build = function
    | Raw.Emp -> emp
    | Raw.Idm -> idm
    | Raw.Var f ->
       let (n,m,_,_) =
         try List.assoc f e
         with Not_found -> failwith "unknown symbol: %s" f
       in
       var n m f
    | Raw.Seq(u,v) -> seq (build u) (build v)
    | Raw.Tns(u,v) -> tns (build u) (build v)
    | Raw.Box(u) -> box (build u)
    | _ -> assert false
    (* | Raw.Gph(n,m,size,nodes,edges) -> assert false *)
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

let add_var g info s f = add_node g { info ; kind = Var(s,f) }
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
  | Var(_,_) -> assert false
  | Box h -> subst g n h


let iter_inner_iports f g =
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (nsources n) ()) g.nodes
let iter_inner_oports f g =
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (ntargets n) ()) g.nodes


let src_pos b i n =
  let p = Gg.Box2.tl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)
let tgt_pos b i n =
  let p = Gg.Box2.bl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)

let gbox g = Box2.v_mid P2.o (gsize g)
let nbox n = Box2.v_mid n.info#pos (nsize n)

let npos n = n.info#pos
let ipos g = function
  | Outer i -> src_pos (gbox g) i (gsources g)
  | Inner(n,i) -> src_pos (nbox n) i (nsources n)
let opos g = function
  | Outer i -> tgt_pos (gbox g) i (gtargets g)
  | Inner(n,i) -> tgt_pos (nbox n) i (ntargets n)

let rec draw_on (draw: canvas) g =
  let draw_node n =
    match n.kind with
    | Var(_,f) ->
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
        | Var(_,f), Var(_,g) -> f=g
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

let find p g =
  Option.map (fun x -> `N x)
    (MSet.find (fun n -> Box2.mem p (nbox n)) g.nodes)

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


let find f g =
  let r = ref `N in
  try
    iter_vertices (fun v -> if f (vinfo g v) then (r := `V v; raise Not_found)) g;
    iter_edges'' (fun e x _ -> if f x then (r := `E e; raise Not_found)) g;
    `N
  with Not_found -> !r

let get_info (s,g) = function
  | S,i -> Seq.get s i
  | I,i -> MSet.nth g.U0.ivertices i
  | E,i -> (MSet.nth g.U0.edges i).einfo
 *)
