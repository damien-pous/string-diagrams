open Types
open Misc

type 'a port = Outer of int | Inner of 'a node * int
and 'a iport = 'a port
and 'a oport = 'a port
and 'a edge = { src: 'a iport; tgt: 'a oport }
and 'a kind = Var of (int*int*label) | Box of 'a graph
and 'a node = { info: 'a; kind: 'a kind }
and 'a graph = {
    sources: int;
    targets: int;
    nodes: 'a node mset;
    edges: 'a edge mset }

let ksrc = function Var(n,_,_) -> n | Box g -> g.sources 
let ktgt = function Var(_,m,_) -> m | Box g -> g.targets 

let nsrc n = ksrc n.kind
let ntgt n = ktgt n.kind

let is_empty g = MSet.is_empty g.nodes && MSet.is_empty g.edges

let out_edge g p = MSet.find (fun e -> e.src = p) g.edges
let out_free g p = out_edge g p = None 
let next g p = Option.map (fun e -> e.tgt) (out_edge g p)
let nexts g p =
  let rec dfs p (nodes,ports) =
    match next g p with
    | None -> nodes, ports
    | Some (Inner(n,_) as q) when not (Set.memq n nodes) ->
       fold (fun i -> dfs (Inner(n,i))) (ntgt n) (Set.add n nodes, Set.add q ports)
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
       fold (fun i -> dfs (Inner(n,i))) (nsrc n) (Set.add n nodes, Set.add q ports)
    | Some q -> nodes, Set.add q ports
  in dfs p (Set.empty,Set.empty)

let reaches g p q = Set.memq q (snd (nexts g p)) (* optimisable *)

let empty n m =
  { sources = n;
    targets = m;
    nodes = MSet.empty;
    edges = MSet.empty }

let idmap n =
  { sources = n;
    targets = n;
    nodes = MSet.empty;
    edges = MSet.init n (fun i -> {src=Outer i; tgt=Outer i}) }

let shift_port n = function
  | Outer i -> Outer (n+i)
  | p -> p

let shift_edges n m =
  MSet.map (fun e -> {src=shift_port n e.src; tgt=shift_port m e.tgt})

let tensor g h =
  { sources = g.sources+h.sources;
    targets = g.targets+g.targets;
    nodes = MSet.union g.nodes h.nodes;
    edges = MSet.union g.edges (shift_edges g.sources g.targets h.edges) }

let seq g h =
  assert (g.targets = h.sources);
  { sources = g.sources;
    targets = g.targets;
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

let gbox info k =
  let n,m = ksrc k, ktgt k in
  let b = { info; kind = k } in
  { sources = n;
    targets = m;
    nodes = MSet.single b;
    edges = MSet.union
              (MSet.init n (fun i -> {src=Outer i; tgt=Inner(b,i)}))
              (MSet.init m (fun j -> {src=Inner(b,j); tgt=Outer j})) }
let box x g = gbox x (Box g)
let var x n m f = gbox x (Var(n,m,f))

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

let add_node g info kind =
  let n = { info; kind } in
  { g with nodes = MSet.add n g.nodes },n

let subst g n h =
  assert (nsrc n = h.sources && ntgt n = h.targets);
  let g_n = rem_node g n in
  let remap_src p = match p with
    | Inner _ -> Some p
    | Outer _ -> prev g p
  in
  let remap_tgt p = match p with
    | Inner _ -> Some p
    | Outer _ -> next g p
  in
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
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (nsrc n) ()) g.nodes
let iter_inner_oports f g =
  MSet.iter (fun n -> fold (fun i () -> f (Inner(n,i))) (ntgt n) ()) g.nodes

let map f g =
  let f = memo f in
  let rec map g =
    { g with
      nodes = MSet.map nmap g.nodes;
      edges = MSet.map (fun e -> { src = pmap e.src; tgt = pmap e.tgt }) g.edges }
  and nmap n = { info = f n.info; kind = kmap n.kind }
  and kmap = function
    | Var(n,m,f) -> Var(n,m,f)
    | Box g -> Box (map g)
  and pmap = function
    | Outer i -> Outer i
    | Inner(n,i) -> Inner(nmap n,i)
  in map g



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

(* checking isomorphism
   naively for now: just try to match edges in all possible ways *)
let iso cmp g h =
  let rec extend1 acc r x y =
    match MSet.case r with
    | None -> Some (MSet.add (x,y) acc)
    | Some (x',y' as p, q) ->
       match x==x', y==y' with
       | true,true -> Some (MSet.union acc r)
       | false,false -> extend1 (MSet.add p acc) q x y
       | _,_ -> None
  in
  let extend1 r x y =
    match x,y with
    | Src i, Src j when i=j -> Some r
    | Inn x, Inn y -> extend1 MSet.empty r x y
    | _ -> None
  in    
  let extend r x y =
    let x,nx = x.einfo,x.neighbours in
    let y,ny = y.einfo,y.neighbours in
    if cmp x y && Seq.size nx = Seq.size ny then
      Seq.ofold2 extend1 r nx ny
    else None
  in
  let rec iso h k r =
    (* Format.printf "iso: %a %a %a@." *)
    (*   (MSet.pp (fun f (x,_) -> Info.ppe f x)) h *)
    (*   (MSet.pp (fun f (y,_) -> Info.ppe f y)) k *)
    (*   (MSet.pp (fun f (x,y) -> Format.fprintf f "%a--%a" I.pp x I.pp y)) r; *)
    match MSet.case h with
    | None -> assert (MSet.is_empty k); true
    | Some(x,h) -> MSet.exists_split (fun y k ->
                       match extend r x y with
                       | Some r -> iso h k r
                       | None -> false
                     ) k
  in
  arity g = arity h &&
    isize g = isize h &&
      esize g = esize h &&
        iso (edges g) (edges h) MSet.empty

let bbox (g: #positionned graph) =  
  let b = ref Gg.Box2.empty in
  iter_infos (fun i ->
      let d = i#radius *. 2. in 
      b := Gg.Box2.union !b (Gg.Box2.v_mid i#pos (Gg.Size2.v d d));
    ) g;
  if Gg.Box2.is_empty !b then Gg.Box2.unit else !b

let draw_on (draw: canvas) ?(iprops=false) (g: #positionned graph) =
  let iprops = if iprops && is_fullprime g then Some (width g) else None in
  let npos = Seq.lmap (fun v -> (vinfo g v)#pos) in
  let draw_source i x =
    let p = x#pos in
    let d = 2. *. x#radius in
    let fill = x#color in
    draw#box ~fill (Gg.Box2.v_mid p (Gg.Size2.v d d));
    draw#text p (string_of_int i)
  in
  let draw_ivertex x =
    let c = x#circle in
    let shape,fill =
      match iprops with
      | Some k when is_forget_point g k x ->
         draw#pentagon,
         if is_anchor g x then Gg.Color.black else x#color
      | _ -> draw#circle,x#color
    in
    shape ~fill c;
    draw#text x#pos x#label
  in
  let draw_edge x n =
    let c = x#circle in
    let fill = x#color in
    draw#path ~fill (Geometry.edge c (npos n));
    if x#get "clique" = Some "true" then draw#circle c;
    draw#text x#pos x#label
  in
  (* Geometry.set_debug draw; *)
  iter_edges draw_edge g;
  iter_sources draw_source g;
  iter_ivertices draw_ivertex g;
  (* draw#box (bbox g); *)
  (* draw#get *)
  ()

let draw ?iprops g =
  let c = new Canvas.basic in
  draw_on c ?iprops g;
  c#get

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
