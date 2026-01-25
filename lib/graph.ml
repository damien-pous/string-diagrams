open Types
open Graph_type
open Misc
open Messages
open Gg

(** * misc *)

exception Incomplete_graph
exception Not_a_graph_term of string
let not_a_graph_term fmt = Format.kasprintf (fun s -> raise (Not_a_graph_term s)) fmt

let iter_positionned_edges g f =
  MSet.iter (fun (i,o) -> f (i,g#ipos i) (o,g#opos o)) g#edges
let iter_iports g f =
  fold (fun i () -> f (Source i)) g#nsources ();  
  MSet.iter (fun n -> fold (fun i () -> f (InnerTarget(n,i))) n#ntargets ()) g#nodes
let iter_oports g f =
  fold (fun i () -> f (Target i)) g#ntargets ();  
  MSet.iter (fun n -> fold (fun i () -> f (InnerSource(n,i))) n#nsources ()) g#nodes


let tcolor t =
  match Typ.get t with
  | Some (n,l) -> Info.get_color n l
  | None -> Constants.black
let icolor g i = tcolor (g#ityp i)
let ocolor g o = tcolor (g#otyp o)


(** checking isomorphism
    !! for now, only correct on connected graphs *)
let rec iso g h =
  Typ.eq g#sources h#sources &&
  Typ.eq g#targets h#targets &&
  MSet.size g#nodes = MSet.size h#nodes &&
  MSet.size g#edges = MSet.size h#edges &&
  let bisim = Hashtbl.create (2 * (MSet.size g#edges + MSet.size h#edges)) in
  let same_kind h k = match h#kind,k#kind with
    | Var f, Var g -> f=g 
    | Box h, Box k -> iso h k
    | _ -> false
  in      
  let rec iso_dn x y =
    match g#next_opt x, h#next_opt y with
    | None, None -> true
    | Some (Target i), Some (Target j) when i=j -> true
    | Some (InnerSource(n,i)), Some (InnerSource(m,j)) when i=j ->
       Hashtbl.mem bisim (n,m) ||
         let _ = Hashtbl.add bisim (n,m) () in
         same_kind n m &&
         forall n#nsources (fun i -> iso_up (InnerSource(n,i)) (InnerSource(m,i))) &&
         forall n#ntargets (fun i -> iso_dn (InnerTarget(n,i)) (InnerTarget(m,i)))
    | _ -> false
  and iso_up x y =
    match g#prev_opt x, h#prev_opt y with
    | None, None -> true
    | Some (Source i), Some (Source j) when i=j -> true
    | Some (InnerTarget(n,i)), Some (InnerTarget(m,j)) when i=j ->
       Hashtbl.mem bisim (n,m) ||
         let _ = Hashtbl.add bisim (n,m) () in
         same_kind n m &&
         forall n#nsources (fun i -> iso_up (InnerSource(n,i)) (InnerSource(m,i))) &&
         forall n#ntargets (fun i -> iso_dn (InnerTarget(n,i)) (InnerTarget(m,i)))
    | _ -> false
  in
  forall g#ntargets (fun i -> iso_up (Target i) (Target i)) &&
    forall g#nsources (fun i -> iso_dn (Source i) (Source i))
let iso_eqn (u,v) (u',v') = iso u v && iso u' v'
let rec iso_env e f =
  match e,f with
  | [],[] -> true
  | (x,(_,d))::e, (x',(_,d'))::f when x=x' ->
     iso_env e f && 
     (match d,d' with
      | T1, T1 -> true
      | T2((n,m),g), T2((n',m'),h) when x=x' && Typ.eq n n' && Typ.eq m m' ->
         (match g,h with
          | None, None -> true
          | Some g, Some h -> iso g h
          | _ -> false)
      | TE e, TE f -> iso_eqn e f
      | _ -> false
     )
  | _ -> false
let iso_envgraph (e,g) (f,h) =
  iso_env e f && iso g h
let iso_state (e,g) (f,h) =
  iso_env e f &&
    match g,h with
    | Trm g, Trm h -> iso g h
    | Eqn (e,_), Eqn (f,_) -> iso_eqn e f
    | _ -> false

(** extracting terms from graphs without empty target nodes
    (building on the depth of each node)
 *)
let to_term (g: graph) =
  if MSet.exists (fun n -> n#targets = []) g#nodes then
    failwith "empty target nodes are not supported yet";
  let rec add n j l =
    if j>n#nsources then l
    else InnerSource(n,j)::add n (j+1) l
  in
  let rec eat n j l =
    if j=n#ntargets+1 then l
    else match l with
         | [] -> not_a_graph_term "missing target"
         | i::q -> match g#prev i  with
                   | InnerTarget(n',j') when n=n' && j=j' -> eat n (j+1) q
                   | _ -> not_a_graph_term "probably needs symmetry"
  in
  let rec slice d = function
    | [] -> Term.idm[],[]
    | j::q ->
         match g#prev j with
         | InnerTarget(n,i) when n#level = d ->
            if i<>1 then not_a_graph_term "did not reach first target first";
            let u,l = slice d (eat n 2 q) in
            Term.tns n#term u, (add n 1 l)            
         | _ ->
            let a = g#otyp j in
            let u,q = slice d q in
            Term.(tns (idm [a]) u),j::q
  in
  let rec up d l =
    let v,l = slice d l in
    if Term.is_id v then
      if l = List.init g#nsources (fun i -> g#next (Source(i+1))) then v
      else not_a_graph_term "sources improperly linked"
    else 
      let u = up (d+1) l in
      Term.(seq u v)
  in up 1 (List.init g#ntargets (fun i -> Target(i+1)))


(** pretty printing graphs *)
let pp mode f g = g#pp mode f
let pp_decl mode f = function
  | T1 -> ()
  | T2 ((n,m),None) -> Format.fprintf f ": %a -> %a" Typ.pp n Typ.pp m
  | T2 ((n,m),Some g) -> Format.fprintf f ": %a -> %a := %a" Typ.pp n Typ.pp m (pp mode) g
  | TE (u,v) -> Format.fprintf f ": %a ≡ %a" (pp mode) u (pp mode) v
let pp_env mode f (e: env) =
  List.iter (fun (n,(l,d)) ->
      Format.fprintf f "%s%a%a\n" n Info.pp_kvl l (pp_decl mode) d
    ) (List.rev e)
let pp_envgraph mode f (e,g) =
  Format.fprintf f "%a -- %a@." (pp_env mode) e (pp mode) g
let pp_state mode f (e,x) =
  match x with
  | Trm u -> Format.fprintf f "%a -- %a@." (pp_env mode) e (pp mode) u
  | Eqn((u,v),_) -> Format.fprintf f "%a -- %a ≡ %a@." (pp_env mode) e (pp mode) u (pp mode) v


class links n m: linked =
  object
    val ilinks = Array.make n None
    val olinks = Array.make m None
    method ilink i = ilinks.(i-1)
    method olink o = olinks.(o-1)
    method ilink_set i p = Array.set ilinks (i-1) (Some p)
    method olink_set o p = Array.set olinks (o-1) (Some p)
    method clear_tables = Array.fill ilinks 0 n None; Array.fill olinks 0 m None
  end

class leveled n m =
  object(self)
    inherit links m n as parent (* note the input/output reversal *)
    val mutable level = None
    val mutable ceiling = None
    method! clear_tables =
      parent#clear_tables;
      level <- None;
      ceiling <- None
    method level =
      match level with
      | Some l -> l
      | None ->
         let l = 1 + fold (fun i ->
                         max (match self#ilink i with
                              | Some (InnerSource(m,_)) -> m#level
                              | _ -> 0
                           )) m 0
         in level <- Some l; l
    method ceiling: fakeiport =
      match ceiling with
      | Some c -> c
      | None -> failwith "missing ceiling"
    method set_ceiling c = ceiling <- Some c
  end

(* generic pregraph *)
class gen_graph nodes edges area =
  object(self)
    inherit Element.proxy area
    inherit links area#nsources area#ntargets
    
    val mutable edges: (iport*oport) mset = edges
    val mutable nodes = nodes

    (* warning: to be called whenever the graph changes *)
    method private rebuild_tables =
      self#clear_tables;
      MSet.iter (fun n -> n#clear_tables) nodes;
      self#compute_tables
    
    method edges = edges
    method nodes = nodes
    method update nodes' edges' =
      (* TODO: sanity checks *)
      nodes <- nodes';
      edges <- edges';
      self#rebuild_tables;

    method private set_edge (i,o) =
      (match i with
      | Source i -> self#ilink_set i o
      | InnerTarget(n,i) -> n#ilink_set i o);
      (match o with
      | Target o -> self#olink_set o i
      | InnerSource(n,o) -> n#olink_set o i)
    
    method private compute_tables =
      MSet.iter self#set_edge edges;
      let next = function
        | `Left -> [],
           if self#nsources = 0 then `Right
           else `LeftI (Source 1)
        | `Right -> [],
           if self#ntargets = 0 then `Left
           else `RightO (Target self#ntargets)
        | `LeftI i -> [], `LeftO (self#next i)
        | `RightO o -> [],`RightI (self#prev o)
        | `LeftO (Target i) -> [],
           if i=1 then `Left
           else `RightO (Target (i-1))
        | `LeftO (InnerSource(n,i)) -> [],
           if i=1 then 
             if n#ntargets = 0 then `RightO(InnerSource(n,n#nsources)) (* out-dangling node *)
             else `LeftI (InnerTarget(n,1))
           else `RightO (InnerSource(n,i-1))
        | `RightI (Source i) -> [],
           if i=self#nsources then `Right
           else `LeftI (Source (i+1))
        | `RightI (InnerTarget(n,i)) -> 
           if i=n#ntargets then 
             if n#nsources = 0 then [n],`LeftI(InnerTarget(n,1)) (* in-dangling node *)
             else [],`RightO (InnerSource(n,n#nsources))
           else [],`LeftI (InnerTarget(n,i+1))
      in
      let collect s =
        let rec collect l x =
          if s=x then l else
            let h,x' = next x in
            collect (h@l) x'
        in        
        let l,x = next s in
        collect l x
      in
      let add l k =
        let n = float_of_int (List.length l+1) in
        List.iteri (fun i x -> x#set_ceiling (k (float_of_int (i+1)/.n))) l
      in
      try
      add (collect `Left) (fun p -> Source (0.5+.p/.2.));
      for i = 1 to self#nsources do
        let i' = float_of_int i in
        let d  = if i=self#nsources then 2. else 1. in
        add (collect (`RightI (Source i))) (fun p -> Source (i'+.p/.d))
      done;
      MSet.iter (fun n ->
          for i = 1 to n#ntargets-1 do
            let i' = float_of_int i in
            add (collect (`RightI (InnerTarget(n,i)))) (fun p -> InnerTarget(n,i'+.p))
          done;          
        ) nodes
      with Incomplete_graph -> ()

    method ipos = function
      | Source i -> self#spos i
      | InnerTarget(n,i) -> n#tpos i
    method opos = function
      | Target i -> self#tpos i
      | InnerSource(n,i) -> n#spos i

    method fakeipos = function
      | Source i -> self#fakespos i
      | InnerTarget(n,i) -> n#faketpos i
    method fakeopos = function
      | Target i -> self#faketpos i
      | InnerSource(n,i) -> n#fakespos i

    method idir = function
      | Source i -> self#sdir i
      | InnerTarget(n,i) -> n#tdir i
    method odir = function
      | Target i -> self#tdir i
      | InnerSource(n,i) -> n#sdir i

    method ityp = function
      | Source i -> self#styp i
      | InnerTarget(n,i) -> n#ttyp i
    method otyp = function
      | Target i -> self#ttyp i
      | InnerSource(n,i) -> n#styp i
    
    method next_opt = function
      | Source i -> self#ilink i
      | InnerTarget(n,i) -> n#ilink i
    method prev_opt = function
      | Target o -> self#olink o
      | InnerSource(n,o) -> n#olink o

    method ifree p = self#next_opt p = None
    method next p = match self#next_opt p with Some q -> q | None -> raise Incomplete_graph
    method nexts p =
      let rec dfs p (nodes,ports) =
        match self#next_opt p with
        | None -> nodes, ports
        | Some (InnerSource(n,_) as q) when not (MSet.mem n nodes) ->
           fold (fun i -> dfs (InnerTarget(n,i))) n#ntargets (MSet.add n nodes, MSet.add q ports)
        | Some q -> nodes, MSet.add q ports
      in dfs p (MSet.empty,MSet.empty)

    method ofree p = self#prev_opt p = None
    method prev p = match self#prev_opt p with Some q -> q | None -> raise Incomplete_graph
    method prevs p =
      let rec dfs p (nodes,ports) =
        match self#prev_opt p with
        | None -> nodes, ports
        | Some (InnerTarget(n,_) as q) when not (MSet.mem n nodes) ->
           fold (fun i -> dfs (InnerSource(n,i))) n#nsources (MSet.add n nodes, MSet.add q ports)
        | Some q -> nodes, MSet.add q ports
      in dfs p (MSet.empty,MSet.empty)

    method rem_edge e =      
      assert (MSet.mem e edges);
      edges <- MSet.rem e edges;
      self#rebuild_tables;
      
    method private unsafe_rem_node n =
      assert (MSet.mem n nodes);
      nodes <- MSet.rem n nodes;
      edges <- MSet.filter
                 (function
                  | InnerTarget(m,_),InnerSource(m',_) -> m<>n && m'<>n
                  | InnerTarget(m,_),_ | _,InnerSource(m,_) -> m<>n
                  | _ -> true) edges;

    method rem_node n =
      self#unsafe_rem_node n;
      self#rebuild_tables;
      
    
    method add_edge (src,tgt) =
      assert (self#ifree src && self#ofree tgt);
      Typ.unify1 ~msg:"src-tgt" (self#ityp src) (self#otyp tgt);
      (* assert (not (self#reaches tgt src));       *)
      edges <- MSet.add (src,tgt) edges;
      self#rebuild_tables;      

    method add_node n =
      nodes <- MSet.add n nodes;
      (* self#rebuild_tables;       *)

    method replace g =
      g#move self#pos; (* TODO: resize h, or create it to fit the current box? *)
      self#update g#nodes g#edges
    
    method subst n h =
      assert (Typ.eq n#sources h#sources && Typ.eq n#targets h#targets);
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
      self#unsafe_rem_node n;
      h#move n#pos;   (* TODO: resize h, or create it to fit the current box? *)
      nodes <- MSet.union nodes h#nodes;
      edges <- MSet.union edges new_edges;
      self#rebuild_tables;
      
    method unbox n =
      match n#kind with
      | Var _ -> assert false
      | Box g -> self#subst n g

    (* textual pretty printing *)
    method private pp_iport f = function
      | Source i -> Format.fprintf f "%i" i
      | InnerTarget(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n nodes) i
    method private pp_oport f = function
      | Target i -> Format.fprintf f "%i" i
      | InnerSource(n,i) -> Format.fprintf f "n%i.%i" (MSet.index n nodes) i
    method private pp_node mode f n =
      match n#kind with
      | Var x -> Format.fprintf f "%s" x
      | Box g -> g#pp mode f
    method private pp_ mode f =
      let first = ref true in
      Format.fprintf f "{";
      MSet.iteri (fun i n ->
          if not !first then Format.fprintf f ",\n "; first := false;
          Format.fprintf f "n%i%t: %a" i (n#pp_kvl mode) (self#pp_node mode) n;
        ) nodes;
      MSet.iter (fun (s,t) ->
          if not !first then Format.fprintf f ",\n "; first := false;
          Format.fprintf f "%a -> %a" self#pp_iport s self#pp_oport t;
        ) self#edges;
      Format.fprintf f "}%t: %a -> %a" (self#pp_kvl mode) Typ.pp self#sources Typ.pp self#targets
    method pp mode f =
      match mode with
      | Rocq -> Term.pp_rocq f self#term
      | Term -> Term.pp f self#term
      | TermIfPossible ->
         (try Term.pp f self#term
          with Not_a_graph_term _ | Incomplete_graph -> self#pp_ Sparse f)
      | _ -> self#pp_ mode f

    method term = to_term (self:>graph)

    
    method edge_curve (i,o) =
      let p,ui = self#ipos i, self#idir i in
      let q,uo = self#opos o, self#odir o in
      let d = V2.(norm (q-p)) /. 3. in
      V2.(p,p+smul d ui,q-smul d uo,q)

    method private draw_interface (draw: canvas) =
      Misc.fold (fun i () ->
          let t = self#styp i in
          match Typ.get t with
          | Some (n,_) -> 
             let color = tcolor t in
             let p = V2.(self#spos i - smul Constants.fontsize (self#sdir i)) in
             draw#text ~color p n
          | None -> ()
        ) self#nsources ();
      Misc.fold (fun i () ->
          let t = self#ttyp i in
          match Typ.get t with
          | Some (n,_) -> 
             let color = tcolor t in
             let p = V2.(self#tpos i + smul Constants.fontsize (self#tdir i)) in
             draw#text ~color p n
          | None -> ()
        ) self#ntargets ();
    
    method! draw (draw: canvas) =
      let draw_node n =
        n#draw draw;
        iter_iports self (fun p ->
            if self#ifree p then
              draw#point ~color:(icolor self p) (self#ipos p)
          );
        iter_oports self (fun p ->
            if self#ofree p then
              draw#point ~color:(ocolor self p) (self#opos p)
          );
        if false && n#nsources=0 then
          draw#segment (n#pos, self#fakeipos n#ceiling)
      in
      let draw_edge e =
        let color = icolor self (fst e) in
        draw#curve ~color (self#edge_curve e)      
      in
      area#draw draw;
      if Constants.editor && not (self#has "fill") then self#draw_interface draw;
      MSet.iter draw_edge edges;
      MSet.iter draw_node nodes

    (* careful: since we use a proxy rather than plain inheritance,
       we need to redefine #move so that it calls the overriden #shift *)
    method! move p = self#shift V2.(p-self#pos)
    method! shift d =
      area#shift d;
      MSet.iter (fun n -> n#shift d) nodes

    method inner_graphs =
      MSet.fold (fun n a -> match n#kind with
                            | Box g -> MSet.add g a
                            | _ -> a) MSet.empty nodes

    val mutable stable = false
    val mutable on_stabilize = (fun () -> true)
    method on_stabilize k = on_stabilize <- k
    method improve ~force =
      (* self#improve_shape;       *)
      let b =
        if (force || not stable) then (
          let b = 
            match self#get "place" with
            | Some "locked" -> false
            | Some "contract" -> Place.contract (self:>graph)
            | _ -> Place.improve (self:>graph)
          in
          if not stable && b then (
            let k = on_stabilize in
            on_stabilize <- (fun () -> true);
            stable <- k();
          ) else stable <-false;
          stable
        ) else true
      in
      MSet.fold (fun g b -> g#improve ~force && b) b self#inner_graphs

    initializer
      self#rebuild_tables
  end


let rectangle_graph n m nodes edges ?pos size l =
  new gen_graph nodes edges
    (new Element.rectangle n m ?pos ~size ~name:"" l)


let polygon_graph n m nodes edges poly =
  new gen_graph nodes edges
    (new Element.polygon n m poly)


(** * algebra of graphs *)

(* empty graph of type n->m *)
let empty n m =
  rectangle_graph
    n m
    MSet.empty
    MSet.empty
    (Constants.empty_size (List.length n) (List.length m))
    []
let emp () = empty [] []

(* identity graph (of type a->a) *)
let idm n =
  let n' = List.length n in
  rectangle_graph
    n n
    MSet.empty
    (fold (fun i -> MSet.add (Source i,Target i)) n' MSet.empty)
    (Constants.idm_size n')
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
let tns g h =
  let size = Size2.v (g#width +. h#width) (max g#height h#height) in
  g#move (P2.v (-. h#width /. 2.) 0.);
  h#move (P2.v (g#width /. 2.) 0.);
  rectangle_graph
    (g#sources @ h#sources) (g#targets @ h#targets)
    (MSet.union g#nodes h#nodes)
    (MSet.union g#edges (shift_edges g#nsources g#ntargets h#edges))
    size
    [] 

(* sequential composition *)
let seq g h =
  assert (Typ.eq g#targets h#sources);
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

(* variable node *)
let var_node n m name l =
  object(self)
    inherit Element.proxy (Element.mk n m ~name l) as parent
    inherit leveled (List.length n) (List.length m)
    method kind = Var name
    method! draw canvas =
      parent#draw canvas;
      if !Constants.labels then canvas#text self#pos name
    method term = Term.var n m name
  end

(* graph node *)
let graph_node g =
  object
    inherit Element.proxy g
    inherit leveled g#nsources g#ntargets
    method kind = Box g
    method term = Term.box g#term
  end

(* graph with a single node *)
let node_graph n =
  new gen_graph
    (MSet.single n)
    (MSet.union
       (MSet.init n#nsources (fun i -> (Source i, InnerSource(n,i))))
       (MSet.init n#ntargets (fun j -> (InnerTarget(n,j), Target j))))
    (new Element.rectangle n#sources n#targets
       ~pos:n#pos ~size:(Constants.expand n#size) ~name:"" [])

(* graph reduced to a variable node *)
let var n m name l = node_graph (var_node n m name l)

(* boxed graph *)
let box g =
  let n = graph_node g in
  n#set "fill" "tgray";
  node_graph n
  
(** * graphs from generalised terms *)

let of_gterm u =
  let rec build = function
    | GTerm.Idm a -> idm a
    | GTerm.Var(n,m,f,l) -> var n m f l
    | GTerm.Seq(u,v) -> seq (build u) (build v)
    | GTerm.Tns(u,v) -> tns (build u) (build v)
    | GTerm.Box(u,_) -> box (build u) (* l *)
    | GTerm.Gph(n,m,nodes,edges,l) -> gph n m nodes edges l
  and gph n m nodes edges l =
    let n',m' = List.length n, List.length m in
    let t = Hashtbl.create (List.length nodes) in
    let nodes = 
      MSet.mapl (fun (n,(k,l)) ->
          let node = match k with
            | GTerm.VNode(n,m,f) -> var_node n m f l
            | GTerm.GNode(t) -> graph_node (build t) (* l *)
          in
          Hashtbl.add t n node;
          node
        ) nodes
    in
    let iport = function
      | Source i -> if 1<=i && i<=n' then Source i,List.nth n (i-1)
                   else failwith "invalid outer source: %i" i
      | InnerTarget(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#ntargets then InnerTarget(n,i),n#ttyp i
             else failwith "invalid inner source: %s.%i" j i
         with Not_found -> failwith "unknown source node: %i" n'
    in
    let oport = function
      | Target i -> if 1<=i && i<=m' then Target i,List.nth m (i-1)
                   else failwith "invalid outer target: %i" i
      | InnerSource(j,i) ->
         try let n = Hashtbl.find t j in
             if 1<=i && i<=n#nsources then InnerSource(n,i),n#styp i
             else failwith "invalid inner target: %s.%i" j i
         with Not_found -> failwith "unknown target node: %i" m'
    in
    let edges = MSet.mapl (fun (i,o) ->
                    let i,s = iport i in
                    let o,t = oport o in
                    Typ.unify1 ~msg:"explicit edge" s t;
                    (i,o)
                  ) edges in
    let box = MSet.fold (fun n -> Box2.union n#box) Box2.empty nodes in
    let pos,size,l =
      if Box2.is_empty box then
        (* TODO: compute and use depth *)
        None,Constants.estimate_size n' m' (MSet.size nodes), l
      else
        let size = Constants.expand (Box2.size box) in
        Some (Box2.mid box), size, l
    in
    rectangle_graph n m nodes edges ?pos size l
  in build u

let graph r =
  let e,u = GTerm.eterm r in
  Env.map of_gterm e, of_gterm u
let map_term_or_equation f = function
  | Trm g -> Trm (f g)
  | Eqn((l,r),s) -> Eqn((f l,f r),s)
let state r =
  let e,x = GTerm.state r in
  let e,x = Env.map of_gterm e, map_term_or_equation of_gterm x in  
  let h = List.map snd (Env.hyps e) in
  let placed u = u#pos <> P2.o in
  let placed_x = match x with Trm g -> placed g | Eqn((l,r),_) -> placed l || placed r in
  let placed (u,v) = placed u || placed v in
  if not (placed_x || List.exists placed h) then (
    let rebox (u,v) = 
      let b = Box2.union u#box v#box in
      u#rebox b; v#rebox b
    in
    List.iter rebox h;
    (match x with
     | Trm g -> g#scale 2.0
     | Eqn((l,r),_) -> rebox (l,r); l#scale 2.0; r#scale 2.0);
    let ph = List.map (fun (l,r) -> Pad.hpad Constants.spacing [l;r]) h in
    let ph = Pad.hpad (3.*.Constants.spacing) ph in
    let g = match x with Trm g -> [g] | Eqn((l,r),_) -> [l;r] in
    let pg = Pad.hpad Constants.spacing g in
    ignore (Pad.vpad (2.*.Constants.spacing) [pg;ph])
  ); (e,x)

(* copy a graph by serialisation (is there a nicer way?) *)
let copy (g: graph) =
  let g' = 
    if can_marshal_closures then marshal_copy g
    else failwith "TODO: copy graph without marshaling"
      (* let s = Format.asprintf "%a" (pp Full) g in *)
      (* let l = Lexing.from_string s in *)
      (* let x = Parser.rawterm Lexer.token l in *)
      (* snd (graph x) *)
  in
  Typ.unify ~msg:"Graph.copy" g#sources g'#sources;
  Typ.unify ~msg:"Graph.copy" g#targets g'#targets;
  g'


exception Found_iport of iport
exception Found_oport of oport
let find g p =
  match MSet.find (fun n -> n#contains p) g#nodes with
  | Some x -> `N x
  | None -> `None
let find_ports g p =
  try
    iter_iports g (fun i -> if Geometry.mem_point p (g#ipos i) then raise (Found_iport i));
    iter_oports g (fun o -> if Geometry.mem_point p (g#opos o) then raise (Found_oport o));
    find g p
  with
  | Found_iport i -> `I i
  | Found_oport o -> `O o

let create_box (g: graph) p =
  let debug_msg fmt = debug_msg "box" fmt in
  let edge_error e msg =
    temporary#curve ~color:Color.red (g#edge_curve e);
    error msg
  in              
  let p = Geometry.clockwise p in
  let cuts = Polygon.fold2 p (fun ij acc ->
                 MSet.fold (fun e acc ->
                     match Geometry.cintersection ij (g#edge_curve e) with
                     | Some(x,(L|E),u) -> `S (e,(x,u)) :: acc
                     | Some(x,R,u) -> `T (e,(x,u)) :: acc
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
    | [] -> if true then warning "ignored empty box creation" else [],[]
    | [`S s] -> if true then warning "empty target boxes unsupported yet" else s,[]
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
  let f ((i,_),p) = (g#ityp i,p) in
  let src = List.map f src in
  let tgt = List.map f tgt in
  let h = polygon_graph src tgt nodes_in edges_in p in
  let b = graph_node h (* [] *) in
  debug_msg "%t" (h#pp Full);
  g#update (MSet.add b nodes_out) (edges_out b);
  b,h

let image (g: graph) =
  let c = new Canvas.basic in
  g#draw c;
  (c#get,g#box)
