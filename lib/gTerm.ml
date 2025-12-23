open Types
open Misc
open Element

type iport = string Types.iport
type oport = string Types.oport

type term =
  | Idm of typs
  | Var of typs * typs * name * kvl
  | Seq of term * term
  | Tns of term * term
  | Box of term * kvl
  | Gph of typs * typs * (string*(kind*kvl)) list * (iport*oport) list * kvl
and kind =
  | VNode of typs * typs * name
  | GNode of term


let rec sources = function
  | Idm n 
  | Var(n,_,_,_) 
  | Gph(n,_,_,_,_) -> n
  | Box(u,_) 
  | Seq(u,_) -> sources u
  | Tns(u,v) -> sources u @ sources v
let rec targets = function
  | Idm m 
  | Var(_,m,_,_)
  | Gph(_,m,_,_,_) -> m
  | Box(v,_) 
  | Seq(_,v) -> targets v
  | Tns(u,v) -> targets u @ targets v

let ksources = function
  | VNode(n,_,_) -> n
  | GNode t -> sources t
let ktargets = function
  | VNode(_,m,_) -> m
  | GNode t -> targets t

let seq u v =
  Typ.unify ~msg:"(sequential composition)" (targets u) (sources v);
  Seq(u,v)

let typ u n m =
  Typ.unify ~msg:"(type cast: sources)" (sources u) n;
  Typ.unify ~msg:"(type cast: targets)" (targets u) m;
  u

let of_raw (e: 'a env) u =
  let sym f l = 
    try let (l',n,m,_) = List.assoc f e in
        merge l l',n,m
    with Not_found -> failwith "unknown symbol: %s" f
  in
  let rec build = function
    | Raw.Emp -> Idm []
    | Raw.Idm t -> Idm t
    | Raw.Var(f,l) -> let (l,n,m) = sym f l in Var(n,m,f,l)
    | Raw.Seq(u,v) -> seq (build u) (build v)
    | Raw.Tns(u,v) -> Tns(build u, build v)
    | Raw.Box(u,l) -> Box(build u, l)
    | Raw.Typ(Raw.Gph(elems,l),n,m) -> gph elems l (Some (n,m))
    | Raw.Gph(elems,l) -> gph elems l None
    | Raw.Typ(u,n,m) -> typ (build u) n m
  and gph elems l t =
    let n =
      List.fold_left (fun n e ->
          match e with
          | Raw.Edge (Source i, _) -> max i n
          | _ -> n
        ) 0 elems
    in
    let m =
      List.fold_left (fun m e ->
          match e with
          | Raw.Edge (_,Target i) -> max i m
          | _ -> m
        ) 0 elems
    in
    let n,m = match t with
      | None -> Typ.flex n, Typ.flex m
      | Some(n',m') when n<=List.length n' && m<=List.length m' -> n',m'
      | _ -> failwith "arity mismatch (explicit graph)"
    in
    if m=[] then failwith "empty target graphs are not yet supported";
    let nodes =
      List.fold_left (fun nodes e ->
          match e with
          | Raw.Node (n,_,_) when List.mem_assoc n nodes -> failwith "duplicate node: %s" n
          | Raw.Node (n,u,l) ->
             let l,node = match u with
               | Raw.Var(f,_) -> let (l,n,m) = sym f l in l,VNode(n,m,f)
               | _ -> l,GNode (build u)
             in
             (n,(node,l))::nodes             
          | _ -> nodes
        ) [] elems
    in
    let kind j =
      try fst (List.assoc j nodes)
      with Not_found -> failwith "undeclared inner node: %s" j
    in
    let iport = function
      | Source i as p ->
         if i>0 then p
         else failwith "invalid outer source: %i" i
      | InnerTarget(j,i) ->
         try 
           if 1<=i && i<=List.length (ktargets (kind j)) then InnerTarget(j,i)
           else failwith "invalid inner source: %s.%i" j i
         with Not_found -> failwith "unknown source node: %i" (List.length n)
    in
    let oport = function
      | Target i as p ->
         if i>0 then p
         else failwith "invalid outer target: %i" i
      | InnerSource(j,i) ->
         try 
           if 1<=i && i<=List.length (ksources (kind j)) then InnerSource(j,i)
           else failwith "invalid inner target: %s.%i" j i
         with Not_found -> failwith "unknown target node: %i" (List.length n)
    in
    let edges =
      List.fold_left (fun edges e ->
          match e with
          | Raw.Edge(i,o) -> (iport i,oport o) :: edges
          | _ -> edges
        ) [] elems
    in
    Gph(n,m,nodes,edges,l)
  in build u       

let env (e: kvl Raw.env) =
  let rec env = function
    | [] -> []
    | (f,(l,t,b))::e ->
       let e = env e in
       match b,t with
       | None,None -> failwith "variables must be given either a type or a body (%s)" f
       | Some b,Some(n,m) ->
          (* if m=0 then failwith "empty target variables are not yet supported"; *)
          let b = typ (of_raw e b) n m in
          (f,(l,n,m,Some b))::e
       | Some b,None ->
          let b = of_raw e b in
          (f,(l,sources b,targets b,Some b))::e
       | None,Some(n,m) -> 
          (* if m=0 then failwith "empty target variables are not yet supported"; *)
          (f,(l,n,m,None))::e
  in
  env (List.rev e)

let envterm (e,t) = let e = env e in e,of_raw e t

let of_equation e (u,v) =
  let u = of_raw e u in
  let v = of_raw e v in
  Typ.unify ~msg:"equation sources" (sources u) (sources v);
  Typ.unify ~msg:"equation targets" (targets u) (targets v);
  (u,v)

let equations (e,l,placed) =
  let h,g = match List.rev l with
    | [] -> assert false
    | g::h -> h,g
  in
  let e = env e in
  e, List.rev_map (of_equation e) h, of_equation e g, placed
  
