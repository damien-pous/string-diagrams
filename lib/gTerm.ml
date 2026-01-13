open Types
open Misc
open Messages

type iport = (string,int) Types.iport
type oport = (string,int) Types.oport

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

let eqn u v =
  Typ.unify ~msg:"(equation: sources)" (sources u) (sources v);
  Typ.unify ~msg:"(equation: targets)" (targets u) (targets v);
  u,v

let sym e f l =
  match List.assoc f e with
  | l',T2((n,m),_) -> Info.merge l l',n,m
  | _ -> failwith "not an morphism variable (%s)" f
  | exception Not_found -> failwith "undefined morphism variable (%s)" f

let rec pp_raw f = function
  | Raw.One -> Format.fprintf f "1"
  | Raw.Idm -> Format.fprintf f "id"
  | Raw.Wld -> Format.fprintf f "_"
  | Raw.Var(n,_) -> Format.fprintf f "%s" n
  | Raw.Seq(u,v) -> Format.fprintf f "Seq(%a,%a)" pp_raw u pp_raw v
  | Raw.Dot(u,v) -> Format.fprintf f "Dot(%a,%a)" pp_raw u pp_raw v
  | Raw.Tns(u,v) -> Format.fprintf f "Tns(%a,%a)" pp_raw u pp_raw v
  | Raw.Typ(u,v) -> Format.fprintf f "Typ(%a,%a)" pp_raw u pp_raw v
  | Raw.Arr(u,v) -> Format.fprintf f "Arr(%a,%a)" pp_raw u pp_raw v
  | Raw.Exp(u,n) -> Format.fprintf f "Exp(%a,%i)" pp_raw u n
  | Raw.Eqn(u,v) -> Format.fprintf f "Eqn(%a,%a)" pp_raw u pp_raw v
  | Raw.Box(u,_) -> Format.fprintf f "[%a]" pp_raw u
  | Raw.Gph(_,_) -> Format.fprintf f "Gph(...)"
  | Raw.Let(n,_,_,u) -> Format.fprintf f "Let(%s,...,%a)" n pp_raw u

let rec typs e = function
  | Raw.One -> []
  | Raw.Wld -> Typ.flex 1
  | Raw.Var (n,l) ->
     if l<>[] then temporary#msg "ignoring type variable infos: %a" Info.pp_kvl l;
     (match List.assoc n e with
      | l,T1 -> [Typ.name n l]
      | _ -> failwith "not an object variable (%s)" n
      | exception Not_found -> [Typ.name n []])
  | Raw.Tns(s,t) -> typs e s @ typs e t
  | Raw.Exp(s,n) -> Typ.exp (typs e s) n
  | r -> failwith "not a tensor expression: %a" pp_raw r

let typ1 e = function
  | Raw.Arr(u,v) ->
     let n,m = typs e u, typs e v in
     if false && m=[] then failwith "empty target morphisms are not yet supported";
     n,m
  | r -> failwith "not a morphism type: %a" pp_raw r

let rec term e = function
  | Raw.Idm -> Idm (Typ.flex 1)
  | Raw.Var(f,l) ->
     (match List.assoc f e with
      | l',T2((n,m),_) -> Var(n,m,f,Info.merge l l')
      | l,T1 -> Idm [Typ.name f l]
      | _ -> failwith "not an term/object variable (%s)" f
      | exception Not_found -> Idm [Typ.name f []])       
  | Raw.Seq(u,v) -> seq (term e u) (term e v)
  | Raw.Dot(u,v) | Raw.Tns(u,v) ->
     Tns (term e u, term e v)
  | Raw.Typ(Raw.Gph(elems,l),t) ->
     let n,m = typ1 e t in
     gph e elems l (Some (n,m))
  | Raw.Typ(u,t) ->
     let n,m = typ1 e t in
     typ (term e u) n m
  | Raw.Box(u,l) -> Box(term e u, l)
  | Raw.Gph(elems,l) -> gph e elems l None
  (* cast objects to morphisms *)
  | r -> Idm (typs e r)
and gph env elems l t =
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
             | Raw.Var(f,_) -> let (l,n,m) = sym env f l in l,VNode(n,m,f)
             | _ -> l,GNode (term env u)
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

let decl e = function
  | None,None -> T1
  | Some t,Some b ->
     let n,m = typ1 e t in
     let b = term e b in
     T2((n,m),Some (typ b n m))
  | None,Some b ->
     let b = term e b in
     T2((sources b,targets b),Some b)
  | Some (Raw.Eqn(u,v)),None ->
     TE(eqn (term e u) (term e v))
  | Some t,None ->
     T2(typ1 e t,None)

let equation e = function
  | Raw.Eqn(u,v) -> eqn (term e u) (term e v)
  | r -> failwith "expecting an equation: %a" pp_raw r

let rec env k e = function
  | Raw.Arr(u,v) ->
     let h = equation e u in
     env k (("",([],TE h))::e) v
  | Raw.Let(n,l,d,u) ->
     let d = decl e d in
     env k ((n,(l,d))::e) u
  | r -> e, k e r

let eterm = env term []
let goal r = env equation [] r, ""
