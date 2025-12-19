open Types
open Graph_type
open Gg

let nsources n = float_of_int n#nsources
let ntargets n = float_of_int n#ntargets

let improve_placement s (g: graph) =
  let repulse = -10.0 in
  let attract = -10.0 in
  let t = Hashtbl.create (MSet.size g#nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  (* repulse *)
  MSet.iter (fun x ->
      let dl = V2.x (Box2.ml_pt x#safebox) -. V2.x (Box2.ml_pt g#box) in
      let dr = V2.x (Box2.mr_pt x#safebox) -. V2.x (Box2.mr_pt g#box) in
      add x (repulse*.(1./. (dr*.dr) -. 1./.(dl*.dl))) V2.ox;
      let db = V2.y (Box2.bm_pt x#safebox) -. V2.y (Box2.bm_pt g#box) in
      let dt = V2.y (Box2.tm_pt x#safebox) -. V2.y (Box2.tm_pt g#box) in
      add x (repulse*.(1./. (dt*.dt) -. 1./.(db*.db))) V2.oy;
      MSet.iter (fun y -> 
          if x<>y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let d' = d -. sqrt (Box2.area x#safebox /. 2.) -. sqrt (Box2.area y#safebox /. 2.) in
              add x (repulse/.(d'*.d'*.d)) xy
              (* let i = Box2.inter x#safebox y#safebox in *)
              (* if not (Box2.is_empty i) then *)
              (*   add x (-. sqrt (Box2.area i) /. d) xy *)
        ) g#nodes
    ) g#nodes;
  (* attract *)
  MSet.iter (fun (i,o) ->
      let x,y = g#ipos i, g#opos o in
      let xy = V2.sub y x in
      (match i with
       | InnerTarget(n,_) ->
          add n (-.attract/.(sqrt (V2.norm xy) *. ntargets n)) xy
       | _ -> ());
      (match o with
       | InnerSource(n,_) ->
          add n (attract/.(sqrt (V2.norm xy) *. nsources n)) xy
       | _ -> ())
    ) g#edges;
  Hashtbl.iter (fun x u ->
      if x#get "fixed" <> Some "true" then
        x#shift (V2.smul s u)) t

let rec improve_placement_depth ?(force=false) s (g: graph) =
  if force || not g#stable then
  let repulse = 0.0 in
  let attract_link = -0.5 in
  let attract_depth = -5.0 in
  let t = Hashtbl.create (MSet.size g#nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  let heights =                 (* heigths of each slice *)
    let rec insert d y = function
      | [] when d=1 -> [y]
      | [] -> 0.::insert (d-1) y []
      | x::q when d=1 -> max x y::q
      | x::q -> x::insert (d-1) y q
    in
    MSet.fold (fun n ->
        let d,h = g#depth n, Box2.h n#box in
        insert d h
      ) [] g#nodes
  in
  let h =                       (* expected vertical placement of each slice *)
    let n = List.length heights in
    let sep = (Box2.h g#box -. Misc.big (+.) (0.) heights) /. float_of_int (n+1) in
    let rec psum s = function
      | [] -> []
      | x::q -> let s = s +. sep in 
                (s +. x/.2.) :: psum (s +. x) q
    in List.nth (0.::psum (V2.y (Box2.bm_pt g#box)) heights)
                (* 0.::.. because depth starts at 1 *)
  in
  (* repulse *)
  MSet.iter (fun x ->
      MSet.iter (fun y ->
          if x<>y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let d' = d -. sqrt (Box2.area x#safebox /. 2.) -. sqrt (Box2.area y#safebox /. 2.) in
              add x (repulse/.(d'*.d'*.d)) xy
        ) g#nodes
    ) g#nodes;
  (* attract to depth *)
  MSet.iter (fun n ->
      let d = g#depth n in
      let h = h d in
      let h' = V2.y n#pos in
      add n (attract_depth*.(h'-.h)) V2.oy
    ) g#nodes;
  (* attract *)
  MSet.iter (fun (i,o) ->
      let x,y = g#ipos i, g#opos o in
      let xy = V2.sub y x in
      let xy = V2.ltr (M2.scale2 (V2.v 10. 1.)) xy in
      (match i with
       | InnerTarget(n,_) -> add n (-.attract_link /. ntargets n) xy
       | _ -> ());
      (match o with
       | InnerSource(n,_) -> add n (attract_link /. nsources n) xy
       | _ -> ())
    ) g#edges;
  (* apply forces *)
  let b =
    Hashtbl.fold (fun x u b ->
        if x#get "fixed" <> Some "true" && V2.norm2 u > 10. then
          (x#shift (V2.smul s u); false)
        else b
      ) t true
  in
  g#set_stable b;
  (* recursively place inner boxes *)
  MSet.iter (improve_placement_depth ~force s) g#inner_graphs

let rec improve_placement_depth' ?(force=false) s (g: graph) =
  if force || not g#stable then
  let repulse = 0.0 in
  let attract_x = 5.0 in
  let attract_y = 5.0 in
  let t = Hashtbl.create (MSet.size g#nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  let mdepth = MSet.fold (fun n -> max (g#depth n)) 0 g#nodes in    
  (* repulse *)
  MSet.iter (fun x ->
      MSet.iter (fun y ->
          if x<>y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let d' = d -. sqrt (Box2.area x#safebox /. 2.) -. sqrt (Box2.area y#safebox /. 2.) in
              add x (repulse/.(d'*.d'*.d)) xy
        ) g#nodes
    ) g#nodes;
  (* horizontal attraction *)
  MSet.iter (fun (i,o) ->
      let x,y = g#ipos i, g#opos o in
      let xy = V2.sub y x in
      let xy = V2.ltr (M2.scale2 (V2.v 1. 0.)) xy in
      (match i with
       | InnerTarget(n,_) -> add n (attract_x /. ntargets n) xy
       | _ -> ());
      (match o with
       | InnerSource(n,_) -> add n (-.attract_x /. nsources n) xy
       | _ -> ())
    ) g#edges;
  (* vertical attraction *)
  MSet.iter (fun n ->
      let _,prevs =
        Misc.fold (fun i (d,l) ->
            let p = g#prev (InnerSource(n,i)) in
            let dp = match p with
              | InnerTarget(m,_) -> g#depth m
              | _ -> mdepth+1
            in
            if dp < d then dp,[g#ipos p]
            else if dp = d then d,(g#ipos p::l)
            else d,l 
          ) n#nsources (mdepth+2,[Box2.tm_pt g#box]) (* TOFIX: default value might be wrong (plafonds) *)
      in
      let _,nexts =
        Misc.fold (fun i (d,l) ->
            let p = g#next (InnerTarget(n,i)) in
            let dp = match p with
              | InnerSource(m,_) -> g#depth m
              | _ -> 0
            in
            if dp > d then dp,[g#opos p]
            else if dp = d then d,(g#opos p::l)
            else d,l 
          ) n#ntargets (-1,[Box2.bm_pt g#box]) (* TOFIX: default value might be wrong (planchers) *)
      in
      let y = P2.y n#pos in
      let v = (List.fold_right (fun p -> (+.) (V2.y p -. y)) prevs 0.)
              /. float_of_int (List.length prevs)
      in
      add n (attract_y *. v) V2.oy;
      let v = (List.fold_right (fun p -> (+.) (V2.y p -. y)) nexts 0.)
              /. float_of_int (List.length nexts)
      in
      add n (attract_y *. v) V2.oy
    ) g#nodes;
  (* apply forces *)
  let b =
    Hashtbl.fold (fun x u b ->
        if x#get "fixed" <> Some "true" && V2.norm2 u > 10. then
          (x#shift (V2.smul s u); false)
        else b
      ) t true
  in
  g#set_stable b;
  (* recursively place inner boxes *)
  MSet.iter (improve_placement_depth' ~force s) g#inner_graphs

let fix x = x#set "fixed" "true"
let unfix x = x#unset "fixed"
