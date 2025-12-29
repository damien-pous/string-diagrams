open Types
open Graph_type
open Gg

let nsources n = float_of_int n#nsources
let ntargets n = float_of_int n#ntargets

let fix x = x#set "fixed" "true"
let unfix x = x#unset "fixed"

let speed = 0.01
let minmove = Constants.point_radius /. 10.

let group g =
  let c = g#pos in
  MSet.iter (fun n -> n#move c) g#nodes

let generic k (g: graph) =
  MSet.iter (fun n ->
      if n#get "shape" = Some "cross" then
        n#setdirs
          [g#ipos (g#prev (InnerSource(n,1))); g#ipos (g#prev (InnerSource(n,2)))]
          [g#opos (g#next (InnerTarget(n,1))); g#opos (g#next (InnerTarget(n,2)))]
    ) g#nodes;
  let t = Hashtbl.create (MSet.size g#nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  k g add;  
  Hashtbl.fold (fun x u b ->
      if x#get "fixed" <> Some "true" && speed *. V2.norm u > minmove then
        (x#shift (V2.smul speed u); false)
      else b
    ) t true

let contract =
  generic (fun g add ->
      let c = g#pos in
      MSet.iter (fun n ->
          let u = V2.(c-n#pos) in
          let d = V2.norm u in
          if d>1.0 then add n (100./.d) u
        ) g#nodes)

let improve_old =
  generic (fun g add -> 
  let repulse = -10.0 in
  let attract = -10.0 in
  (* repulsion forces *)
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
  (* attraction forces *)
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
    ) g#edges
  )

let improve_tmp =
  generic (fun g add ->
  let repulse = 0.0 in
  let attract_link = -0.5 in
  let attract_level = -5.0 in
  let heights =                 (* heigths of each slice *)
    let rec insert d y = function
      | [] when d=1 -> [y]
      | [] -> 0.::insert (d-1) y []
      | x::q when d=1 -> max x y::q
      | x::q -> x::insert (d-1) y q
    in
    MSet.fold (fun n ->
        let d,h = n#level, Box2.h n#box in
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
                (* 0.::.. because level starts at 1 *)
  in
  (* box repulsion *)
  if repulse<>0.0 then 
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
  (* attract to level *)
  MSet.iter (fun n ->
      let d = n#level in
      let h = h d in
      let h' = V2.y n#pos in
      add n (attract_level*.(h'-.h)) V2.oy
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
    ) g#edges)

let improve =
  generic (fun g add -> 
  let repulse = 0.0 in
  let attract_x = 5.0 in
  let attract_y = 4.0 in
  let mlevel = MSet.fold (fun n -> max n#level) 0 g#nodes in    
  (* Format.eprintf "%i@." (Random.int 10); *)
  (* box repulsion *)
  if repulse<>0.0 then 
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
  MSet.iter (fun n ->
      if n#nsources=0 then
      let x,y = n#pos, g#fakeipos n#ceiling in
      let xy = V2.sub y x in
      let xy = V2.ltr (M2.scale2 (V2.v 1. 0.)) xy in
        add n attract_x xy
    ) g#nodes;
  (* vertical attraction *)
  MSet.iter (fun n ->
      let _,prevs =
        if n#nsources = 0 then 0,[g#fakeipos n#ceiling] else
        Misc.fold (fun i (d,l) ->
            let p = g#prev (InnerSource(n,i)) in
            let dp = match p with
              | InnerTarget(m,_) -> m#level
              | _ -> mlevel+1
            in
            if dp < d then dp,[g#ipos p]
            else if dp = d then d,(g#ipos p::l)
            else d,l 
          ) n#nsources (mlevel+2,[Box2.tm_pt g#box]) 
      in
      let _,nexts =
        Misc.fold (fun i (d,l) ->
            let p = g#next (InnerTarget(n,i)) in
            let dp = match p with
              | InnerSource(m,_) -> m#level
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
    ) g#nodes)
