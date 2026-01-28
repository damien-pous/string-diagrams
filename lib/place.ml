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
        match (g#prev_opt (InnerSource(n,1)), g#prev_opt (InnerSource(n,2))),
              (g#next_opt (InnerTarget(n,1)), g#next_opt (InnerTarget(n,2))) with
        | (Some a,Some b),(Some c,Some d) ->
           n#setdirs
             [g#ipos a, g#idir a; g#ipos b, g#idir b]
             [g#opos c, g#odir c; g#opos d, g#odir d]
        | _ -> ()
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
              let d' = d -. sqrt (Box2.area x#box /. 2.) -. sqrt (Box2.area y#box /. 2.) in
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
            match g#prev_opt (InnerSource(n,i)) with
            | Some p -> 
               let dp = match p with
                 | InnerTarget(m,_) -> m#level
                 | _ -> mlevel+1
               in
               if dp < d then dp,[g#ipos p]
               else if dp = d then d,(g#ipos p::l)
               else d,l 
            | None -> d,l
          ) n#nsources (mlevel+2,[Box2.tm_pt g#box]) 
      in
      let _,nexts =
        Misc.fold (fun i (d,l) ->
            match g#next_opt (InnerTarget(n,i)) with
            | Some p ->
               let dp = match p with
              | InnerSource(m,_) -> m#level
              | _ -> 0
               in
               if dp > d then dp,[g#opos p]
               else if dp = d then d,(g#opos p::l)
               else d,l
            | None -> d,l
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
