open Types
open Graph_type
open Gg

let repulse = -10.0
let attract = -10.0

let improve_placement s (g: graph) =
  Format.eprintf "%f@." s;
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
       | InnerTarget(n,_) -> add n (-.attract/.(sqrt (V2.norm xy) *. float_of_int n#targets)) xy
       | _ -> ());
      (match o with
       | InnerSource(n,_) -> add n (attract/.(sqrt (V2.norm xy) *. float_of_int n#sources)) xy
       | _ -> ())
    ) g#edges;
  Hashtbl.iter (fun x u ->
      if x#get "fixed" <> Some "true" then
        x#shift (V2.smul s u)) t

let fix x = x#set "fixed" "true"
let unfix x = x#unset "fixed"
