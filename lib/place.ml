open Graph
open Gg

let improve_placement s g =
  let t = Hashtbl.create (MSet.size g.nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
  in
  (* repulse *)
  MSet.iter (fun x -> let x = x.info in 
      MSet.iter (fun y -> let y = y.info in
          if x!=y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let i = Box2.inter x#safebox y#safebox in
              if not (Box2.is_empty i) then
                add x (-. sqrt (Box2.area i) /. d) xy
        ) g.nodes
    ) g.nodes;
  (* attract *)
  MSet.iter (fun e ->
      let x,y = ipos g e.src, opos g e.tgt in
      let xy = V2.sub y x in
      (match e.src with
       | Inner(n,_) -> add n.info 1. xy
       | _ -> ());
      (match e.tgt with
       | Inner(n,_) -> add n.info (-1.) xy
       | _ -> ())
    ) g.edges;
  Hashtbl.iter (fun x u ->
      if x#get "fixed" <> Some "true" then
        x#shift (V2.smul s u)) t

let fix x = x.info#set "fixed" "true"
let unfix x = x.info#unset "fixed"

let scale k g =
  let s = g.gsort in
  s.size <- V2.smul k s.size
