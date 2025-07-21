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
  MSet.iter (fun x -> 
      MSet.iter (fun y -> 
          if x!=y then
            let xy = V2.sub (npos y) (npos x) in
            let d = V2.norm xy in
            if d = 0.0 then
              add x 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let i = Box2.inter x.ninfo#safebox y.ninfo#safebox in
              if not (Box2.is_empty i) then
                add x (-. sqrt (Box2.area i) /. d) xy
        ) g.nodes
    ) g.nodes;
  (* attract *)
  MSet.iter (fun e ->
      let x,y = ipos g e.src, opos g e.tgt in
      let xy = V2.sub y x in
      (match e.src with
       | Inner(n,_) -> add n (1./.float_of_int (ntargets n)) xy
       | _ -> ());
      (match e.tgt with
       | Inner(n,_) -> add n (-1./.float_of_int (nsources n)) xy
       | _ -> ())
    ) g.edges;
  Hashtbl.iter (fun x u ->
      if x.ninfo#get "fixed" <> Some "true" then
        nshift x (V2.smul s u)) t

let fix x = x.ninfo#set "fixed" "true"
let unfix x = x.ninfo#unset "fixed"
