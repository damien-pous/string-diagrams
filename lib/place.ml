open Graph
open Gg

let improve_placement s g =
  let t = Hashtbl.create (MSet.size g.nodes) in
  let add x nx s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | _,u -> Hashtbl.replace t x (nx,V2.add u v)
    | exception Not_found -> Hashtbl.add t x (nx,v)
  in
  (* repulse *)
  MSet.iter (fun nx -> let x = nx.ninfo in 
      MSet.iter (fun ny -> let y = ny.ninfo in
          if x!=y then
            let xy = V2.sub y#pos x#pos in
            let d = V2.norm xy in
            if d = 0.0 then
              add x nx 1.0 (V2.ltr (M2.rot2 (Random.float (2.*.Float.pi))) V2.ox)
            else
              let i = Box2.inter x#safebox y#safebox in
              if not (Box2.is_empty i) then
                add x nx (-. sqrt (Box2.area i) /. d) xy
        ) g.nodes
    ) g.nodes;
  (* attract *)
  MSet.iter (fun e ->
      let x,y = ipos g e.src, opos g e.tgt in
      let xy = V2.sub y x in
      (match e.src with
       | Inner(n,_) -> add n.ninfo n 1. xy
       | _ -> ());
      (match e.tgt with
       | Inner(n,_) -> add n.ninfo n (-1.) xy
       | _ -> ())
    ) g.edges;
  Hashtbl.iter (fun x (n,u) ->
      if x#get "fixed" <> Some "true" then
        nshift n (V2.smul s u)) t

let fix x = x.ninfo#set "fixed" "true"
let unfix x = x.ninfo#unset "fixed"
