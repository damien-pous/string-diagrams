open Types
open Graph_type
open Gg

let improve_placement s (g: graph) =
  let t = Hashtbl.create (MSet.size g#nodes) in
  let add x s v =
    let v = V2.smul s v in
    match Hashtbl.find t x with
    | u -> Hashtbl.replace t x (V2.add u v)
    | exception Not_found -> Hashtbl.add t x v
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
              let i = Box2.inter x#safebox y#safebox in
              if not (Box2.is_empty i) then
                add x (-. sqrt (Box2.area i) /. d) xy
        ) g#nodes
    ) g#nodes;
  (* attract *)
  MSet.iter (fun (i,o) ->
      let x,y = g#ipos i, g#opos o in
      let xy = V2.sub y x in
      (match i with
       | InnerTarget(n,_) -> add n (1./.float_of_int n#targets) xy
       | _ -> ());
      (match o with
       | InnerSource(n,_) -> add n (-1./.float_of_int n#sources) xy
       | _ -> ())
    ) g#edges;
  Hashtbl.iter (fun x u ->
      if x#get "fixed" <> Some "true" then
        x#shift (V2.smul s u)) t

let fix x = x#set "fixed" "true"
let unfix x = x#unset "fixed"
