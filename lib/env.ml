open Types

let hyps l =
  List.rev
    (List.filter_map (function
         | x, (_, TE e) -> Some (x,e)
         | _ -> None
       ) l)

let map f =
  List.map (function
      | (_, (_, T1)) as xv -> xv
      | x, (l, T2(t,b)) -> x, (l, T2(t,Option.map f b))
      | x, (l, TE(u,v)) -> x, (l, TE(f u,f v))
    )

let rec emap f = function
  | [] -> []
  | (x,(l,v))::q ->
     let q = emap f q in
     let v = 
       match v with
       | T1 -> T1
       | T2(t,b) -> T2(t,Option.map (f q) b)
       | TE(u,v) -> TE(f q u, f q v)
     in (x,(l,v))::q
