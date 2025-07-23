open Gg
open Vg

type point = P2.t

type t = { points: point list; start: point } 

let start start = { points=[]; start }

let last p = match p.points with
  | [] -> p.start
  | q::_ -> q

let close i j =
  V2.norm2 (V2.sub i j) <= Constants.pradius

let extend p q =
  (* or, up to some threshold? *)
  if close q (last p) || close q p.start then p
  else { p with points=q::p.points }

let to_path p = List.fold_right P.line p.points (P.empty |> P.sub p.start) |> P.close

let fold2 p f x =
  let last,x = List.fold_right (fun q' (q,acc) -> q', f (q,q') acc) p.points (p.start,x) in
  f (last,p.start) x

let fold3 p f x =
  match p.points with
  | [] -> x
  | start'::_ ->
     let last',last,x =
       List.fold_right (fun q'' (q',q,acc) -> q'', q', f (q,q',q'') acc) p.points
         (p.start,start',x)
     in
     f (last,last',p.start) x

(* let p = List.fold_left extend (start 0) [1;2;3;4;5;0];; *)
(* let _ = fold2 p (fun p q -> q@[p]) [] *)
(* let _ = fold3 p (fun p q -> q@[p]) [] *)

let is_degenerate p = List.length p.points <= 1
let is_simple p = not (is_degenerate p) (* TOIMPROVE *)

let get3 p =
  match p.points with
  | [] | [_] -> failwith "degenerate polynom"
  | u::v::_ -> v,u,p.start

let least cmp p =
  if is_degenerate p then failwith "degenerate polynom";
  let cmp (_,x,_) (_,y,_) = cmp x y in
  let min m n = if cmp m n < 0 then m else n in
  fold3 p min (get3 p)

let rev p = { p with points=List.rev p.points }
