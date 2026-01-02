type 'a t = { points: 'a list; start: 'a } 

let map f p = { points = List.map f p.points; start = f p.start }

let start start = { points=[]; start }

let last p = match p.points with
  | [] -> p.start
  | q::_ -> q

let extend p q =
  (* or, up to some threshold? *)
  if q = last p || q = p.start then p
  else { p with points=q::p.points }

let to_path p = Vg.(List.fold_right P.line p.points (P.empty |> P.sub p.start) |> P.close)

let fold1 p f x =
  List.fold_right f p.points (f p.start x)

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

let get3 p =
  match p.points with
  | [] | [_] -> failwith "degenerate polynom"
  | u::v::_ -> v,u,p.start

let least_point cmp p =
  List.fold_left (fun m i -> if cmp i m < 0 then i else m) p.start p.points
  
let least_triple cmp p =
  if is_degenerate p then failwith "degenerate polynom";
  let cmp (_,x,_) (_,y,_) = cmp x y in
  let min m n = if cmp m n < 0 then m else n in
  fold3 p min (get3 p)

let rev p = { p with points=List.rev p.points }

let triangle a b c = { start=a; points=[b;c] }

let rec split_last = function
  | [] -> assert false
  | [x] -> [],x
  | x::q -> let q,z = split_last q in x::q,z

let rotate p =
  if p.points = [] then p
  else let points,start = split_last p.points in
       { start; points=p.start::points}
  
let filter f p =
  let rec walk a = function
    | b::(c::q as cq) ->
       if f a b c then b::walk b cq
       else walk c q
    | l -> l
  in
  rotate {p with points=walk p.start p.points}
