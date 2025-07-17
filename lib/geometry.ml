open Gg
open Vg
open Types
open Misc

let nopic = new Canvas.void
let debug = ref nopic
let set_debug pic = debug := pic
let unset_debug () = debug := nopic


(* directed lines *)
let line' x d = { point=x; dir=V2.unit d }
let line x y = line' x (V2.sub y x)

(* circles *)
let circle center radius = { center; radius }

(** random point in [-s;s]x[-s;s] *)
let random2 s =
  V2.v
    (Float.random ~min:(-.s) ~len:(2. *. s) ())
    (Float.random ~min:(-.s) ~len:(2. *. s) ())

(** distance *)
let dist p q = V2.norm (V2.sub p q)

(** is a point inside a drawable element *)
let inside p x = Box2.mem p x#box

(** barycenter of a triangle *)
let center = function
  | [] -> V2.zero
  | l -> let p = big V2.add V2.zero l in
         let n = float_of_int (List.length l) in
         V2.(/) p n

(** centre of the incercle of a triangle  *)
let mid3 a b c =
  let ab = dist a b in
  let bc = dist b c in
  let ca = dist c a in
  V2.(/)
    (V2.add (V2.smul bc a) 
    (V2.add (V2.smul ca b) 
            (V2.smul ab c)))
    (ab +. bc +. ca)

(** angle between two vectors *)
let angle u v =
  (* TODO: use V2.angle ? *)
  let a = acos ((V2.dot u v) /. (V2.norm u *. V2.norm v)) in
  if V2.dot u (V2.ortho v) >= 0. then -.a else a

(** (signed) distance between p and directed line xy *)
let line_dist l p =  V2.dot (V2.ortho l.dir) (V2.sub p l.point)

(** orientation type *)
type lr = L|R
let swap = function L -> R | R -> L

(** is p on the left or on the right of the directed line xy *)
let side l p =  
  if V2.dot (V2.ortho l.dir) (V2.sub p l.point) >= 0. then L else R

(** intersection of two lines *)
let line_inter x y =
  let dx,dy' = x.dir, V2.ortho y.dir in
  let s = V2.dot (V2.sub y.point x.point) dy' /. V2.dot dx dy' in
  V2.add x.point (V2.smul s dx)



(** find the lines through x tangent to circle c, return the left or right tangent points *)
let tangent_point x c o =
  let two = match o with L -> 2. | R -> -2. in
  let m = P2.mid x c.center in
  let a = M2.rot2 (two *. asin (c.radius /. dist x c.center)) in
  let mc = V2.sub c.center m in
  let r = V2.add m (V2.ltr a mc) in
  r, V2.unit (V2.sub r c.center)

(** point of the circle c bissecting xcy *)
let bisect_point x y c =
  let m = mid3 x y c.center in
  let d = V2.sub m c.center in
  V2.add c.center (V2.smul (c.radius/.V2.norm d) d), d  
  (* let cx = V2.sub x c.center in *)
  (* let cy = V2.sub y c.center in *)
  (* let a = angle cx cy /. 2. in *)
  (* let a = match side (line x y) c.center with L -> a | R -> -.a in *)
  (* let d = V2.smul c.radius (V2.ltr (M2.rot2 a) (V2.unit cx)) in *)
  (* V2.add c.center d, d *)

(** returns the control point between x and y such that
    the corresponding quadratic Bezier curve visits z at time 0.5 *)
let qcurve_control_mid x y z =
  let m = P2.mid x y in
  V2.add z (V2.sub z m)

(** returns the control point between x and y such that
    the corresponding quadratic Bezier curve visits z at time 0<t<1 *)
let qcurve_control x y z t =
  V2.(/)
    (V2.sub z
       (V2.add
          (V2.smul (sqr (1.-.t)) x)
          (V2.smul (sqr t) y)
    ))
    (2.*.t*.(1.-.t))

(** find a control point between x and y such that
    the corresponding quadratic Bezier curve passes through z, orthogonally to s *)
let qcurve_control_ortho x y z s =
  let zx,zy = V2.sub z x, V2.sub z y in
  let zx,zy = V2.dot zx s, V2.dot zy s in
  let d = zx -. zy in
  if Float.abs d < 0.00001 then
    (* t=0.5 is the solution when d=0 *)
    Some (qcurve_control_mid x y z)
  else
  let delta = zx *. zy in
  if delta < 0. then None
  else
    let delta = sqrt delta in
    let t1 = (zx +. delta) /. d in
    let t2 = (zx -. delta) /. d in
    match 0.<t1 && t1<1., 0.<t2 && t2<1. with
    | false,false -> None
    | true,false -> Some (qcurve_control x y z t1)
    | false,true -> Some (qcurve_control x y z t2)
    | true,true ->
       Printf.printf "Warning: two potential control points: %g and %g\n%!" t1 t2;
       Some (qcurve_control x y z t1)

(** (approximate) length of the Bezier curve from x to y with control point pt *)
let bezier_length x y pt =
  let n = 100 in
  let xp = V2.sub pt x in
  let yp = V2.sub pt y in
  let pos t = V2.add (V2.smul (sqr (1.-.t)) xp) (V2.smul (sqr t) yp) in
  let t i = float_of_int i /. float_of_int n in
  let l = ref 0.0 in
  let p = ref (pos (t 0)) in
  for i = 1 to n-1 do
    let p' = pos (t i) in
    l := !l +. dist !p p';
    p := p';
  done;
  !l

(** look for a value x in [a;b] minimising [f x] *)
let minimize a b f =
  let n = 50 in
  let c = (a+.b)/.2. in
  let fc = f c in
  let m = ref (fc, c) in
  for i = 0 to n do
    let t = a +. (b -. a)*.float_of_int i /. float_of_int n in
    let ft = f t in
    if ft < fst !m then m := (ft,t)
  done;
  snd !m

let start x = P.empty |> P.sub x

let classify c x y =
  let d = line_dist (line x y) c.center in
  if d > c.radius then `L
  else if d < -. c.radius then `R
  else
    let dxy = dist x y in
    let e = V2.dot (V2.sub c.center x) (V2.sub y x) /. dxy in
    if 0. < e && e < dxy && c.radius < min (dist x c.center) (dist y c.center) then `C else `N

let xcurve  c ?(lx=false) ?(ly=false) x y =
  match
    match classify c x y with
    | `N -> `N
    | `R when false && (lx || ly) -> `C
    | `R when (lx || ly) ->
       let b,d = bisect_point x y c in
       let tx,_ = tangent_point x c R in
       let ty,_ = tangent_point y c L in
       let l = line' b (V2.ortho d) in
       let cx = line_inter l (line x (if lx then c.center else tx)) in
       let cy = line_inter l (line y (if ly then c.center else ty)) in
       !debug#segment cx cy;
       !debug#point cx;
       !debug#point cy;
       let b = P2.mid cx cy in
       `B(cx,b,cy)
    | l when true ->
       let tx,dx = tangent_point x c L in
       let ty,dy = tangent_point y c R in
       !debug#segment x tx;
       !debug#segment y ty;
       let m = mid3 x y c.center in
       !debug#line (line m c.center);
       let d = V2.add dx dy in
       if V2.norm2 d > 0.5 then `D d
       else if false then `T (tx,angle dx dy,ty)
       else
         let _ = !debug#point ~color:Color.blue m in
         if l<>`R then `D (V2.sub c.center m) else `D (V2.sub m c.center) 
         
    | _ ->
       (* let m = P2.mid x y in *)
       (* V2.add (V2.sub c m) (V2.ortho (V2.sub y x)) *)
       let xy' = V2.ortho (V2.sub y x) in
       let u = V2.unit xy' in
       let ru = V2.smul c.radius u in
       let ax,ay = angle u (V2.sub x c.center), angle u (V2.sub y c.center) in
       let a =
         minimize ay ax
           (fun a ->
             let ru = V2.ltr (M2.rot2 a) ru in
             match qcurve_control_ortho x y (V2.add c.center ru) ru with
             | Some p -> bezier_length x y p
             | None -> Float.infinity)
       in `D (V2.ltr (M2.rot2 a) ru)
  with
    | `N -> fun x -> x
    | `C -> !debug#point ~color:Constants.gray c.center; P.qcurve c.center y
    | `T(tx,angle,ty) ->
       !debug#point ~color:Constants.gray tx; 
       !debug#point ~color:Constants.gray ty; 
       fun p -> 
       P.line tx p |> P.earc ~cw:true ~angle (Size2.v c.radius c.radius) ty |> P.line y
    | `B(cx,b,cy) ->
       !debug#point ~color:Constants.gray cx; 
       !debug#point ~color:Constants.gray cy; 
       (* fun p -> P.line cx p |> P.line b |> P.line cy |> P.line y *)
       fun p -> P.qcurve cx b p |> P.qcurve cy y
    | `D d -> 
       let ru = V2.smul c.radius (V2.unit d) in
       !debug#point ~color:Constants.gray (V2.add c.center ru); 
       match qcurve_control_ortho x y (V2.add c.center ru) ru with
       | Some p ->
          !debug#point ~color:Constants.gray p; 
          P.qcurve p y
       | None -> fun x -> x
let curve c ?lx ?ly x y =
  start x |> xcurve c ?lx ?ly x y

let edge1 c x =
  let xc = V2.sub c.center x in
  let u = V2.smul c.radius (V2.unit xc) in
  let y = V2.add c.center u in
  let s = V2.smul (1.2 *. Float.pow (dist x c.center /. c.radius) 0.3) (V2.ortho u) in
  start x |> P.qcurve (V2.add y s) y |> P.qcurve (V2.sub y s) x |> P.close
  
let edge2 c x y =
  let d = line_dist (line' x (V2.ortho (V2.sub x y))) c.center in 
  match classify c x y with
  | `C when d <= c.radius || d >= dist x y -. c.radius -> P.empty
  | _ -> start x |> xcurve c x y |> xcurve c y x |> P.close

let edge3 c x y z =
  let classify = classify c in
  let curve = xcurve c in
  let m = center [x;y;z] in
  let ok p = dist c.center p > c.radius && dist c.center m < dist p m in
  let cycle x y z = start x |> curve x z |> curve z y |> curve y x |> P.close in
  let cycle' x y z = start x |> curve ~ly:true x z |> curve ~lx:true z y |> curve y x |> P.close in
  let cycle'' x y z = start x |> curve ~lx:true ~ly:true x z |> curve ~lx:true ~ly:true z y |> curve ~lx:true ~ly:true y x |> P.close in
  let path x y z = 
    match classify x y, classify y z, classify z x with 
    | `L,`L,`L -> cycle'' x y z
    |  _,`L,`L -> cycle' x y z
    | `L, _,`L -> cycle' y z x
    | `L,`L, _ -> cycle' z x y
    | `R,`R,`R -> cycle'' x z y
    |  _,`R,`R -> cycle' y x z
    | `R, _,`R -> cycle' z y x
    | `R,`R, _ -> cycle' x z y
    | `C,`C,`C -> cycle  x y z
    |  _,`C,`C when ok z -> cycle x y z 
    | `C, _,`C when ok x -> cycle x y z
    | `C,`C, _ when ok y -> cycle x y z
    | _ -> P.empty
  in
  match side (line x y) z with
  | L -> path x y z
  | R -> path x z y
  
let edge_gen c l =
  List.fold_right
    (fun v acc -> acc |> P.sub (c.center) |> P.line v)
    l
    (P.empty |> P.circle c.center c.radius)

let edge c l =
  let fallback p =
    if P.is_empty p then edge_gen c l else p
  in
  match l with
  | [x] -> fallback (edge1 c x)
  | [x;y] -> fallback (edge2 c x y)
  | [x;y;z] -> fallback (edge3 c x y z)
  | _ -> edge_gen c l


let scale_box s b =
  let c = Box2.mid b in
  let m = M3.mul (M3.move2 c)
         (M3.mul (M3.scale2 (V2.v s s))
                 (M3.move2 (V2.neg c))) in
  Box2.tr m b

