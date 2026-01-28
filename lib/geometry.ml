open Gg
open Types
open Misc
open Messages

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

(** intersection of two lines *)
let line_inter x y =
  let dx,dy' = x.dir, V2.ortho y.dir in
  let s = V2.dot (V2.sub y.point x.point) dy' /. V2.dot dx dy' in
  V2.add x.point (V2.smul s dx)



(** orientation type *)
type lr = L|E|R
let swap = function L -> R | E -> E | R -> L
let lr r = 
  if r=0.0 then E
  else if r<0.0 then R
  else L

let cross u v = V2.x u *. V2.y v -. V2.y u *. V2.x v

(** orientation of three points (positive iff C is on the left of AB) *)
let orient_ a b c = cross (V2.sub b a) (V2.sub c a)

(** exported function (L iff C is on the left of AB) *)
let orient a b c = lr (orient_ c a b)

(** is p on the left or on the right of the directed line xy *)
let side l p = orient l.point (V2.add l.point l.dir) p

(** oriented intersection of two segments (allowing A,C,D, excluding B) *)
let intersection (a,b) (c,d) =  
  let oa = orient_ c d a in
  let ob = orient_ c d b in
  let oc = orient a b c in          
  let od = orient a b d in
  if ob <> 0.0 && lr oa <> lr ob && oc<>od then
    Some(V2.(smul (1./.(ob-.oa)) (smul ob a - smul oa b)), oc)
  else None

(** point of the cubic Bezier curve (a,b,c,d) at time t\in[0;1] *)
let cpoint t a b c d =
  let t' = 1.-.t in
  let t2 = t*.t in
  let t'2 = t'*.t' in
  V2.(smul (t'*.t'2) a + smul (3.*.t*.t'2) b + smul (3.*.t2*.t') c + smul (t2*.t) d)

let cintersection ab (c,x,y,d) =
  let step = Constants.point_radius /. dist c d in
  let rec walk t p =
    let t = min 1. (t +. step) in
    if t=1. then None
    else let q = cpoint t c x y d in
         match intersection ab (p,q) with
         | None -> walk t q
         | Some(p,o) -> Some(p,o,V2.(unit (q-p)))
  in walk 0. c

(** ensuring a polygon is presented clockwise *)
let clockwise p =
  let attempt cmp k p =
    let x,y,z = Polygon.least_triple cmp p in
    match orient x y z with
    | L -> Polygon.rev p
    | R -> p
    | E -> k p
  in
  let swap p = P2.v (P2.y p) (P2.x p) in
  let cmp1 = V2.compare in
  let cmp2 x y = V2.compare (swap x) (swap y) in  
  let cmp3 x y = cmp1 y x in
  let cmp4 x y = cmp2 y x in
  let fail p = warning "could not orient polygon"; p in
  if Polygon.is_degenerate p then p
  else attempt cmp1 (attempt cmp2 (attempt cmp3 (attempt cmp4 fail))) p

(** membership of a point in a polygon *)
let mem_poly a p =
  let xmin = P2.x (Polygon.least_point V2.compare p) in
  if P2.x a < xmin then false else
    let b = P2.v (xmin -. 1.) 0. in
    let isect (c,d) =
      let oa = orient c d a in
      let ob = orient c d b in
      let oc = orient a b c in          
      let od = orient a b d in
      if oa <> ob && oc<>od then oc
      else E
    in         
    0 <> Polygon.fold2 p (fun ij n -> 
             match isect ij with
             | L -> n-1
             | R -> n+1
             | E -> n
           ) 0
let mem_point a p = V2.(norm2 (p-a)) <= sqr (2. *. Constants.point_radius)

let poly_box p =
  Polygon.fold1 p (fun x b -> Box2.add_pt b x) Box2.empty

let smoothen =
  Polygon.filter
    (fun a b c ->
      let ab=dist a b in
      let ac=dist a c in
      let bc=dist b c in
      ab > 3.0 && bc > 3.0 && 1.8 *. ac > dist a b +. dist b c
    )

(* TMP

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

 *)

let scale_box s b =
  let c = Box2.mid b in
  let m = M3.mul (M3.move2 c)
         (M3.mul (M3.scale2 (V2.v s s))
                 (M3.move2 (V2.neg c))) in
  Box2.tr m b
