let failwith fmt =
  Format.kasprintf failwith fmt
let assertk b k =
  (if not b then k()); assert b

let pp_print_sep sep f () =
  Format.pp_print_string f sep

let pp_print_list sep =
  Format.pp_print_list ~pp_sep:(pp_print_sep sep)

let rec big b z = function
  | [] -> z
  | [x] -> x
  | x::q -> b x (big b z q)

let sqr x = x *. x

let comp g f x = g (f x)

let memo f =
  let m = ref [] in
  fun x ->
  try List.assq x !m
  with Not_found ->
    let y = f x in
    m := (x,y) :: !m;
    y

let memo' n f =
  let t = Hashtbl.create n in
  (fun () -> Hashtbl.clear t),
  (fun x ->
    try Hashtbl.find t x
    with Not_found -> let y = f x in Hashtbl.add t x y; y)

let rec iter n f x = match n with 0 -> x | n -> iter (n-1) f (f x)
let rec fold f n x = match n with 0 -> x | n -> fold f (n-1) (f n x)

let forall n f = fold (fun i b -> b && f i) n true
let exists n f = fold (fun i b -> b || f i) n false

let rec unique_assq = function
  | [] -> true
  | (x,_)::q -> not (List.mem_assq x q) && unique_assq q



let float_of_string x =
  try float_of_string x
  with _ -> failwith "not a float: %s" x
    
let p2_of_string s =
  let i = String.index s ',' in
  Gg.P2.v (float_of_string (String.sub s 0 i))
    (float_of_string (String.sub s (i+1) (String.length s-i-1)))

let string_of_p2 p =
  Format.sprintf "%g,%g" (Gg.P2.x p) (Gg.P2.y p)


let marshal_copy (x: 'a): 'a =
  Marshal.(from_string (to_string x [Closures]) 0)

let can_marshal_closures =
  try marshal_copy (fun () -> true) ()
  with Failure _ -> Format.eprintf "warning: cannot Marshal closures on this platform@."; false

type 'a ref = 'a Store.Ref.t
let store = Store.create ()
let ref x = Store.Ref.make store x
let (!) x = Store.Ref.get store x
let (:=) x v = Store.Ref.set store x v
let capture () = Store.capture store
let restore = Store.restore store
