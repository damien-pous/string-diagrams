let failwith fmt =
  Format.kasprintf failwith fmt

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

let rec iter n f x = match n with 0 -> x | n -> iter (n-1) f (f x)
let rec fold f n x = match n with 0 -> x | n -> fold f (n-1) (f n x)
