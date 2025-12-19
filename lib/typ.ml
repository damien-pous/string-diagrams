open Misc

type t = s ref
and s = R of string | F | L of t

type ts = t list

let rec repr x = match !x with
  | R n -> Some n,x
  | F -> None,x
  | L r -> let r' = repr r in x := L (snd r'); r'

let unify1 ~msg x y =
  match repr x, repr y with
  | (Some n,_), (Some m,_) ->
     if n<>m then failwith "type mismatch (%s<>%s), %s" n m msg
  | (None,x), (_,y) 
  | (_,y), (None,x) -> if x!=y then x := L y
let rec unify ~msg h k = match h,k with
  | [],[] -> ()
  | a::h,b::k -> unify1 ~msg a b; unify ~msg h k
  | _,_ -> failwith "arity mismatch, %s" msg

let eq1 x y =
  match repr x, repr y with
  | (Some n,_), (Some m,_) -> n=m
  | (None,x), (None,y) -> x==y
  | _,_ -> false
let rec eq h k =
  match h,k with
  | [],[] -> true
  | a::h,b::k -> eq1 a b && eq h k
  | _,_ -> false

let name n = ref (R n)
let flex1 () = ref F
let flex n = List.init n (fun _ -> flex1())

let get x = fst (repr x)

let pp1 f x =
  match get x with
  | None -> Format.pp_print_char f '_'
  | Some n -> Format.pp_print_string f n
let pp f = function
  | [] -> Format.pp_print_char f '1'
  | l -> pp_print_list "*" pp1 f l
  
      
