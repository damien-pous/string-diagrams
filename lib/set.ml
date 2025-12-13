type 'a t = 'a list

let empty = []
let single x = [x]
let rec add x = function
  | [] -> [x]
  | y::q as l -> match compare x y with
                 | -1 -> x::l
                 | 0 -> l
                 | _ -> y::add x q
let rec union h = function
  | [] -> h
  | x::q -> union (add x h) q
let rec mem x = function
  | [] -> false
  | y::q -> match compare x y with
            | -1 -> false
            | 0 -> true
            | _ -> mem x q

              
