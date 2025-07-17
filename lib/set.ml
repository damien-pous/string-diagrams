type 'a t = 'a list

let empty = []
let single x = [x]
let union = List.append
let add x q = x::q
let memq = List.memq

              
