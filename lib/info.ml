open Misc

type kv = string*string
type kvl = kv list

let kv k v =
  (match k with
  | "radius" -> ignore(float_of_string v)
  | "pos" | "shift" | "size" -> ignore(p2_of_string v)
  | "color" -> ignore (Constants.color v)
  | _ -> ());
  k,v

let merge h k =
  List.append h
    (List.filter (fun (i,_) -> not (List.mem_assoc i h)) k)

let pos p = merge ["pos",string_of_p2 p]

let pp_kvl f l =
  if l<>[] then
    Format.fprintf f "<%a>"
      (pp_print_list ";" (fun f (k,v)-> Format.fprintf f "%s=%s" k v))
      l

let mem = List.mem_assoc
let get = List.assoc
let get_opt = List.assoc_opt

let get_color n l =
  match get "color" l with
  | c -> Constants.color c
  | exception Not_found -> Constants.id_color n

class gen (l: kvl) =
  object(self)
    val kvl = ref l
    method has k = List.mem_assoc k !kvl 
    method get k = List.assoc_opt k !kvl
    method set k v = self#unset k; kvl := (k,v) :: !kvl 
    method unset k = kvl := List.remove_assoc k !kvl
  end
