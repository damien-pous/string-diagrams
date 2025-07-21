open Gg
open Types
open Misc

type kv = string*string
type kvl = kv list

type 'a env = (name*(kvl*int*int*'a option)) list

let envmap h =
  List.map (function
      | f,(l,n,m,None) -> f,(l,n,m,None)
      | f,(l,n,m,Some t) -> f,(l,n,m,Some (h t)))

let float_of_string x =
  try float_of_string x
  with _ -> failwith "not a float: %s" x
    
let p2_of_string s =
  let i = String.index s ',' in
  P2.v (float_of_string (String.sub s 0 i))
    (float_of_string (String.sub s (i+1) (String.length s-i-1)))

let string_of_p2 p =
  Format.sprintf "%g,%g" (P2.x p) (P2.y p)

let kv k v =
  (match k with
  | "radius" -> ignore(float_of_string v)
  | "pos" | "shift" | "size" -> ignore(p2_of_string v)
  | "color" -> ignore (Constants.color v)
  | _ -> ());
  k,v

let pp_kvl f l =
  if l<>[] then
    Format.fprintf f "<%a>"
      (pp_print_list ";" (fun f (k,v)-> Format.fprintf f "%s=%s" k v))
      l

class holder (l: kvl) =
  object(self)
    val mutable kvl = l
    method private kvl = kvl
    method private has k = List.mem_assoc k kvl 
    method private rem k = kvl <- List.remove_assoc k kvl 
    method private add k v = self#rem k; kvl <- (k,v)::kvl 
    method get k = List.assoc_opt k kvl
    method set k v = kvl <- (k,v)::List.remove_assoc k kvl
    method unset k = kvl <- List.remove_assoc k kvl
  end

class printer l =
  object(self)
    inherit holder l
    method private update_kvl = ()
    method pp mode f = self#update_kvl; if mode=Full then pp_kvl f self#kvl
    method pp_empty mode = mode<>Full || (self#update_kvl; self#kvl=[])
  end

class positioner pos size l =
  object(self)
    inherit printer l
    val mutable pos = pos
    val mutable placed = false
    val mutable size = size
    val mutable sized = false
    val mutable color = Constants.gray
    method pos = pos    
    method size = size
    method box = Box2.v_mid pos size
    method safebox = Box2.v_mid pos (V2.smul 1.1 size)
    method color = color
    method set_color c = color <- c
    method placed = placed    
    method move p = pos <- p; placed <- true
    method shift d = self#move V2.(pos + d)
    method scale s = size <- V2.smul s size; sized <- true
    method! private update_kvl =
      if placed then self#add "pos" (string_of_p2 pos);
      if sized then self#add "size" (string_of_p2 size)
    initializer
      (match self#get "pos" with Some p -> pos <- p2_of_string p; placed <- true | None -> ());
      (match self#get "size" with Some s -> size <- p2_of_string s; sized <- true | None -> ());
      (match self#get "color" with Some c -> color <- Constants.color c | None -> ());      
  end

let gen_at = new positioner
let gen = gen_at P2.o

let merge h k =
  List.append h
    (List.filter (fun (i,_) -> not (List.mem_assoc i h)) k)
