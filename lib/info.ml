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

let merge h k =
  List.append h
    (List.filter (fun (i,_) -> not (List.mem_assoc i h)) k)

let pos p = merge ["pos",string_of_p2 p]

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

class virtual printer l =
  object(self)
    inherit holder l
    method virtual private update_kvl: unit
    method pp_infos mode f = self#update_kvl; if mode=Full then pp_kvl f self#kvl
  end

class rectangle_area ?(pos=P2.o) size l =
  object(self)
    inherit printer l
    val mutable pos = pos
    val mutable placed = false
    val mutable size = size
    val mutable sized = false
    val mutable color = Constants.gray
    method pos = pos    
    method size = size
    method width = Size2.w size 
    method height = Size2.h size
    method box = Box2.v_mid pos size
    method contains p = Box2.mem p self#box
    method safebox = Box2.v_mid pos (V2.smul 1.1 size)
    method color = color
    method shift d = self#on_shift d; pos <- V2.(pos + d); placed <- true
    method move p = self#shift V2.(p-pos)
    method scale s = size <- V2.smul s size; sized <- true
    method draw_boundary (draw: canvas) = draw#box self#box
    method private on_shift _ = ()
    method private update_kvl =
      if placed then self#add "pos" (string_of_p2 pos);
      if sized then self#add "size" (string_of_p2 size)
    initializer
      (match self#get "pos" with Some p -> pos <- p2_of_string p; placed <- true | None -> ());
      (match self#get "size" with Some s -> size <- p2_of_string s; sized <- true | None -> ());
      (match self#get "color" with Some c -> color <- Constants.color c | None -> ());      
  end

class polygon_area poly l =
  let box = Geometry.poly_box poly in
  let pos = Box2.mid box in
  let size = Box2.size box in
  object
    inherit rectangle_area ~pos size l as parent
    val mutable poly = poly
    method! contains p = Geometry.mem_poly p poly
    method! private on_shift d =
      parent#on_shift d;
      poly <- Polygon.map (V2.add d) poly
    method! scale _ = failwith "TODO: scale polygon"
    method! draw_boundary (draw: canvas) = draw#polygon poly
  end

class proxy (a: area): area =
  object(self)
    method get = a#get
    method set = a#set
    method unset = a#unset
    method pp_infos = a#pp_infos
    method pos = a#pos    
    method size = a#size
    method width = a#width 
    method height = a#height
    method box = a#box
    method contains = a#contains
    method safebox = a#safebox
    method color = a#color
    method shift d = self#on_shift d; a#shift d
    method private on_shift _ = ()
    method move = a#move
    method scale = a#scale
    method draw_boundary = a#draw_boundary
  end
