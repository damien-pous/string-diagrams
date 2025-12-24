open Gg
open Types
open Misc

type kv = string*string
type kvl = kv list

type 'a env = (name*(kvl*typs*typs*'a option)) list

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

class proxy (e: #element): element =
  object
    method get = e#get
    method set = e#set
    method unset =e#unset
    method pp_kvl = e#pp_kvl

    method sources = e#sources
    method targets = e#targets
    method nsources = e#nsources
    method ntargets = e#ntargets
    method spos = e#spos
    method tpos = e#tpos
    method sdir = e#sdir
    method tdir = e#tdir
    method styp = e#styp
    method ttyp = e#ttyp

    method pos = e#pos
    method size = e#size
    method color = e#color
    method box = e#box
    method width = e#width
    method height = e#height
    method safebox = e#safebox
    method contains = e#contains  
    method move = e#move
    method shift = e#shift
    method scale = e#scale
    method rebox = e#rebox  
    method draw = e#draw
  end

class virtual gen n m ?(pos=P2.o) ~name (l: kvl) =
  let n' = List.length n in 
  let m' = List.length m in 
  object(self)
    val mutable kvl = l
    method private kvl = kvl
    method private has k = List.mem_assoc k kvl 
    method private rem k = kvl <- List.remove_assoc k kvl 
    method private add k v = self#rem k; kvl <- (k,v)::kvl 
    method get k = List.assoc_opt k kvl
    method set k v = kvl <- (k,v)::List.remove_assoc k kvl
    method unset k = kvl <- List.remove_assoc k kvl

    val mutable pos = pos
    val mutable placed = false
    method pos = pos    
    method shift d = pos <- V2.(pos + d); placed <- true
    method move p = self#shift V2.(p-pos)

    method private update_kvl =
      if placed then self#add "pos" (string_of_p2 pos)
    method pp_kvl mode f = self#update_kvl; if mode=Full then pp_kvl f self#kvl

    method virtual private box: box
    method safebox = Box2.(v_mid pos (V2.smul 1.1 (size self#box)))

    val mutable color = Constants.gray
    method color = color
    
    method sources: typs = n
    method targets: typs = m
    method nsources = n'
    method ntargets = m'
    method styp i = List.nth n (i-1)
    method ttyp i = List.nth m (i-1)
    initializer
      (match self#get "pos" with Some p -> pos <- p2_of_string p; placed <- true | None -> ());
      color <- Constants.color' ?color:(self#get "color") name
  end


let top_pos b i n =
  let p = Gg.Box2.tl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)
let bot_pos b i n =
  let p = Gg.Box2.bl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. float_of_int n) in
  Gg.V2.add p (Gg.V2.v (d *. (float_of_int (2*i-1))) 0.)


class rectangle n m ?pos ~size ~name l =
  object(self)
    inherit gen n m ?pos ~name l as parent
    val mutable size = size
    val mutable sized = false
    method size = size    
    method width = Size2.w size 
    method height = Size2.h size
    method box = Box2.v_mid pos size
    method contains p = Box2.mem p self#box
    method scale s = size <- V2.smul s size; sized <- true
    method rebox b = pos <- Box2.mid b; size <- Box2.size b; sized <- true
    method spos i = top_pos self#box i self#nsources 
    method tpos i = bot_pos self#box i self#ntargets 
    method sdir (_: int) = V2.(zero-oy)
    method tdir (_: int) = V2.(zero-oy)
    method private fill =
      match self#get "fill" with Some c -> Constants.color c | None -> color    
    method draw (draw: canvas) =
      draw#box ~fill:self#fill self#box
    method! private update_kvl =
      parent#update_kvl;
      if sized then self#add "size" (string_of_p2 size)
    initializer
      (match self#get "size" with Some s -> size <- p2_of_string s; sized <- true | None -> ())
  end

class circle n m ?pos ~radius ~name l =
  object(self)
    inherit gen n m ?pos ~name l as parent
    val mutable radius = radius
    val mutable sized = false
    method size = V2.v (2.*.radius) (2.*.radius)
    method width = 2.*.radius
    method height = 2.*.radius
    method box = Box2.v_mid pos self#size
    method contains p = Geometry.dist p pos <= radius
    method scale s = radius <- s *. radius; sized <- true
    method rebox (_: box): unit = failwith "cannot rebox a circular area"
    method spos i =
      V2.add pos (V2.polar radius (Float.pi *. float_of_int (self#nsources-i+1) /. float_of_int (self#nsources+1)))
    method tpos i =
      V2.add pos (V2.polar radius (-. Float.pi *. float_of_int (self#ntargets-i+1) /. float_of_int (self#ntargets+1)))
    method sdir i = V2.polar (-1.) (Float.pi *. float_of_int (self#nsources-i+1) /. float_of_int (self#nsources+1))
    method tdir i = V2.polar 1. (-. Float.pi *. float_of_int (self#ntargets-i+1) /. float_of_int (self#ntargets+1))
    method private fill =
      match self#get "fill" with Some c -> Constants.color c | None -> color    
    method draw (draw: canvas) =
      draw#circle ~fill:self#fill {center = pos; radius }
    method! private update_kvl =
      parent#update_kvl;
      if sized then self#add "radius" (string_of_float radius)
    initializer
      (match self#get "radius" with Some s -> radius <- float_of_string s; sized <- true | None -> ())
  end

class point n m ?pos ~name l: element =
  object(self)
    inherit gen n m ?pos ~name l
    method size = V2.v (2.*.Constants.pradius) (2.*.Constants.pradius)
    method width = 2.*.Constants.pradius
    method height = 2.*.Constants.pradius
    method box = Box2.v_mid pos self#size
    method contains p = Geometry.dist p pos <= Constants.pradius
    method scale _ = ()
    method rebox _ = failwith "cannot rebox a point area"
    method spos _ = pos
    method tpos _ = pos
    method sdir i = V2.polar (-1.) (Float.pi *. float_of_int (self#nsources-i+1) /. float_of_int (self#nsources+1))
    method tdir i = V2.polar 1. (-. Float.pi *. float_of_int (self#ntargets-i+1) /. float_of_int (self#ntargets+1))
    method draw (draw: canvas) =
      draw#point ~color pos
  end

class polygon n m poly =
  let box = Geometry.poly_box poly in
  let pos = Box2.mid box in
  let size = Box2.size box in
  let n,spos = List.split n in
  let m,tpos = List.split m in
  let spos,sdir = List.split spos in
  let tpos,tdir = List.split tpos in
  object(self)
    inherit rectangle n m ~pos ~size ~name:"" [] as parent
    val mutable poly = poly
    val mutable spos = spos
    val mutable tpos = tpos
    method! spos i = List.nth spos (i-1)
    method! tpos i = List.nth tpos (i-1)
    method! sdir i = List.nth sdir (i-1)
    method! tdir i = List.nth tdir (i-1)
    method! contains p = Geometry.mem_poly p poly
    method! shift d =
      parent#shift d;
      poly <- Polygon.map (V2.add d) poly;
      spos <- List.map (V2.add d) spos;
      tpos <- List.map (V2.add d) tpos;
    method! scale _ = failwith "TODO: scale polygon"
    method! draw (draw: canvas) =
      draw#polygon ~fill:self#fill poly
    initializer
      self#add "fill" "tgray"
  end

let mk n m ~name l =
  if List.mem_assoc "radius" l then
    new circle n m ~radius:Constants.circle_size ~name l
  else
    match List.assoc_opt "shape" l with
    | None | Some "rect" ->
       let n' = List.length n in 
       let m' = List.length m in
       let size = Constants.var_size n' m' in
       new rectangle n m ~size ~name l
    | Some "circle" ->
       new circle n m ~radius:Constants.circle_size ~name l
    | Some "point" ->
       new point n m ~name l
    | Some s -> failwith "unknown shape: %s" s
