open Gg
open Types
open Misc

class proxy (e: #element): element =
  object
    method has = e#has
    method get = e#get
    method set = e#set
    method unset =e#unset
    method pp_kvl = e#pp_kvl

    method sources = e#sources
    method targets = e#targets
    method nsources = e#nsources
    method ntargets = e#ntargets
    method fnsources = e#fnsources
    method fntargets = e#fntargets
    method spos = e#spos
    method faketpos = e#faketpos
    method fakespos = e#fakespos
    method tpos = e#tpos
    method sdir = e#sdir
    method tdir = e#tdir
    method styp = e#styp
    method ttyp = e#ttyp
    method setdirs = e#setdirs
    method improve_shape = e#improve_shape

    method pos = e#pos
    method size = e#size
    method color = e#color
    method box = e#box
    method width = e#width
    method height = e#height
    method contains = e#contains  
    method move = e#move
    method shift = e#shift
    method scale = e#scale
    method rebox = e#rebox  
    method draw = e#draw
  end

class virtual gen n m ?(pos=P2.o) ~name (l: kvl) =
  let nsources = List.length n in 
  let ntargets = List.length m in 
  object(self)
    val fnsources = float_of_int nsources 
    val fntargets = float_of_int ntargets 

    inherit Info.gen l

    val pos = ref pos
    val placed = ref false
    method pos = !pos    
    method shift d = pos := V2.(!pos + d); placed := true
    method move p = self#shift V2.(p - !pos)

    method private update_kvl =
      if !placed then self#set "pos" (string_of_p2 !pos)
    method pp_kvl mode f = self#update_kvl; if mode=Full then Info.pp_kvl f !kvl

    method virtual fakespos: float -> point
    method virtual faketpos: float -> point
    method spos i = self#fakespos (float_of_int i)
    method tpos i = self#faketpos (float_of_int i)

    method improve_shape = ()
    method setdirs (_: (point*vector) list) (_: (point*vector) list) = ()
    
    method virtual private box: box

    val color = ref Constants.gray
    method color = !color
    
    method sources: typs = n
    method targets: typs = m
    method nsources = nsources
    method ntargets = ntargets
    method fnsources = fnsources
    method fntargets = fntargets
    method styp i = List.nth n (i-1)
    method ttyp i = List.nth m (i-1)
    val nsources = nsources
    val ntargets = ntargets
    initializer
      (match self#get "pos" with Some p -> pos := p2_of_string p; placed := true | None -> ());
      color := Info.get_color name l
  end


let top_pos b i n =
  let p = Gg.Box2.tl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. n) in
  Gg.V2.add p (Gg.V2.v (d *. (2.*.i-.1.)) 0.)
let bot_pos b i n =
  let p = Gg.Box2.bl_pt b in
  let w = Gg.Box2.w b in
  let d = w /. (2. *. n) in
  Gg.V2.add p (Gg.V2.v (d *. (2.*.i-.1.)) 0.)


class rectangle n m ?pos ~size ~name l =
  object(self)
    inherit gen n m ?pos ~name l as parent
    val size = ref size
    val sized = ref false
    method size = !size    
    method width = Size2.w !size 
    method height = Size2.h !size
    method box = Box2.v_mid !pos !size
    method contains p = Box2.mem p self#box
    method scale s = size := V2.smul s !size; sized := true
    method rebox b = pos := Box2.mid b; size := Box2.size b; sized := true
    method fakespos i = top_pos self#box i fnsources 
    method faketpos i = bot_pos self#box i fntargets 
    method sdir (_: int) = V2.(zero-oy)
    method tdir (_: int) = V2.(zero-oy)
    method private fill =
      match self#get "fill" with Some c -> Constants.color c | None -> !color
    method draw (draw: canvas) =
      draw#box ~fill:self#fill self#box
    method! private update_kvl =
      parent#update_kvl;
      if !sized then self#set "size" (string_of_p2 !size)
    initializer
      (match self#get "size" with Some s -> size := p2_of_string s; sized := true | None -> ())
  end

class circle n m ?pos ?(radius=Constants.circle_radius) ~name l =
  object(self)
    inherit gen n m ?pos ~name l as parent
    val radius = ref radius
    val sized = ref false
    method size = V2.v (2.*. !radius) (2.*. !radius)
    method width = 2.*. !radius
    method height = 2.*. !radius
    method box = Box2.v_mid !pos self#size
    method contains p = Geometry.dist p !pos <= !radius
    method scale s = radius := s *. !radius; sized := true
    method rebox (_: box): unit = failwith "cannot rebox a circular area"
    method fakespos i =
      V2.add !pos (V2.polar (0.66*. !radius) (Float.pi *. (fnsources-.i+.1.) /. (fnsources+.1.)))
    method faketpos i =
      V2.add !pos (V2.polar (0.66*. !radius) (-. Float.pi *. (fntargets-.i+.1.) /. (fntargets+.1.)))
    method sdir i = V2.polar (-1.) (Float.pi *. float_of_int (nsources-i+1) /. float_of_int (nsources+1))
    method tdir i = V2.polar 1. (-. Float.pi *. float_of_int (ntargets-i+1) /. float_of_int (ntargets+1))
    method private fill =
      match self#get "fill" with Some c -> Constants.color c | None -> !color
    method draw (draw: canvas) =
      draw#circle ~fill:self#fill {center = !pos; radius = !radius }
    method! private update_kvl =
      parent#update_kvl;
      if !sized then self#set "radius" (string_of_float !radius)
    initializer
      (match self#get "radius" with Some s -> radius := float_of_string s; sized := true | None -> ())
  end

class point n m ?pos ?(radius=Constants.point_radius) ~name l =
  object
    inherit circle n m ?pos ~radius ~name l as parent
    method! rebox _ = failwith "cannot rebox a point area"
    method! spos i = if !Constants.edit_mode then parent#fakespos (float_of_int i) else !pos
    method! tpos i = if !Constants.edit_mode then parent#faketpos (float_of_int i) else !pos
    method! fakespos _ = !pos
    method! faketpos _ = !pos
  end

class cross n m ?pos ?(radius=Constants.cross_radius) ~name l: element =
  let _ = match n,m with
    | [a;b],[b';a'] when Typ.eq [a;b] [a';b'] -> ()
    | _ -> failwith "invalid cross type (%a -> %a)" Typ.pp n Typ.pp m
  in
  object
    inherit point n m ?pos ~radius ~name l
    val adir = ref (V2.polar 1. (-. Float.pi /. 3.))
    val bdir = ref (V2.polar 1. (-2. *. Float.pi /. 3.))      
    method! rebox _ = failwith "cannot rebox a cross area"
    method! sdir = function
      | 1 -> !adir
      | 2 -> !bdir
      | _ -> assert false
    method! tdir = function
      | 1 -> !bdir
      | 2 -> !adir
      | _ -> assert false
    method! setdirs i o =
      let d (p,dp) (q,dq) =
        (* V2.(unit (q-p)) *)
        let n = Geometry.dist p q /. 4. in
        V2.(unit (q-p-smul n (dp+dq)))
      in
      match i,o with
      | [a;b],[b';a'] ->
         adir := d a a';
         bdir := d b b';
      | _ -> assert false
  end

class triangle n m ?pos ?(radius=Constants.triangle_radius) ~name l: element =
  let _ = match n,m with
    (* | [a;b],[c] when Typ.eq [a;b] [c;c] -> () *)
    | [_;_],[_] -> ()
    | [],[_] -> ()
    | _ -> failwith "invalid triangle type (%a -> %a)" Typ.pp n Typ.pp m
  in
  let v1 = V2.polar (1.) (-.Float.pi/.4.) in
  let v2 = V2.polar (-1.) (Float.pi/.4.) in
  object(self)
    inherit point n m ?pos ~radius ~name l
    method! rebox _ = failwith "cannot rebox a triangle area"
    method! spos = function
      | 1 -> V2.(!pos - smul (0.66*. !radius) v1)
      | 2 -> V2.(!pos - smul (0.66*. !radius) v2)
      | _ -> assert false
    method! tpos = function
      | _ -> V2.(!pos - smul (0.66*. !radius) oy)
    method! sdir = function
      | 1 -> v1
      | 2 -> v2
      | _ -> assert false
    method! tdir = function
      | _ -> V2.v 0. (-1.)    
    method! draw (draw: canvas) =
      draw#polygon ~fill:self#fill
        V2.(Polygon.triangle
         (!pos - smul !radius v1)
         (!pos - smul !radius v2)
         (!pos - smul !radius oy))
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
    val poly = ref poly
    val spos = ref spos
    val tpos = ref tpos
    method! spos i = List.nth !spos (i-1)
    method! tpos i = List.nth !tpos (i-1)
    method! fakespos i =
      if i<=1. then self#spos 1
      else if i>=fnsources then self#spos nsources
      else
        let p = i-.floor i in
        let i,j = parent#fakespos (floor i), parent#fakespos (ceil i) in
        V2.(smul p i + smul (1.-.p) j)        
    method! faketpos i =
      if i<=1. then self#tpos 1
      else if i>=fntargets then self#tpos ntargets
      else
        let p = i-.floor i in
        let i,j = parent#faketpos (floor i), parent#faketpos (ceil i) in
        V2.(smul p i + smul (1.-.p) j)
    method! sdir i = List.nth sdir (i-1)
    method! tdir i = List.nth tdir (i-1)
    method! contains p = Geometry.mem_poly p !poly
    method! shift d =
      parent#shift d;
      poly := Polygon.map (V2.add d) !poly;
      spos := List.map (V2.add d) !spos;
      tpos := List.map (V2.add d) !tpos;
    method! scale _ = failwith "TODO: scale polygon"
    method! draw (draw: canvas) =
      draw#polygon (* ~border:0.5 *) ~fill:self#fill !poly
    method! improve_shape =
      poly := Geometry.smoothen !poly
    initializer
      self#set "fill" "tgray"
  end

let mk_rect n m ~name l =
  let n' = List.length n in 
  let m' = List.length m in
  let size = Constants.var_size n' m' in
  new rectangle n m ~size ~name l
  
let mk n m ~name l =
  if Info.mem "radius" l then
    new circle n m ~name l
  else
    match Info.get_opt "shape" l with
    | Some "rect" ->
       mk_rect n m ~name l
    | Some "square" ->
       let d = 2.*.Constants.circle_radius in
       let size = Size2.v d d in
       new rectangle n m ~size ~name l
    | Some "circle" ->
       new circle n m ~name l
    | Some "point" ->
       new point n m ~name l
    | Some "cross" ->
       new cross n m ~name l
    | Some "triangle" ->
       new triangle n m ~name l
    | Some s -> failwith "unknown shape: %s" s
    | None ->
       match n,m with
       | [],[a] -> new triangle n m ~name (Info.merge l (Typ.kvl a))
       | [a;b],[c] when Typ.eq [a;b] [c;c] -> new triangle n m ~name (Info.merge l (Typ.kvl a))
       | [a;b],[c;d] when Typ.eq [a;b] [d;c] -> new cross n m ~name (Info.merge l (Typ.kvl a))
       | _ -> mk_rect n m ~name l
       
