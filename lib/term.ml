open Types

type 'a s = 
  | Id of int
  | Seq of 'a s * 'a s
  | Tns of 'a s * 'a s
  | Box of int * 'a * int
type 'a u = 'a s

module U = struct
type 'a t = 'a s

let rec nsrc = function
  | Id n -> n
  | Box(n,_,_) -> n
  | Seq(u,_) -> nsrc u
  | Tns(u,v) -> nsrc u + nsrc v

let rec ntgt = function
  | Id n -> n
  | Box(_,_,m) -> m
  | Seq(_,v) -> ntgt v
  | Tns(u,v) -> ntgt u + ntgt v

let id k = Id k
let box n x m = Box(n,x,m)
let rec seq u v =
  match u,v with
  | Id _,w | w,Id _ -> w
  | Seq(u,v),w -> seq u (Seq(v,w))
  | _ -> Seq(u,v)
let seq u v = assert (ntgt u = nsrc v); seq u v
let rec tns u v =
  match u,v with
  | Id 0,w | w,Id 0 -> w
  | Tns(u,v),w -> tns u (Tns(v,w))
  | _ -> Seq(u,v)

let rec size = function
  | Id _ -> 0
  | Box(_,_,_) -> 1
  | Seq(u,v) | Tns(u,v) -> size u + size v

module I(X: ALGEBRA) = struct
  let rec eval = function
    | Id k      -> X.id k
    | Box(n,f,m) -> X.box n f m
    | Seq(u,v)   -> X.seq (eval u) (eval v)
    | Tns(u,v)   -> X.tns (eval u) (eval v)
end

let map f = 
  let rec map = function
    | Id k      -> Id k
    | Box(n,x,m) -> Box(n,f.fi x,m)
    | Seq(u,v)   -> Seq(map u, map v)
    | Tns(u,v)   -> Tns(map u, map v)
  in map

type l = BOT | SEQ | TNS 
let head = function
  | Seq(_,_) -> SEQ
  | Tns(_,_) -> TNS
  | _        -> BOT

let pp mode =
  let ppx f x = x#pp mode f in
  let rec pp o f u =
    let i = head u in
    let paren fmt = if o <= i then fmt else "("^^fmt^^")" in
    let pp = pp i in
    match u with
    | Id 1       -> Format.fprintf f "id"
    | Id k       -> Format.fprintf f "id %i" k
    | Seq(u,v)   -> Format.fprintf f (paren "%a ; %a") pp u pp v
    | Tns(u,v)   -> Format.fprintf f (paren "%a * %a") pp u pp v
    | Box(_,x,_) -> ppx f x
in pp BOT

end

let nsrc (_,u,_) = U.nsrc u
let ntgt (_,u,_) = U.ntgt u
let size (_,u,_) = U.size u

type 'a t = 'a seq * 'a u * 'a seq

let decorate s u t =
  assert(Seq.size s = U.nsrc u && Seq.size t = U.ntgt u);
  (s,u,t)

let map f (s,u,t) = (Seq.imap f.fs s, U.map f u, Seq.imap f.ft t)

let pp mode f (s,u,t) =
  Format.fprintf f "#(%i,%i) %a" (Seq.size s) (Seq.size t) (U.pp mode) u

module SI(X:SALGEBRA) = struct
  module UI = U.I(X.U)
  let eval (s,u,t) = X.decorate s (UI.eval u) t
end

let of_raw merge env u =
  let rec of_raw = function
    | Types.Id n -> U.id n
    | Types.Box(f,x) ->
       let (n,x,m) =
         try let n,y,m = List.assoc f env in n,merge y x,m
         with Not_found -> 1,x,1
       in U.box n x m
    | Types.Seq(u,v) -> U.seq (of_raw u) (of_raw v)
    | Types.Tns(u,v) -> U.tns (of_raw u) (of_raw v)    
  in of_raw u
