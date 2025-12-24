open Types

type term =
  | Idm of typs
  | Var of typs * typs * name
  | Seq of term * term
  | Tns of term * term
  | Box of term

let rec sources = function
  | Idm n 
  | Var(n,_,_) -> n
  | Box u 
  | Seq(u,_) -> sources u
  | Tns(u,v) -> sources u @ sources v
let rec targets = function
  | Idm m 
  | Var(_,m,_) -> m
  | Box v 
  | Seq(_,v) -> targets v
  | Tns(u,v) -> targets u @ targets v

let idm n = Idm n
let var n m x = Var(n,m,x)
let box u = Box u

let rec is_id = function
  | Idm _ -> true
  | Var _ | Seq(_,_) | Box _ -> false
  | Tns(u,v) -> is_id u && is_id v

let rec seq u v =
  match u,v with
  | Seq(u,v),w -> seq u (Seq(v,w))
  | _ -> Seq(u,v)
let seq u v = 
  if is_id u then v
  else if is_id v then u
  else seq u v
let seq u v =
  assert (Typ.eq (targets u) (sources v));
  seq u v

let rec tns u v =
  match u,v with
  | Idm h,Idm k -> Idm (h@k)
  | Idm [],w | w,Idm [] -> w
  | Tns(u,v),w -> tns u (Tns(v,w))
  | _ -> Tns(u,v)

type l = BOT | SEQ | TNS 
let head = function
  | Seq(_,_) -> SEQ
  | Tns(_,_) -> TNS
  | _        -> BOT

let pp =
  let rec pp o f u =
    let i = head u in
    let paren fmt = if o <= i then fmt else "("^^fmt^^")" in
    let pp = pp i in
    match u with
    | Idm []     -> ()
    | Idm n      -> Format.fprintf f "id_%a" Typ.pp n
    | Var(_,_,n) -> Format.fprintf f "%s" n
    | Seq(u,v)   -> Format.fprintf f (paren "%a ; %a") pp u pp v
    | Tns(u,v)   -> Format.fprintf f (paren "%a·%a") pp u pp v
    | Box u      -> Format.fprintf f "[%a]" pp u
in pp BOT
