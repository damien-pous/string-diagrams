open Types

type term =
  | Emp
  | Idm
  | Var of int * int * name
  | Seq of term * term
  | Tns of term * term
  | Box of term

let rec sources = function
  | Emp -> 0
  | Idm -> 1
  | Var(n,_,_) -> n
  | Box u -> sources u
  | Seq(u,_) -> sources u
  | Tns(u,v) -> sources u + sources v
let rec targets = function
  | Emp -> 0
  | Idm -> 1
  | Var(_,m,_) -> m
  | Box v -> targets v
  | Seq(_,v) -> targets v
  | Tns(u,v) -> targets u + targets v

let emp = Emp
let idm = Idm
let var n m x = Var(n,m,x)
let box u = Box u

let rec is_id = function
  | Emp | Idm -> true
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
let seq u v = assert (targets u = sources v); seq u v

let rec tns u v =
  match u,v with
  | Emp,w | w,Emp -> w
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
    | Idm        -> Format.fprintf f "id"
    | Emp        -> () (* Format.fprintf f "unit" *)
    | Var(_,_,n) -> Format.fprintf f "%s" n
    | Seq(u,v)   -> Format.fprintf f (paren "%a ; %a") pp u pp v
    | Tns(u,v)   -> Format.fprintf f (paren "%a * %a") pp u pp v
    | Box u      -> Format.fprintf f "[%a]" pp u
in pp BOT
