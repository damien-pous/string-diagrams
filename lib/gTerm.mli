open Types
open Info

type port = Raw.port

type term =
  private
  | Emp
  | Idm
  | Var of int * int * name * kvl
  | Seq of term * term
  | Tns of term * term
  | Box of term * kvl
  | Gph of int * int * (string*(kind*kvl)) list * (port*port) list * kvl
and kind =
  private
  | VNode of int * int * name
  | GNode of term

val env: kvl Raw.env -> term env
val of_raw: 'a env -> kvl Raw.term -> term
val envterm: kvl Raw.envterm -> term env * term

