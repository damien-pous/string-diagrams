open Types
open Info

type iport = string Types.iport
type oport = string Types.oport

type term =
  private
  | Emp
  | Idm
  | Var of int * int * name * kvl
  | Seq of term * term
  | Tns of term * term
  | Box of term * kvl
  | Gph of int * int * (string*(kind*kvl)) list * (iport*oport) list * kvl
and kind =
  private
  | VNode of int * int * name
  | GNode of term

val env: kvl Raw.env -> term env
val of_raw: 'a env -> kvl Raw.term -> term
val envterm: kvl Raw.envterm -> term env * term
val equations: kvl Raw.equations -> term env * (term*term) list * (term*term) * bool

