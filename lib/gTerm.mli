open Types
open Element

type iport = (string,int) Types.iport
type oport = (string,int) Types.oport

type term =
  private
  | Idm of typs
  | Var of typs * typs * name * kvl
  | Seq of term * term
  | Tns of term * term
  | Box of term * kvl
  | Gph of typs * typs * (string*(kind*kvl)) list * (iport*oport) list * kvl
and kind =
  private
  | VNode of typs * typs * name
  | GNode of term

val env: kvl Raw.env -> term env
val of_raw: 'a env -> kvl Raw.term -> term
val envterm: kvl Raw.envterm -> term env * term
val equations: kvl Raw.equations -> term env * (term*term) list * (term*term) * bool

