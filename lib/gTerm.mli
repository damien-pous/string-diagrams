open Types

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

val eterm: Raw.term -> term eterm
val goal: Raw.term -> term goal
