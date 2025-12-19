open Types

type term =
  private
  | Idm of typs
  | Var of typs * typs * name
  | Seq of term * term
  | Tns of term * term
  | Box of term

val idm: typs -> term
val var: typs -> typs -> name -> term
val seq: term -> term -> term
val tns: term -> term -> term
val box: term -> term

val is_id: term -> bool

val pp: formatter -> term -> unit

(* TODO: environments *)
