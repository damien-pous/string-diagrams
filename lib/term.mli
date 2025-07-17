open Types

type term =
  private
  | Emp
  | Idm
  | Var of int * int * name
  | Seq of term * term
  | Tns of term * term
  | Box of term

val emp: term
val idm: term
val var: int -> int -> name -> term
val seq: term -> term -> term
val tns: term -> term -> term
val box: term -> term

val pp: formatter -> term -> unit

(* TODO: environments *)
