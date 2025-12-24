open Types
open Vg

(* let active _ = true *)
(* let active _ = false *)
let active k = List.mem k ["always"]

let debug0 = new Canvas.void
let debug1 = new Canvas.basic
let debug k = if active k then debug1 else debug0
let debug_msg k fmt =
  if active k then Format.eprintf ("debug %s: "^^fmt^^"@.") k
  else Format.ifprintf Format.err_formatter fmt

let temporary =
  object(self)
    inherit Canvas.basic as parent
    val mutable messages = []
    method messages = String.concat "\n" (List.rev messages)
    method! clear = parent#clear; debug1#clear
    method clear_all = self#clear; messages <- []
    method! get = I.blend parent#get debug1#get 
    method msg: 'a. ('a, formatter, unit) format -> 'a
      = Format.kasprintf (fun s -> messages <- s::messages)
  end

exception User_error of string
let error fmt =
  Format.kasprintf (fun s -> raise (User_error s)) fmt

let catch f x y k =
  match f x with
  | y -> k(); y
  | exception User_error s -> temporary#msg "Error: %s" s; k(); y
  | exception e -> k(); raise e
