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
  object
    inherit Canvas.basic as parent
    method! clear = parent#clear; debug1#clear
    method! get = I.blend parent#get debug1#get 
  end

exception Abort of string
exception User_error of string
exception Program_error of string
let abort fmt =
  Format.kasprintf (fun s -> raise (Abort s)) fmt
let user_error fmt =
  Format.kasprintf (fun s -> raise (User_error s)) fmt
let program_error fmt =
  Format.kasprintf (fun s -> raise (Program_error s)) fmt

let output = ref (print_endline, (fun ()->()))
let set_output msg clr = output := (msg,clr)
let clear() = temporary#clear; snd !output ()
let message fmt = Format.kasprintf (fun s -> fst !output s) fmt

let warning fmt = message ("warning: "^^fmt)
let error fmt = message ("error: "^^fmt)

let catch f x y k =
  match f x with
  | y -> k(); y
  | exception Program_error s -> error "%s" s; k(); y
  | exception Abort s -> message "abort: %s" s; k(); y
  | exception User_error s 
  | exception Failure s -> warning "%s" s; k(); y
  | exception e -> warning "%s" (Printexc.to_string e); k(); y

