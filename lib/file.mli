open Diagrams
open Types

(* exporting lists of images as multipage documents *)
val multi_pdf: (image * box) list -> string -> unit
val multi_svg: (image * box) list -> string -> unit (* broken? *)

(* single image versions *)
val pdf: image -> box -> string -> unit
val svg: image -> box -> string -> unit

(* alternative for single page SVG documents, via vg.cairo *)
val svg_via_vg: image -> box -> string -> unit


(* below, files are given by their basenames,
   extensions ".sd", ".sdp", ".pdf" or ".svg" are automativally added *)

open Graph_type

(* reading from / writing to SD files (string diagrams) *)

module SD: sig

val read: string -> env*graph
val write: string -> env*graph -> unit

(* exporting to both PDF & SVG *)
val export: string -> env*graph -> unit

(* does the given SD file already exists *)
val exists: string -> bool

end

(* reading from SDP files (string diagram proofs) *)

module SDP: sig

val read: string -> equations
val write: string -> equations -> unit

(* does the given SDP file already exists *)
val exists: string -> bool

end
