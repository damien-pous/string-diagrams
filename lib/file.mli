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

(* reading from / writing to string diagrams files *)

val read: string -> state
val write: string -> state -> unit

(* does the given SD file already exists *)
val exists: string -> bool


class virtual io:
  object
    method virtual file: string
    inherit [state] Types.io
  end
