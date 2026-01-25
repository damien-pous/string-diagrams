open Diagrams
open Diagrams_cairo
open File

let check f =
  Format.printf "checking file %s@." f;
  let f = Filename.chop_extension f in
  ignore(read f)

let export f =
  let f = Filename.chop_extension f in
  let _,g = read f in
  match g with
  | Types.Eqn _ -> failwith "TODO: export equations"
  | Types.Trm g ->
     let i = Graph.image g in
     File.multi_pdf [i] f;
     File.multi_svg [i] f

let _ =
  Arg.(parse
         [ "-export", String export, "f\texport file f.sd to pdf and svg" ]
         check
         "sd [-export file, file]*")
