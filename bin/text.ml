open Diagrams
open Diagrams_cairo
open File

let check f =
  ignore(read f)

let export f =
  let _,g = read f in
  match g with
  | Types.Eqn _ -> failwith "TODO: export equations"
  | Types.Trm g ->
     let i = Graph.image g in
     File.multi_pdf [i] f;
     File.multi_svg [i] f

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.sd (no output)" ]
         export
         "sd [-check file, file]*")
