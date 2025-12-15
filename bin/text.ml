open Diagrams_cairo

let check f =
  ignore(File.read f)

let export f =
  File.export f (File.read f)

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.sd (no output)" ]
         export
         "sd [-check file, file]*")
