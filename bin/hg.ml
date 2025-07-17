open Diagrams_cairo

let check f =
  ignore(File.read f)

let export f =
  File.export f (File.read f)

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.hg (no output)" ]
         export
         "hg [-check file, file]*")
