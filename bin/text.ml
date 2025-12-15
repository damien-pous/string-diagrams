open Diagrams_cairo
open File.SD

let check f =
  ignore(read f)

let export f =
  export f (read f)

let _ =
  Arg.(parse
         [ "-check", String check, "f\tcheck file f.sd (no output)" ]
         export
         "sd [-check file, file]*")
