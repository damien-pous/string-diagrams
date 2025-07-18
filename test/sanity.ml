open Diagrams

(* let _ =  *)
(*   try *)
(*     let i = open_in "/home/damien/git/hypergraph/test.pdot" in *)
(*     let l = Lexing.from_channel i in *)
(*     ignore (Parser.dotlines Lexer.dotline l); *)
(*     close_in i *)
(*   with e -> raise e *)

let from_string s =
  try
    let l = Lexing.from_string s in
    let et = Parser.envterm Lexer.token l in
    Graph.envgraph et
  with e -> Format.eprintf "error parsing\n%s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Full)

let same = Graph.iso_envgraph
let pp = Graph.pp_envgraph Sparse

let test env s =
  let s = env^s in  
  try
    let t = from_string s in
    let s' = to_string t in
    let t' = from_string s' in
    let _ =
      same t t' ||
        (Format.eprintf "Sanity: reparsing mismatch\n%s\n%a\n%s\n%a@." s pp t s' pp t'; failwith "iso") in
    ()
  with e -> Format.eprintf "Sanity: error on %s@." s; raise e

let test_iso_ b env u u' =
  let s = env^u in
  let s' = env^u' in
  try
    let t = from_string s in
    let t' = from_string s' in
    same t t' = b ||
      (Format.eprintf "Sanity: failed iso:\nt = %a\nt'= %a@." pp t pp t';
       if not b then Format.eprintf "should be different";
       failwith "iso")
  with e -> Format.eprintf "Sanity: error on %s %s ~ %s@." env u u'; raise e
let test_iso = test_iso_ true
let test_niso = test_iso_ false


let _ = test "" "{ 1->1, size=1,1 }: 1->1"

let _ = test "" ""
let _ = test "" "id"
let _ = test "" "id*id"
let _ = test "" "id;id"
let _ = test "" "[id]"
let _ = test "" "[id]*id"
let _ = test "" "[id];id"

(* TODO: test ill-typed expressions *)

let _ = test_iso "" "id" "id;id"
let _ = test_niso "" "id" "id*id"
let _ = test_niso "" "id" "[id]"

let e = "let f: 2->1 in let g: 1->1 in let h: 1->1 in "
let _ = test e "f*id;f"
let _ = test e "(f*id;f)*id;f;g"
let _ = test e "[f*id;f]*id;f"
let _ = test_iso e "f" "f;id"
let _ = test_iso e "f" "id*id;f"
let _ = test_iso e "f" "f*"
let _ = test_iso e "f" "*f"
let _ = test_iso e "f;(g;h)" "(f;g);h"
let _ = test_iso e "f*(g*h)" "(f*g)*h"
let _ = test_iso e "f;(g;h);[g]" "(f;g);h;[g;id]"
let _ = test_niso e "f;[g;h]" "[f;g];h"
