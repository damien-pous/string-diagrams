open Diagrams

let from_string s =
  try
    let l = Lexing.from_string s in
    let et = Parser.envterm Lexer.token l in
    Graph.envgraph et
  with e -> Format.eprintf "error parsing\n%s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Full)
let to_string' = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Term)

let same = Graph.iso_envgraph
let pp = Graph.pp_envgraph Sparse

let test env s =
  let s = env^" "^s in  
  try
    (* Format.eprintf "Sanity: looking at\n%s@." s; *)
    let t = from_string s in
    (* Format.eprintf "Sanity: parsed as\n%a@." (Graph.pp_envgraph Full) t; *)
    let s' = to_string t in
    (* Format.eprintf "Sanity: reprinted as\n%s@."  s'; *)
    let t' = from_string s' in
    (* Format.eprintf "Sanity: reparsed as\n%a@." (Graph.pp_envgraph Full) t'; *)
    let _ =
      same t t' ||
        (Format.eprintf "Sanity: graph reparsing mismatch\n%s\n%a\n%s\n%a@." s pp t s' pp t'; failwith "iso") in
    let s' = to_string' t in
    let t' = from_string s' in
    let _ =
      same t t' ||
        (Format.eprintf "Sanity: term reparsing mismatch\n%s\n%a\n%s\n%a@." s pp t s' pp t'; failwith "iso (via terms)") in
    ()
  with e -> Format.eprintf "Sanity: error on %s@." s; raise e

let test_iso_ b env u u' =
  let s = env^" "^u in
  let s' = env^" "^u' in
  try
    let t = from_string s in
    let t' = from_string s' in
    same t t' = b ||
      (Format.eprintf "Sanity: failed iso:\nt = %a\nt'= %a@." pp t pp t';
       if not b then Format.eprintf "should be different@.";
       failwith "iso")
  with e -> Format.eprintf "Sanity: error on %s %s ~ %s@." env u u'; raise e
let test_iso = test_iso_ true
let test_niso = test_iso_ false

(* let _ = test "let f: 1->1 in" "[f]" *)
let _ = test "" "{ 1->1 }: 1->1"
let _ = test "" "{ 1->1 }<size=2,2>"
let _ = test "" "id"
let _ = test "" "id*id"
let _ = test "" "id;id"
let _ = test "" "[id]"
let _ = test "" "[id]*id"
let _ = test "" "[id];id"

let _ = test "" "{}"
let _ = test "" ": 0->0"
let _ = test "" ""

(* TODO: test ill-typed expressions *)

let _ = test_iso "" "id" "id;id"
let _ = test_niso "" "id" "id*id"
let _ = test_niso "" "id" "[id]"

let e = "let f: 2->1 in let g: 1->1 in let h: 1->1 in"
let _ = test e "f"
let _ = test e "f*id"
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

let e = "let m: 2->1 in let n: 0->1 in"
let _ = test e "id * n ; m"
let _ = test e "n * id ; m"
let _ = test e "n * n ; m"

let e = "let m: 1->2 in let n: 1->0 in"
let _ = test e "m ; id * n"
let _ = test e "m ; n * id"
let _ = test e "m ; n * n"

let e = "let f: 0->1 in let g: 1->0 in let m: 1->2 in"
let _ = test_iso e "g;f" "g*f"
let _ = test_iso e "g;f" "f*g"
let _ = test_iso e "f*(m;g*id)" "m;(g*f*id)"
let _ = test_niso e "f*(m;g*id)" "m;(g*id*f)"

let e = "let m: 2->0 in let n: 0->1 in let n': 0->1 in"
let _ = test e "n*id; m"
let _ = test_niso e "n*id; m" "n'*id; m"

let e = "let n: 0->1 in let n': 0->1 in let m: 2->0 in let k: 0->2 in"
let _ = test e "n"
let _ = test e "k"
let _ = test e "n*k"
let _ = test e "n*k*k;m*m*id"
let _ = test_niso e "n*k*k;m*m*id" "n'*k*k;m*m*id"

let e =
  "let g: 1 -> 1 in
   let b: 2 -> 2 in"
let _ = test e "g*id ; b"
let _ = test e "id*g ; b"
(** should eventually go through *)
(* let _ = test e "b ; id*g" *)
(* let _ = test e "b ; g*id" *)
(* let _ = test e "id*g ; b ; g*id" *)

(** should eventually go through *)
(* let e = "let m: 2->0 in let k: 0->2 in" *)
(* let _ = test e "id*k*id ; m*m" *)
