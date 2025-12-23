open Diagrams

let from_string s =
  try
    let l = Lexing.from_string s in
    let et = Parser.envterm Lexer.token l in
    Graph.envgraph et
  with e -> Format.eprintf "error parsing\n%s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Full)
let to_string' = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Term)

let same g h =
  Typ.unify ~msg:"" (snd g)#sources (snd h)#sources;
  Typ.unify ~msg:"" (snd g)#targets (snd h)#targets;
  assert (Typ.eq (snd g)#sources (snd h)#sources);
  Graph.iso_envgraph g h
let pp = Graph.pp_envgraph Sparse

let test_graph env s =
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
        (Format.eprintf "Sanity: graph reparsing mismatch\n%s\n\n%a\n\n%s\n\n%a@." s pp t s' pp t'; failwith "iso") in
    ()
  with e -> Format.eprintf "Sanity: error on %s@." s; raise e

let test_term env s =
  test_graph env s;
  let s = env^" "^s in  
  try
    (* Format.eprintf "Sanity: looking at\n%s@." s; *)
    let t = from_string s in
    (* Format.eprintf "Sanity: parsed as\n%a@." (Graph.pp_envgraph Full) t; *)
    let s' = to_string' t in
    let t' = from_string s' in
    let _ =
      same t t' ||
        (Format.eprintf "Sanity: term reparsing mismatch\n%s\n\n%a\n\n%s\n\n%a@." s pp t s' pp t'; failwith "iso (via terms)") in
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
       failwith "iso (directly)")
  with e -> Format.eprintf "Sanity: error on %s %s ~ %s@." env u u'; raise e
let test_iso = test_iso_ true
let test_niso = test_iso_ false

let test = test_term

let _ = test "" "{1->1}"
let _ = test "" "{1->1}: _->_"
let _ = test "" "{1->1}: A->A"
let _ = test "" "{1->1}<size=2,2>"
let _ = test "" "id"
let _ = test "" "id.id"
let _ = test "" "id;id"
let _ = test "" "[id]"
let _ = test "" "[id].id"
let _ = test "" "[id];id"

let _ = test "let f: A->A in" "f"
let _ = test "let f: A->A in" "f;f"
let _ = test "let f: A->A in" "f.f"
let _ = test "let f: A->A in" "[f]"
let _ = test "let f: A->A in" "f.id"
let _ = test "let f: A->A in" "[f].id"

(* let _ = test "" "{}" *)
(* let _ = test "" ": 1->1" *)
(* let _ = test "" "" *)

(* TODO: test ill-typed expressions *)

let _ = test_iso "" "id" "id;id"
(* let _ = test_niso "" "id" "id.id" *)
let _ = test_niso "" "id" "[id]"

let e = "let f: A*A->A in let g: A->A in let h: A->A in"
let _ = test e "f"
let _ = test e "f.id"
let _ = test e "f.id;f"
let _ = test e "(f.id;f).id;f;g"
let _ = test e "[f.id;f].id;f"
let _ = test_iso e "f" "f;id"
let _ = test_iso e "f" "id.id;f"
let _ = test_iso e "f" "f."
let _ = test_iso e "f" ".f"
let _ = test_iso e "f;(g;h)" "(f;g);h"
let _ = test_iso e "f.(g.h)" "(f.g).h"
let _ = test_iso e "f;(g;h);[g]" "(f;g);h;[g;id]"
let _ = test_niso e "f;[g;h]" "[f;g];h"

let e = "let m: A*A->A in let n: 1->A in"
let _ = test e "id . n ; m"
let _ = test e "n . id ; m"
let _ = test e "n . n ; m"

let e =
  "let g: A -> A in
   let b: A*A -> A*A in"
let _ = test e "g.id ; b"
let _ = test e "id.g ; b"

let _ = test e "b ; id.g"
let _ = test e "b ; g.id"
let _ = test e "id.g ; b ; g.id"

let e =
  "let a: A*A -> A in
   let d: A*A*A -> A in
   let b: A*A -> A in
   let c: A*A -> A*A in"
let _ = test e "(id.c.id.id);(id.id.b.id);(id.id.a);d"

let e = 
  "let m<color=red>: A -> B in
   let n<color=blue>: B -> C in"
let _ = test e "m;n"

let e = 
  "let m<color=red>: A*A -> A in
   let n<color=blue>: B*B -> B in
   let x<color=violet>: B*A -> A*B in"
let _ = test e "x.x"
let _ = test e "id.x.id;m.n"
let _ = test e
"{n1: x,
 n2: m,
 n3: m,
 n4: n,
 n5: n,
 n6: x,
 1 -> n2.1,
 3 -> n1.2,
 2 -> n1.1,
 n1.1 -> n2.2,
 n2.1 -> n3.1,
 6 -> n4.2,
 n3.1 -> 1,
 n4.1 -> 2,
 n1.2 -> n5.1,
 4 -> n5.2,
 n5.1 -> n6.1,
 5 -> n6.2,
 n6.2 -> n4.1,
 n6.1 -> n3.2}: A*B*A*B*A*B -> A*B"


(** should eventually go through [test_term]: empty target nodes *)

let test = test_graph

let e = "let m: A->A*A in let n: A->1 in"
let _ = test e "m ; id . n"
let _ = test e "m ; n . id"
(* let _ = test e "m ; n . n" *)

let e = "let f: 1->A in let g: A->1 in let m: A->A*A in"
let _ = test_iso e "g;f" "g.f"
let _ = test_iso e "g;f" "f.g"
let _ = test_iso e "f.(m;g.id)" "m;(g.f.id)"
let _ = test_niso e "f.(m;g.id)" "m;(g.id.f)"

let e = "let m: A*A->1 in let n: 1->A in let n': 1->A in"
(* let _ = test e "n.id; m" *)
let _ = test_niso e "n.id; m" "n'.id; m"

let e = "let n: 1->A in let n': 1->A in let m: A*A->1 in let k: 1->A*A in"
let _ = test e "n"
let _ = test e "k"
let _ = test e "n.k"
let _ = test e "n.k.k;m.m.id"
let _ = test_niso e "n.k.k;m.m.id" "n'.k.k;m.m.id"

(* let e = "let m: A*A->1 in let k: 1->A*A in" *)
(* let _ = test e "id.k.id ; m.m" *)
