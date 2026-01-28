open Diagrams

let from_string s =
  try
    let l = Lexing.from_string s in
    let r = Parser.rawterm Lexer.token l in
    Graph.graph r
  with e -> Format.eprintf "error parsing\n%s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Full)
let to_string' = Format.kasprintf (fun s -> s) "%a" (Graph.pp_envgraph Term)

let same g h =
  Typ.unify ~msg:"" (snd g)#sources (snd h)#sources;
  Typ.unify ~msg:"" (snd g)#targets (snd h)#targets;
  assert (Typ.eq (snd g)#sources (snd h)#sources);
  Graph.iso_envgraph g h
let pp = Graph.pp_envgraph Sparse


let state_from_string s =
  try
    let l = Lexing.from_string s in
    let r = Parser.rawterm Lexer.token l in
    Graph.state r
  with e -> Format.eprintf "error parsing state\n%s@." s; raise e
let state_to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp_state Full)
let state_to_string' = Format.kasprintf (fun s -> s) "%a" (Graph.pp_state Term)
let same_state (e,g) (f,h) =
  Types.(match g,h with
  | Eqn((l,r),_), Eqn((l',r'),_) -> same (e,l) (f,l') && same (e,r) (f,r')
  | Trm g, Trm h -> same (e,g) (f,h)
  | _ -> false)
let pp_state = Graph.pp_state Sparse


let test_graph env s =
  let s = env^" -- "^s in  
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
  let s = env^" -- "^s in  
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

let test_state_graph env s =
  let s = env^" -- "^s in  
  try
    (* Format.eprintf "Sanity: looking at\n%s@." s; *)
    let t = state_from_string s in
    (* Format.eprintf "Sanity: parsed as\n%a@." (Graph.pp_envgraph Full) t; *)
    let s' = state_to_string t in
    (* Format.eprintf "Sanity: reprinted as\n%s@."  s'; *)
    let t' = state_from_string s' in
    (* Format.eprintf "Sanity: reparsed as\n%a@." (Graph.pp_envgraph Full) t'; *)
    let _ =
      same_state t t' ||
        (Format.eprintf "Sanity: state reparsing mismatch\n%s\n\n%a\n\n%s\n\n%a@." s pp_state t s' pp_state t'; failwith "iso") in
    ()
  with e -> Format.eprintf "Sanity: error on %s@." s; raise e

let test_state_term env s =
  test_state_graph env s;
  let s = env^" -- "^s in  
  try
    (* Format.eprintf "Sanity: looking at\n%s@." s; *)
    let t = state_from_string s in
    (* Format.eprintf "Sanity: parsed as\n%a@." (Graph.pp_envgraph Full) t; *)
    let s' = state_to_string' t in
    let t' = state_from_string s' in
    let _ =
      same_state t t' ||
        (Format.eprintf "Sanity: state reparsing mismatch\n%s\n\n%a\n\n%s\n\n%a@." s pp_state t s' pp_state t'; failwith "iso (via terms)") in
    ()
  with e -> Format.eprintf "Sanity: error on %s@." s; raise e

let test_iso_ b env u u' =
  let s = env^" -- "^u in
  let s' = env^" -- "^u' in
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
let _ = test "" "id·id"
let _ = test "" "id;id"
let _ = test "" "[id]"
let _ = test "" "[id]·id"
let _ = test "" "[id];id"

let _ = test "f: A->A" "f"
let _ = test "f: A->A" "f;f"
let _ = test "f: A->A" "f·f"
let _ = test "f: A->A" "[f]"
let _ = test "f: A->A" "f·id"
let _ = test "f: A->A" "[f]·id"

(* let _ = test "" "{}" *)
(* let _ = test "" ": 1->1" *)
(* let _ = test "" "" *)

(* TODO: test ill-typed expressions *)

let _ = test_iso "" "id" "id;id"
(* let _ = test_niso "" "id" "id·id" *)
let _ = test_niso "" "id" "[id]"

let e = "f: A*A->A g: A->A h: A->A"
let _ = test e "f"
let _ = test e "f·id"
let _ = test e "f·id;f"
let _ = test e "(f·id;f)·id;f;g"
let _ = test e "[f·id;f]·id;f"
let _ = test_iso e "f" "f;id"
let _ = test_iso e "f" "id·id;f"
let _ = test_iso e "f" "f·1"
let _ = test_iso e "f" "1·f"
let _ = test_iso e "f;(g;h)" "(f;g);h"
let _ = test_iso e "f·(g·h)" "(f·g)·h"
let _ = test_iso e "f;(g;h);[g]" "(f;g);h;[g;id]"
let _ = test_niso e "f;[g;h]" "[f;g];h"

let e = "m: A*A->A n: 1->A"
let _ = test e "id · n ; m"
let _ = test e "n · id ; m"
let _ = test e "n · n ; m"

let e =
  "g: A -> A
   b: A*A -> A*A"
let _ = test e "g·id ; b"
let _ = test e "id·g ; b"

let _ = test e "b ; id·g"
let _ = test e "b ; g·id"
let _ = test e "id·g ; b ; g·id"

let e =
  "a: A^2 -> A
   d: A^3 -> A
   b: A^2 -> A
   c: A^2 -> A*A"
let _ = test e "(id·c·id·id);(id·id·b·id);(id·id·a);d"

let _ = test "f: _^2^2 -> _" "f"

let e = 
  "m<color=red>: A -> B
   n<color=blue>: B -> C"
let _ = test e "m;n"

let e = 
  "m<color=red>: A*A -> A
   n<color=blue>: B*B -> B
   x<color=violet>: B*A -> A*B"
let _ = test e "x·x"
let _ = test e "id·x·id;m·n"
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
 n6.1 -> n3.2}: (A*B)^3 -> A*B"

let _ = List.iter (fun (_,e,s) -> test_state_term e s) Examples.list
let _ = List.iter (fun (_,e,s) -> test_state_graph e s) Examples.list'

(** should eventually go through [test_term]: empty target nodes *)

let test = test_graph

let e = "m: A->A*A n: A->1"
let _ = test e "m ; id · n"
let _ = test e "m ; n · id"
(* let _ = test e "m ; n · n" *)

let e = "f: 1->A g: A->1 m: A->A*A"
let _ = test_iso e "g;f" "g·f"
let _ = test_iso e "g;f" "f·g"
let _ = test_iso e "f·(m;g·id)" "m;(g·f·id)"
let _ = test_niso e "f·(m;g·id)" "m;(g·id·f)"

let e = "m: A*A->1 n: 1->A n': 1->A"
(* let _ = test e "n·id; m" *)
let _ = test_niso e "n·id; m" "n'·id; m"

let e = "n: 1->A n': 1->A m: A*A->1 k: 1->A*A"
let _ = test e "n"
let _ = test e "k"
let _ = test e "n·k"
let _ = test e "n·k·k;m·m·id"
let _ = test_niso e "n·k·k;m·m·id" "n'·k·k;m·m·id"

(* let e = "m: A*A->1 k: 1->A*A" *)
(* let _ = test e "id·k·id ; m·m" *)
