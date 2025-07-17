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
  with e -> Format.eprintf "error parsing  %s@." s; raise e

let to_string = Format.kasprintf (fun s -> s) "%a" (Graph.pp Full)

let iso = Graph.iso
let gpp = Graph.pp Sparse
let rpp = Term.pp Sparse
(* let rpp_full = Term.spp Full *)

let test s =
  let t = from_string s in
  let s' = to_string t in
  try
    let t' = from_string s' in
      let _ =
    iso (Graph.of_term t) (Graph.of_term t') ||
      (Format.eprintf "Error: reparsing mismatch\n%s\n%a\n%s\n%a@." s rpp t s' rpp t'; failwith "iso") in
    ()
    ;
    let g = Graph.of_term t in
    let g' = Graph.of_term (PTerm.get t) in
    let g'' = Graph.of_term (NTerm.get t) in
    let _ = iso g g' || (Format.eprintf "Sanity failed iso:\ng = %a\ng'= %a@." gpp g gpp g'; failwith "iso") in
    let _ = iso g' g'' || (Format.eprintf "Sanity failed iso:\ng' = %a\ng''= %a@." gpp g' gpp g''; failwith "iso") in
    let _ = iso g g'' || (Format.eprintf "Sanity failed iso:\ng = %a\ng''= %a@." gpp g gpp g''; failwith "iso") in
    ()
  with e -> Format.eprintf "init input was %s@." s; raise e

let test_iso s s' =
  let t = from_string s in
  let t' = from_string s' in
  let g = Graph.of_term t in
  let g' = Graph.of_term t' in
  iso g g' ||
    (Format.eprintf "Sanity failed iso:\nt = %a\nt'= %a@." rpp t rpp t'; failwith "iso")
  

let _ = test "a|(b|c)"
let _ = test "(a|b)|c"
let _ = test "a.(b.c)"
let _ = test "(a.b).c"
let _ = test "(12)(12)a"
let _ = test "a|b|0|(214)(c|d)|lfe"
let _ = test "(23)la|(13)lb|lc"
let _ = test "a.b"
let _ = test "f((23)la|(13)lb)"
let _ = test "f((23)la|(13)lb)|c<color=yellow>"
let _ = test "a<pos=0.5,-0.3>"
let _ = test "f<pos=0.5,1>((23)la|(13)lb)|c<pos=0,-.3;color=yellow>"
let _ = test "#3 a<pos=0,1;color=orange>"
let _ = test "#2 a<pos=1,-2.3;color=orange>"
let _ = test "{43}a<color=orange>"
let _ = test "a<color=orange>|b.c"
let _ = test "lra"
let _ = test "{1}a|{2}b|{3}c"
let _ = test "f{132}a"
let _ = test "f{213}a"
let _ = test "{234}d"
let _ = test "f{234}d"
let _ = test "f(134)ld"
let _ = test "f{324}d"
let _ = test "f({123}a | {12}b | {324}d)"
let _ = test "la"
let _ = test "#3 lb"
let _ = test "#<color=orange>,<color=yellow> lb"
let _ = test "(13)lfa"
let _ = test "#3 (12)a"
let _ = test "{23}f(12)a"

let _ = test_iso "a|(b|c)" "(a|b)|c"
let _ = test_iso "#3 a|(b|c)" "#3 (a|b)|c"
let _ = test_iso "ffa" "ffra"
let _ = test_iso "#4 ffa" "#4 ffra"
let _ = test_iso "lla" "rlla"
let _ = test_iso "#4 lla" "#4 rlla"
let _ = test_iso "fa | b" "f(a | lb)"
let _ = test_iso "#4 fa | b" "#4 f(a | lb)"

let _ = test_iso "{23}a" "(123)la"
let _ = test_iso "{23}fa" "(123)lfa"
let _ = test_iso "{23}f(12)a" "(123)lf(12)a"
let _ = test_iso "{23}fra" "(123)lfra"
let _ = test_iso "(134)ld" "{324}d"
let _ = test_iso "f(134)ld" "f{324}d"

let _ = test_iso "*(u,v,w)" "f({14}u|{24}v|{34}w)"
let _ = test_iso "*(u,v,w)" "f((24)llu|(241)llv|(13)(24)llw)"
let _ = test_iso "*(u,v,w)" "fs(lrv,(13)lw,(23)lu)"

(* FA3 *)
let _ = test_iso "fs(a,fs(b,c,0),0)" "fs(fs(a,b,0),c,0)"

(* FX *)
let _ = test_iso
          "ff ((23)lla | (42)llb | (14)l(*(g,re,d) | *(-f,rh,c)))"
          "ff ((13)lle | (14)llh | (24)l(*(a,rc,rd) | *(b,r-f,rg)))"
let _ = test_iso
          "ff ((13)lle | (14)llh | (24)l(*(a,rc,rd) | *(b,r-f,rg)))"
          "ff ((1234)lf((243)llre | (1243)llg | (24)(13)lld) | (1234)lf((24)(13)llc | (1243)ll-f | (243)llrh) | (243)lla | (23)llb)"
let _ = test_iso
          "ff ((13)lle | (14)llh | (24)l(*(a,rc,rd) | *(b,r-f,rg)))"
          "ff ((1234)lf((243)llre | (1243)lld | (24)(13)llg) | (1234)lf((1243)llc | (24)(13)ll-f | (243)llrh) | (42)llb | (23)lla)"

(* FD *)
let _ = test_iso
          "ff ((23)lla | (42)llb | (14)l(fs(*(ri,rh,c), *(-f,rg,e), *(d,rk,rj))))"
          "ff ((13)llk | (14)llh | (24)l(fs(*(g,i,j), *(b,r-f,rd), *(a,rc,re))))"

                                            
let _ = test_tw "0" (-1)
let _ = test_tw "a" (-1)
let _ = test_tw "#1 0" 0
let _ = test_tw "#2 0" 1
let _ = test_tw "#3 a" 2
let _ = test_tw "a.b" 2
let _ = test_tw "(13)la" 2
let _ = test_tw "*(a,b,c)" 3
let _ = test_tw "*(a.a,b.b,c.c)" 3
let _ = test_tw "fs(a,b,c)" 3
let _ = test_tw "ffffs(a,b,c)" 3
