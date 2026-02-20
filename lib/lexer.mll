{
  open Parser
  let dig_of_char c = int_of_char c - int_of_char '0'
  let diglist_of_string s =
    List.init (String.length s) (fun i -> dig_of_char s.[i])
  let numlist_of_string x s =
    (* TOFIX, fails with "1" ",2"  *)
    let n = String.length s in
    let rec split acc i =
      if i=n then acc
      else match String.index_from_opt s i ',' with
           | None -> int_of_string (String.sub s i (n-i)) :: acc
           | Some j -> split (int_of_string (String.sub s i (j-i-1)) :: acc) (j+1)
    in List.rev (split [int_of_string x] 0)
  let keyval k v = KEYVAL(Info.kv k v)
  let p2_of_strings x y =
    Gg.P2.v (float_of_string x) (float_of_string y)
}

let lstart = ['a'-'z' 'A'-'Z' '!' '?']
let letter = ['a'-'z' 'A'-'Z' '0'-'9' '!' '?' '\'']
let key = letter+
let word = letter*
let name = lstart word

let ndigit = ['1'-'9']
let digit = ['0'-'9']
let int = digit+
let nint = ndigit digit*

let frac = '.' digit*
let sign = ['-' '+']
let exp = ['e' 'E'] sign? int
let float = sign? digit* frac? exp?

let pos = float ',' float

let blank = [ ' ' '\r' '\t' '\n' ]

rule token = parse
  | blank                                  { token lexbuf }
  | "//"                                   { comment lexbuf }
  | '('                                    { LPAR }
  | ')'                                    { RPAR }
  | '<'                                    { LT }
  | '>'                                    { GT }
  | '['                                    { LSQR }
  | ']'                                    { RSQR }
  | '{'                                    { LBRK }
  | '}'                                    { RBRK }
  | ','                                    { COMMA }
  | ':'                                    { COLON }
  | ';' | ";;" | "\\;"                     { SEMI }
  | (* ° *) "\xC2\xB0"
  | (* °° *) "\xC2\xB0\xC2\xB0"
  | (* ∘ *) "\xE2\x88\x98" | "\\circ"
  | (* ∘∘ *) "\xE2\x88\x98\xE2\x88\x98"    { CIRC }
  | '.'
  | (* · *) "\194\183" | "\\cdot"          { DOT }
  | '*'
  | (* ⊗ *) "\226\138\151" | "\\otimes"    { TENSOR }
  | '_'                                    { UNDER }
  | '^'                                    { HAT }
  | '='
  | (* ≡' *) "\226\137\161\'" 
  | (* ≡ *) "\226\137\161"                 { EQ }
  | ":="                                   { EQDEF }
  | "~>"
  | "->"                                   { TO }
  | "id" | "idmap"                         { ID }
  | "--" '-'*
  | "==" '='*                              { BAR }
  | "unit"                                 { UNIT }
  | int as n                               { INT (int_of_string n) } 
  (* | float as x                             { FLOAT (float_of_string x) } *)
  (** cycles&permutations: at least two elements, if comma then arbitrary ints, otherwise digits *)
  (* | '(' (ndigit ndigit+ as s) ')'          { PRM (Perm.of_cycle (diglist_of_string s)) } *)
  (* | '(' (nint as x) ((',' nint)+ as q) ')' { PRM (Perm.of_cycle (numlist_of_string x q)) } *)
  (* | '[' (ndigit ndigit+ as s) ']'          { PRM (Perm.of_list (diglist_of_string s)) } *)
  (* | '[' (nint as x) ((',' nint)+ as q) ']' { PRM (Perm.of_list (numlist_of_string x q)) } *)
  | name as s                              { NAME s }
  | name as s '.' (nint as i)              { IPORT (s,int_of_string i) }
  | "pos=" (pos as p)
  | "pos=(" (pos as p) ')'                 { keyval "pos" p }
  | "size=" (pos as p)
  | "size=(" (pos as p) ')'                { keyval "size" p }
  | "shift=" (pos as v)
  | "shift=(" (pos as v) ')'               { keyval "shift" v }
  | "scale=" (float as x)                  { keyval "scale" x }
  | "radius=" (float as x)                 { keyval "radius" x }
  | (key as k) '=' (word as v)             { keyval k v }
  | (key as k) "=\"" ([^'"']* as v) '"'    { keyval k v }
  | (key as k) "='" ([^''']* as v) '''     { keyval k v }
  | eof                                    { EOF }
  | _ as c                                 { Misc.failwith "Lexing error near `%c'" c }

and comment = parse
  | '\n'                                   { token lexbuf }
  | eof                                    { EOF }
  | _                                      { comment lexbuf }
