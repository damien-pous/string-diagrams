%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT BAR
%token COMMA COLON SEMI CIRC TENSOR DOT UNDER HAT
%token ID EQ EQDEF TO UNIT EOF
%token <Types.name> NAME
%token <int> INT
%token <Types.kv> KEYVAL
%token <Types.name*int> IPORT

(* %token <Types.perm> PRM *)
(* %token <float> FLOAT *)

%nonassoc EQ
%left COLON
%right TO
%left SEMI
%right CIRC
%left DOT
%left TENSOR
%left HAT

%type <Types.Raw.term> rawterm
%start rawterm


%{
    open Types
    open Raw
%}

%%

(* untyped terms (can be objects, morphisms, equations) *)
term:    
(* |                                               { Emp } *)
| n=INT                                         { if n=1 then One (* unit object *)
                                                  else Misc.failwith "invalid token (%i)" n }
| UNIT                                          { One }        (* unit object *)
| ID                                            { Idm }        (* identity morphism *)
| UNDER                                         { Wld }        (* anonymous object *)
| f=NAME l=kvl                                  { Var(f,l) }   (* variable (either object, morphism, or hypothesis) *)
| u=term SEMI v=term                            { Seq(u,v) }   (* forward composition *)
| u=term CIRC v=term                            { Seq(v,u) }   (* backward composition *)
| u=term DOT v=term                             { Dot(u,v) }   (* tensor product on morphisms *)
| u=term TENSOR v=term                          { Tns(u,v) }   (* tensor product on objects *)
| u=term COLON t=term                           { Typ(u,t) }   (* explicit type annotation *)
| s=term TO t=term                              { Arr(s,t) }   (* arrow type for morphisms *)
| t=term HAT n=INT                              { Exp(t,n) }   (* object exponentiation *)
| u=term EQ v=term                              { Eqn(u,v) }   (* equation *)
| LSQR u=term RSQR l=kvl                        { Box(u,l) }   (* box *)
| LBRK g=separated_list(COMMA, elem) RBRK l=kvl { Gph(g,l) }   (* explicit diagram description *)
| LPAR t=term RPAR                              { t }

(* term in environment *)
eterm:
| f=NAME l=kvl d=decl u=eterm                   { Let(f,l,d,u) }
| BAR u=term                                    { u }

(* environment declarations *)
decl:
|                                               { None,None }
| COLON t=term b=option(body)                   { Some t,b }
| EQDEF b=term                                  { None,Some b }

(* symbol definitions *)
body:
| EQDEF u=term { u }

(* explicit diagram elements *)
elem:
| n=NAME l=kvl COLON u=term  { Node(n,u,l) } (* node  *)
| i=iport TO o=oport         { Edge(i,o) }   (* edge *)

(* input ports *)
iport:
| i=INT   { Source i }                      (* outer input *)
| p=IPORT { InnerTarget(fst p,snd p) }      (* inner input *)

(* output ports *)
oport:
| i=INT   { Target i }                      (* outer output *)
| p=IPORT { InnerSource(fst p,snd p) }      (* inner output *)

(* annotations (key-value lists: pos, size, color, shape, etc...) *)
kvl:
| LT h=separated_list(SEMI, KEYVAL) k=kvl GT { h @ k }
| { [] }

(* main entry point *)
rawterm:
| u=eterm EOF { u }
