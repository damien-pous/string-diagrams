%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT BAR
%token COMMA COLON SEMI CIRC TENSOR DOT UNDER HAT
%token ID LET EQ EQDEF IN TO EOF
%token <Types.name> NAME
%token <int> INT
%token <Types.kv> KEYVAL
%token <Types.name*int> IPORT

(* %token <Types.perm> PRM *)
(* %token <float> FLOAT *)

%left COLON
%right TO
%nonassoc EQ
%left SEMI
%right CIRC
%left DOT
%left TENSOR
%left HAT

%type <Types.Raw.term> rawterm
%type <Types.Raw.term> rocqgoal
%start rawterm rocqgoal


%{
    open Types
    open Raw
%}

%%

term:
(* |                                               { Emp } *)
| n=INT                                         { if n=1 then One 
                                                  else Misc.failwith "invalid token (%i)" n }
| ID                                            { Idm }
| UNDER                                         { Wld }
| f=NAME l=kvl                                  { Var(f,l) }
| u=term SEMI v=term                            { Seq(u,v) }
| u=term CIRC v=term                            { Seq(v,u) }
| u=term DOT v=term                             { Dot(u,v) }
| u=term TENSOR v=term                          { Tns(u,v) }
| u=term COLON t=term                           { Typ(u,t) }
| s=term TO t=term                              { Arr(s,t) }
| t=term HAT n=INT                              { Exp(t,n) }
| u=term EQ v=term                              { Eqn(u,v) }
| LSQR u=term RSQR l=kvl                        { Box(u,l) }
| LBRK g=separated_list(COMMA, elem) RBRK l=kvl { Gph(g,l) }
| LPAR t=term RPAR                              { t }

eterm:
| LET f=NAME l=kvl d=decl IN u=eterm            { Let(f,l,d,u) }
| u=term                                        { u }

decl:
|                                               { None,None }
| COLON t=term b=option(body)                   { Some t,b }
| EQDEF b=term                                  { None,Some b }

body:
| EQDEF u=term { u }

elem:
| n=NAME l=kvl COLON u=term  { Node(n,u,l) }
| i=iport TO o=oport         { Edge(i,o) }

iport:
| i=INT   { Source i }
| p=IPORT { InnerTarget(fst p,snd p) }

oport:
| i=INT   { Target i }
| p=IPORT { InnerSource(fst p,snd p) }
  
kvl:
| LT h=separated_list(SEMI, KEYVAL) k=kvl GT { h @ k }
| { [] }

rawterm:
| u=eterm EOF { u }


rocqgoal:
| f=NAME l=kvl d=decl u=rocqgoal                { Let(f,l,d,u) }
| BAR u=term EOF                                { u }
