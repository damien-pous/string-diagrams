%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT
%token COMMA COLON SEMI CIRC STAR DOT UNDER
%token ID LET EQ EQDEF IN TO EOF
%token <Types.name> NAME
%token <Types.perm> PRM
%token <int> INT
%token <float> FLOAT
%token <Element.kv> KEYVAL
%token <Types.name*int> IPORT

%left COLON
%left SEMI
%right CIRC
%left DOT

%type <Element.kvl Types.Raw.term> justterm
%type <Element.kvl Types.Raw.envterm> envterm
%type <Element.kvl Types.Raw.equations> equations
%start justterm envterm equations


%{
    open Types
    open Raw
%}

%%

term:
|                                               { Emp }
| ID                                            { Idm (Typ.flex 1)}
| ID UNDER t=typs                               { Idm t }
| f=NAME l=kvl                                  { Var(f,l) }
| u=term SEMI v=term                            { Seq(u,v) }
| u=term CIRC v=term                            { Seq(v,u) }
| u=term DOT v=term                             { Tns(u,v) }
| u=term t=mtyp                                 { Typ(u,fst t, snd t) }
| LSQR u=term RSQR l=kvl                        { Box(u,l) }
| LBRK g=separated_list(COMMA, elem) RBRK l=kvl { Gph(g,l) }
| LPAR t=term RPAR                              { t }

elem:
| n=NAME l=kvl COLON u=term  { Node(n,u,l) }
| i=iport TO o=oport         { Edge(i,o) }

iport:
| i=INT   { Source i }
| p=IPORT { InnerTarget(fst p,snd p) }

oport:
| i=INT   { Target i }
| p=IPORT { InnerSource(fst p,snd p) }

env:
| h=list(decl) { h }

decl:
| LET f=NAME l=kvl t=option(mtyp) b=option(body) IN { (f,(l,t,b)) }

mtyp:
| COLON n=typs TO m=typs { (n,m) }

typ:
| UNDER { Typ.flex1() }
| a=NAME { Typ.name a }

typs:
| n=INT { if n=1 then [] else Misc.failwith "invalid type (%i)" n }
| l=separated_nonempty_list(STAR,typ) { l }

body:
| EQDEF u=term { u }
  
kvl:
| LT h=separated_list(SEMI, KEYVAL) k=kvl GT { h @ k }
| { [] }

equation:
| u=term EQ v=term { (u,v) }

justterm:
| u=term EOF { u }

envterm:
| e=env u=term EOF { (e,u) }

equations:
| e=env l=separated_nonempty_list(TO,equation) EOF { (e,l,false) }
| e=env LSQR l=separated_nonempty_list(TO,equation) RSQR EOF { (e,l,true) }

