%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT
%token COMMA COLON SEMI STAR
%token ID LET EQ EQDEF IN TO EOF
%token <Types.name> NAME
%token <Types.perm> PRM
%token <int> INT
%token <float> FLOAT
%token <Info.kv> KEYVAL
%token <Types.name*int> IPORT

%left COLON
%left SEMI
%left STAR

%type <Info.kvl Types.Raw.term> justterm
%type <Info.kvl Types.Raw.envterm> envterm
%type <Info.kvl Types.Raw.equations> equations
%start justterm envterm equations


%{
    open Types
    open Raw
%}

%%

term:
|                                               { Emp }
| ID                                            { Idm }
| f=NAME l=kvl                                  { Var(f,l) }
| u=term SEMI v=term                            { Seq(u,v) }
| u=term STAR v=term                            { Tns(u,v) }
| u=term t=typ                                  { Typ(u,fst t, snd t) }
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
| LET f=NAME l=kvl t=option(typ) b=option(body) IN { (f,(l,t,b)) }

typ:
| COLON n=INT TO m=INT { (n,m) }

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

