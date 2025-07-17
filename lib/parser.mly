%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT
%token COMMA COLON SEMI DOT STAR DASH
%token ID SIZE LET EQ IN TO EOF
%token <Types.name> NAME
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> INT
%token <float> FLOAT
%token <Info.kv> KEYVAL

%left SEMI
%left STAR


%type <Info.kvl Types.Raw.envterm> envterm
%start envterm


%{
    open Types.Raw
%}

%%

term:
|                    { Emp }
| ID                 { Idm }
| f=NAME             { Var f }
| u=term SEMI v=term { Seq(u,v) }
| u=term STAR v=term { Tns(u,v) }
| LSQR u=term RSQR   { Box u }
| LPAR t=term RPAR   { t }
| LBRK n=INT TO m=INT
       SIZE EQ w=float COMMA h=float
       nodes=list(node)
       edges=list(edge)
  RBRK               { Gph(n,m,Gg.Size2.v w h,nodes,edges) }

node:
| n=INT l=kvl COLON u=term { (n,l,u) }

edge:
| DASH i=port o=port { (i,o) }

port:
| i=INT { Outer i }
| n=INT DOT i=INT { Inner(n,i) }

float:
| i=INT { float_of_int i }
| x=FLOAT { x }

env:
| h=list(decl) { h }

decl:
| LET f=NAME l=kvl t=typ b=body IN { (f,(l,t,b)) }

typ:
| COLON n=INT TO m=INT { Some (n,m) }
| { None }

body:
| EQ u=term { Some u }
| { None }
  
kvl:
| LT h=separated_list(SEMI, KEYVAL) k=kvl GT { h @ k }
| { [] }

envterm:
| e=env u=term EOF { (e,u) }
  
