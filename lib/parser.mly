%token LPAR RPAR LBRK RBRK LSQR RSQR LT GT
%token COMMA COLON SEMI STAR
%token ID LET EQ IN TO EOF
%token <Types.name> NAME
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> INT
%token <float> FLOAT
%token <Info.kv> KEYVAL
%token <Types.name*int> IPORT

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
| LBRK 
     nodes=list(node) 
     edges=list(edge) 
     s=KEYVAL 
  RBRK COLON n=INT TO m=INT
                     { Gph(n,m,Info.get_size [s],nodes,edges) }

node:
| n=NAME l=kvl COLON u=term COMMA { (n,l,u) }

edge:
| i=port TO o=port COMMA { (i,o) }

port:
| i=INT { Outer i }
| p=IPORT { Inner(fst p,snd p) }

(* float: *)
(* | i=INT { float_of_int i } *)
(* | x=FLOAT { x } *)

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
  
