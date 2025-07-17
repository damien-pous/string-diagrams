%token LPAR RPAR LBRK RBRK LT GT COMMA COLON LET EQ IN TO EOF
%token ID SEMI STAR 
%token <Types.name> NAME
%token <Types.perm> PRM
%token <Types.inj> INJ
%token <int> INT
%token <Info.kv> KEYVAL

%left SEMI
%left STAR
%nonassoc PRM INJ CNV

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
| LBRK u=term RBRK   { Box u }
| LPAR t=term RPAR   { t }

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
  
