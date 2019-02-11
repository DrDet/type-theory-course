%{
  open Tree;;
%}

%token <string> VAR
%token LEFT_BRACKET RIGHT_BRACKET
%token LAMBDA
%token DOT
%token EOF

%start main
%type <Tree.tree> main
%%

main:
    LAMBDA VAR DOT main         { Abstruction($2, $4) }
    |appl LAMBDA VAR DOT main   { Application($1, Abstruction($3, $5)) }
    |appl                       { $1 }

appl:
    appl atom   { Application($1, $2) }
    |atom       { $1 }

atom:
    LEFT_BRACKET main RIGHT_BRACKET     { $2 }
    |VAR                                { Variable($1) }
