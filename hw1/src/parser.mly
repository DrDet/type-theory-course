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
    LAMBDA VAR DOT main         { Abstr(Var($2), $4) }
    |appl LAMBDA VAR DOT main   { Appl($1, Abstr(Var($3), $5)) }
    |appl                       { $1 }

appl:
    appl atom   { Appl($1, $2) }
    |atom       { $1 }

atom:
    LEFT_BRACKET main RIGHT_BRACKET     { $2 }
    |VAR                                { Var($1) }
