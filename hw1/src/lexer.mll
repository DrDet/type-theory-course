{
open Parser
}

let variable = ['a' - 'z'] + ['a' - 'z' '0' - '9' '\'']*
let white_space = [' ' '\t'] +

rule main = parse
        | white_space   { main lexbuf }
        | variable as v { VAR(v) }
        | "\\"          { LAMBDA }
        | "."           { DOT }
        | "("           { LEFT_BRACKET }
        | ")"           { RIGHT_BRACKET }
        | eof           { EOF }
