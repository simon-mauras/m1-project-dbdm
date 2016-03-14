{
open Parser;;
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

rule main = parse
  | [' ' '\t' '\n']             { main lexbuf }
  | "SOURCE"                    { SOURCE }
  | "TARGET"                    { TARGET }
  | "MAPPING"                   { MAPPING }
  | letter(letter|digit)* as s  { NAME s }
  | digit+ as s                 { CONSTANT (int_of_string s) }
  | "->"                        { ARROW }
  | "$"                         { DOLLAR }
  | ","                         { COMMA }
  | "."                         { DOT }
  | "("                         { LPAR }
  | ")"                         { RPAR }
  | eof                         { EOF }

