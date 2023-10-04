(* lexer.mll *)

{
  open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\r' '\n']    { token lexbuf }
  | "|"                     { OR }
  | "&"                     { AND }
  | "!"                     { NOT }
  | "->"                    { IMPL }
  | "<-"                    { IIMPL }
  | "<->"                   { EQUIV }
  | "("                     { LPAREN }
  | ")"                     { RPAREN }
  | "TRUE"                  { TRUE }
  | "FALSE"                 { FALSE }
  | (letter | '_')
    (letter | digit | '_')* as var  { VAR(var) }
  | eof            { EOF }
