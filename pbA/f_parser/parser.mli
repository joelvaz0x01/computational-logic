
(* The type of tokens. *)

type token = 
  | TRUE
  | STRING of (string)
  | RPAR
  | OR
  | NOT
  | LPAR
  | IMP
  | FALSE
  | EQUIV
  | EOL
  | EOF
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Formula.formula_t list)
