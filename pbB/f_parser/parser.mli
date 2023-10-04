
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TRUE
  | RPAREN
  | OR
  | NOT
  | LPAREN
  | IMPL
  | IIMPL
  | FALSE
  | EQUIV
  | EOF
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Formula.formula_t)
