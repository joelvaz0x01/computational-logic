(*
  to compile the parser...
  ocamllex lexer.mll
  menhir -v parser.mly --infer
  ocamlc -a formula.ml misc.ml parser.mli parser.ml lexer.ml f_parser.ml -o f_parser.cma
*)

open Misc

type formula_t = Formula.formula_t =
  | Var of string
  | Not of formula_t
  | And of formula_t * formula_t
  | Or of formula_t * formula_t
  | Implies of formula_t * formula_t
  | Equiv of formula_t * formula_t
  | True
  | False

let rec string_of_formula form =
  match form with
  | Var v -> v
  | True -> "TRUE"
  | False -> "FALSE"
  | Implies (f, g) ->
      "(" ^ string_of_formula f ^ " -> " ^ string_of_formula g ^ ")"
  | Equiv (f, g) ->
      "(" ^ string_of_formula f ^ " <-> " ^ string_of_formula g ^ ")"
  | And (f, g) -> "(" ^ string_of_formula f ^ " & " ^ string_of_formula g ^ ")"
  | Or (f, g) -> "(" ^ string_of_formula f ^ " | " ^ string_of_formula g ^ ")"
  | Not (Var v) -> "!(" ^ v ^ ")"
  | Not f -> "!" ^ string_of_formula f

let parse filename =
  let oc = if filename = "stdin" then stdin else open_in filename in
  let lexbuf = Lexing.from_channel oc in
  let _ = init_pos lexbuf filename in
  try
    let fl = Parser.main Lexer.token lexbuf in
    (* List.iter (fun x -> Printf.printf "%s\n" (string_of_formula x))  *)
    Some fl
  with
  | LError msg ->
      print_lex_error msg;
      None
  | SError pos ->
      print_syntax_error pos;
      None
