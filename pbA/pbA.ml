(**
  @author: João Domingues (a48897)
  @author: Joel Vaz (a48906)
  @source: https://v2.ocaml.org/api/List.html
  @source: https://v2.ocaml.org/api/Option.html
*)

open F_parser

(**
  @description:
    A função parse_formula converte a fórmula do tipo `F_parser.formula_t = F_parser__Formula.formula_t` em string.
  @parameters:
    formula -> Fórmula gerada pelo parse "stdin".
    min_val -> Variável lexicofraficamente mais pequena encontrada na função min_lex_val.
  @return:
    String da fórmula convertida em NOR gates.
*)
let rec parse_formula formula min_value =
  match formula with
  | Var a -> a
  | False -> "(" ^ min_value ^ " % (" ^ min_value ^ " % " ^ min_value ^ "))"
  | True -> parse_formula (Not False) min_value
  | And (a, b) -> "((" ^ parse_formula a min_value ^ " % " ^ parse_formula a min_value ^ ") % (" ^ parse_formula b min_value ^ " % " ^ parse_formula b min_value ^ "))"
  | Implies (a, b) -> parse_formula (Or (Not a, b)) min_value
  | Equiv (a, b) -> parse_formula (And (Implies (a, b), Implies (b, a))) min_value
  | Not(Or (a, b)) -> "(" ^ parse_formula a min_value ^ " % " ^ parse_formula b min_value ^ ")"
  | Or (a, b) -> "((" ^ parse_formula a min_value ^ " % " ^ parse_formula b min_value ^ ") % (" ^ parse_formula a min_value ^ " % " ^ parse_formula b min_value ^ "))"
  | Not a -> "(" ^ parse_formula a min_value ^ " % " ^ parse_formula a min_value ^ ")"

(**
  @description:
    A função min_lex_val gera a variável lexicofraficamente mais pequena para os casos True e False.
  @parameters:
    formula -> Fórmula gerada pelo parse "stdin".
    min_val -> Variável lexicofraficamente mais pequena encontrada na fórmula.
  @return:
    Variável lexicofraficamente mais pequena para ser utilizada na fórmula final.
*)
let rec min_lex_val formula min_val =
  match formula with
  | Var a -> if a < min_val then a else min_val
  | True | False -> min_val
  | Not a -> min_lex_val a min_val
  | Or (a, b) | And (a, b) | Implies (a, b) | Equiv (a, b) ->
    let a_side, b_side = min_lex_val a min_val, min_lex_val b min_val in
    if a_side < b_side then a_side else b_side

(**
   @description:
    Função "main" que permite que as remanescentes funções possam ser executadas.
*)
let () =
  let input_formula = parse "stdin" |> Option.get in
  let value = min_lex_val (List.nth input_formula 0) "Z" in
  print_endline (parse_formula (List.nth input_formula 0) value)

(*
  Exemplo de execução:
  
    Sample input 1: ((A|B)&C)
    Sample output 1: ((((A % B) % (A % B)) % ((A % B) % (A % B))) % (C % C))

    Sample input 2: (!(A|B))
    Sample output 2: (A % B)

    Sample input 3: (TRUE)
    Sample output 3: ((Z % (Z % Z)) % (Z % (Z % Z)))
*)