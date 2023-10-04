(**
  @author: João Domingues (a48897)
  @author: Joel Vaz (a48906)
  @source: https://v2.ocaml.org/api/List.html
  @source: https://v2.ocaml.org/api/Option.html
*)

open F_parser

(**
  @description:
    A função fnc converte uma fórmula do tipo `F_parser.formula_t = F_parser__Formula.formula_t` para a Forma Normal Conjuntiva (FNC).
  @parameters:
    formula -> Fórmula gerada pelo parse "stdin".
  @return:
    Fórmula do tipo `F_parser.formula_t = F_parser__Formula.formula_t` convertida para a FNC.
*)
let fnc formula =
  let rec implFree = function
    | Not a -> Not (implFree a)
    | Or (a, b) -> Or (implFree a, implFree b)
    | And (a, b) -> And (implFree a, implFree b)
    | Implies (a, b) -> Or (implFree (Not a), implFree b)
    | Equiv (a, b) -> And (implFree (Implies (a, b)), implFree (Implies (b, a)))
    | a -> a
  in
  let rec nnfc = function
    | Not (Not a) -> nnfc a
    | Not (And (a, b)) -> Or (nnfc (Not a), nnfc (Not b))
    | Not (Or (a, b)) -> And (nnfc (Not a), nnfc (Not b))
    | And (a, b) -> And (nnfc a, nnfc b)
    | Or (a, b) -> Or (nnfc a, nnfc b)
    | a -> a
  in
  let rec distr = function
    | And (a, b), c -> And (distr (a, c), distr (b, c))
    | c, And (a, b) -> And (distr (c, a), distr (c, b))
    | a, b -> Or (a, b)
  in
  let rec cnfc = function
    | Or (a, b) -> distr (cnfc a, cnfc b)
    | And (a, b) -> And (cnfc a, cnfc b)
    | a -> a
  in
  formula |> implFree |> nnfc |> cnfc

(**
  @description:
    A função fnc_to_list converte uma fórmula FNC em list * list.
  @parameters:
    formula -> Fórmula FNC.
  @return:
    list * list de uma fórmula FNC.
*)
let rec fnc_to_list = function
  | Var a -> [ [ a ] ]
  | Not (Var a) -> [ [ "-" ^ a ] ]
  | True -> []
  | And (a, b) -> fnc_to_list a @ fnc_to_list b
  | Or (a, b) -> [ List.hd (fnc_to_list a) @ List.hd (fnc_to_list b) ]
  | _ -> [ [] ]

(**
  @description:
    A função change_literal_signal remove ou adiciona o sinal de negação a um literal.
  @parameters:
    literal -> Literal a remover ou adicionar o sinal de negação.
  @return:
    string com o literal com o sinal de negação removido ou adicionado.
*)
let change_literal_signal = function
  | literal when String.contains literal '-' ->
      String.sub literal 1 (String.length literal - 1)
  | literal -> "-" ^ literal

(**
  @description:
    A função remove_literal remove um literal de uma fórmula FNC list * list.
  @parameters:
    literal -> Literal a remover.
    formula -> Fórmula FNC list * list.
  @return:
    string list * list de uma fórmula FNC. 
*)
let remove_literal literal formula =
  let rec remove_clauses literal changed_literal = function
    | [] -> []
    | clause :: tail when List.mem literal clause ->
        remove_clauses literal changed_literal tail
    | clause :: tail when List.mem changed_literal clause ->
        List.filter (fun x -> x <> changed_literal) clause
        :: remove_clauses literal changed_literal tail
    | clause :: tail -> clause :: remove_clauses literal changed_literal tail
  in
  formula |> remove_clauses literal (literal |> change_literal_signal)

(**
  @description:
    A função unit_propagate remove todas as cláusulas unitárias de uma fórmula FNC list * list.
  @parameters:
    formula -> Fórmula FNC list * list.
  @return:
    string list * list de uma fórmula FNC.
*)
let rec unit_propagate = function
  | [] -> []
  | main_clause :: remain_clauses ->
      if List.length main_clause = 1 then
        let literal = main_clause |> List.hd in
        unit_propagate (remove_literal literal remain_clauses)
      else main_clause :: unit_propagate remain_clauses

(**
  @description:
    A função dpll aplica o algoritmo DPLL a uma fórmula FNC list * list.
  @parameters:
    s -> Fórmula FNC list * list.
  @return:
    string com o resultado do algoritmo DPLL (SAT ou UNSAT).
*)
let rec dpll s =
  let s' = unit_propagate s in
  if s' = [] then "SAT"
  else if List.mem [] s' then "UNSAT"
  else
    let hd_s' = s' |> List.hd |> List.hd in
    if dpll (remove_literal hd_s' s') = "SAT" then "SAT"
    else dpll (remove_literal (hd_s' |> change_literal_signal) s')

(**
  @description:
    Função "main" que permite que as remanescentes funções possam ser executadas.
*)
let () =
  parse "stdin" |> Option.get |> fnc |> fnc_to_list
  |> List.sort (fun x y -> compare (List.length x) (List.length y))
  |> dpll |> print_endline
