type formula_t =
  | Var of string
  | Not of formula_t
  | And of formula_t * formula_t
  | Or of formula_t * formula_t
  | Implies of formula_t * formula_t
  | Equiv of formula_t * formula_t
  | True
  | False
