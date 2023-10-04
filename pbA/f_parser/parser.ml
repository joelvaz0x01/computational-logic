
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TRUE
    | STRING of (
# 6 "src/parser.mly"
       (string)
# 16 "src/parser.ml"
  )
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
  
end

include MenhirBasics

# 1 "src/parser.mly"
  
  open Formula
  open Misc

# 38 "src/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState03 : (('s, _menhir_box_main) _menhir_cell1_NOT, _menhir_box_main) _menhir_state
    (** State 03.
        Stack shape : NOT.
        Start symbol: main. *)

  | MenhirState04 : (('s, _menhir_box_main) _menhir_cell1_LPAR, _menhir_box_main) _menhir_state
    (** State 04.
        Stack shape : LPAR.
        Start symbol: main. *)

  | MenhirState08 : (('s, _menhir_box_main) _menhir_cell1_formula, _menhir_box_main) _menhir_state
    (** State 08.
        Stack shape : formula.
        Start symbol: main. *)

  | MenhirState10 : (('s, _menhir_box_main) _menhir_cell1_formula, _menhir_box_main) _menhir_state
    (** State 10.
        Stack shape : formula.
        Start symbol: main. *)

  | MenhirState12 : (('s, _menhir_box_main) _menhir_cell1_formula, _menhir_box_main) _menhir_state
    (** State 12.
        Stack shape : formula.
        Start symbol: main. *)

  | MenhirState14 : (('s, _menhir_box_main) _menhir_cell1_formula, _menhir_box_main) _menhir_state
    (** State 14.
        Stack shape : formula.
        Start symbol: main. *)

  | MenhirState18 : (('s, _menhir_box_main) _menhir_cell1_formulaList, _menhir_box_main) _menhir_state
    (** State 18.
        Stack shape : formulaList.
        Start symbol: main. *)

  | MenhirState19 : (('s, _menhir_box_main) _menhir_cell1_EOL, _menhir_box_main) _menhir_state
    (** State 19.
        Stack shape : EOL.
        Start symbol: main. *)

  | MenhirState22 : ((('s, _menhir_box_main) _menhir_cell1_formulaList, _menhir_box_main) _menhir_cell1_nonempty_list_EOL_, _menhir_box_main) _menhir_state
    (** State 22.
        Stack shape : formulaList nonempty_list(EOL).
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_formula = 
  | MenhirCell1_formula of 's * ('s, 'r) _menhir_state * (Formula.formula_t)

and ('s, 'r) _menhir_cell1_formulaList = 
  | MenhirCell1_formulaList of 's * ('s, 'r) _menhir_state * (Formula.formula_t list)

and ('s, 'r) _menhir_cell1_nonempty_list_EOL_ = 
  | MenhirCell1_nonempty_list_EOL_ of 's * ('s, 'r) _menhir_state * (unit list)

and ('s, 'r) _menhir_cell1_EOL = 
  | MenhirCell1_EOL of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAR = 
  | MenhirCell1_LPAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Formula.formula_t list) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 30 "src/parser.mly"
         ( True )
# 118 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_02 =
  fun () ->
    (
# 31 "src/parser.mly"
          ( False )
# 126 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_03 =
  fun v ->
    (
# 32 "src/parser.mly"
                ( Var v )
# 134 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_04 =
  fun f ->
    (
# 33 "src/parser.mly"
                          ( f )
# 142 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_05 =
  fun a b ->
    (
# 34 "src/parser.mly"
                               ( Or(a, b) )
# 150 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_06 =
  fun a b ->
    (
# 35 "src/parser.mly"
                                ( And(a, b) )
# 158 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_07 =
  fun f ->
    (
# 36 "src/parser.mly"
                    ( Not f )
# 166 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_08 =
  fun a b ->
    (
# 37 "src/parser.mly"
                                ( Implies(a, b))
# 174 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_09 =
  fun a b ->
    (
# 38 "src/parser.mly"
                                  ( Equiv(a, b))
# 182 "src/parser.ml"
     : (Formula.formula_t))

let _menhir_action_10 =
  fun f ->
    (
# 25 "src/parser.mly"
                ([f])
# 190 "src/parser.ml"
     : (Formula.formula_t list))

let _menhir_action_11 =
  fun f fl ->
    (
# 26 "src/parser.mly"
                                        (f::fl)
# 198 "src/parser.ml"
     : (Formula.formula_t list))

let _menhir_action_12 =
  fun _startpos__3_ fl ->
    let _ = (
# 27 "src/parser.mly"
                                 ( print_syntax_error (_startpos__3_) ; fl )
# 206 "src/parser.ml"
     : (Formula.formula_t list)) in
    prerr_string "Menhir: misuse: the semantic action associated with the production\nformulaList -> formulaList nonempty_list(EOL) error\nis expected to abort the parser, but does not do so.\n";
    assert false

let _menhir_action_13 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 216 "src/parser.ml"
     : (unit list))

let _menhir_action_14 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 224 "src/parser.ml"
     : (unit list))

let _menhir_action_15 =
  fun r ->
    (
# 22 "src/parser.mly"
                               ( List.rev r)
# 232 "src/parser.ml"
     : (Formula.formula_t list))

let _menhir_action_16 =
  fun x ->
    (
# 218 "<standard.mly>"
    ( [ x ] )
# 240 "src/parser.ml"
     : (unit list))

let _menhir_action_17 =
  fun x xs ->
    (
# 220 "<standard.mly>"
    ( x :: xs )
# 248 "src/parser.ml"
     : (unit list))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | EOF ->
        "EOF"
    | EOL ->
        "EOL"
    | EQUIV ->
        "EQUIV"
    | FALSE ->
        "FALSE"
    | IMP ->
        "IMP"
    | LPAR ->
        "LPAR"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | RPAR ->
        "RPAR"
    | STRING _ ->
        "STRING"
    | TRUE ->
        "TRUE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formulaList -> _menhir_box_main =
    fun _menhir_stack ->
      let MenhirCell1_formulaList (_menhir_stack, _, r) = _menhir_stack in
      let _v = _menhir_action_15 r in
      MenhirBox_main _v
  
  let rec _menhir_run_21 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_EOL -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_EOL (_menhir_stack, _menhir_s) = _menhir_stack in
      let (x, xs) = ((), _v) in
      let _v = _menhir_action_14 x xs in
      _menhir_goto_list_EOL_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_EOL_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_25 _menhir_stack
      | MenhirState19 ->
          _menhir_run_21 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_error_run_22 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_formulaList as 'stack) -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      let _menhir_stack = MenhirCell1_nonempty_list_EOL_ (_menhir_stack, _menhir_s, _v) in
      let _startpos = _menhir_lexbuf.Lexing.lex_start_p in
      let MenhirCell1_nonempty_list_EOL_ (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_formulaList (_menhir_stack, _, fl) = _menhir_stack in
      let _startpos__3_ = _startpos in
      let _ = _menhir_action_12 _startpos__3_ fl in
      _eRR ()
  
  let rec _menhir_error_goto_nonempty_list_EOL_ : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _v _menhir_s ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_error_run_22 _menhir_stack _menhir_lexbuf _v _menhir_s
      | MenhirState19 ->
          _menhir_error_run_20 _menhir_stack _menhir_lexbuf _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_error_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_EOL -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _v ->
      let MenhirCell1_EOL (_menhir_stack, _menhir_s) = _menhir_stack in
      let (x, xs) = ((), _v) in
      let _v = _menhir_action_17 x xs in
      _menhir_error_goto_nonempty_list_EOL_ _menhir_stack _menhir_lexbuf _v _menhir_s
  
  let rec _menhir_run_27 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | EOL ->
          let f = _v in
          let _v = _menhir_action_10 f in
          _menhir_goto_formulaList _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_08 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formula -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState08
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState08 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_formula as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | EOL | OR | RPAR ->
          let MenhirCell1_formula (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_05 a b in
          _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formula -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState10
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState10 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_formula as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | EOF | EOL | IMP | OR | RPAR ->
          let MenhirCell1_formula (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_08 a b in
          _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formula -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState12
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formula -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_formula (_menhir_stack, _menhir_s, a) = _menhir_stack in
      let b = _v in
      let _v = _menhir_action_09 a b in
      _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_formula : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState22 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState14 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState04 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_24 : type  ttv_stack. (((ttv_stack, _menhir_box_main) _menhir_cell1_formulaList, _menhir_box_main) _menhir_cell1_nonempty_list_EOL_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | EOL ->
          let MenhirCell1_nonempty_list_EOL_ (_menhir_stack, _, _) = _menhir_stack in
          let MenhirCell1_formulaList (_menhir_stack, _menhir_s, fl) = _menhir_stack in
          let f = _v in
          let _v = _menhir_action_11 f fl in
          _menhir_goto_formulaList _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_formula -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState14
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState14
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState14 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_formula as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | EOF | EOL | OR | RPAR ->
          let MenhirCell1_formula (_menhir_stack, _menhir_s, a) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_06 a b in
          _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState03
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState03 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_NOT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | EOL | RPAR ->
          let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
          let f = _v in
          let _v = _menhir_action_07 f in
          _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState04
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState04 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_LPAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAR (_menhir_stack, _menhir_s) = _menhir_stack in
          let f = _v in
          let _v = _menhir_action_04 f in
          _menhir_goto_formula _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | OR ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQUIV ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_formula (_menhir_stack, _menhir_s, _v) in
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_goto_formulaList : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_formulaList (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | EOL ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState18
      | EOF ->
          let _ = _menhir_action_13 () in
          _menhir_run_25 _menhir_stack
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EOL ->
          let _menhir_stack = MenhirCell1_EOL (_menhir_stack, _menhir_s) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState19
      | EOF ->
          let _menhir_stack = MenhirCell1_EOL (_menhir_stack, _menhir_s) in
          let _v = _menhir_action_13 () in
          _menhir_run_21 _menhir_stack _v
      | FALSE | LPAR | NOT | STRING _ | TRUE ->
          let x = () in
          let _v = _menhir_action_16 x in
          _menhir_goto_nonempty_list_EOL_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          let x = () in
          let _v = _menhir_action_16 x in
          _menhir_error_goto_nonempty_list_EOL_ _menhir_stack _menhir_lexbuf _v _menhir_s
  
  and _menhir_goto_nonempty_list_EOL_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_22 : type  ttv_stack. ((ttv_stack, _menhir_box_main) _menhir_cell1_formulaList as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_nonempty_list_EOL_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState22 _tok
      | STRING _v_1 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v_1 in
          let _v = _menhir_action_03 v in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState22 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState22
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState22 _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_EOL -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_EOL (_menhir_stack, _menhir_s) = _menhir_stack in
      let (x, xs) = ((), _v) in
      let _v = _menhir_action_17 x xs in
      _menhir_goto_nonempty_list_EOL_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRUE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | STRING _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let v = _v in
          let _v = _menhir_action_03 v in
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | NOT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LPAR ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | FALSE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
