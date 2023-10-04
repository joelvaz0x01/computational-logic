%{
open Formula
%}

%token NOT LPAREN RPAREN OR AND IMPL IIMPL EQUIV TRUE FALSE EOF
%token <string> VAR

%left OR
%left AND
%left IMPL
%left IIMPL
%left EQUIV
%left NOT

%start main
%type <Formula.formula_t> main

%%

main:
  | formula EOF             { $1 }

formula:
  | formula OR formula      { Or($1, $3) }
  | formula AND formula     { And($1, $3) }
  | NOT formula             { Not($2) }
  | formula IMPL formula    { Implies($1, $3) }
  | formula IIMPL formula   { Implies($3, $1) }
  | formula EQUIV formula   { Equiv($1, $3) }
  | LPAREN formula RPAREN   { $2 }
  | VAR                     { Var($1) }
  | TRUE                    { True }
  | FALSE                   { False }
