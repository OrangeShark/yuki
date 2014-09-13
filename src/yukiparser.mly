%{

%}
(* keywords *)
%token IF
%token ELSE
%token WHILE
%token FOR
%token INT
%token REAL 
%token CHAR
%token STRING
%token BOOLEAN
%token TRUE
%token FALSE
%token RETURN
%token IMPORT
(* types *)
%token <string> Ident
%token <float> Real 
%token <int32> Int
%token <string> String
%token <char> Char
%token UNIT
(* operators *)
%token PLUS MINUS TIMES DIVIDE MOD
%token OR AND
%token NOT
%token LT LE GT GE EQ NE
%token ASSIGN
(* delimiters *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMI
%token EOF
(* associativity *)
%left OR AND
%left EQ NE LE LT GT GE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc UMINUS
%start program

%%
program:
  imports declaration_list EOF { }

empty: { }

imports: 
    imports import  { }
  | empty { }

import:
  IMPORT Ident SEMI { }

declarations:
    declarations declaration { }
  | declaration { }

declaration:
    constant { }
  | func { }

constant:
  ty Ident ASSIGN expression SEMI { }

func:
  function_prototype compound_stmt { }

ty:
    UNIT { }
  | INT { }
  | REAL { }
  | STRING { }
  | BOOL { }
  | CHAR { }
  | Ident { }

function_prototype:
  ty Ident LPAREN params RPAREN { }

params:
    param_list { }
  | empty { }

param_list:
    param_list COMMA param { }
  | param { }

param: ty Ident { }

pattern: Ident { }

statements:
    compound_stmt { }
  | expression_stmt { }
  | branch_stmt { }
  | iteration_stmt { }
  | return_stmt { }
  | var_dec_stmt { }
  | assignment_stmt { }

compound_stmt: LBRACE statement_list RBRACE { }

statement_list:
    statement_list statement { }
  | statement { }

expression_stmt:
  expression SEMI { }

branch_stmt:
    IF LPAREN expression RPAREN statement { }
  | IF LPAREN expression RPAREN statement ELSE statement { }

iteration_stmt:
    RETURN SEMI { }
  | RETURN EXPRESSION SEMI { }

var_dec_stmt:
    ty Ident SEMI { }
  | ty Ident ASSIGN expression SEMI { }

assignment_stmt:
  Ident ASSIGN expression SEMI { }

expression:
    binary { }
  | unary { }
  | call { }
  | variable { }
  | literal { }
  | paren_expr { }

binary:
    expression PLUS expression { }
  | expression MINUS expression { }
  | expression TIMES expression { }
  | expression DIVIDE expression { }
  | expression MOD expression { }
  | expression LT expression { }
  | expression LE expression { }
  | expression GT expression { }
  | expression GE expression { }
  | expression EQ expression { }
  | expression NE expression { }
  | expression AND expression { }
  | expression OR expression { }

unary: 
    MINUS expression %prec UMINUS { }
  | NOT expression { }

call:
  expression LPAREN arguments RPAREN { }

arguments:
    argument_list { }
  | empty { }

argument_list:
    argument_list COMMA expression { }
  | expression { }

variable:
  Ident { }

literal:
    Int { }
  | Real { }
  | boolean { }
  | String { }
  | Char { }

boolean:
    TRUE { }
  | FALSE { }

paren_expr:
  LPAREN expression RPAREN { }
%%
