%{

%}
(* keywords *)
%token IF
%token ELSE
%token WHILE
%token FOR
%token INT
%token FLOAT
%token VOID
%token CHAR
%token STRING
%token BOOLEAN
%token TRUE
%token FALSE
%token VAR
%token FUNC
%token CLASS
%token CONSTR
%token RETURN
%token IMPORT
(* types *)
%token <string> Ident
%token <float> Float
%token <int32> Int
%token <string>String
%token <char> Char
(* operators *)
%token PLUS MINUS TIMES DIVIDE MOD
%token OR AND
%token NOT
%token LT LE GT GE EQ NE
%token PIPE
%token DOT
%token ASSIGN
(* delimiters *)
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token COMMA
%token SEMI
%left OR AND
%left EQ NE LE LT GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD
%nonassoc UMINUS
%start program

%%
program:
  imports declaration_list { }

imports:            { }
  | imports import  { }

import:
  IMPORT Ident SEMI { }

declaration_list:
  declaration_list declaration { }
  | declaration { }
  
%%
