%{
  open Ast
%}
/* keywords */
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
%token IN
/* types */
%token <string> Ident
%token <float> Real 
%token <int32> Int
%token <string> String
%token <char> Char
%token UNIT
/* operators */
%token PLUS MINUS TIMES DIVIDE MOD
%token OR AND
%token NOT
%token LT LE GT GE EQ NE
%token ASSIGN
/* delimiters */
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token SEMI
%token EOF
/* associativity */
%left OR 
%left AND
%left EQ NE 
%left LE LT GT GE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc UMINUS

%start program
%type <Ast.prog> program

%%
program:
  imports declarations EOF { Program($1, $2) }

empty: { }

imports: 
    imports import  { $2::$1 }
  | empty { [] }

import:
  IMPORT Ident SEMI { Import($2) }

declarations:
    declarations declaration { $2::$1 }
  | declaration { [$1] }

declaration:
    constant { $1 }
  | func { $1 }

constant:
  ty Ident ASSIGN expression SEMI { Constant($1, $2, $4) }

func:
  function_prototype compound_stmt { Function($1, $2) }

ty:
    UNIT { UnitType }
  | INT { IntType }
  | REAL { RealType }
  | STRING { StringType }
  | BOOLEAN { BooleanType }
  | CHAR { CharacterType }
  | Ident { IdentifierType($1) }

function_prototype:
  ty Ident LPAREN params RPAREN { Prototype($1, $2, $4) }

params:
    param_list { $1 }
  | empty { [] }

param_list:
    param_list COMMA param { $3::$1 }
  | param { [] }

param: ty Ident { Parameter($1, $2) }

pattern: Ident { PatIdent($1) }

statement:
    compound_stmt { $1 }
  | expression_stmt { $1 }
  | branch_stmt { $1 }
  | iteration_stmt { $1 }
  | return_stmt { $1 }
  | var_dec_stmt { $1 }
  | assignment_stmt { $1 }

compound_stmt: LBRACE statement_list RBRACE { Compound($2) }

statement_list:
    statement_list statement { $2::$1 }
  | statement { [$1] }

expression_stmt:
  expression SEMI { Expression($1) }

branch_stmt:
    IF LPAREN expression RPAREN statement { Branch($3, $5, Skip) }
  | IF LPAREN expression RPAREN statement ELSE statement { Branch($3, $5, $7) }

iteration_stmt:
    WHILE LPAREN expression RPAREN statement { While($3, $5) }
  | FOR LPAREN pattern IN expression RPAREN statement { For($3, $5, $7) }

return_stmt:
    RETURN SEMI { Return(UnitValue) }
  | RETURN expression SEMI { Return($2) }

var_dec_stmt:
    ty Ident SEMI { VariableDec($1, $2, UnitValue) }
  | ty Ident ASSIGN expression SEMI { VariableDec($1, $2, $4) }

assignment_stmt:
  Ident ASSIGN expression SEMI { Assignment($1, $3) }

expression:
    binary { $1 }
  | unary { $1 }
  | call { $1 }
  | variable { $1 }
  | literal { $1 }
  | paren_expr { $1 }

binary:
    expression PLUS expression { Plus($1, $3) }
  | expression MINUS expression { Minus($1, $3) }
  | expression TIMES expression { Times($1, $3) }
  | expression DIVIDE expression { Divide($1, $3) }
  | expression MOD expression { Mod($1, $3) }
  | expression LT expression { LessThan($1, $3) }
  | expression LE expression { LessEqual($1, $3) }
  | expression GT expression { GreaterThan($1, $3) }
  | expression GE expression { GreaterEqual($1, $3) }
  | expression EQ expression { Equal($1, $3) }
  | expression NE expression { NotEqual($1, $3) }
  | expression AND expression { And($1, $3) }
  | expression OR expression { Or($1, $3) }

unary: 
    MINUS expression %prec UMINUS { Uminus($2) }
  | NOT expression { Not($2) }

call:
  expression LPAREN arguments RPAREN { Call($1, $3) }

arguments:
    argument_list { $1 }
  | empty { [] }

argument_list:
    argument_list COMMA expression { $3::$1 }
  | expression { [$1] }

variable:
  Ident { Variable($1) }

literal:
    Int { IntValue($1) }
  | Real { RealValue($1) }
  | boolean { $1 }
  | String { StringValue($1) }
  | Char { CharValue($1) }

boolean:
    TRUE { BoolValue(true) }
  | FALSE { BoolValue(false) }

paren_expr:
  LPAREN expression RPAREN { $2 }
%%
