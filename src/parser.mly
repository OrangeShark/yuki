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
    compound_stmt { Block($1) }
  | expression SEMI { Ast.Expression($1) }
  | IF LPAREN expression RPAREN statement { Branch($3, $5, Skip) }
  | IF LPAREN expression RPAREN statement ELSE statement { Branch($3, $5, $7) }
  | WHILE LPAREN expression RPAREN statement { While($3, $5) }
  | FOR LPAREN pattern IN expression RPAREN statement { For($3, $5, $7) }
  | RETURN SEMI { Return(UnitValue) }
  | RETURN expression SEMI { Return($2) }
  | ty Ident SEMI { VariableDec($1, $2, UnitValue) }
  | ty Ident ASSIGN expression SEMI { VariableDec($1, $2, $4) }
  | Ident ASSIGN expression SEMI { Assignment($1, $3) }

compound_stmt:
    LBRACE statement_list RBRACE { $2 }

statement_list:
    statement statement_list { $1::$2 }
  | statement { [$1] }

expression:
    expression PLUS expression { Plus($1, $3) }
  | expression MINUS expression { Minus($1, $3) }
  | expression TIMES expression { Times($1, $3) }
  | expression DIVIDE expression { Divide($1, $3) }
  | expression MOD expression { Modulus($1, $3) }
  | expression LT expression { LessThan($1, $3) }
  | expression LE expression { LessEqual($1, $3) }
  | expression GT expression { GreaterThan($1, $3) }
  | expression GE expression { GreaterEqual($1, $3) }
  | expression EQ expression { Equal($1, $3) }
  | expression NE expression { NotEqual($1, $3) }
  | expression AND expression { And($1, $3) }
  | expression OR expression { Or($1, $3) }
  | MINUS expression %prec UMINUS { Uminus($2) }
  | NOT expression { Not($2) }
  | Ident LPAREN arguments RPAREN { Call($1, $3) }
  | Ident { Variable($1) }
  | Int { IntValue($1) }
  | Real { RealValue($1) }
  | TRUE { BoolValue(true) }
  | FALSE { BoolValue(false) }
  | String { StringValue($1) }
  | Char { CharValue($1) }
  | LPAREN expression RPAREN { $2 }

arguments:
    argument_list { $1 }
  | empty { [] }

argument_list:
    expression COMMA argument_list { $1::$3 }
  | expression { [$1] }

%%
