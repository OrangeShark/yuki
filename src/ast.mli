(* Abstract Syntax Tree *)

type datatype =
  | IdentifierType of string
  | IntType
  | UnitType 
  | RealType
  | StringType
  | BooleanType
  | CharacterType

type expr = 
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Modulus of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | LessThan of expr * expr
  | LessEqual of expr * expr
  | GreaterThan of expr * expr
  | GreaterEqual of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Uminus of expr
  | Not of expr
  | Variable of string
  | Call of string * expr list
  | UnitValue
  | IntValue of int32
  | RealValue of float
  | StringValue of string
  | BoolValue of bool
  | CharValue of char

type pattern = 
  | PatIdent of string

type stmt =
  | Skip
  | Block of stmts
  | Expression of expr
  | VariableDec of datatype * string * expr
  | Assignment of string * expr
  | Branch of expr * stmt * stmt 
  | While of expr * stmt 
  | For of pattern * expr * stmt
  | Return of expr

and stmts = stmt list

type param = Parameter of datatype * string

type proto = Prototype of datatype * string * param list

type import = Import of string

type decl =
  | Function of proto * stmts
  | Constant of datatype * string * expr

(* prog - Represents a program *)
type prog = Program of import list * decl list
