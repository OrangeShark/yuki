(* Abstract Syntax Tree *)

type value =
  | IntValue of int32
  | FloatValue of float
  | StringValue of string
  | BoolValue of bool
  | CharValue of char

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
  | ArrayAccess of expr * expr
  | Value of value

type statement =
  | Skip
  | Assignment of string * expr
  | Branch of expr * statement list * statement list
  | While of expr * statement list
  | Return of expr

type datatype =
  | IdentifierType of string
  | VoidType
  | FloatType
  | StringType
  | BooleanType
  | CharacterType

type param = Parameter of datatype * string

type classDecl =
  | Attribute of datatype * string * expr
  | Constructor of param list * statement list
  | Method of datatype * string * param list * statement list

type proto = Prototype of datatype * param list

type import = Import of string

type decl =
  | Function of proto * statement list
  | Class of string * classDecl list
  | GVariable of datatype * expr

(* prog - Represents a program *)
type prog = Program of import list * decl list
