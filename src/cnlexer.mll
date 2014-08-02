{
  open Lexing
  open Parser

  exception SyntaxError of string
   
  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table =
    create_hashtable 20 [
      ("if", IF);
      ("else", ELSE);
      ("while", WHILE);
      ("for", FOR);
      ("int", INT);
      ("float", FLOAT);
      ("void", VOID);
      ("char", CHAR);
      ("string", STRING);
      ("boolean", BOOLEAN);
      ("true", TRUE);
      ("false", FALSE);
      ("var", VAR);
      ("func", FUNC);
      ("class", CLASS);
      ("constructor", CONSTR);
      ("return", RETURN);
      ("import", IMPORT)
    ]

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

let digit = ['0'-'9']
let character = ['a'-'z''A'-'Z']
let underscore = ['_']
let identifier = character (character|digit|underscore)*
let str = '"'_*'"'
let open_comment = "/*"
let close_comment = "*/"
let space = [' ''\t']

rule lex = parse
  | digit+ as inum 
    { let num = Int32.of_string inum in
      Int num
    }
  | digit*'.'digit+ as fnum
    { let num = float_of_string fnum in
      Float num
    }
  | identifier as word
    { try
        let token = Hashtbl.find keyword_table word in
        token
      with Not_found ->
        Ident word
    }
  | '"'   { string_state (Buffer.create 16) lexbuf }
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIVIDE }
  | '%'   { MOD }
  | "||"  { OR }
  | "&&"  { AND }
  | '!'   { NOT }
  | '<'   { LT }
  | "<="  { LE }
  | '>'   { GT }
  | ">="  { GE }
  | "=="  { EQ }
  | "!="  { NE }
  | "|>"  { PIPE }
  | '.'   { DOT }
  | '='   { ASSIGN }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | ','   { COMMA }
  | ';'   { SEMI }
  | open_comment { comment_state lexbuf }
  | space+ { lex lexbuf }
  | '\n'  { incr_linenum lexbuf; lex lexbuf }
  | _  
    { raise (SyntaxError("Unrecognized character: " ^ Lexing.lexeme lexbuf)) }
  | eof
    { raise End_of_file }
and string_state buf = parse
  | '"'           { String (Buffer.contents buf) }
  | [^'"']+ as s  { Buffer.add_string buf s; string_state buf lexbuf }
  | _             { raise (SyntaxError("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError("Reached EOF on a string")) }
and comment_state = parse
  | close_comment { lex lexbuf }
  | _             { comment_state lexbuf }
  | eof           { raise (SyntaxError("Reached EOF on a comment")) }

{

}
