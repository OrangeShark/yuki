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
      ("and", AND);
      ("or", OR);
      ("for", FOR);
      ("in", IN);
      ("int", INT);
      ("real", REAL);
      ("char", CHAR);
      ("string", STRING);
      ("bool", BOOLEAN);
      ("true", TRUE);
      ("false", FALSE);
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
let identifier = (character|underscore) (character|digit|underscore)*
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
      Real num
    }
  | identifier as word
    { try
        let token = Hashtbl.find keyword_table word in
        token
      with Not_found ->
        Ident word
    }
  | "()"  { UNIT }
  | '"'   { string_state (Buffer.create 16) lexbuf }
  | '\''[^'\'']'\'' as c { Char c.[1] }
  | '+'   { PLUS }
  | '-'   { MINUS }
  | '*'   { TIMES }
  | '/'   { DIVIDE }
  | '%'   { MOD }
  | '!'   { NOT }
  | '<'   { LT }
  | "<="  { LE }
  | '>'   { GT }
  | ">="  { GE }
  | '='   { EQ }
  | "!="  { NE }
  | "<-"  { ASSIGN }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | ','   { COMMA }
  | ';'   { SEMI }
  | open_comment { comment_state lexbuf }
  | space+ { lex lexbuf }
  | '\n'  { incr_linenum lexbuf; lex lexbuf }
  | _  
    { raise (SyntaxError("Unrecognized character: " ^ Lexing.lexeme lexbuf)) }
  | eof   { EOF }
and string_state buf = parse
  | '"'           { String (Buffer.contents buf) }
  | [^'"']+ as s  { Buffer.add_string buf s; string_state buf lexbuf }
  | _             { raise (SyntaxError("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError("Reached EOF on a string")) }
and comment_state = parse
  | close_comment { lex lexbuf }
  | '\n'          { incr_linenum lexbuf; comment_state lexbuf }
  | _             { comment_state lexbuf }
  | eof           { raise (SyntaxError("Reached EOF on a comment")) }

{

}
