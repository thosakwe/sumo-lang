{
  open Sumo_parser
}

let white = [' ' '\r' '\t']+
let digit = ['0'-'9']
let exp = ['e' 'E'] ['-' '+']? digit+
let int_literal = '-'? digit+ exp?
let float_literal = '-'? digit* '.' digit+ exp?
let hex = "0x" ['A'-'F' 'a'-'f' '0'-'9']+
let lower_id = ['a'-'z' '_'] ['a'-'z' '0'-'9' '_']*
let upper_id = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*
let c_name = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule read = parse
  | white { read lexbuf }
  | '\n' { read lexbuf }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LCURLY }
  | '}' { RCURLY }
  | '(' { LPAREN }
  | ')' { RPAREN }

  | "->" { ARROW }
  | ':' { COLON }
  | ',' { COMMA }
  | '.' { DOT }
  | '=' { EQUALS }
  | '|' { PIPE }
  | ';' { SEMI }
  | '?' { QUESTION }

  | "external" { EXTERNAL }
  | "fn" { FN }
  | "public" { VIS Visibility.Public }
  | "protected" { VIS Visibility.Protected }
  | "private" { VIS Visibility.Private }
  | "return" { RETURN }
  | "this" { THIS }

  | "true" { BOOL true }
  | "false" { BOOL false }
  | int_literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | hex { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_literal { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
  | lower_id { LOWER_ID (Lexing.lexeme lexbuf) }
  | upper_id { UPPER_ID (Lexing.lexeme lexbuf) }
  | c_name { C_NAME (Lexing.lexeme lexbuf) }

  | eof { EOF }