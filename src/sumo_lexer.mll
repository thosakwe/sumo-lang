{
  open Lexing
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
let c_name = '#' ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule read = parse
  | white { read lexbuf }
  | '\n' { new_line lexbuf; read lexbuf }
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
  | ';' { SEMI }
  | '?' { QUESTION }

  | '*' { TIMES }
  | '/' { DIV }
  | '%' { MOD }
  | '+' { PLUS }
  | '-' { MINUS }
  | "<<" { SHL }
  | ">>" { SHR }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "==" { BOOL_EQ }
  | "!=" { BOOL_NEQ }
  | '&' { BW_AND }
  | '^' { BW_XOR }
  | '|' { BW_OR }
  | "&&" { BOOL_AND }
  | "||" { BOOL_OR }
  | "++" { INCR }
  | "--" { DECR }
  | '!' { BOOL_NOT }
  | '~' { BW_NOT }

  | "*=" { TIMES_EQUALS }
  | "/=" { DIV_EQUALS }
  | "%=" { MOD_EQUALS }
  | "+=" { PLUS_EQUALS }
  | "-=" { MINUS_EQUALS }
  | "<<=" { SHL_EQUALS }
  | ">>=" { SHR_EQUALS }
  | "&=" { BW_AND_EQUALS }
  | "^=" { BW_XOR_EQUALS }
  | "|=" { BW_OR_EQUALS }
  | "&&=" { BOOL_AND_EQUALS }
  | "||=" { BOOL_OR_EQUALS }

  | "do" { DO }
  | "else" { ELSE }
  | "external" { EXTERNAL }
  | "final" { FINAL }
  | "fn" { FN }
  | "for" { FOR }
  | "if" { IF }
  | "public" { VIS Visibility.Public }
  | "protected" { VIS Visibility.Protected }
  | "private" { VIS Visibility.Private }
  | "return" { RETURN }
  | "this" { THIS }
  | "type" { TYPE }
  | "var" { VAR }
  | "while" { WHILE }

  | "true" { BOOL true }
  | "false" { BOOL false }
  | "none" { NONE }
  | int_literal { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | hex { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_literal { DOUBLE (float_of_string (Lexing.lexeme lexbuf)) }
  | c_name { C_NAME (Lexing.lexeme lexbuf) }
  | lower_id { LOWER_ID (Lexing.lexeme lexbuf) }
  | upper_id { UPPER_ID (Lexing.lexeme lexbuf) }

  | eof { EOF }
  
and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }