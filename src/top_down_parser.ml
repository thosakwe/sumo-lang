open Ast
open Lexing
open Sumo_parser

let all_tokens lexbuf =
  let rec next_token lst =
    match Sumo_lexer.read lexbuf with
    | EOF -> lst
    | _ as tok -> next_token (lst @ [tok])
  in
  next_token []

let next_span lexbuf =
  let out = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  out

let rec parse_stmt lexbuf errors tokens =
  let start = lexbuf.lex_curr_p in
  let span () = (start, lexbuf.lex_curr_p) in
  match tokens with
  | RETURN :: rest -> begin
    let (new_tokens, new_errors, value) = parse_expr lexbuf errors rest in
    let node = Return (span (), value) in
    (new_tokens, new_errors, Some node)
  end
  | _ -> begin
    match parse_expr lexbuf errors tokens with
    | (new_tokens, new_errors, Some value) -> (new_tokens, new_errors, Some (Expr (span (), value)))
    | _ -> (tokens, errors, None)
  end

and parse_expr lexbuf errors = function
  | INT v :: rest -> (rest, errors, Some (IntLiteral ((next_span lexbuf), v)))
  | DOUBLE v :: rest -> (rest, errors, Some (DoubleLiteral ((next_span lexbuf), v)))
  | BOOL v :: rest -> (rest, errors, Some (BoolLiteral ((next_span lexbuf), v)))
  | _ as tokens -> (tokens, errors, None)