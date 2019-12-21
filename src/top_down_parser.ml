open Sumo_parser

let lex_to_seq lexbuf =
  let rec next_token () =
    match Sumo_lexer.read lexbuf with
    | EOF -> Seq.Nil
    | _ as tok -> Seq.Cons (tok, next_token)
  in
  next_token

let loc lexbuf =
  let open Lexing in
  let result = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
  lexbuf.lex_start_p <- lexbuf.lex_curr_p;
  result

  (*   | v = BOOL { Ast.BoolLiteral ($loc, v) }
  | v = id { Ast.Ref ($loc, v) }
  | LPAREN; v = expr; RPAREN { Ast.Paren ($loc, v) }
  | t = expr; LPAREN; a = separated_list(COMMA, expr); RPAREN { Ast.Call ($loc, t, a) } *)

let next_expr f lexbuf =
  let tok = f lexbuf in
  let span = loc lexbuf in
  match tok with
  | INT v -> Some (Ast.IntLiteral (span, v))
  | DOUBLE v -> Some (Ast.DoubleLiteral (span, v))
  | BOOL v -> (Some (Ast.BoolLiteral (span, v)))
  | _ -> None

(* let next_expr = function
   | Seq.Nil -> (None, Seq.Nil)
   | Seq.Cons (tok, next) ->
    let expr =
      match tok with
      | INT v -> Ast.IntLiteral *)