open Sumo

let () =
  let lexbuf = Lexing.from_string "1 2 3 4 5 6 7" in
  match Top_down_parser.next_expr Sumo_lexer.read lexbuf with
  | Some (Ast.IntLiteral (_, v)) -> print_int v; print_newline ()
  | _ -> print_endline "None :("
  (* let seq = Top_down_parser.lex_to_seq lexbuf in
  let lst = List.of_seq seq in
  print_endline (string_of_int (List.length lst)) *)