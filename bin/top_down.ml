open Sumo

let () =
  let lexbuf = Lexing.from_string "1 2 3 4 5 6 7" in
  let tokens = Top_down_parser.all_tokens lexbuf in
  match Top_down_parser.parse_stmt lexbuf tokens with
  | (_, Some (Expr _)) -> print_endline "A"
  | _ -> print_endline "B"