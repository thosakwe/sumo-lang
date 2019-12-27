let parse_compilation_unit in_file =
  let lexbuf = Lexing.from_channel (open_in in_file) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = in_file };
  try 
    Ok (Sumo_parser.compilation_unit Sumo_lexer.read lexbuf)
  with
  | Sumo_parser.Error ->
    let error_msg = "fatal error: syntax error at " ^ (Sema.string_of_position lexbuf.lex_curr_p) in
    Error error_msg