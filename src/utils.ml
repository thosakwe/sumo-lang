(** Dan Bernstein's string hash, used to compute RTTI hashes from qualified names. *)
let djb2 str =
  (* https://stackoverflow.com/a/7666577/5673558
   * unsigned long hash(unsigned char *str)
     {
     unsigned long hash = 5381;
     int c;

     while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

     return hash;
     } *)

  let chars = List.of_seq (String.to_seq str) in
  let fold_char hash c =
    ((hash lsl 5) + hash) + c 
  in

  List.fold_left fold_char 5381 (List.map int_of_char chars)

let string_of_file filename =
  let rec next_line chan =
    try
      let line = input_line chan in
      [line] @ next_line chan
    with End_of_file ->
      close_in chan;
      []
  in
  let chan = open_in filename in
  let lines = next_line chan in
  String.concat "\n" lines

let parse_compilation_unit in_file =
  let text = string_of_file in_file in
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = in_file };

  let result =
    try 
      Ok (Sumo_parser.compilation_unit Sumo_lexer.read lexbuf)
    with
    | Sumo_parser.Error ->
      let error_msg = "fatal error: syntax error at " ^ (Sema.string_of_position lexbuf.lex_curr_p) in
      Error error_msg
  in
  (text, result)

(* let string_of_error e =
   let ((start, _), level, msg) = e in
   (string_of_error_level level)
   ^ ": "
   ^ (string_of_position start)
   ^ ": "
   ^ msg *)

let dump_error source_text e =
  let open Lexing in
  let (span, level, error_msg) = e in
  let (start_pos, end_pos) = span in
  let text_region = 
    let rec find_newline index =
      if index >= (String.length source_text) then
        index
      else if (String.get source_text index) = '\n' then
        index
      else
        find_newline (index + 1)
    in
    let endl = find_newline end_pos.pos_cnum in
    String.sub source_text (start_pos.pos_bol) (endl - start_pos.pos_bol)
  in
  let resetAll = "\u{001b}[0m" in
  let bold = "\u{001b}[1m" in
  let green = "\u{001b}[32m" in
  let color = match level with
    | Sema.Error -> "\u{001b}[31m"
    | Sema.Warning -> "\u{001b}[35m"
  in

  let first_line str =
    let rec select index = function
      | [] -> str
      | first :: rest -> begin
          if index >= start_pos.pos_bol then
            first
          else
            select (index + (String.length first)) rest
        end
    in

    select start_pos.pos_cnum (String.split_on_char '\n' str)
  in

  print_string bold;
  print_string (Sema.string_of_position start_pos);
  print_string ": ";
  print_string color;
  print_string (Sema.string_of_error_level level);
  print_string ": ";
  print_string resetAll;
  print_string bold;
  print_string error_msg;
  print_string resetAll;
  print_newline ();
  print_endline (first_line text_region);

  let start_index = (start_pos.pos_cnum - start_pos.pos_bol) in
  let rec print_spaces index =
    if start_index = 1 then
      ()
    else if index >= start_index then
      ()
    else begin
      print_string " ";
      print_spaces (index + 1)
    end
  in

  let rec print_highlight index =
    let is_done =
      index >= end_pos.pos_cnum 
      || ((index > start_pos.pos_cnum) && ((String.get source_text index) = '\n'))
    in
    if is_done then
      ()
    else begin
      let ch = if index = start_pos.pos_cnum then '^' else '~' in
      print_char ch;
      print_highlight (index + 1)
    end
  in

  print_spaces 0;
  print_string green;
  print_highlight start_pos.pos_cnum;
  print_string resetAll;
  print_newline ()

let length_of_map map =
  let open Sema in
  let fold _ _ count = count + 1 in
  StringMap.fold fold map 0