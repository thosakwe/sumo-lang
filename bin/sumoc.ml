open Sumo

let in_file = ref ""
let out_file = ref ""

let set_in_file v = in_file := v
let set_out_file v = out_file := v

let specs = [
  ("-o", Arg.String set_out_file, "The object file to be created.")
]

let usage = "usage: sumoc -o <out_file> <in_file>"

let () =
  (* Parse args, and parse the file. *)
  let () = Arg.parse specs set_in_file usage in
  try
    match !in_file with
    | ""  ->
      let () = prerr_endline "fatal error: no input file provided" in
      ignore (exit 1)
    | _ ->
      let lexbuf = Lexing.from_channel (open_in !in_file) in
      let c_unit = Sumo_parser.compilation_unit Sumo_lexer.read lexbuf in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !in_file };

      (* Compile it. *)
      let name = Filename.remove_extension (Filename.basename !in_file) in
      let context = Llvm_compiler.compile name c_unit Sema.empty_universe in

      match context.errors with
      | [] -> begin
          let llvm_ir = Llvm.string_of_llmodule context.llvm_module in

          match !out_file with
          | "" -> print_endline llvm_ir
          | _ as fname ->
            let chan = open_out fname in
            output_string chan llvm_ir;
            close_out chan
        end
      | _ as errors -> begin
          let dump_error e =
            let open Lexing in
            let ((start, _), level, msg) = e in
            print_string start.pos_fname;
            print_string ":";
            print_int start.pos_lnum;
            print_string ":";
            print_int start.pos_cnum;
            print_string ":";
            print_string (Llvm_compiler.string_of_error_level level);
            print_string ": ";
            print_endline msg
          in
          List.iter dump_error errors
        end
  with
  | Sys_error msg ->
    prerr_endline ("fatal error: " ^ msg);
    ignore (exit 1)