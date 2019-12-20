open Sumo

let compile_only = ref false
let emit_asm = ref false
let emit_llvm = ref false
let in_file = ref ""
let out_file = ref ""

let set r v =
  r := v

let set_in_file = set in_file
let set_out_file v = out_file := v

let specs = [
  ("-c", Arg.Set compile_only, "Compile only; do not link. Produces an object file.");
  ("-o", Arg.String (set out_file), "The file to be created.");
  ("-S", Arg.Set emit_asm, "Emit Assembly code.");
  ("-m", Arg.Set emit_llvm, "Emit LLVM IR.");
]

let usage = "usage: sumoc [-Sc] -o <out_file> <in_file>"

let () =
  (* Parse args, and parse the file. *)
  (* TODO: Multiple inputs *)
  let () = Arg.parse specs (set in_file) usage in
  try
    match !in_file with
    | ""  ->
      let () = prerr_endline "fatal error: no input file provided" in
      ignore (exit 1)
    | _ ->
      let lexbuf = Lexing.from_channel (open_in !in_file) in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !in_file };

      let c_unit = Sumo_parser.compilation_unit Sumo_lexer.read lexbuf in

      (* Compile it. *)
      let name = Filename.remove_extension (Filename.basename !in_file) in
      let context = Llvm_compiler.compile name c_unit Sema.empty_universe in

      match context.errors with
      | [] -> begin
          let llvm_ir = Llvm.string_of_llmodule context.llvm_module in
          if !emit_llvm then
            match !out_file with
            | "" -> print_endline llvm_ir
            | _ as fname ->
              let chan = open_out fname in
              output_string chan llvm_ir;
              close_out chan
          else
            let output_path = 
              match !out_file with
              | "" -> (Filename.remove_extension !in_file) ^ ".o"
              | self -> self
            in
            let filetype = if !emit_asm then "asm" else "obj" in
            let llc_args = [
              "llc";
              ("-o " ^ output_path);
              ("-filetype=" ^ filetype)
            ]
            in
            let llc_invocation = String.concat " " llc_args in
            let llc_out = Unix.open_process_out llc_invocation in
            output_string llc_out llvm_ir;
            let status = Unix.close_process_out (llc_out) in
            match status with
            | Unix.WEXITED 0 -> ()
            | Unix.WEXITED code ->
              prerr_endline (llc_invocation ^ "` terminated with error code " ^ (string_of_int code) ^ ".")
            | _ ->
              prerr_endline ("Running `" ^ llc_invocation ^ "` resulted in an error.")
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
            print_string ": ";
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