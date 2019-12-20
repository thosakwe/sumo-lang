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

      (* Compile it. *)
      let name = Filename.remove_extension (Filename.basename !in_file) in
      let context = Llvm_compiler.compile name c_unit Sema.empty_universe in
      let llvm_ir = Llvm.string_of_llmodule context.llvm_module in

      match !out_file with
      | "" -> print_endline llvm_ir
      | _ as fname ->
        let chan = open_out fname in
        output_string chan llvm_ir;
        close_out chan
  with
  | Sys_error msg ->
    prerr_endline ("fatal error: " ^ msg);
    ignore (exit 1)