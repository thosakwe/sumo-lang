open Sumo

let compile_only = ref false
let emit_asm = ref false
let emit_llvm = ref false
let in_file = ref ""
let out_file = ref ""
let optimization_level = ref 0
let verbose = ref false

let set r v =
  r := v

let set_optimization_level v () =
  optimization_level := v

let specs = [
  ("-c", Arg.Set compile_only, "Compile only; do not link. Produces an object file.");
  ("-o", Arg.Set_string out_file, "The file to be created.");
  ("-O", Arg.Set_int optimization_level, "The optimization level for Assembly/object files.");
  ("-O0", Arg.Unit (set_optimization_level 0), "Shorthand for -O=0.");
  ("-O1", Arg.Unit (set_optimization_level 1), "Shorthand for -O=1.");
  ("-O2", Arg.Unit (set_optimization_level 2), "Shorthand for -O=2.");
  ("-O3", Arg.Unit (set_optimization_level 3), "Shorthand for -O=3.");
  ("-S", Arg.Set emit_asm, "Emit Assembly code.");
  ("-emit-llvm", Arg.Set emit_llvm, "Emit LLVM IR.");
  ("-verbose", Arg.Set verbose, "Produce verbose output, including SSA form.");
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

      let c_unit = 
        try 
          Sumo_parser.compilation_unit Sumo_lexer.read lexbuf 
        with
        | Sumo_parser.Error ->
          prerr_endline ("fatal error: syntax error at " ^ (Sema.string_of_position lexbuf.lex_curr_p));
          ignore (exit 1);
          []
      in

      (* Compile it. *)
      (* let name = Filename.remove_extension (Filename.basename !in_file) in *)

      (* Compile to LLVM, by chaining sema and codegen. *)
      let result = Llvm_of_ast.compile_single_ast !in_file c_unit !verbose in

      (* let result = Llvm_compiler.compile name c_unit Sema.empty_universe in *)

      let output_path =
        let ext = if !emit_llvm then ".ll" else if !emit_asm then ".s" else ".o" in
        match !out_file with
        | "" -> (Filename.remove_extension !in_file) ^ ext
        | self -> self
      in

      let (errors, _, _, _) = Sema.organize_errors result.errors in

      let dump_error e =
        print_endline (Sema.string_of_error e)
      in
      List.iter dump_error result.errors;

      match errors with
      | [] -> begin
          let llvm_ir = Llvm.string_of_llmodule result.llvm_module in
          if !emit_llvm then
            let chan = open_out output_path in
            output_string chan llvm_ir;
            close_out chan
          else
            let filetype = if !emit_asm then "asm" else "obj" in
            let llc_args = [
              "llc";
              ("-o " ^ output_path);
              ("-O=" ^ (string_of_int !optimization_level));
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
              prerr_endline (llc_invocation ^ "` terminated with error code " ^ (string_of_int code) ^ ".");
              ignore (exit code)
            | _ ->
              prerr_endline ("Running `" ^ llc_invocation ^ "` resulted in an error.");
              ignore (exit 1)
        end
      | _ -> ignore (exit 1) 
  with
  | Sys_error msg ->
    prerr_endline ("fatal error: " ^ msg);
    ignore (exit 1)