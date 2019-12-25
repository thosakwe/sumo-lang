type result =
  {
    errors: Sema.error list;
    llvm_module: Llvm.llmodule;
  }

(**  Top-level function that joins together Sema and LLVM. *)
let compile_single_ast path c_unit =
  let (universe, ssa_context) = Ssa_of_ast.compile_single_ast path c_unit in
  Ssa.dump_universe universe;
  let llvm_context = Llvm_of_ssa.compile_universe path ssa_context.errors universe in
  { errors = llvm_context.errors; llvm_module = llvm_context.llvm_module }
(* match llvm_context.errors with
   | [] ->
   Ssa.dump_universe universe;
   Llvm.dump_module llvm_context.llvm_module;
   Llvm_analysis.assert_valid_module llvm_context.llvm_module;
   ()
   | _ as errors -> begin
    let dump_error e =
      print_endline (Sema.string_of_error e)
    in
    List.iter dump_error errors
    (* ignore (exit 1) *)
   end *)