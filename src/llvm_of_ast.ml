type result =
  {
    errors: Sema.error list;
    llvm_module: Llvm.llmodule option;
  }

(**  Top-level function that joins together Sema and LLVM. *)
let compile_single_ast path c_unit verbose =
  let (universe, ssa_context) = Ssa_of_ast.compile_single_ast path c_unit in
  if verbose then begin 
    print_endline "===================================";
    print_endline "              SSA DUMP             ";
    print_endline "===================================";
    Ssa.dump_universe universe 
  end;

  let (ssa_errors, _, _, _) = Sema.organize_errors ssa_context.errors in

  match ssa_errors with
  | [] ->
    let llvm_context = Llvm_of_ssa.compile_universe path ssa_context.errors universe in
    if verbose then begin
      print_endline "===================================";
      print_endline "              LLVM DUMP            ";
      print_endline "===================================";
      Llvm.dump_module llvm_context.llvm_module;
      print_endline "===================================";
      print_endline "            LLVM ANALYSIS          ";
      print_endline "===================================";
      Llvm_analysis.assert_valid_module llvm_context.llvm_module;
      print_endline "LLVM module is valid! ✅"
    end;
    { errors = llvm_context.errors; llvm_module = Some llvm_context.llvm_module }

  | _ ->
    if verbose then begin
      print_string "SSA compilation failed with ";
      print_int (List.length ssa_errors);
      print_endline " error(s). Aborting without producing LLVM."
    end;
    { errors = ssa_context.errors; llvm_module = None }