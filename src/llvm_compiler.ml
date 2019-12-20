open Sema

type context =
  {
    module_name: string;
    universe: universe;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
  }

(** Compiles a compilation unit into LLVM, given the provided
    universe as context. *)
let compile name c_unit universe =
  (* First, create a module full of "unresolved" symbols. *)
  (* Then, add that module to the universe. *)
  let unresolved_of_decl = function
    | Ast.FuncDecl (_, vis, func) -> (Ast.name_of_func func, (vis, UnresolvedFunc func))
  in
  let symbols = StringMap.of_seq (List.to_seq (List.map unresolved_of_decl c_unit)) in
  let unresolved_module = {name; symbols} in
  let new_universe = {modules = StringMap.add name unresolved_module universe.modules } in

  (*
    Now that we have the module, make an initial context.
    Then, resolve every symbol against this context, which in turn will produce
    new context values. Once all resolution is done, we can then emit the LLVM code. *)
  let llvm_context = Llvm.global_context () in
  let initial_context =
    {
      module_name = name;
      universe = new_universe;
      llvm_context;
      llvm_module = Llvm.create_module llvm_context name;
    } 
  in

  (* TODO: Compile declarations in turn *)
  (* TODO: Use optimizers *)
  (* TODO: Return IR instead of dumping *)
  Llvm.dump_module initial_context.llvm_module