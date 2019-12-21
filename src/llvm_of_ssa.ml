open Ssa

type context =
  {
    errors: Sema.error list;
    llvm_builder: Llvm.llbuilder;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    llvm_scope: Llvm.llvalue Scope.t;
  }

let rec compile_universe module_name errors universe =
  let llvm_context = Llvm.create_context () in
  let llvm_module = Llvm.create_module llvm_context module_name in

  (* Create an initial scope containing forward declarations
   * of all known functions. *)

  (* There's probably a more elegant way to do this, but basically:
   * 1. Get a list of all modules.
   * 2. Get a list of all symbols for a given module.
   * 3. Map each module's list of symbols into (name, LLVM values).
   * 4. Concat all these lists into one.
   * 5. Turn these pairs into a map. *)
  let map_of_all_symbols =
    let list_of_modules = List.of_seq (StringMap.to_seq universe.modules) in
    let list_of_symbols (_, m_ref) =
      let pairs = List.of_seq (StringMap.to_seq (!m_ref).symbols) in
      let llvm_of_pair out_list (name, (_, sym)) =
        match sym with
        | TypeSymbol _ -> out_list
        | VarSymbol (qualified_name, typ) ->
          let llvm_type = compile_type llvm_context typ in
          let value = Llvm.declare_global llvm_type qualified_name llvm_module in
          out_list @ [(name, value)]
        | FuncSymbol (llvm_name, params, returns, _) ->
          let llvm_function_type = compile_function_signature llvm_context params returns in
          let value = Llvm.declare_function llvm_name llvm_function_type llvm_module in
          out_list @ [(name, value)]
      in
      List.fold_left llvm_of_pair [] pairs
    in
    let list_of_lists_of_symbols =
      List.map list_of_symbols list_of_modules
    in
    let all_symbols = List.concat list_of_lists_of_symbols in
    StringMap.of_seq (List.to_seq all_symbols)
  in

  let initial_context =
    {
      errors;
      llvm_context;
      llvm_module;
      llvm_builder = Llvm.builder llvm_context;
      llvm_scope = Scope.RootScope map_of_all_symbols;
    }
  in

  (* Once we have our scope, compile each function. *)
  let compile_functions_in_module m_ref =
    let compile_one_function f =
      compile_function initial_context f;
      ()
    in
    List.iter compile_one_function (!m_ref).compiled_functions
  in
  let compile_module_pair _ m_ref = compile_functions_in_module m_ref in
  StringMap.iter compile_module_pair universe.modules;
  initial_context

and compile_function context (name, params, returns, _) =
  let llvm_function_type = compile_function_signature context.llvm_context params returns in
  let _ = Llvm.define_function name llvm_function_type context.llvm_module in
  ()

and compile_function_signature llvm_context params returns =
  let llvm_params =
    let llvm_of_param p = compile_type llvm_context p in
    List.map llvm_of_param params
  in
  let llvm_returns = compile_type llvm_context returns in
  Llvm.function_type llvm_returns (Array.of_list llvm_params)

and compile_type context = function
  | IntType -> Llvm.i64_type context
  | DoubleType -> Llvm.double_type context
  | BoolType -> Llvm.i8_type context
  | VoidType -> Llvm.void_type context
  (* Note: This case should never be reached. *)
  | UnknownType -> Llvm.void_type context