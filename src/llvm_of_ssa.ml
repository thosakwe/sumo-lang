open Ssa

type context =
  {
    errors: Sema.error list;
    builder: Llvm.llbuilder;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    scope: Llvm.llvalue Scope.t;
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
      builder = Llvm.builder llvm_context;
      scope = Scope.RootScope map_of_all_symbols;
    }
  in

  (* Once we have our scope, compile each function. *)
  let compile_functions_in_module m_ref =
    let compile_one_function f =
      let _ = compile_function initial_context f in
      ()
    in
    List.iter compile_one_function (!m_ref).compiled_functions
  in
  let compile_module_pair _ m_ref = compile_functions_in_module m_ref in
  StringMap.iter compile_module_pair universe.modules;
  initial_context

and compile_function context (name, params, returns, instrs) =
  let llvm_function_type = compile_function_signature context.llvm_context params returns in
  let func = Llvm.define_function name llvm_function_type context.llvm_module in
  let new_scope_map = StringMap.add name func StringMap.empty in
  let new_scope = Scope.ChildScope (context.scope, new_scope_map) in
  let new_context = { context with scope = new_scope } in
  let final_ctx =
    let compile_one_instr context (_, instr) =
      let (new_ctx, _) = compile_instr context instr in
      new_ctx
    in
    List.fold_left compile_one_instr new_context instrs
  in
  (final_ctx, func)

and compile_instr context = function
  | Value value -> compile_value context value
  | Return (_, value) ->
    let (new_ctx, llvm_value)  = compile_value context value in
    (new_ctx, Llvm.build_ret llvm_value context.builder)
  | ReturnVoid ->
    (context, Llvm.build_ret_void context.builder)
  (* Create a new scope with the given value. *)
  | VarAssn (name, typ, value) ->
    let llvm_type = compile_type context.llvm_context typ in
    let (new_ctx, llvm_value) = compile_value context value in
    let variable = Llvm.build_alloca llvm_type name new_ctx.builder in
    let _ = Llvm.build_store llvm_value variable new_ctx.builder in
    let new_scope = Scope.add name variable context.scope in
    ({ new_ctx with scope = new_scope }, variable)

and compile_value context = function
  | IntLiteral v -> (context, Llvm.const_int (Llvm.i64_type context.llvm_context) v)
  | DoubleLiteral v -> (context, Llvm.const_float (Llvm.double_type context.llvm_context) v)
  | BoolLiteral v -> 
    let value = if v then 1 else 0 in
    (context, Llvm.const_int (Llvm.i8_type context.llvm_context) value)
  | VarGet (name, _) ->
    let target = Scope.find name context.scope in
    (context, Llvm.build_load target name context.builder)
  | FunctionCall (_, name, args) ->
    let target = Scope.find name context.scope in
    let (new_ctx, llvm_args) =
      let compile_arg (context, out_list) arg =
        let (new_ctx, value) = compile_value context arg in
        (new_ctx, out_list @ [value])
      in
      List.fold_left compile_arg (context, []) args
    in
    (new_ctx, Llvm.build_call target (Array.of_list llvm_args) name new_ctx.builder)

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