open Ssa

type context =
  {
    errors: Sema.error list;
    builder: Llvm.llbuilder;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    scope: Llvm.llvalue Scope.t;
  }

let rec compile_universe module_path errors universe =
  let llvm_context = Llvm.create_context () in
  let llvm_module = Llvm.create_module llvm_context module_path in

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
    let list_of_symbols (path, m_ref) =
      let pairs = List.of_seq (StringMap.to_seq (!m_ref).symbols) in
      let llvm_of_pair out_list (name, (_, sym)) =
        match sym with
        | TypeSymbol _ -> out_list
        | VarSymbol (_, qualified_name, typ) ->
          let llvm_type = compile_type llvm_context typ in
          let value = Llvm.declare_global llvm_type qualified_name llvm_module in
          out_list @ [(name, value)]
        | FuncSymbol (ext, llvm_name, params, returns, _) ->
          (* If this is the module we are compiling, then don't forward-declare anything
           * because names can only be defined once. Otherwise you get main.1, foo.1, etc.
           *
           * The only exception is "external" functions. *)
          if (module_path == path) && (not ext) then
            out_list
          else

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
  let compile_functions_in_module context m_ref =
    let compile_one_function context f =
      let (new_ctx, _) = compile_function context f in
      new_ctx
    in
    List.fold_left compile_one_function context (!m_ref).compiled_functions
  in
  let folder _ m_ref context =
    compile_functions_in_module context m_ref
  in
  StringMap.fold folder universe.modules initial_context

and compile_function context (name, params, returns, instrs) =
  let llvm_function_type = compile_function_signature context.llvm_context params returns in
  let func = Llvm.define_function name llvm_function_type context.llvm_module in

  (* Create a new scope, with the function name and params injected. *)
  let new_scope_map =
    let fold_param (map, index) (name, _) =
      let value = Llvm.param func index in
      let new_map = StringMap.add name value map in
      (new_map, index + 1)
    in

    let (map, _) = List.fold_left fold_param (StringMap.empty, 0) params in
    StringMap.add name func map
  in


  let new_scope = Scope.ChildScope (context.scope, new_scope_map) in
  let entry_block = Llvm.entry_block func in
  let new_builder = Llvm.builder context.llvm_context in
  Llvm.position_at_end entry_block new_builder;
  let new_context = { context with scope = new_scope; builder = new_builder } in
  let final_ctx =
    let compile_one_instr context (span, instr) =
      let (new_ctx, _) = compile_instr context span instr in
      new_ctx
    in
    List.fold_left compile_one_instr new_context instrs
  in
  (final_ctx, func)

and compile_instr context span = function
  | Value value -> compile_value context span value
  | Return (_, value) ->
    let (new_ctx, llvm_value)  = compile_value context span value in
    (new_ctx, Llvm.build_ret llvm_value context.builder)
  | ReturnVoid ->
    (context, Llvm.build_ret_void context.builder)

and compile_value context span = function
  | IntLiteral v -> (context, Llvm.const_int (Llvm.i64_type context.llvm_context) v)
  | DoubleLiteral v -> (context, Llvm.const_float (Llvm.double_type context.llvm_context) v)
  | BoolLiteral v -> 
    let value = if v then 1 else 0 in
    (context, Llvm.const_int (Llvm.i8_type context.llvm_context) value)
  | CastIntToDouble inner ->
    let (new_ctx, llvm_inner) = compile_value context span inner in
    let double_type = Llvm.double_type context.llvm_context in
    let new_value = Llvm.build_sitofp llvm_inner double_type "tmp" context.builder in
    (new_ctx, new_value)
  | CastDoubleToInt inner ->
    let (new_ctx, llvm_inner) = compile_value context span inner in
    let int_type = Llvm.i64_type context.llvm_context in
    let new_value = Llvm.build_fptosi llvm_inner int_type "tmp" context.builder in
    (new_ctx, new_value)
  | VarGet (name, _) ->
    if not (Scope.mem name context.scope) then
      (* let error_msg = Scope.does_not_exist name in *)
      let error_msg =
        "LLVM compiler error: The SSA form emitted VarGet \""
        ^ name
        ^ "\", but the current context has no variable with that name."
      in
      let new_ctx = emit_error context span error_msg in
      (new_ctx, Llvm.const_null (Llvm.i64_type context.llvm_context))
    else
      let target = Scope.find name context.scope in
      (context, Llvm.build_load target name context.builder)
  (* Create a new scope with the given value. *)
  | VarSet (name, typ, value) ->
    let llvm_type = compile_type context.llvm_context typ in
    let (new_ctx, llvm_value) = compile_value context span value in
    let variable = Llvm.build_alloca llvm_type name new_ctx.builder in
    let _ = Llvm.build_store llvm_value variable new_ctx.builder in
    let new_scope = Scope.replace name variable context.scope in
    ({ new_ctx with scope = new_scope }, variable)
  | FunctionCall (returns, name, args) ->
    match Llvm.lookup_function name context.llvm_module with
    | None ->
      let error_msg = Scope.does_not_exist name in
      let new_ctx = emit_error context span error_msg in
      let dump_pair name _ =
        print_endline name
      in
      Scope.iter dump_pair context.scope;
      (new_ctx, Llvm.const_null (Llvm.i64_type context.llvm_context))
    | Some target ->
      let (new_ctx, llvm_args) =
        let compile_arg (context, out_list) arg =
          let (new_ctx, value) = compile_value context span arg in
          (new_ctx, out_list @ [value])
        in
        List.fold_left compile_arg (context, []) args
      in
      (* If the function returns void, return no value. *)
      let return_name =
        match returns with
        | VoidType -> ""
        | _ -> "tmp"
      in
      (new_ctx, Llvm.build_call target (Array.of_list llvm_args) return_name new_ctx.builder)

and compile_function_signature llvm_context params returns =
  let llvm_params =
    let llvm_of_param p = compile_type llvm_context p in
    let param_types = List.map (function (_, t) -> t) params in
    List.map llvm_of_param param_types
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

and emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}