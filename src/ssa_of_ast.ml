open Ssa

type context =
  {
    this_module: string;
    errors: Sema.error list;
    scope: symbol Scope.t;
    universe: universe;
  }

let rec compile_single_ast path c_unit =
  let (context, universe) = load_ast_into_universe (Ssa.default_universe) path c_unit in
  let dump_module _ module_ref =
    let m = !module_ref in
    let dump_symbol name (_, sym) =
      print_endline (name ^ ": " ^ (string_of_symbol sym))
    in
    let dump_func f =
      print_endline (string_of_func f)
    in
    print_endline ("Module \"" ^ m.path ^ "\":");
    StringMap.iter dump_symbol m.symbols;
    List.iter dump_func m.compiled_functions
  in
  StringMap.iter dump_module universe.modules;
  List.iter (function x -> prerr_endline (Sema.string_of_error x)) context.errors;
  ()

and load_ast_into_universe universe path c_unit =
  (* Before we actually compile anything, forward-declare all functions
   * in the module. After this, then we can then compile the actual
   * functions, and then compile everything into LLVM. *)
  (* TODO: Imports *)
  (* TODO: Forward-declare all types/records/structs before functions *)

  let default_context =
    let root_scope = Scope.of_seq (List.to_seq [
        ("int", TypeSymbol IntType);
        ("double", TypeSymbol DoubleType);
        ("bool", TypeSymbol BoolType);
        ("void", TypeSymbol VoidType)
      ])
    in
    {
      this_module = path;
      errors = [];
      scope = root_scope;
      universe = default_universe
    }
  in
  let ref_context = ref { default_context with universe = universe } in

  let symbols =
    let pairs =
      let pair_of_decl self =
        match self with
        | Ast.FuncDecl (_, vis, func) -> begin
            let s = Ast.signature_of_func func in
            let (new_ctx, params, returns) = compile_function_signature !ref_context s in
            ref_context := new_ctx;
            match func with
            | Ast.ExternalFunc (_, c_name_opt, name, _) ->
              let c_name =
                match c_name_opt with
                | None -> name
                | Some cn -> cn
              in
              let symbol = FuncSymbol (c_name, params, returns, self) in
              (name, (vis, symbol))
            | Ast.ConcreteFunc (_, name, _, _) ->
              let qualified = Sema.qualify [path; name] in
              let symbol = FuncSymbol (qualified, params, returns, self) in
              (name, (vis, symbol))
          end
      in
      List.map pair_of_decl c_unit
    in
    StringMap.of_seq (List.to_seq pairs)
  in

  (* Now that we have a new universe, compile the bodies of each function. *)
  let this_module = ref { path; symbols; compiled_functions = [] } in
  let new_universe =
    {
      modules = StringMap.add path this_module universe.modules
    }
  in

  (* Create a new scope containing all symbols in this module. *)
  (* TODO: Imports *)
  let unwrap_symbol (_, x) = x in
  let unwrapped_symbols = StringMap.map unwrap_symbol symbols in
  let new_scope = Scope.ChildScope ((!ref_context).scope, unwrapped_symbols) in
  let new_context = {!ref_context with scope = new_scope; universe = new_universe } in

  (* Compile them, add them to the module, and return the universe. *)
  let compile_decl (context, out_list) = function
    | Ast.FuncDecl (_, _, func) -> compile_function (context, out_list) func
    (* | _ -> (context, out_list) *)
  in
  let (final_ctx, compiled_functions) = List.fold_left compile_decl (new_context, []) c_unit in
  this_module := {!this_module with compiled_functions };

  (final_ctx, new_universe)

and compile_function_signature context (_, params, returns) =
  let compile_one_type (context, type_list) = function
    | Ast.RegularParam (_, _, typ) -> 
      let (new_ctx, result) = compile_type context typ in
      (new_ctx, type_list @ [result])
    (* TODO: Handle this.x params *)
    | Ast.ThisParam _ ->
      (context, type_list @ [UnknownType])
  in
  let (ctx_after_params, param_types) = List.fold_left compile_one_type (context, []) params in
  let (new_ctx, return_type) = compile_type ctx_after_params returns in
  (new_ctx, param_types, return_type)

and compile_function (context, out_list) = function
  | Ast.ExternalFunc _ -> (context, out_list)
  | Ast.ConcreteFunc (span, name, fsig, block) ->
    compile_concrete_function context out_list (span, name, fsig, block)

and compile_concrete_function context out_list (span, name, fsig, stmts) =
  (* If this module is not in the universe, don't compile it. *)
  if not (StringMap.mem context.this_module context.universe.modules) then
    let error_msg = "No module exists at path \"" ^ context.this_module ^ "\"." in
    let new_ctx = emit_error context span error_msg in
    (new_ctx, out_list)
  else
    (* Figure out which module we are in, so we can then work out the qualified name. *)
    let this_module = StringMap.find context.this_module context.universe.modules in
    let {path; _} = !this_module in
    let qualified = Sema.qualify [path; name] in

    (* Compile the function signature, so we can get param+return types *)
    let (ctx_after_sig, params, returns) = compile_function_signature context fsig in

    (* Make a new scope+context, with all params injected as values. *)
    let ctx_with_params =
      let new_scope =
        let combined_list =
          let (_, ast_params, _) = fsig in
          let param_names = List.map Ast.name_of_param ast_params in
          List.combine param_names params
        in
        let pair_list =
          let pair_of_combined (name, typ) =
            (name, VarSymbol (name, typ))
          in
          List.map pair_of_combined combined_list
        in
        let pair_seq = List.to_seq pair_list in
        let child_map = StringMap.of_seq pair_seq in
        Scope.ChildScope (context.scope, child_map)
      in
      {ctx_after_sig with scope = new_scope}
    in

    (* Next, compile the statements in turn. *)
    let (ctx_after_stmts, instrs, _) = List.fold_left compile_stmt (ctx_with_params, [], returns) stmts in

    (* Finally, just create the function object. *)
    let func = (qualified, params, returns, instrs) in
    (ctx_after_stmts, out_list @ [func])

and compile_stmt (context, out_list, expected_return) = function
  (* If we get an expression, just compile it. *)
  (* TODO: Do these actually generate? *)
  | Ast.Expr (span, v) -> begin
      match compile_expr context v with
      | (new_ctx, _, None) -> (new_ctx, out_list, expected_return)
      | (new_ctx, _, (Some value)) ->
        (new_ctx, out_list @ [(span, Value value)], expected_return)
    end
  | Ast.Return (span, value_opt) -> begin
      let (new_ctx, actual_return_type, value) = match value_opt with
        | None -> (context, VoidType, None)
        (* If we are returning a value, compile it, and compare the resulting type. *)
        | Some v -> begin
            let (new_ctx, typ, value) = compile_expr context v in
            (new_ctx, typ, value)
          end
      in

      (* If we can't cast, report an error. *)
      (* TODO: Support ad-hoc casts *)
      if not (can_cast_type actual_return_type expected_return) then
        let left = string_of_type actual_return_type in
        let right = string_of_type expected_return in
        let error_msg = "Cannot return " ^ left ^ " from a function declared to return " ^ right ^ "." in
        ((emit_error new_ctx span error_msg), out_list, expected_return)
      else
        (* Otherwise, emit a return. *)
        let instr =
          match value with
          | None -> ReturnVoid
          | Some v -> Return (expected_return, v)
        in
        (new_ctx, (out_list @ [(span, instr)]), expected_return)
    end
  (* If we reach variable declarations, then each one will create a new context. *)
  | Ast.VarDecl decls ->
    (* TODO: Handle duplicate symbols *)
    let compile_var_decl (context, out_list) (span, _, name, expr) =
      (* Compile the expression. If resolution fails, emit an error.
          Otherwise, inject the value in the scope. *)
      let (new_ctx, typ, value_opt) = compile_expr context expr in
      match value_opt with
      | None -> 
        let error_msg = "Compiling this variable declaration produced an error." in
        ((emit_error new_ctx span error_msg), out_list)
      | Some value -> begin
          (* TODO: SSA variables - get a unique name for each *)
          let ssa_name = name in
          let sym = VarSymbol (ssa_name, typ) in
          let new_scope = Scope.add name sym context.scope in
          let instr = VarAssn (ssa_name, typ, value) in
          (({ new_ctx with scope = new_scope }), (out_list @ [(span, instr)]))
        end
    in
    let (new_ctx, new_out_list) = List.fold_left compile_var_decl (context, out_list) decls in
    (new_ctx, new_out_list, expected_return)
  (* TODO: If we get a block, we need to create a new scope, AND a new block. *)
  | Ast.Block (_, stmts) -> 
    let new_scope = Scope.ChildScope (context.scope, StringMap.empty) in
    let child_context = { context with scope = new_scope } in
    let compile_one_stmt (context, out_list) stmt =
      let (new_ctx, new_out_list, _) = compile_stmt (context, out_list, expected_return) stmt in
      (new_ctx, new_out_list)
    in
    let (new_ctx, new_out_list) = List.fold_left compile_one_stmt (child_context, out_list) stmts in
    (new_ctx, new_out_list, expected_return)

and compile_expr context = function
  (* TODO: Other exprs *)
  | Ast.IntLiteral (_, v) -> (context, IntType, Some (IntLiteral v))
  | Ast.DoubleLiteral (_, v) -> (context, DoubleType, Some (DoubleLiteral v))
  | Ast.BoolLiteral (_, v) -> (context, BoolType, Some (BoolLiteral v))
  | Ast.Paren (_, inner) -> compile_expr context inner
  (* If we find a reference, just figure out if it's a value. *)
  | Ast.Ref (span, name) -> begin
      if not (Scope.mem name context.scope) then
        let error_msg = Scope.does_not_exist name in
        ((emit_error context span error_msg), UnknownType, None)
      else
        let not_a_value sym = 
          let error_msg = "The name \"" ^ name ^ "\" (resolves to " ^ (string_of_symbol sym) ^ ") does not resolve to a value." in
          ((emit_error context span error_msg), UnknownType, None)
        in
        match Scope.find name context.scope with
        | VarSymbol (name, typ) -> (context, typ, Some (VarGet (name, typ)))
        | _ as sym -> not_a_value sym
    end
  | _ -> (context, UnknownType, None)

and compile_type context = function
  | Ast.TypeRef (span, name) -> begin
      if not (Scope.mem name context.scope) then
        let error_msg = Scope.does_not_exist name in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, UnknownType)
      else
        let not_a_type sym =
          let error_msg = (string_of_symbol sym) ^ " is not a type." in
          let new_ctx = emit_error context span error_msg in
          (new_ctx, UnknownType)
        in
        match Scope.find name context.scope with
        | TypeSymbol typ -> (context, typ)
        | _ as sym -> not_a_type sym
    end

(** Shortcut for emitting an error, and returning a new context object. *)
and emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}

(** Checks if a can be casted to b. *)
and can_cast_type a b =
  (* TODO: Check classes for inheritance *)
  (* TODO: Support casts from primitive types *)
  match (a, b) with
  (* | (IntType, DoubleType) -> true *)
  | _ -> a == b