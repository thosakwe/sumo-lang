open Ssa

type context =
  {
    this_module: string;
    errors: Sema.error list;
    scope: symbol Scope.t;
    universe: universe;
  }

let rec compile_single_ast path c_unit =
  let universe = load_ast_into_universe (Ssa.default_universe) path c_unit in
  let dump_module _ module_ref =
    let m = !module_ref in
    let dump_symbol name (_, sym) =
      print_endline (name ^ ": " ^ (string_of_symbol sym))
    in
    print_endline ("Module \"" ^ m.path ^ "\":");
    StringMap.iter dump_symbol m.symbols
  in
  StringMap.iter dump_module universe.modules;
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
  let new_context = {!ref_context with scope = new_scope } in

  (* Compile them, add them to the module, and return the universe. *)
  let compile_decl (context, out_list) = function
    | Ast.FuncDecl (_, _, func) -> compile_function (context, out_list) func
    (* | _ -> (context, out_list) *)
  in
  let (_, compiled_functions) = List.fold_left compile_decl (new_context, []) c_unit in
  this_module := {!this_module with compiled_functions };

  new_universe

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

and compile_concrete_function context out_list (span, name, fsig, _) =
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

    (* Finally, just create the function object. *)
    let func = (qualified, params, returns, []) in
    (ctx_after_sig, out_list @ [func])

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