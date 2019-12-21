open Ssa

type context =
  {
    errors: Sema.error list;
    scope: symbol Scope.t;
    universe: universe;
  }

let default_context =
  let root_scope = Scope.of_seq (List.to_seq [
      ("int", TypeSymbol IntType);
      ("double", TypeSymbol DoubleType);
      ("bool", TypeSymbol BoolType);
      ("void", TypeSymbol VoidType)
    ])
  in
  {
    errors = [];
    scope = root_scope;
    universe = default_universe
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

  let ref_context = ref { default_context with universe = universe } in

  let symbols =
    let pairs =
      let pair_of_decl = function
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
              let symbol = FuncSymbol (c_name, params, returns) in
              (name, (vis, symbol))
            | Ast.ConcreteFunc (_, name, _, _) ->
              let qualified = Sema.qualify [path; name] in
              let symbol = FuncSymbol (qualified, params, returns) in
              (name, (vis, symbol))
          end
      in
      List.map pair_of_decl c_unit
    in
    StringMap.of_seq (List.to_seq pairs)
  in

  {
    modules = StringMap.add path (ref { path; symbols }) universe.modules
  }

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