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

let rec compile_ast_into_universe universe path c_unit =
  (* Before we actually compile anything, forward-declare all functions
   * in the module. After this, then we can then compile the actual
   * functions, and then compile everything into LLVM. *)
  (* TODO: Imports *)
  (* TODO: Forward-declare all types/records/structs before functions *)

  let context = { default_context with universe = universe } in

  let symbols =
    let pairs =
      let pair_of_decl = function
        | Ast.FuncDecl (_, vis, func) -> begin
            let s = Ast.signature_of_func func in
            let (params, returns) = compile_function_signature context s in
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

  let new_module = { path; symbols } in

  universe

and compile_function_signature context (_, params, returns) =
  ()

and compile_type context = function
  | Ast.TypeRef (span, name) -> IntType

(** Shortcut for emitting an error, and returning a new context object. *)
and emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}