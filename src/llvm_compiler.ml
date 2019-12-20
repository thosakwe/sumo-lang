open Sema

type context =
  {
    module_name: string;
    universe: universe;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    scope: Sema.symbol StringMap.t;
    errors: error list;
  }
and error_level =
  | Error
  | Warning
and error = Ast.span * error_level * string

(** Compiles a compilation unit into LLVM, given the provided
    universe as context. *)
let rec compile name c_unit universe =
  (* First, create a module full of "unresolved" members. *)
  (* Then, add that module to the universe. *)
  let unresolved_of_decl = function
    | Ast.FuncDecl (_, vis, func) -> (Ast.name_of_func func, (vis, UnresolvedFunc func))
  in
  let members = StringMap.of_seq (List.to_seq (List.map unresolved_of_decl c_unit)) in
  let unresolved_module = {name; members} in
  let new_universe = {modules = StringMap.add name unresolved_module universe.modules } in

  (*
    Now that we have the module, make an initial context.
    Then, resolve every symbol against this context, which in turn will produce
    new context values. Once all resolution is done, we can then emit the LLVM code. *)
  let llvm_context = Llvm.global_context () in
  let scope = StringMap.empty in
  let initial_context =
    {
      module_name = name;
      universe = new_universe;
      llvm_context;
      llvm_module = Llvm.create_module llvm_context name;
      scope;
      errors = [];
    } 
  in

  (* TODO: Compile declarations in turn. *)

  (* TODO: Use optimizers *)
  (* TODO: Return IR instead of dumping *)
  Llvm.dump_module initial_context.llvm_module

(** Converts a Sema type (not AST) into LLVM. *)
and llvm_of_sema_type context = function
  | IntType -> Llvm.i64_type context
  | FloatType -> Llvm.double_type context
  | BoolType -> Llvm.i8_type context
  | OptionalType inner -> begin
      (* Optional scalars manifest as a struct of { exists: bool; value: T } *)
      (* TODO: optional types for pass-by-reference *)
      Llvm.struct_type context [| 
        Llvm.i8_type context;
        (llvm_of_sema_type context inner)
      |]
    end

(** Attempts to convert an AST type into a Sema type, also returning a
    new version of the context (i.e. if any errors occur.) 

    Returns: (new_context, typ option)
*)
and sema_of_ast_typ context = function
  (* Optional types are handled by simply wrapping the inner type.
      If inner resolution fails, then this resolution will also fail. *)
  | Ast.OptionalType (_, inner) -> begin
      match (sema_of_ast_typ context inner) with
      | (new_ctx, None) -> (new_ctx, None)
      | (new_ctx, Some typ) -> (new_ctx, Some (OptionalType typ))
    end
  (* Type references are resolved by trying to look up the name in the scope.
      The resolution is only successful if a Type is found. *)
  | Ast.TypeRef (span, name) -> begin
      if not (StringMap.mem name context.scope) then
        let error_msg = "No type named \"" ^ name ^ "\" exists in this context." in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, None)
      else
        (* At this point, any resolution failure should return the same message,
            so here's a helper function. *)
        let failure =
          let error_msg = "The name \"" ^ name ^ "\" does not resolve to a type." in
          emit_error context span error_msg
        in
        match (StringMap.find name context.scope) with
        | ModuleMember (module_name, symbol_name) -> begin
            match lookup_symbol context module_name symbol_name with
            (* If we find a type, we also need to be sure we have access to it. *)
            | (vis, Type typ) -> begin
                match vis with
                (* If it's public, we have access to it. *)
                | Visibility.Public -> (context, Some typ)
                | _ ->
                  (* Otherwise, we only have access if we are in the same module. *)
                  if module_name = context.module_name then
                    (context, Some typ)
                  else
                    let error_msg = "The type \"" ^ symbol_name ^ "\" cannot be accessed in this context." in
                    let new_ctx = emit_error context span error_msg in
                    (new_ctx, None)
              end
            | _ -> (failure, None)
          end
        | _ -> (failure, None)
    end

(* General helpers *)

(** Shortcut for emitting an error, and returning a new context object. *)
and emit_error context span error_msg =
  let error = (span, Error, error_msg) in
  {context with errors = context.errors @ [error]}

(** Looks up a symbol, which we know exists.  *)
and lookup_symbol context module_name symbol_name =
  let m = StringMap.find module_name context.universe.modules in
  StringMap.find symbol_name m.members