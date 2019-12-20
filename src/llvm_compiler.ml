open Sema

type context =
  {
    module_name: string;
    universe: universe;
    scope: Sema.symbol StringMap.t;
    errors: error list;
    expected_return_type: typ;

    llvm_builder: Llvm.llbuilder;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    llvm_scope: Llvm.llvalue StringMap.t;
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
  let llvm_builder = Llvm.builder llvm_context in
  let scope = StringMap.empty in
  let llvm_scope = StringMap.empty in
  let initial_context =
    {
      module_name = name;
      universe = new_universe;
      scope;
      errors = [];
      expected_return_type = VoidType;

      llvm_builder;
      llvm_context;
      llvm_module = Llvm.create_module llvm_context name;
      llvm_scope;
    } 
  in

  (* TODO: Compile declarations in turn. *)

  (* TODO: Use optimizers *)
  (* TODO: Return IR instead of dumping *)
  Llvm.dump_module initial_context.llvm_module

and compile_stmt context = function
  (* If we hit a return, make sure that we are returning the correct type. *)
  | Ast.Return (span, v_opt) -> begin
      let (new_ctx, actual_return_type, value) = match v_opt with
        | None -> (context, VoidType, None)
        (* If we are returning a value, compile it, and compare the resulting type. *)
        | Some v -> begin
            let (new_ctx, typ, value) = compile_expr context v in
            (new_ctx, typ, Some value)
          end
      in

      if not (can_cast_type actual_return_type new_ctx.expected_return_type) then
        (* TODO: More descriptive error *)
        let error_msg = "Cast failure" in
        ((emit_error new_ctx span error_msg), None)
      else
        match value with
        | None -> (new_ctx, Some(Llvm.build_ret_void context.llvm_builder))
        | Some v -> (new_ctx, Some(Llvm.build_ret v context.llvm_builder))
    end
  | _ -> (context, None)

and compile_expr context = function
  | Ast.IntLiteral (_, v) -> (context, IntType, Llvm.const_int (Llvm.i64_type context.llvm_context) v)
  | Ast.DoubleLiteral (_, v) -> (context, DoubleType, Llvm.const_float (Llvm.double_type context.llvm_context) v)
  | Ast.BoolLiteral (_, v) ->
    let value = if v then 1 else 0 in
    (context, BoolType, Llvm.const_int (Llvm.i8_type context.llvm_context) value)
  (* TODO: If we hit an identifier, we have to look it up to see if we can access it. *)
  | Ast.Ref (_, _) ->
    (context, IntType, Llvm.const_int (Llvm.i8_type context.llvm_context) 48)

(** Converts a Sema type (not AST) into LLVM. *)
and llvm_of_sema_type context = function
  | IntType -> Llvm.i64_type context
  | DoubleType -> Llvm.double_type context
  | BoolType -> Llvm.i8_type context
  | VoidType -> Llvm.void_type context
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

(** Checks if a can be casted to b. *)
and can_cast_type a b =
  (* TODO: Check classes for inheritance *)
  match (a, b) with
  | _ -> a == b

(* General helpers *)

(** Shortcut for emitting an error, and returning a new context object. *)
and emit_error context span error_msg =
  let error = (span, Error, error_msg) in
  {context with errors = context.errors @ [error]}

(** Looks up a symbol, which we know exists.  *)
and lookup_symbol context module_name symbol_name =
  let m = StringMap.find module_name context.universe.modules in
  StringMap.find symbol_name m.members