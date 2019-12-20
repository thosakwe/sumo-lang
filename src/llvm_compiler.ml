open Sema

type context =
  {
    module_name: string;
    universe: universe;
    scope: Sema.symbol Scope.t;
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
  let llvm_scope = StringMap.empty in
  let initial_context =
    {
      module_name = name;
      universe = new_universe;
      scope = Scope.of_seq (List.to_seq [
          ("int", TypeSymbol IntType);
          ("double", TypeSymbol DoubleType);
          ("bool", TypeSymbol BoolType);
          ("void", TypeSymbol VoidType)
        ]);
      errors = [];
      expected_return_type = VoidType;

      llvm_builder;
      llvm_context;
      llvm_module = Llvm.create_module llvm_context name;
      llvm_scope;
    } 
  in

  (* TODO: Compile members in turn. *)
  let compile_member context = function
    | (name, (vis, UnresolvedFunc func)) -> begin
        let (new_ctx, new_member, _) = compile_func context func in
        let m = StringMap.find context.module_name new_ctx.universe.modules in
        let new_module_members = StringMap.add name (vis, new_member) m.members in
        let new_m = {m with members = new_module_members} in
        let new_modules = StringMap.add name new_m new_ctx.universe.modules in
        let new_universe = {modules = new_modules} in
        {new_ctx with universe = new_universe}
      end
    | _ -> context
  in
  let uncompiled_members = List.of_seq (StringMap.to_seq unresolved_module.members) in
  List.fold_left compile_member initial_context uncompiled_members

(* Return the final context. *)

(* TODO: Use optimizers *)
(* TODO: Return IR instead of dumping *)

and compile_decl context = function
  | Ast.FuncDecl (_, _, func) -> compile_func context func

and compile_func context func = 
  match func with
  (* TODO: Pass params[] to Func() *)
  (* If we hit an external function, simply declare it. *)
  | Ast.ExternalFunc (span, c_name_opt, name, sign) -> begin
      (* Resolve the signature. *)
      (* TODO: Report errors on signature resolution failure. *)
      match compile_func_signature context sign with
      | (new_ctx, None, _) -> (new_ctx, (UnresolvedFunc func), None)
      | (new_ctx, Some llvm_sign, sema_returns) -> begin
          (* Get the C Name, so we can ddclare it. *)
          let c_name = match c_name_opt with
            | None -> name
            | Some v -> v
          in
          let value = Llvm.declare_function c_name llvm_sign context.llvm_module in
          let resolved_func = Func (span, name, [], sema_returns) in
          (new_ctx, resolved_func, Some value)
        end

    end
  (* If we hit an actual function, define it. *)
  | Ast.ConcreteFunc (span, name, sign, block) -> begin
      (* Resolve the signature. *)
      (* TODO: Report errors on signature resolution failure. *)
      match compile_func_signature context sign with
      | (new_ctx, None, _) -> (new_ctx, (UnresolvedFunc func), None)
      | (new_ctx, Some llvm_sign, sema_returns) -> begin
          (* If we can compile the signature, create an LLVM function, and
              compile everything into it. *)
          let func = Llvm.define_function name llvm_sign context.llvm_module in
          let entry_block = Llvm.entry_block func in
          let child_builder = Llvm.builder context.llvm_context in
          let block_stmt = Ast.Block (span, block) in
          Llvm.position_at_end entry_block child_builder;

          (* Create a new child context with the given builder. *)
          let child_context = {
            new_ctx with 
            llvm_builder = child_builder;
            expected_return_type = sema_returns;
          } in

          let (result_ctx, result) = compile_stmt child_context block_stmt in
          let resolved_func = Func (span, name, [], sema_returns) in
          (result_ctx, resolved_func, result)
        end
    end

and compile_func_signature context sign =
  (* TODO: Compile params *)
  let (_, _, ast_returns) = sign in
  match sema_of_ast_typ context ast_returns with
  | (new_ctx, None) -> (new_ctx, None, VoidType)
  | (new_ctx, (Some returns)) -> begin
      let llvm_returns = llvm_of_sema_type context.llvm_context returns in
      (* TODO: Put params in this array *)
      let llvm_sig = Llvm.function_type llvm_returns [| |] in
      (new_ctx, Some llvm_sig, returns)
    end

and compile_stmt context = function
  (* If we hit a return, make sure that we are returning the correct type. *)
  (* TODO: Only allow certain types to be returned (no closures yet) *)
  | Ast.Return (span, v_opt) -> begin
      let (new_ctx, actual_return_type, value) = match v_opt with
        | None -> (context, VoidType, None)
        (* If we are returning a value, compile it, and compare the resulting type. *)
        | Some v -> begin
            let (new_ctx, typ, value) = compile_expr context v in
            (new_ctx, typ, value)
          end
      in

      if not (can_cast_type actual_return_type new_ctx.expected_return_type) then
        let left = string_of_type actual_return_type in
        let right = string_of_type new_ctx.expected_return_type in
        let error_msg = "Cannot return " ^ left ^ " from a function declared to return " ^ right ^ "." in
        ((emit_error new_ctx span error_msg), None)
      else
        match value with
        | None -> (new_ctx, Some(Llvm.build_ret_void context.llvm_builder))
        | Some v -> (new_ctx, Some(Llvm.build_ret v context.llvm_builder))
    end
  (* If we get an expression, just compile it. *)
  (* TODO: Do these actually generate? *)
  | Ast.Expr (_, v) ->
    let (new_ctx, _, value) = compile_expr context v in
    (new_ctx, value)
  (* TODO: If we get a block, we need to create a new scope, and a new block. *)
  | Ast.Block (_, stmts) -> 
    let compile_one_stmt context stmt =
      let (new_ctx, _) = compile_stmt context stmt in
      new_ctx
    in
    let new_ctx = List.fold_left compile_one_stmt context stmts in
    (new_ctx, None)
  | _ -> (context, None)

and compile_expr context = function
  | Ast.IntLiteral (_, v) ->
    (context, IntType, Some (Llvm.const_int (Llvm.i64_type context.llvm_context) v))
  | Ast.DoubleLiteral (_, v) ->
    (context, DoubleType, Some (Llvm.const_float (Llvm.double_type context.llvm_context) v))
  | Ast.BoolLiteral (_, v) ->
    let value = if v then 1 else 0 in
    (context, BoolType, Some (Llvm.const_int (Llvm.i8_type context.llvm_context) value))
  | Ast.Paren (_, inner) -> compile_expr context inner
  (* TODO: If we hit an identifier, we have to look it up to see if we can access it. *)
  | Ast.Ref (span, name) ->
    (* If the name doesn't exist, report an error. *)
    if not (Scope.mem name context.scope) then
      let error_msg = Scope.does_not_exist name in
      let new_ctx = emit_error context span error_msg in
      (new_ctx, VoidType, None)
    else
      (* Otherwise, ensure it's a value. *)
      (* "failure" is just a helper to create the same error in different cases. *)
      let failure =
        let error_msg = "The name \"" ^ name ^ "\" does not resolve to a type." in
        emit_error context span error_msg
      in
      match Scope.find name context.scope with
      (* TODO: Finish this resolution logic *)
      (* TODO: Also missing ModuleMember lookup *)
      | ValueSymbol (_, typ) -> (context, typ, None)
      | _ -> (failure, VoidType, None)

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
      if not (Scope.mem name context.scope) then
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
        match (Scope.find name context.scope) with
        | TypeSymbol typ -> (context, Some typ)
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
  (* TODO: Support casts from primitive types *)
  match (a, b) with
  (* | (IntType, DoubleType) -> true *)
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

and string_of_error_level = function
  | Error -> "error"
  | Warning -> "warning"