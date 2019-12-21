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
    llvm_scope: Llvm.llvalue Scope.t;
  }

(** Compiles a compilation unit into LLVM, given the provided
    universe as context. *)
let rec compile name c_unit universe =
  (* First, create a module full of "unresolved" members. *)
  (* Then, add that module to the universe. *)
  (* TODO: Add unresolved modules to scope, for lazy evaluation. *)
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

  (* Create an initial scope. *)
  (* Create ModuleMembers for all unresolved members, and add them to the scope. *)
  let root_scope = Scope.of_seq (List.to_seq [
      ("int", TypeSymbol IntType);
      ("double", TypeSymbol DoubleType);
      ("bool", TypeSymbol BoolType);
      ("void", TypeSymbol VoidType)
    ])
  in
  let initial_scope =
    let module_name = name in
    let f name _ = ModuleMember (module_name, name) in
    let m = StringMap.mapi f members in
    Scope.ChildScope (root_scope, m)
  in

  let llvm_context = Llvm.global_context () in
  let llvm_builder = Llvm.builder llvm_context in
  let llvm_scope = Scope.empty in
  let initial_context =
    {
      module_name = name;
      universe = new_universe;
      scope = initial_scope;
      errors = [];
      expected_return_type = VoidType;

      llvm_builder;
      llvm_context;
      llvm_module = Llvm.create_module llvm_context name;
      llvm_scope;
    } 
  in

  (* TODO: Compile members in turn. *)
  let uncompiled_members = List.of_seq (StringMap.to_seq unresolved_module.members) in
  let compile_one_member context member =
    let (new_ctx, _) = compile_member context member in
    new_ctx
  in
  let final_ctx = List.fold_left compile_one_member initial_context uncompiled_members in

  (* Return the final context, after validation.. *)
  (* Llvm.dump_module final_ctx.llvm_module; *)
  (* Llvm_analysis.assert_valid_module final_ctx.llvm_module; *)
  final_ctx

(* TODO: Handle duplicate symbols. *)
and compile_member context = function
  (* If we find an unresolved function, compile it, and add it to the scope. *)
  | (name, (vis, UnresolvedFunc func)) -> begin
      let (new_ctx, new_member, llvm_func_opt) = compile_func context func in
      match llvm_func_opt with
      | None -> (new_ctx, UnresolvedFunc func)
      | Some llvm_func -> begin
          let m = StringMap.find context.module_name new_ctx.universe.modules in
          let new_module_members = StringMap.add name (vis, new_member) m.members in
          let new_m = {m with members = new_module_members} in
          let new_modules = StringMap.add name new_m new_ctx.universe.modules in
          let new_universe = {modules = new_modules} in
          let new_scope =
            match new_member with
            | Func (span, _, params, returns) ->
              let func_type = FunctionType (params, returns) in
              Scope.replace name (ValueSymbol (span, func_type)) new_ctx.scope
            | _ -> new_ctx.scope
          in
          let new_llvm_scope = Scope.replace name llvm_func new_ctx.llvm_scope in
          let result_ctx = 
            {
              new_ctx with
              universe = new_universe;
              llvm_scope = new_llvm_scope;
              scope = new_scope;
            }
          in
          (result_ctx, new_member)
        end
    end
  | (_, (_, self)) -> (context, self)


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
          let resolved_func = Func (span, c_name, [], sema_returns) in
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
  let (_, params, ast_returns) = sign in
  match sema_of_ast_typ context ast_returns with
  | (new_ctx, None) -> (new_ctx, None, VoidType)
  | (new_ctx, (Some returns)) -> begin
      let llvm_returns = llvm_of_sema_type context.llvm_context returns in
      (* This function compiles a parameter type into LLVM, and returns a new context. *)
      let compile_param (context, llvm_params) = function
        | Ast.RegularParam (_, _, ast_type) -> begin
            match  sema_of_ast_typ context ast_type with
            | (new_ctx, None) -> (new_ctx, llvm_params)
            | (new_ctx, Some sema_type) ->
              let llvm_type = llvm_of_sema_type new_ctx.llvm_context sema_type in
              (new_ctx, llvm_params @ [llvm_type])
          end
        (* TODO: Compile this.x params within a class *)
        (* TODO: Produce an error when a this.param is seen OUTSIDE of a class. *)
        | _ -> (context, llvm_params)
      in
      let (result_ctx, llvm_params) = List.fold_left compile_param (new_ctx, []) params in
      let llvm_sig = Llvm.function_type llvm_returns (Array.of_list llvm_params)  in
      (result_ctx, Some llvm_sig, returns)
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
  (* If we reach variable declarations, then each one will create a new context. *)
  | Ast.VarDecl decls ->
    (* TODO: Handle duplicate symbols *)
    let compile_var_decl context (span, _, name, expr) =
      (* Compile the expression. If resolution fails, emit an error.
          Otherwise, inject the value in the scope. *)
      let (new_ctx, typ, value_opt) = compile_expr context expr in
      match value_opt with
      | None -> 
        let error_msg = "Compiling this variable declaration produced an error." in
        emit_error new_ctx span error_msg
      | Some value -> begin
          (* TODO: SSA variables *)
          let sym = ValueSymbol (span, typ) in
          (* Emit the LLVM variable, and return the new context. *)
          let llvm_type = Llvm.type_of value in
          let variable = Llvm.build_alloca llvm_type name new_ctx.llvm_builder in
          let _ = Llvm.build_store value variable new_ctx.llvm_builder in
          let new_llvm_scope = Scope.add name variable context.llvm_scope in
          let new_scope = Scope.add name sym context.scope in
          { new_ctx with scope = new_scope; llvm_scope = new_llvm_scope }
        end
    in
    let new_ctx = List.fold_left compile_var_decl context decls in
    (* TODO: Should we really return None here??? *)
    (new_ctx, None)

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
  | Ast.Ref (span, name) -> begin
      (* If the name doesn't exist, report an error. *)
      if not (Scope.mem name context.scope) then
        let error_msg = Scope.does_not_exist name in
        let new_ctx = emit_error context span error_msg in
        (* let dump name sym =
           print_string (name ^ ": ");
           print_endline (string_of_symbol sym)
           in
           Scope.iter dump context.scope; *)
        (new_ctx, VoidType, None)
      else
        (* Otherwise, ensure it's a value. *)
        (* "failure" is just a helper to create the same error in different cases. *)
        let sym = Scope.find name context.scope in
        match sym with
        (* TODO: Finish this resolution logic *)
        | ValueSymbol (_, typ) ->
          (* Fetch the LLVM value. *)
          let llvm_value = Scope.find name context.llvm_scope in
          let value = match typ with
            | FunctionType _ ->  llvm_value
            | _ -> Llvm.build_load llvm_value name context.llvm_builder
          in
          (context, typ, Some value)
        (* If we find a module member, determine if we have access, and if it's a value. *)
        | ModuleMember (module_name, symbol_name) -> begin
            (* TODO: If we find a value, we also need to be sure we have access to it. *)
            let (vis, member) = lookup_module_member context module_name symbol_name in
            let can_access =
              match vis with
              | Visibility.Public -> true
              | _ -> module_name = context.module_name
            in
            let cannot_access =
              let error_msg = "The type \"" ^ symbol_name ^ "\" cannot be accessed in this context." in
              let new_ctx = emit_error context span error_msg in
              (new_ctx, VoidType, None)
            in
            (* TODO: Qualify names to fetch globals/functions *)
            match compile_member context (symbol_name, (vis, member)) with
            | (new_ctx, Global (_, typ)) ->
              if not can_access then
                cannot_access
              else
                let llvm_value = Scope.find name new_ctx.llvm_scope in
                let value = Llvm.build_load llvm_value name context.llvm_builder in
                (new_ctx, typ, Some value)
            | (new_ctx, Func (_, name, params, returns)) -> 
              if not can_access then
                cannot_access
              else begin
                match Llvm.lookup_function name new_ctx.llvm_module with
                | None -> 
                  let error_msg = ("No LLVM function named \"" ^ name ^ "\" has been defined. This is a compiler bug. :(") in
                  ((emit_error context span error_msg), VoidType, None)
                | Some func ->
                  let ftyp = FunctionType (params, returns) in
                  (new_ctx, ftyp, Some func)
              end
            | (new_ctx, _) -> 
              let error_msg = module_name ^ "." ^ symbol_name ^ " is not a value." in
              ((emit_error new_ctx span error_msg), VoidType, None)
          end
        | _ -> 
          let error_msg = "The name \"" ^ name ^ "\" (resolves to " ^ (string_of_symbol sym) ^ ") does not resolve to a value." in
          ((emit_error context span error_msg), VoidType, None)
    end
  | Ast.Call (span, target, args) -> begin
      (* TODO: Validate arg types, etc. *)
      match compile_expr context target with
      | (new_ctx, _, None) ->
        let error_msg = "Evaluation of the call target produced an error." in
        let failure = emit_error new_ctx span error_msg in
        (failure, VoidType, None)
      (* TODO: Check target type to ensure it's a function. *)
      | (new_ctx, FunctionType (_, returns), Some llvm_target) -> begin
          (* Compile each arg into an LLVM value, but also return a new context. *)
          let llvm_of_arg (context, output_list) arg =
            match compile_expr context arg with
            | (new_ctx, _, None) -> (new_ctx, output_list)
            | (new_ctx, _, Some value) -> 
              (new_ctx, output_list @ [value])
          in
          let (next_ctx, llvm_args) = List.fold_left llvm_of_arg (new_ctx, []) args in
          let llvm_arg_array = Array.of_list llvm_args in
          (* If the function returns void, return no value. *)
          match returns with
          | VoidType -> 
            let value = Llvm.build_call llvm_target llvm_arg_array "" context.llvm_builder in
            (next_ctx, returns, Some value)
          | _ -> 
            let value = Llvm.build_call llvm_target llvm_arg_array "tmp" context.llvm_builder in
            (* TODO: Get return type from FunctionType *)
            (next_ctx, returns, Some value)
        end
      | (new_ctx, typ, _) ->
        let error_msg = "Cannot call a value of type " ^ (string_of_type typ) ^ " as a function." in
        let next_ctx = emit_error new_ctx span error_msg in
        (next_ctx, VoidType, None)
    end

(** Converts a Sema type (not AST) into LLVM. *)
and llvm_of_sema_type context = function
  | IntType -> Llvm.i64_type context
  | DoubleType -> Llvm.double_type context
  | BoolType -> Llvm.i8_type context
  | VoidType -> Llvm.void_type context
  (* | OptionalType inner -> begin
      (* Optional scalars manifest as a struct of { exists: bool; value: T } *)
      (* TODO: optional types for pass-by-reference *)
      Llvm.struct_type context [| 
        Llvm.i8_type context;
        (llvm_of_sema_type context inner)
      |]
    end *)
  | FunctionType (params, returns) ->
    let llvm_returns = llvm_of_sema_type context returns in
    let llvm_params = List.map (function x -> llvm_of_sema_type context x) params in
    Llvm.function_type llvm_returns (Array.of_list llvm_params)

(** Attempts to convert an AST type into a Sema type, also returning a
    new version of the context (i.e. if any errors occur.) 

    Returns: (new_context, typ option)
*)
and sema_of_ast_typ context = function
  (* Optional types are handled by simply wrapping the inner type.
      If inner resolution fails, then this resolution will also fail. *)
  (* | Ast.OptionalType (_, inner) -> begin
      match (sema_of_ast_typ context inner) with
      | (new_ctx, None) -> (new_ctx, None)
      | (new_ctx, Some typ) -> (new_ctx, Some (OptionalType typ))
    end *)
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
            match lookup_module_member context module_name symbol_name with
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
and lookup_module_member context module_name symbol_name =
  let m = StringMap.find module_name context.universe.modules in
  StringMap.find symbol_name m.members

and string_of_error_level = function
  | Error -> "error"
  | Warning -> "warning"