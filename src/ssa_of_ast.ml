open Ssa
open Ssa_context

let rec compile_single_ast path c_unit =
  let (context, universe) = load_ast_into_universe (Ssa.default_universe) path c_unit in
  (* List.iter (function x -> prerr_endline (Sema.string_of_error x)) context.errors; *)
  (universe, context)

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
      block_is_dead = false;
      this_module = path;
      errors = [];
      scope = root_scope;
      universe = default_universe;
      namer = Namer.empty;
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
              let symbol = FuncSymbol (true, c_name, params, returns, self) in
              (name, (vis, symbol))
            | Ast.ConcreteFunc (_, name, _, _) ->
              let qualified = Sema.qualify_function_name path name in
              let symbol = FuncSymbol (false, qualified, params, returns, self) in
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
    | Ast.RegularParam (_, name, typ) -> 
      let (new_ctx, result) = compile_type context typ in
      (new_ctx, type_list @ [(name, result)])
    (* TODO: Handle this.x params *)
    | Ast.ThisParam (_, name) ->
      (context, type_list @ [(name, UnknownType)])
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
    let qualified = Sema.qualify_function_name path name  in

    (* Compile the function signature, so we can get param+return types *)
    let (ctx_after_sig, params, returns) = compile_function_signature context fsig in

    (* Make a new scope+context, with all params injected as values. *)
    let ctx_with_params =
      let new_scope =
        (* let combined_list =
           let (_, ast_params, _) = fsig in
           let param_names = List.map Ast.name_of_param ast_params in
           List.combine param_names params
           in *)
        let pair_list =
          let fold_param (out_list, index) (name, typ) =
            let symbol = ParamSymbol (name, index, typ) in
            let result = (name, symbol) in
            (out_list @ [result], index + 1)
          in
          let (folded_params, _) = List.fold_left fold_param ([], 0) params in
          folded_params
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

and compile_stmt (initial_context, out_list, expected_return) stmt = 
  (* If the current block has already returned/broken, issue a warning. *)
  let handle_dead_code span initial_context =
    if initial_context.block_is_dead then
      emit_error initial_context span "Dead code."
    else initial_context
  in

  match stmt with
  (* If we get an expression, just compile it. *)
  (* TODO: Do these actually generate? *)
  | Ast.Expr (span, v) -> begin
      let context = handle_dead_code span initial_context in
      match compile_expr context v with
      | (new_ctx, _, None) -> (new_ctx, out_list, expected_return)
      | (new_ctx, _, (Some value)) ->
        (new_ctx, out_list @ [(span, Value value)], expected_return)
    end
  | Ast.Return (span, value_opt) -> begin
      let context = handle_dead_code span initial_context in
      let (new_ctx, actual_return_type, value) = match value_opt with
        | None -> (context, VoidType, None)
        (* If we are returning a value, compile it, and compare the resulting type. *)
        | Some v -> begin
            let (new_ctx, typ, value) = compile_expr context v in
            (new_ctx, typ, value)
          end
      in

      (* Attempt to cast the value. If we can't cast, report an error.
       * Note that cast_value will produce the necessary error. *)
      match cast_value new_ctx span value actual_return_type expected_return with
      | (_, Error _) ->
        let error_msg =
          "Cannot return a value of type "
          ^ string_of_type actual_return_type
          ^ " from a function declared to return "
          ^ string_of_type expected_return
          ^ "."
        in
        ((emit_error new_ctx span error_msg), out_list, expected_return)
      | (out_ctx, Ok coerced_value) ->
        (* Otherwise, emit a return. *)
        let instr =
          match coerced_value with
          | None -> ReturnVoid
          | Some v -> Return (expected_return, v)
        in
        ({out_ctx with block_is_dead = true}, (out_list @ [(span, instr)]), expected_return)
    end
  (* If we reach variable declarations, then each one will create a new context. *)
  | Ast.VarDecl decls ->
    (* TODO: Handle duplicate symbols *)
    let compile_var_decl (initial_context, out_list) (span, final, declared_type, name, expr) =
      (* Compile the expression. If resolution fails, emit an error.
          Otherwise, inject the value in the scope. *)
      let context = handle_dead_code span initial_context in
      let (new_ctx, typ, value_opt) = compile_expr context expr in
      match value_opt with
      | None -> 
        let error_msg = "Compiling this variable declaration produced an error." in
        ((emit_error new_ctx span error_msg), out_list)
      | Some _ -> begin
          (* If we have a declared type, cast it before assigning. *)
          let (ctx_after_type, target_type) = match declared_type with
            | None -> (new_ctx, typ)
            | Some t -> compile_type new_ctx t
          in

          match cast_value ctx_after_type span value_opt typ target_type with
          | (ctx_after_cast, Error _) ->
            (ctx_after_cast, out_list)
          | (ctx_after_cast, Ok coerced_value_opt) -> begin
              match coerced_value_opt with
              | None -> 
                let error_msg =
                  "The right-hand side of this variable declaration did not produce a valid value."
                in
                ((emit_error ctx_after_cast span error_msg), out_list)
              | Some coerced_value ->
                (* TODO: SSA variables - get a unique name for each *)
                let ssa_name = name in
                let sym = VarSymbol (final, ssa_name, target_type) in
                let new_scope = Scope.add name sym context.scope in
                let new_instrs = [
                  (span, Value (VarCreate (ssa_name, target_type)));
                  (span, Value (VarSet (ssa_name, target_type, coerced_value)));
                ]
                in
                (({ ctx_after_cast with scope = new_scope }), (out_list @ new_instrs))
            end
        end
    in
    let (new_ctx, new_out_list) = List.fold_left compile_var_decl (initial_context, out_list) decls in
    (new_ctx, new_out_list, expected_return)
  (* If we get a block, we need to create a new scope, AND a new block. *)
  | Ast.Block (span, stmts) -> 
    let context = handle_dead_code span initial_context in
    let new_scope = Scope.ChildScope (context.scope, StringMap.empty) in
    let child_context = { context with scope = new_scope } in
    let compile_one_stmt (context, out_list) stmt =
      let (new_ctx, new_out_list, _) = compile_stmt (context, out_list, expected_return) stmt in
      (new_ctx, new_out_list)
    in
    let (ctx_after_stmts, new_out_list) = List.fold_left compile_one_stmt (child_context, out_list) stmts in
    let (name, new_namer) = Namer.next_name "block" ctx_after_stmts.namer in
    let new_ctx = { 
      context with 
      namer = new_namer; 
      errors = ctx_after_stmts.errors ;
      block_is_dead = context.block_is_dead || ctx_after_stmts.block_is_dead;
    } in
    let instrs = [
      (span, Block (name, new_out_list));
      (span, Jump name);
    ]
    in
    (new_ctx, (out_list @ instrs), expected_return)

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
          let error_msg = "The name \"" ^ name ^ "\" resolves to " ^ (string_of_symbol sym) ^ ", which is not a value." in
          ((emit_error context span error_msg), UnknownType, None)
        in
        match Scope.find name context.scope with
        | VarSymbol (_, name, typ) -> (context, typ, Some (VarGet (name, typ)))
        | ParamSymbol (name, index, typ) -> (context, typ, Some (ParamGet (index, name, typ)))
        | _ as sym -> not_a_value sym
    end
  | Ast.Assign (span, target, op, value) -> compile_assign context (span, target, op, value)
  (* If we find a call, there's quite a bit we have to do properly resolve it. *)
  | Ast.Call (span, target, args) -> begin
      (* 1. Make sure target is a function.
       * 2. Ensure correct # of args
       * 3. Ensure correct arg types
       * 4. TODO: Create new instances of classes, or invoke closures.
      *)

      (* It's important to note, though, that at this time, we don't have closures,
       * so the only expressions that can actually be called are identifiers. *)
      match Ast.innermost_expr target with
      (* If it IS an identifier, look it up in the scope, to see if it's a function. *)
      | Ast.Ref (span, name) -> begin
          (* If it doesn't exist, report an error, of course. *)
          if not (Scope.mem name context.scope) then
            let error_msg = (Scope.does_not_exist name) ^ " It cannot be called as a function." in
            let new_ctx = emit_error context span error_msg in
            (new_ctx, UnknownType, None)
          else
            match Scope.find name context.scope with
            (* If we find a function, then verify the number of args. *)
            | FuncSymbol (_, func_name, params, returns, _) -> begin
                if (List.length args) != (List.length params) then 
                  let error_msg =
                    "The function \"" ^ func_name ^ "\" expects "
                    ^ (string_of_int (List.length params))
                    ^ " argument(s), but "
                    ^ (string_of_int (List.length args))
                    ^ " argument(s) were provided instead."
                  in
                  ((emit_error context span error_msg), UnknownType, None)
                else
                  (* If we have the correct number of args, then perform type-checking. *)
                  let params_to_args = List.combine params args in
                  let check_one_pair (context, out_list, success) ((name, param_type), arg) =
                    (* The type-check only fails if we reach a cast failure. *)
                    let (new_ctx, typ, value_opt) = compile_expr context arg in
                    let cast_failure = 
                      let error_msg =
                        "Cannot cast argument of type " ^ (string_of_type typ)
                        ^ " to type " ^ (string_of_type param_type)
                        ^ " for argument \"" ^ name
                        ^ "\"."
                      in
                      ((emit_error new_ctx span error_msg), out_list, false)
                    in
                    match cast_value context span value_opt typ param_type with
                    | (out_ctx, Error _) ->
                      (out_ctx, out_list, false)
                    | (out_ctx, Ok coerced_value_opt) ->
                      match coerced_value_opt with
                      | None -> cast_failure
                      | Some value -> (out_ctx, (out_list @ [value]), success)
                  in
                  let (new_ctx, compiled_args, success) =
                    List.fold_left check_one_pair (context, [], true) params_to_args
                  in
                  if not success then
                    (new_ctx, UnknownType, None)
                  else
                    (* Everything is okay, emit the call. *)
                    let value = FunctionCall (returns, func_name, compiled_args) in
                    (new_ctx, returns, Some value)
              end
            | _ as sym ->  
              let error_msg = "The name \"" ^ name ^ "\" resolves to " ^ (string_of_symbol sym) ^ ", which is not a value." in
              ((emit_error context span error_msg), UnknownType, None)
        end
      | _ ->
        let error_msg = "Only top-level symbols may be called as functions." in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, UnknownType, None)
    end
  (* Compile both the LHS and RHS, and then try to combine them. *)
  | Ast.Binary (span, lhs_ast, op, rhs_ast) -> begin
      let (ctx_after_left, lhs_type, lhs_opt) = compile_expr context lhs_ast in
      match lhs_opt with
      | None ->
        let error_msg =
          "The left hand side of this expression does not produce a valid value." 
        in
        let new_ctx = emit_error ctx_after_left span error_msg in
        (new_ctx, UnknownType, None)
      (* Check if the LHS type has the given operator. *)
      | Some lhs -> begin
          let (ctx_after_right, rhs_type, rhs_opt) = compile_expr ctx_after_left rhs_ast in
          match lhs_type with
          (* For native numbers, both operands must be the same type.
           * If this condition is met, emit a corresponding instruction. *)
          | IntType | DoubleType -> begin
              match cast_value ctx_after_right span rhs_opt rhs_type lhs_type with
              | (new_ctx, Error _) -> (new_ctx, UnknownType, None)
              | (ctx_after_cast, Ok coerced_rhs_opt) -> begin
                  match coerced_rhs_opt with
                  | None -> 
                    let error_msg =
                      "The right hand side of this expression does not produce a valid value." 
                    in
                    let new_ctx = emit_error ctx_after_cast span error_msg in
                    (new_ctx, UnknownType, None)
                  | Some rhs -> begin
                      let value = 
                        match lhs_type with
                        | IntType -> IntArithmetic (lhs, op, rhs)
                        | _ -> DoubleArithmetic (lhs, op, rhs)
                      in
                      (ctx_after_cast, lhs_type, Some value)
                    end
                end
            end
          | _ ->
            let error_msg =
              "The type "
              ^ string_of_type lhs_type
              ^ " has no '"
              ^ (Ast.string_of_binary_op op)
              ^ "' operator."
            in
            let new_ctx = emit_error ctx_after_right span error_msg in
            (new_ctx, UnknownType, None)
        end
    end

(** Compiles an assignment expression. *)
and compile_assign context = function
  (* If we get a +=, *=, etc. just desugar it. *)
  | (span, target, (Ast.BinaryAssign op), rhs) ->
    let target_expr = Ast.expr_of_assign_target target in
    let new_rhs = Ast.Binary (span, target_expr, op, rhs) in
    let new_assign = (span, target, Ast.Equals, new_rhs) in
    compile_assign context new_assign
  (* To re-assign a local variable, it must exist in the context, and be reassignable.
   * In addition, we must be able to cast the value to whatever type is expected. *)
  | (span, Ast.VariableTarget(_, name), Ast.Equals, rhs) -> begin
      if not (Scope.mem name context.scope) then
        let error_msg = Scope.does_not_exist name in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, UnknownType, None)
      else
        match Scope.find name context.scope with
        | VarSymbol (final, name, expected_type) -> begin
            if final then
              let error_msg =
                "The symbol \"" ^ name
                ^ "\" is final, and cannot be reassigned."
              in
              let new_ctx = emit_error context span error_msg in
              (new_ctx, UnknownType, None)
            else
              (* If this is a mutable symbol, try to compile+cast the value. *)
              match compile_expr context rhs with
              | (new_ctx, _, None) -> (new_ctx, UnknownType, None)
              | (ctx_after_expr, actual_type, Some value) -> begin
                  match cast_value ctx_after_expr span (Some value) actual_type expected_type with
                  | (new_ctx, Error _) -> (new_ctx, UnknownType, None)
                  | (new_ctx, Ok coerced_value_opt) -> begin
                      match coerced_value_opt with
                      | None ->
                        let error_msg =
                          "A value of type void cannot be used as the right-hand" 
                          ^ " side of an assignment."
                        in
                        ((emit_error new_ctx span error_msg), expected_type, coerced_value_opt)
                      | Some coerced_value ->
                        let items = [
                          VarSet (name, expected_type, coerced_value);
                          VarGet (name, expected_type);
                        ]
                        in
                        (new_ctx, expected_type, Some (Multi items))
                    end
                end
          end
        | _ as sym ->
          let error_msg =
            "The name \"" ^ name ^ "\" resolves to "
            ^ string_of_symbol sym
            ^ ", which is not a reassignable symbol."
          in
          let new_ctx = emit_error context span error_msg in
          (new_ctx, UnknownType, None)
    end
(* TODO: Handle other assignment cases *)
(* | (span, _, _, _) ->
   let error_msg = "I don't know how to compile this assignment (yet!)." in
   let new_ctx = emit_error context span error_msg in
   (new_ctx, None) *)

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

and emit_warning context span error_msg =
  let error = (span, Sema.Warning, error_msg) in
  {context with errors = context.errors @ [error]}

and cast_value context span value_opt from_type to_type =
  if from_type = to_type then
    (context, Ok value_opt)
  else
    match (from_type, to_type, value_opt) with
    | (IntType, DoubleType, Some value) ->
      let new_value = Some (CastIntToDouble value) in
      (context, Ok new_value)
    | (DoubleType, IntType, Some value) ->
      let new_value = Some (CastDoubleToInt value) in
      let warning_msg = "Casting a double to int loses precision." in
      let new_ctx = emit_warning context span warning_msg in
      (new_ctx, Ok new_value)
    | _ ->
      let left = string_of_type from_type in
      let right = string_of_type to_type in
      let error_msg = "Cannot cast a value of type " ^ left ^ " to " ^ right ^ "." in
      let new_ctx = emit_error context span error_msg in
      (new_ctx, Error ())

(** Checks if a can be casted to b. *)
(* and can_cast_type a b =
   (* TODO: Check classes for inheritance *)
   (* TODO: Support casts from primitive types *)
   match (a, b) with
   (* | (IntType, DoubleType) -> true *)
   | _ -> a == b *)