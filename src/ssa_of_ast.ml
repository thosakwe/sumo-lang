open Ssa
open Ssa_context

let builtin_scope =
  Scope.of_seq (List.to_seq [
      ("int", TypeSymbol IntType);
      ("double", TypeSymbol DoubleType);
      ("bool", TypeSymbol BoolType);
      ("void", TypeSymbol VoidType)
    ])

let rec compile_single_ast path c_unit =
  let (context, universe, _) = load_ast_into_universe (Ssa.default_universe) path c_unit in
  (* List.iter (function x -> prerr_endline (Sema.string_of_error x)) context.errors; *)
  (universe, context)

and load_ast_into_universe universe path (directives, decls) =
  (* Before we actually compile anything, forward-declare all functions/types
   * in the module. After this, then we can then compile the actual
   * functions, and then compile everything into LLVM. *)
  (* TODO: Imports *)

  let default_context =
    {
      block_is_dead = false;
      this_module = path;
      errors = [];
      (* scope = Scope.empty; *)
      scope = builtin_scope;
      universe = default_universe;
      namer = Namer.empty;
    }
  in

  let initial_context = { default_context with universe = universe } in

  (* Load all imported modules, and extract imported symbols.
   * We also need to warn on duplicate symbols.
   * Lastly, we'll need to merge in the symbols from the current module.
   * TODO: If the local module overrides a symbol, issue a warning. *)

  (* TODO: Don't reload modules... *)
  (* TODO: Load packages_path from config, if any. *)

  let ctx_after_imports =
    let fold_import context = function
      | Ast.ImportDirective (span, raw_import_path, modifier_opt) -> begin
          let import_path = raw_import_path ^ ".sumo" in
          let _ = span, modifier_opt in
          let relative_path = Filename.concat (Filename.dirname path) import_path in
          if not (Sys.file_exists relative_path) then
            let error_msg =
              "File does not exist: "
              ^ Filename.quote relative_path
              ^ "." 
            in
            let new_ctx = emit_error context span error_msg in
            new_ctx
          else if (import_path = path) || (relative_path = path) then
            let error_msg = "A module cannot import itself." in
            let new_ctx = emit_error context span error_msg in
            new_ctx
          else
            let (ctx_after_parse, c_unit) =
              match Utils.parse_compilation_unit relative_path with
              | Ok result -> (context, result)
              | Error _ ->
                let error_msg = "Failed to parse the module at this path; it cannot be imported." in
                let new_ctx = emit_error context span error_msg in
                (new_ctx, ([], []))
            in
            (* Remove any compiled functions from the imported module. *)
            let (ctx_after_load, universe_after_load, imported_module) =
              let (ctx, univ, raw_imported_module) =
                load_ast_into_universe ctx_after_parse.universe relative_path c_unit
              in
              let stripped_module = { !raw_imported_module with compiled_functions = [] } in
              (ctx, univ, ref stripped_module)
            in

            (* Create symbols for everything in the module. *)
            let fold_symbol name _ context =
              if Scope.mem name context.scope then
                let error_msg =
                  "A symbol named \""
                  ^ name
                  ^ "\" was already defined before this import, so there is a conflict."
                in
                emit_error context span error_msg
              else
                (* TODO: Heed hide, show *)
                (* TODO: Warn on hide/show nonexistent symbol *)
                let symbol = ImportedSymbol (imported_module, name) in
                let new_scope = Scope.replace name symbol context.scope in
                { context with scope = new_scope }
            in

            let ctx_before_fold =
              { ctx_after_parse with 
                errors = ctx_after_load.errors;
                universe = universe_after_load;
              } 
            in
            StringMap.fold fold_symbol (!imported_module).symbols ctx_before_fold
        end
      (* | _ -> out_list *)
    in
    List.fold_left fold_import initial_context directives
  in

  (* After importing symbols, merge in builtin values. *)

  (* TODO: Can symbols be resolved lazily? *)

  (* Compile types first. *)
  let (ctx_after_types, type_symbol_pairs) =
    let (ctx_after_pairs, pairs) =
      let fold_type_decl (context, pair_list) self =
        match self with
        | Ast.TypeDecl (_, vis, name, typ) -> begin
            let (new_ctx, ssa_typ) = compile_type context typ in
            let symbol = TypeSymbol ssa_typ in
            let pair = (name, (vis, symbol)) in
            let new_scope = Scope.replace name symbol context.scope in
            ({ new_ctx with scope = new_scope }, pair_list @ [pair])
          end
        | _ -> (context, pair_list)
      in
      List.fold_left fold_type_decl (ctx_after_imports, []) decls
    in
    (ctx_after_pairs, pairs)
  in

  let (ctx_after_symbols, symbols) =
    let (ctx_after_pairs, pairs) =
      let fold_decl (context, pair_list) self =
        match self with
        | Ast.FuncDecl (_, vis, func) -> begin
            let s = Ast.signature_of_func func in
            let (new_ctx, params, returns) = compile_function_signature context s in
            match func with
            | Ast.ExternalFunc (_, c_name_opt, name, _) ->
              let c_name =
                match c_name_opt with
                | None -> name
                | Some cn -> cn
              in
              let symbol = FuncSymbol (true, c_name, params, returns, self) in
              let pair = (name, (vis, symbol)) in
              (new_ctx, pair_list @ [pair])
            | Ast.ConcreteFunc (_, name, _, _) ->
              let qualified = Sema.qualify_function_name path name in
              let symbol = FuncSymbol (false, qualified, params, returns, self) in
              let pair = (name, (vis, symbol)) in
              (new_ctx, pair_list @ [pair])
          end
        | _ -> (context, pair_list)
      in
      List.fold_left fold_decl (ctx_after_types, []) decls
    in
    let combined_pairs = type_symbol_pairs @ pairs in
    (ctx_after_pairs, (StringMap.of_seq (List.to_seq combined_pairs)))
  in

  (* Now that we have a new universe, compile the bodies of each function. *)
  let this_module = ref { path; symbols; compiled_functions = [] } in
  let new_universe =
    {
      modules = StringMap.add path this_module ctx_after_imports.universe.modules
    }
  in

  (* Create a new scope containing all symbols in this module. *)
  (* TODO: Imports *)
  let unwrap_symbol (_, x) = x in
  let unwrapped_symbols = StringMap.map unwrap_symbol symbols in
  let new_scope = Scope.ChildScope (ctx_after_symbols.scope, unwrapped_symbols) in
  let new_context = {ctx_after_symbols with scope = new_scope; universe = new_universe } in

  (* Compile them, add them to the module, and return the universe. *)
  let compile_decl (context, out_list) = function
    | Ast.FuncDecl (span, _, func) -> begin
        let (new_ctx, new_out_list, returns_opt) = compile_function (context, out_list) func in
        let result_ctx = match returns_opt with
          | None | Some VoidType -> new_ctx
          | Some returns -> 
            if new_ctx.block_is_dead then
              new_ctx
            else
              let error_message =
                "This function is declared to return "
                ^ (string_of_type returns)
                ^ ", but doesn't return a value through all paths."
              in
              emit_error new_ctx span error_message
        in
        ({ result_ctx with block_is_dead = false }, new_out_list)
      end
    (* We have already compiled types. *)
    | Ast.TypeDecl _ -> (context, out_list)
    (* | _ -> (context, out_list) *)
  in
  let (final_ctx, compiled_functions) = List.fold_left compile_decl (new_context, []) decls in
  this_module := {!this_module with compiled_functions };

  ({ final_ctx with block_is_dead = false }, new_universe, this_module)

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
  | Ast.ExternalFunc _ -> (context, out_list, None)
  | Ast.ConcreteFunc (span, name, fsig, block) ->
    compile_concrete_function context out_list (span, name, fsig, block)

and compile_concrete_function context out_list (span, name, fsig, stmts) =
  (* If this module is not in the universe, don't compile it. *)
  if not (StringMap.mem context.this_module context.universe.modules) then
    let error_msg = "No module exists at path \"" ^ context.this_module ^ "\"." in
    let new_ctx = emit_error context span error_msg in
    (new_ctx, out_list, None)
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
    (ctx_after_stmts, out_list @ [func], Some returns)

and compile_stmt (initial_context, out_list, expected_return) stmt = 
  (* If the current block has already returned/broken, issue a warning. *)
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
        | None -> 
          (context, VoidType, None)
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
  | Ast.VarDecl (_, decls) ->
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

          (* TODO: SSA variables - get a unique name for each *)
          let ssa_name = name in

          (* If something goes wrong, inject an Unknown. *)
          let inject_unknown context =
            let sym = VarSymbol (final, ssa_name, UnknownType) in
            let new_scope = Scope.add name sym context.scope in
            { context with scope = new_scope }
          in

          let emit_error_and_inject context span error_msg =
            let new_ctx = emit_error context span error_msg in
            inject_unknown new_ctx
          in

          match target_type with 
          | UnknownType -> begin
              let error_msg = match value_opt with
                | Some (OptionalNone UnknownType) -> 
                  "If the right-hand side of a variable declaration is \"none\", you must declare its type."
                | _ ->
                  "The right-hand side of this variable declaration produced a value of unknown type."
              in
              ((emit_error_and_inject ctx_after_type span error_msg), out_list)
            end
          | _ -> begin
              match cast_value ctx_after_type span value_opt typ target_type with
              | (ctx_after_cast, Error _) ->
                (inject_unknown ctx_after_cast, out_list)
              | (ctx_after_cast, Ok coerced_value_opt) -> begin
                  match coerced_value_opt with
                  | None -> 
                    let error_msg =
                      "The right-hand side of this variable declaration did not produce a valid value."
                    in
                    ((emit_error_and_inject ctx_after_cast span error_msg), out_list)
                  | Some coerced_value ->
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
        end
    in
    let (new_ctx, new_out_list) = List.fold_left compile_var_decl (initial_context, out_list) decls in
    (new_ctx, new_out_list, expected_return)
  (* If we get a block, we need to create a new scope, AND a new block. *)
  | Ast.Block (span, stmts) -> 
    let context = handle_dead_code span initial_context in
    let (ctx_after_block, block_instrs, _) =
      compile_block "block" context expected_return (span, stmts)
    in
    let new_ctx =
      {
        context with
        namer = ctx_after_block.namer; 
        errors = ctx_after_block.errors ;
        block_is_dead = context.block_is_dead || ctx_after_block.block_is_dead;
      }
    in
    (new_ctx, out_list @ block_instrs, expected_return)
  | Ast.If (span, main_clause, else_if_clauses, else_clause_opt) ->
    (* TODO: Detect dead code in if clauses *)
    let context = handle_dead_code span initial_context in

    (* Create an "if_end" block, that we'll jump to at the end of everything. *)
    let (if_end_name, namer_after_if_end) = Namer.next_name "if_end" context.namer in
    let (else_clause_name, namer_after_else) = Namer.next_name "else" namer_after_if_end in
    let initial_instrs = [
      (span, Block(if_end_name, []));
    ]
    in

    (* Preparation to compile the first if block. *)
    (* let (first_block_name, next_namer) = Namer.next_name "if_alt" namer_after_else in *)
    let (first_block_name, next_namer) = Namer.next_name "if_alt" namer_after_else in
    let ctx_after_name = { context with namer = next_namer } in

    (* Compile each clause into a block, jumping to if_end at the end.
     * If the condition is not matched, jump to the next one. *)
    let rec compile_clauses (context, out_list) block_name = function
      | [] -> (context, out_list)
      | clause :: rest -> begin
          match compile_if_clause ctx_after_name clause block_name if_end_name expected_return with
          | Error new_ctx -> compile_clauses (new_ctx, out_list) block_name rest
          (* We've created a new block. 
           * However, compile_block returns multiple instructions. Only take the first.
           * Generate a conditional jump.
           * Otherwise, jump to the next block. The thing is, that block can be either
           * the next clause's block, the "else," or "if_end." *)
          | Ok (ctx_after_clause, cond, block_instrs) -> begin
              let final_jump_target = match else_clause_opt with
                | None -> if_end_name
                | Some _ -> else_clause_name
              in
              let (next_ctx, next_block_name) =
                match rest with
                | [] -> (ctx_after_clause, final_jump_target)
                | _ ->
                  let (block_name, next_namer) = Namer.next_name "if_alt" ctx_after_clause.namer in
                  ({ ctx_after_clause with namer = next_namer; block_is_dead = ctx_after_clause.block_is_dead; }, block_name)
              in

              let new_instrs = [
                List.hd block_instrs;
                (span, JumpIf (cond, block_name, next_block_name));
              ]
              in
              let new_out_list = out_list @ new_instrs in
              compile_clauses (next_ctx, new_out_list) next_block_name rest
            end
        end
    in

    let all_clauses = [main_clause] @ else_if_clauses in
    let (ctx_after_clauses, instrs_after_clauses) =
      compile_clauses (ctx_after_name, []) first_block_name all_clauses
    in

    (* Compile else block, if any. *)
    let (ctx_after_else, else_instrs) = match else_clause_opt with
      | None -> (ctx_after_clauses, [])
      | Some else_clause ->
        let (_, else_clause_as_block) = Ast.block_of_stmt else_clause in
        let (new_ctx, instrs, _) =
          let final_jump = [(span, Jump if_end_name)] in
          compile_block_extra
            else_clause_name ctx_after_clauses expected_return
            (span, else_clause_as_block) true [] final_jump
        in
        ({new_ctx with block_is_dead = context.block_is_dead }, [List.hd instrs])
    in

    (* Any further instructions in the current context must exist within the if_end block.
     * Emit a PositionAtEnd. *)
    let new_out_list =
      out_list
      @ initial_instrs
      @ else_instrs
      @ instrs_after_clauses
      @ [ (span, PositionAtEnd if_end_name) ]
    in
    (ctx_after_else, new_out_list, expected_return)
  | Ast.While (span, cond, body)
  | Ast.DoWhile (span, body, cond) -> begin
      (* Create an "if_end" block, that we'll jump to at the end of everything. *)
      let context = handle_dead_code span initial_context in
      let (while_end_name, namer_after_while_end) = Namer.next_name "while_end" context.namer in
      let (main_block_name, new_namer) =
        Namer.next_name "while_main_block" namer_after_while_end
      in

      let initial_instrs = [(span, Block(while_end_name, []))] in

      (* Next, compile the body and cond. *)
      (* TODO: Inject loop names into scope, to support break/continue *)

      let (ctx_after_cond, compiled_cond_opt) =
        let ctx_after_namer = { context with namer = new_namer } in
        let (ctx_after_cond, typ, value_opt) = compile_expr ctx_after_namer cond in
        match cast_value ctx_after_cond span value_opt typ BoolType with
        | (new_ctx, Error _)
        | (new_ctx, Ok (None)) -> (new_ctx, None)
        | (new_ctx, Ok (Some value)) -> (new_ctx, Some value)
      in

      match compiled_cond_opt with
      | None -> (ctx_after_cond, out_list, expected_return)
      | Some compiled_cond -> begin

          (* Compile the main work block, and insert the conditional code where needed. *)
          let jump_instr = JumpIf (compiled_cond, main_block_name, while_end_name) in
          let postlude = match stmt with
            | _ -> [(span, jump_instr)]
          in

          let ({errors = block_errors ; _}, block_instrs) =
            let (new_ctx, instrs, _) =
              let (_, body_as_block) = Ast.block_of_stmt body in
              compile_block_extra
                main_block_name ctx_after_cond expected_return
                (span, body_as_block) true [] postlude
            in
            ({new_ctx with block_is_dead = ctx_after_cond.block_is_dead }, [List.hd instrs])
          in

          (* If we have a while, then create an initial "if" to check if we should loop. *)
          let initial_jump = match stmt with
            | Ast.While _ -> JumpIf (compiled_cond, main_block_name, while_end_name)
            | _ -> Jump main_block_name
          in

          (* Any subsequent instructions should be in the end block. *)
          let new_instrs =
            initial_instrs
            @ block_instrs
            @ [ (span, initial_jump)]
            @ [ (span, PositionAtEnd while_end_name)]
          in
          let new_ctx = { ctx_after_cond with errors = block_errors } in
          (new_ctx, out_list @ new_instrs, expected_return)
        end
    end
  (* The for loop is just sugar for a while loop; treat it as such. *)
  | Ast.ForLoop (span, init_opt, cond, actions, body) -> begin
      let context = handle_dead_code span initial_context in
      let (_, block_body) = Ast.block_of_stmt body in
      let loop_body = block_body @ actions in
      let while_loop = Ast.While (span, cond, Ast.Block (span, loop_body)) in
      let init = match init_opt with None -> [] | Some v -> [v] in

      (* Compile each of the initializers, in turn. *)
      let (ctx_after_init, out_list_after_init) =
        let fold_initializer (context, out_list) stmt =
          let (new_ctx, new_out_list, _) = compile_stmt (context, out_list, expected_return) stmt in
          (new_ctx, new_out_list)
        in
        List.fold_left fold_initializer (context, out_list) init
      in

      compile_stmt (ctx_after_init, out_list_after_init, expected_return) while_loop
    end

and compile_if_clause context clause name if_end_name expected_return =
  (* Create a new block for this condition. 
   * Afterwards, jump to the end_label.
   * If the condition is not matched, jump to the otherwise_label. *)

  (* Compile the if condition. The logic for actually executing the body is identical
   * for all cases, so combine them. *)
  let (ctx_after_cond, compiled_cond_opt, prelude) = match clause with
    | Ast.BasicIfClause (span, cond, _) -> begin
        let (ctx_after_expr, typ, value_opt) = compile_expr context cond in
        match cast_value ctx_after_expr span value_opt typ BoolType with
        | (new_ctx, Error _) 
        | (new_ctx, Ok (None)) -> (new_ctx, (None), [])
        | (ctx_after_cast, Ok (Some coerced_cond)) -> (ctx_after_cast, (Some coerced_cond), [])
      end
    (* The input values must all be optionals. The clause will only execute if
     * all of them have valid values. *)
    | Ast.NullCheckIfClause (span, decls, _) -> begin
        if decls = [] then
          let error_msg = "A null-checking if statement must contain at least one variable declaration." in
          ((emit_error context span error_msg), (None), [])
        else
          let compile_one_decl (context, conditions, prelude) (span, final, type_opt, name, rhs_ast) = 
            (* These variable declarations must not declare a type. *)
            match type_opt with
            | Some _ -> 
              let error_msg = "This variable declaration must not declare its own type." in
              ((emit_error context span error_msg), conditions, prelude)
            | _ -> begin
                let (ctx_after_rhs, rhs_type, rhs_opt) = compile_expr context rhs_ast in
                match (rhs_type, rhs_opt) with
                (* If the value exists, then dereference it, and inject it into the scope. *)
                | ((OptionalType inner_type), (Some rhs)) ->
                  let (deref_name, namer_after_deref) = Namer.next_name "deref" ctx_after_rhs.namer in
                  let deref_symbol = VarSymbol (final, deref_name, inner_type) in
                  let new_scope_map = StringMap.add name deref_symbol StringMap.empty in
                  let new_scope = Scope.ChildScope (ctx_after_rhs.scope, new_scope_map) in
                  let new_ctx = { ctx_after_rhs with scope = new_scope; namer = namer_after_deref; } in
                  let new_prelude = [
                    (span, Value (VarCreate (deref_name, inner_type)));
                    let deref = OptionalGet (inner_type, rhs) in
                    (span, Value (VarSet (deref_name, inner_type, deref)));
                  ]
                  in
                  (new_ctx, conditions @ [OptionalNullCheck rhs], prelude @ new_prelude)
                | (UnknownType, _) ->
                  let error_msg = "Cannot null-dereference a value of unknown type." in
                  ((emit_error ctx_after_rhs span error_msg), conditions, prelude)
                | _ ->
                  let error_msg = "Every variable in a null-checking if statement must have an optional type." in
                  ((emit_error ctx_after_rhs span error_msg), conditions, prelude)
              end

          in

          (* Compile all decls, and return the new scope and condition.
           * All conditions must be reduced into a single AND. *)
          let (ctx_after_decls, conditions, prelude) = List.fold_left compile_one_decl (context, [], []) decls in
          match conditions with
          | [] -> 
            let error_msg = "Every variable declaration in this if statement produced an error." in
            ((emit_error ctx_after_decls span error_msg), None, [])
          | _ ->
            let make_and a b =  BoolCompare (a, Ast.BooleanAnd, b) in
            let combined_condition = List.fold_left make_and (BoolLiteral true) conditions in
            (ctx_after_decls, Some combined_condition, prelude)
      end
  in
  match compiled_cond_opt with
  | None -> Error ctx_after_cond
  (* If we successfully compiled it, compile the body. *)
  | Some compiled_cond -> begin
      let (span, body) = match clause with
        | Ast.BasicIfClause (span, _, body) -> (span, body)
        | Ast.NullCheckIfClause (span, _, body) -> (span, body)
      in

      let (ctx_after_block, block_instrs, _) =
        let (_, body_block) = Ast.block_of_stmt body in
        compile_block_extra name ctx_after_cond expected_return (span, body_block) true prelude []
      in
      let final_ctx = { ctx_after_block with block_is_dead = context.block_is_dead } in
      Ok (final_ctx, compiled_cond, block_instrs @ [(span, Jump if_end_name)])
    end

and compile_block name initial_context expected_return (span, stmts) =
  compile_block_extra name initial_context expected_return (span, stmts) false [] []

and compile_block_extra
    name initial_context expected_return (span, stmts)
    use_verbatim_name prelude_instrs postlude_instrs =
  let context = handle_dead_code span initial_context in
  let new_scope = Scope.ChildScope (context.scope, StringMap.empty) in
  let child_context = { context with scope = new_scope } in
  let compile_one_stmt (context, out_list) stmt =
    let (new_ctx, new_out_list, _) = compile_stmt (context, out_list, expected_return) stmt in
    (new_ctx, new_out_list)
  in
  let (ctx_after_stmts, new_out_list) = List.fold_left compile_one_stmt (child_context, []) stmts in
  let (name, new_namer) = 
    if use_verbatim_name then
      (name, ctx_after_stmts.namer)
    else
      Namer.next_name name ctx_after_stmts.namer 
  in
  let new_ctx = { 
    context with 
    namer = new_namer; 
    errors = ctx_after_stmts.errors ;
    block_is_dead = context.block_is_dead || ctx_after_stmts.block_is_dead;
  } in
  let instrs = [
    (span, Block (name, prelude_instrs @ new_out_list @ postlude_instrs));
    (span, Jump name);
  ]
  in
  (new_ctx, instrs, expected_return)

and compile_expr context = function
  (* TODO: Other exprs *)
  | Ast.IntLiteral (_, v) -> (context, IntType, Some (IntLiteral v))
  | Ast.DoubleLiteral (_, v) -> (context, DoubleType, Some (DoubleLiteral v))
  | Ast.BoolLiteral (_, v) -> (context, BoolType, Some (BoolLiteral v))
  | Ast.Paren (_, inner) -> compile_expr context inner
  | Ast.NoneLiteral -> (context, UnknownType, Some (OptionalNone UnknownType))
  | Ast.StructLiteral (span, fields) -> begin
      (* Warn on duplicates *)
      let fold_field (context, value_pairs, type_pairs) (span, name, value_ast) =
        let name_is_equal (n, _) = (n = name) in
        match List.find_opt name_is_equal value_pairs with
        | Some _ -> 
          let error_msg = "Duplicate structure field \"" ^ name ^ "\"." in
          let new_ctx = emit_error context span error_msg in
          (new_ctx, value_pairs, type_pairs)
        | None -> begin
            match  compile_expr context value_ast with
            | (new_ctx, _, None) -> (new_ctx, value_pairs, type_pairs)
            | (new_ctx, typ, (Some value)) -> 
              let value_pair = (name, value) in
              let type_pair = (name, typ) in
              (new_ctx, value_pairs @ [value_pair], type_pairs @ [type_pair])
          end
      in
      let (ctx_after_pairs, value_pairs, type_pairs) =
        List.fold_left fold_field (context, [], []) fields
      in
      match value_pairs with
      | [] ->
        let error_msg = "A structure literal must contain at least one field." in
        let new_ctx = emit_error ctx_after_pairs span error_msg in
        (new_ctx, UnknownType, None)
      | _ ->
        let value_map = StringMap.of_seq (List.to_seq value_pairs) in
        let type_map = StringMap.of_seq (List.to_seq type_pairs) in
        let struct_type = StructType type_map in
        let value = StructLiteral (struct_type, value_map) in
        (ctx_after_pairs, struct_type, Some value)
    end
  (* If we find a reference, just figure out if it's a value. *)
  | Ast.Ref (span, name) -> begin
      if not (Scope.mem name context.scope) then
        let error_msg = (Scope.does_not_exist name) ^ " It cannot be referenced." in
        ((emit_error context span error_msg), UnknownType, None)
      else
        let not_a_value sym = 
          let error_msg = "The name \"" ^ name ^ "\" resolves to " ^ (string_of_symbol sym) ^ ", which is not a value." in
          ((emit_error context span error_msg), UnknownType, None)
        in
        let rec resolve_sym = function
          | VarSymbol (_, name, typ) -> (context, typ, Some (VarGet (name, typ)))
          | ParamSymbol (name, index, typ) -> (context, typ, Some (ParamGet (index, name, typ)))
          | ImportedSymbol (module_ref, name) as sym -> begin
              let (vis, symbol) = StringMap.find name (!module_ref).symbols in
              match vis with
              | Visibility.Public -> resolve_sym symbol
              | _ -> 
                let error_msg =
                  "You do not have access to the symbol " 
                  ^ (string_of_symbol sym)
                  ^ " in this context."
                in
                ((emit_error context span error_msg), UnknownType, None)
            end
          | _ as sym -> not_a_value sym
        in

        resolve_sym (Scope.find name context.scope)
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
            let rec resolve_sym = function
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
                      ({ new_ctx with block_is_dead = context.block_is_dead }, returns, Some value)
                end
              | ImportedSymbol (module_ref, name) as sym -> begin
                  let (vis, symbol) = StringMap.find name (!module_ref).symbols in
                  match vis with
                  | Visibility.Public -> resolve_sym symbol
                  | _ -> 
                    let error_msg =
                      "You do not have access to call the symbol " 
                      ^ (string_of_symbol sym)
                      ^ " in this context."
                    in
                    ((emit_error context span error_msg), UnknownType, None)
                end
              | _ as sym ->  
                let error_msg =
                  "The name \"" ^ name ^ "\" resolves to "
                  ^ (string_of_symbol sym) ^ ", which is not a callable value."
                in
                ((emit_error context span error_msg), UnknownType, None)
            in
            resolve_sym (Scope.find name context.scope)
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
          match (lhs_type, op) with
          (* For native numbers, both operands must be the same type.
           * If this condition is met, emit a corresponding instruction. *)
          | (IntType, _) | (DoubleType, _) -> begin
              (* If we have a double, then make sure we're not doing a bitwise operation. *)
              match (lhs_type, op) with
              | (DoubleType, Ast.Shift _)
              | (DoubleType, Ast.Bitwise _) ->
                let error_msg = "Double-precision numbers do not support bitwise operations." in
                let new_ctx = emit_error ctx_after_right span error_msg in
                (new_ctx, UnknownType, None)
              (* If we do have a shift, the right must be an int. *)
              (* TODO: No boolean ops *)
              | (IntType, Ast.Shift _) -> begin
                  match cast_value ctx_after_right span rhs_opt rhs_type IntType with
                  | (new_ctx, Error _) -> (new_ctx, UnknownType, None)
                  | (ctx_after_cast, Ok coerced_rhs_opt) -> begin
                      match coerced_rhs_opt with
                      | None -> 
                        let error_msg =
                          "The right hand side of this bit-shift expression does not produce a valid value." 
                        in
                        let new_ctx = emit_error ctx_after_cast span error_msg in
                        (new_ctx, UnknownType, None)
                      | Some rhs -> begin
                          let value = IntArithmetic (lhs, op, rhs) in
                          (ctx_after_cast, lhs_type, Some value)
                        end
                    end
                end
              | _ -> begin
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
                          let result_type =
                            match op with
                            | Lt | Lte | Gt | Gte | Eq | Neq -> BoolType
                            | _ -> lhs_type
                          in
                          (ctx_after_cast, result_type, Some value)
                        end
                    end
                end
            end
          (* Booleans only have ==, !=, &&, ||. Cast both operands to booleans. *)
          | (BoolType, (Eq | Neq | BooleanAnd | BooleanOr) ) -> begin
              match cast_value ctx_after_right span rhs_opt rhs_type lhs_type with
              | (new_ctx, Error _)
              | (new_ctx, Ok (None)) -> (new_ctx, UnknownType, None)
              | (ctx_after_cast, Ok (Some rhs)) ->
                let value = BoolCompare (lhs, op, rhs) in
                (ctx_after_cast, BoolType, Some value)
            end
          | _ ->
            let error_msg =
              "The type "
              ^ string_of_type lhs_type
              ^ " has no binary '"
              ^ (Ast.string_of_binary_op op)
              ^ "' operator."
            in
            let new_ctx = emit_error ctx_after_right span error_msg in
            (new_ctx, UnknownType, None)
        end
    end
  (* TODO: Implement unaries *)
  | Ast.Unary (span, expr, op) -> begin
      let (ctx_after_expr, expr_type, value_opt) = compile_expr context expr in
      let failure =
        let error_msg =
          "The inner expression of this unary operation does not produce a valid value." 
        in
        let new_ctx = emit_error ctx_after_expr span error_msg in
        (new_ctx, UnknownType, None)
      in
      match (expr_type, op) with
      | (BoolType, Ast.LogicalNot) -> begin
          match value_opt with
          | None -> failure
          | Some value -> (ctx_after_expr, BoolType, (Some (BooleanNegate value)))
        end
      | (IntType, Ast.BitwiseNot) -> begin
          match value_opt with
          | None -> failure
          | Some value -> (ctx_after_expr, IntType, (Some (BitwiseNegate value)))
        end
      | (IntType, (Ast.UnaryPlus | Ast.UnaryMinus)) -> begin
          match value_opt with
          | None -> failure
          | Some value -> begin 
              let result = match op with
                | Ast.UnaryPlus -> Positive (IntType, value)
                | _ -> Negative (IntType, value)
              in
              (ctx_after_expr, IntType, Some result) 
            end
        end
      | ((IntType | DoubleType), 
         (Ast.PrefixDecrement | Ast.PrefixIncrement 
         | Ast.PostfixDecrement | Ast.PostfixIncrement)) ->  begin

          (* Either add or subtract one. *)
          let apply_incr value = 
            let incr_op = match op with
              | Ast.PrefixIncrement | Ast.PostfixIncrement -> Ast.Plus
              | _ -> Ast.Minus
            in

            match expr_type with
            | IntType -> IntArithmetic (value, incr_op, (IntLiteral 1))
            | _ -> IntArithmetic (value, incr_op, (DoubleLiteral 1.0))
          in

          (* Assignments can only be done to l-values (assign_target.) *)
          match Ast.innermost_expr expr with
          | Ast.Ref (_, name) -> begin
              let var_get = VarGet (name, expr_type) in
              let (final_ctx, result) = 
                match op with
                | Ast.PrefixIncrement | Ast.PrefixDecrement ->
                  let result = Multi [
                      VarSet (name, expr_type, (apply_incr var_get));
                      var_get;
                    ]
                  in
                  (ctx_after_expr, result)
                | _ -> 
                  let (tmp_name, new_namer) = Namer.next_name "tmp_postfix" context.namer in
                  let tmp_get = VarGet (tmp_name, expr_type) in
                  let result = Multi [
                      VarCreate (tmp_name, expr_type);
                      VarSet (tmp_name, expr_type, var_get);
                      VarSet (name, expr_type, (apply_incr tmp_get));
                      tmp_get;
                    ]
                  in
                  ({ ctx_after_expr with namer = new_namer }, result)
              in
              (final_ctx, expr_type, Some result)
            end
          | _ -> 
            let error_msg =
              "This operator can only be performed on l-values." 
            in
            let new_ctx = emit_error ctx_after_expr span error_msg in
            (new_ctx, UnknownType, None)
        end
      | _ ->
        let error_msg =
          "The type "
          ^ string_of_type expr_type
          ^ " has no unary '"
          ^ (Ast.string_of_unary_op op)
          ^ "' operator."
        in
        let new_ctx = emit_error ctx_after_expr span error_msg in
        (new_ctx, UnknownType, None)

    end
  | Ast.GetField (span, expr, name) -> begin
      match compile_expr context expr with
      | (ctx_after_expr, _, None) -> (ctx_after_expr, UnknownType, None)
      | (ctx_after_expr, expr_type, Some lhs) -> begin
          match expr_type with
          (* Find the index of the given field. *)
          | StructType field_types ->
            let find_field n typ (context, out_typ, out_index, current_index) =
              if n <> name then
                (context, out_typ, out_index, current_index + 1)
              else
                (context, typ, current_index, current_index + 1)
            in
            let initial_data = (ctx_after_expr, UnknownType, -1, 0) in
            let (ctx_after_find, field_type, field_index, _) =
              StringMap.fold find_field field_types initial_data
            in
            if field_index = -1 then
              let error_msg =
                string_of_type (StructType field_types)
                ^ " has no getter named \"" ^ name ^ "\"."
              in
              let new_ctx = emit_error ctx_after_find span error_msg in
              (new_ctx, UnknownType, None)
            else
              let value = GetElement (field_type, lhs, field_index) in
              (ctx_after_find, field_type, Some value)
          | _ -> 
            let error_msg =
              "Values of type " ^ (string_of_type expr_type) ^ " do not have any fields."
            in
            let new_ctx = emit_error context span error_msg in
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
        let error_msg = (Scope.does_not_exist name) ^ " It cannot be assigned." in
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
  | (_, Ast.FieldTarget(span, lhs_ast, name), Ast.Equals, rhs_ast) -> begin
      let (ctx_after_lhs, lhs_type, lhs_opt) = compile_expr context lhs_ast in
      match lhs_type with
      | StructType field_types -> begin
          let find_field n typ (context, out_typ, out_index, current_index) =
            if n <> name then
              (context, out_typ, out_index, current_index + 1)
            else
              (context, typ, current_index, current_index + 1)
          in
          let initial_data = (ctx_after_lhs, UnknownType, -1, 0) in
          let (ctx_after_find, field_type, field_index, _) =
            StringMap.fold find_field field_types initial_data
          in
          if field_index = -1 then
            let error_msg =
              string_of_type (StructType field_types)
              ^ " has no setter named \"" ^ name ^ "\"."
            in
            let new_ctx = emit_error ctx_after_find span error_msg in
            (new_ctx, UnknownType, None)
          else
            match lhs_opt with
            | None ->
              let error_msg = "Could not resolve the left-hand side target of this field assignment." in
              let new_ctx = emit_error ctx_after_find span error_msg in
              (new_ctx, UnknownType, None)
            | Some lhs -> begin
                let (ctx_after_rhs, rhs_type, rhs_opt) = compile_expr ctx_after_find rhs_ast in
                (* match rhs_opt with *)
                match cast_value ctx_after_rhs span rhs_opt rhs_type field_type with
                | (ctx_after_cast, Error _) ->
                  (ctx_after_cast, UnknownType, None)
                | (ctx_after_cast, Ok coerced_rhs_opt) -> begin
                    match coerced_rhs_opt with
                    | None ->
                      let error_msg = "Could not resolve the right-hand side of this field assignment." in
                      let new_ctx = emit_error ctx_after_cast span error_msg in
                      (new_ctx, UnknownType, None)
                    | Some coerced_rhs ->
                      let value = SetElement (field_type, lhs, field_index, coerced_rhs) in
                      (ctx_after_cast, field_type, Some value)
                  end
              end
        end
      | _ ->
        let error_msg =
          "Values of type " ^ (string_of_type lhs_type) ^ " do not have any fields."
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
        let error_msg = 
          "No type named \"" ^ name ^ "\" exists in this context."
        in
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
  | Ast.OptionalType (_, inner) -> begin
      match compile_type context inner with
      | (new_ctx, UnknownType) -> (new_ctx, UnknownType)
      | (new_ctx, OptionalType inner) -> (new_ctx, OptionalType inner)
      | (new_ctx, t) -> (new_ctx, OptionalType t)
    end
  | Ast.StructType (span, fields) -> begin
      let fold_field (context, ssa_fields) (_, name, typ) =
        let (new_ctx, ssa_typ) = compile_type context typ in
        (new_ctx, ssa_fields @ [(name, ssa_typ)])
      in
      match fields with
      | [] ->
        let error_msg = "A structure type must have at least one field." in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, UnknownType)
      | _ ->
        let (new_ctx, ssa_fields) = List.fold_left fold_field (context, []) fields in
        (new_ctx, StructType (StringMap.of_seq (List.to_seq ssa_fields)))
    end

and handle_dead_code span initial_context =
  if initial_context.block_is_dead then
    emit_warning initial_context span "Dead code."
  else initial_context

(** Shortcut for emitting an error, and returning a new context object. *)
and emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}

and emit_warning context span error_msg =
  let error = (span, Sema.Warning, error_msg) in
  {context with errors = context.errors @ [error]}

and cast_value context span value_opt from_type to_type =
  let failure =
    let left = string_of_type from_type in
    let right = string_of_type to_type in
    let error_msg = "Cannot cast a value of type " ^ left ^ " to " ^ right ^ "." in
    let new_ctx = emit_error context span error_msg in
    (new_ctx, Error ())
  in
  if from_type = to_type then
    (context, Ok value_opt)
  else
    let double_to_int_warning = "Casting a double to int loses precision." in

    match (from_type, to_type, value_opt) with
    | (IntType, DoubleType, Some value) ->
      let new_value = Some (CastIntToDouble value) in
      (context, Ok new_value)
    | (DoubleType, IntType, Some value) ->
      let new_value = Some (CastDoubleToInt value) in
      let new_ctx = emit_warning context span double_to_int_warning in
      (new_ctx, Ok new_value)
    | (UnknownType, (OptionalType inner), (Some (OptionalNone UnknownType))) ->
      (context, Ok (Some (OptionalNone inner)))
    | (_, (OptionalType inner), (Some value)) -> begin
        if from_type = inner then
          (context, Ok (Some (OptionalSome (inner, value))))
        else 
          match (from_type, inner) with
          | (IntType, DoubleType) ->
            let new_value = Some ((OptionalSome (DoubleType, (CastIntToDouble value)))) in
            (context, Ok new_value)
          | (DoubleType, IntType) ->
            let new_value = Some ((OptionalSome (IntType, (CastDoubleToInt value)))) in
            let new_ctx = emit_warning context span double_to_int_warning in
            (new_ctx, Ok new_value)
          | _ -> failure
      end
    (* To cast one struct to another, make sure they have the same names.
     * If so, then try to cast each "from" value to the corresponding "to" type.
     * If everything works, return a StructLiteral. *)
    | (StructType from_types, StructType to_types, Some value) -> begin
        let names map =
          let fold_name name _ out_list = out_list @ [name] in
          StringMap.fold fold_name map [] 
        in
        let from_names, to_names = (names from_types), (names to_types) in
        if from_names <> to_names then
          failure
        else
          let fold_field name from_type (context, pair_list, index, success) =
            let to_type = StringMap.find name to_types in
            let get_field = GetElement (from_type, value, index) in
            match cast_value context span (Some get_field) from_type to_type with
            | (ctx_after_cast, Error _)
            | (ctx_after_cast, Ok None) ->
              (ctx_after_cast, pair_list, index + 1, false)
            | (ctx_after_cast, Ok (Some coerced_value)) ->
              let new_pair = (name, coerced_value) in
              (ctx_after_cast, pair_list @ [new_pair], index + 1, success)
          in
          let (ctx_after_fields, pair_list, _, success) =
            StringMap.fold fold_field from_types (context, [], 0, true)
          in
          if not success then
            (ctx_after_fields, Error ())
          else
            let value_map = StringMap.of_seq (List.to_seq pair_list) in
            let coerced_value = StructLiteral (to_type, value_map) in
            (ctx_after_fields, Ok (Some coerced_value))
      end
    (* TODO: Optional struct casts? *)
    | _ ->  failure

(** Checks if a can be casted to b. *)
(* and can_cast_type a b =
   (* TODO: Check classes for inheritance *)
   (* TODO: Support casts from primitive types *)
   match (a, b) with
   (* | (IntType, DoubleType) -> true *)
   | _ -> a == b *)