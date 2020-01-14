open Cast_value
open Compile_block
open Compile_expr
open Compile_type
open Pattern_match
open Ssa
open Ssa_context

let rec compile_stmt (initial_context, out_list, expected_return) stmt = 
  (* If the current block has already returned/broken, issue a warning. *)
  match stmt with
  (* If we get an expression, just compile it. *)
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
      compile_block "block" context compile_stmt expected_return (span, stmts)
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
            else_clause_name ctx_after_clauses compile_stmt expected_return
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
                main_block_name ctx_after_cond compile_stmt expected_return
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
  (* A pattern matching statement is a glorified if statement. *)
  | Ast.MatchStmt (span, cond, clauses) -> begin
      let context = handle_dead_code span initial_context in
      let (ctx_after_cond, actual_type, compiled_cond_opt) = compile_expr context cond in
      (* Convert each match clause into an if clause. 
       * Then, convert them into a series of nested if/else statements.
       * Finally, compile this generated if statement. *)
      let initial_data = (ctx_after_cond, []) in
      let fold_clause (context, out_list) (span, pattern, block) =
        let (new_ctx, names, expected_type) = deduce_type_from_pattern context StringMap.empty pattern in
        (* TODO: Something with the names *)
        let _ = names in
        (* Ensure the condition is castable to the expected type.
         * If so, create a block, which destructs the condition, and then
         * runs whatever was in the original `block`. *)
        match cast_value new_ctx span compiled_cond_opt actual_type expected_type with
        | (ctx_after_cast, Error _)
        | (ctx_after_cast, Ok (None)) -> (ctx_after_cast, out_list)
        | (ctx_after_cast, Ok (Some coerced_value)) ->  begin
            (* Convert patterns into if clauses *)
            (* TODO: Destruct objects into assignments *)
            let (ctx_after_cond, new_cond) = condition_of_pattern ctx_after_cast coerced_value pattern in
            print_endline (string_of_value new_cond);
            (ctx_after_cond, out_list @ [new_cond, block])
          end
      in
      (* TODO: Combine units into an if statement *)
      let (ctx_after_units, if_units) = List.fold_left fold_clause initial_data clauses in
      let _ = if_units in
      (ctx_after_units, out_list, expected_return)
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
        compile_block_extra name ctx_after_cond compile_stmt expected_return (span, body_block) true prelude []
      in
      let final_ctx = { ctx_after_block with block_is_dead = context.block_is_dead } in
      Ok (final_ctx, compiled_cond, block_instrs @ [(span, Jump if_end_name)])
    end