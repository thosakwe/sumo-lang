open Cast_value
open Ssa
open Ssa_context

(** Compiles an assignment expression. *)
let rec compile_assign context compile_expr = function
  (* If we get a +=, *=, etc. just desugar it. *)
  | (span, target, (Ast.BinaryAssign op), rhs) ->
    let target_expr = Ast.expr_of_assign_target target in
    let new_rhs = Ast.Binary (span, target_expr, op, rhs) in
    let new_assign = (span, target, Ast.Equals, new_rhs) in
    compile_assign context compile_expr new_assign
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
      (* If we find a class, it can be either a field, or a setter.
           * TODO: Call getters.
           * TODO: Check fields from parent types. *)
      | Class (_, _, _, _, members, _) -> begin
          let find_field n (_, member) (context, out_typ, out_index, current_index) =
            if n <> name then
              (context, out_typ, out_index, current_index + 1)
            else
              (* TODO: Check if we have access to the class. *)
              match member with
              | ClassField (_, _, _, field_type, _) ->  
                (context, field_type, current_index, current_index + 1)
              | _ -> (context, out_typ, out_index, current_index + 1)
          in
          let initial_data = (ctx_after_lhs, UnknownType, -1, 0) in
          let (ctx_after_find, field_type, field_index, _) =
            StringMap.fold find_field members initial_data
          in
          if field_index = -1 then
            let error_msg =
              string_of_type lhs_type
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