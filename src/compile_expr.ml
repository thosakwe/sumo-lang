open Cast_value
open Compile_assign
open Ssa
open Ssa_context

let rec compile_expr context = function
  | Ast.IntLiteral (_, v) -> (context, IntType, Some (IntLiteral v))
  | Ast.DoubleLiteral (_, v) -> (context, DoubleType, Some (DoubleLiteral v))
  | Ast.BoolLiteral (_, v) -> (context, BoolType, Some (BoolLiteral v))
  | Ast.Paren (_, inner) -> compile_expr context inner
  | Ast.NoneLiteral _ -> (context, UnknownType, Some (OptionalNone UnknownType))
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
  | Ast.Assign (span, target, op, value) -> compile_assign context compile_expr (span, target, op, value)
  (* If we find a call, there's quite a bit we have to do properly resolve it. *)
  | Ast.Call (span, target, raw_args) -> begin
      (* 1. Make sure target is a function.
       * 2. Ensure correct # of args
       * 3. Ensure correct arg types
       * 4. TODO: Invoke closures.
      *)

      let rec invoke_symbol prelude_args sym =
        let actual_args = (prelude_args @ raw_args) in
        match sym with

        (* If we find a function, then verify the number of args. *)
        | FuncSymbol (_, func_name, params, returns, _) -> begin
            if (List.length actual_args) != (List.length params) then 
              let error_msg =
                "The function \"" ^ func_name ^ "\" expects "
                ^ (string_of_int (List.length params))
                ^ " argument(s), but "
                ^ (string_of_int (List.length actual_args))
                ^ " argument(s) were provided instead."
              in
              ((emit_error context span error_msg), UnknownType, None)
            else
              (* If we have the correct number of args, then perform type-checking. *)
              let params_to_args = List.combine params actual_args in
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
            | Visibility.Public -> invoke_symbol [] symbol
            | _ -> 
              let error_msg =
                "You do not have access to call the symbol " 
                ^ (string_of_symbol sym)
                ^ " in this context."
              in
              ((emit_error context span error_msg), UnknownType, None)
          end
        (* If we get a variant constructor, type-check it, and emit a StructLiteral. *)
        | ConstructorSymbol (variant_type, variant_index, (name, arg_types)) -> begin
            let (ctx_after_args, compiled_args, success) =
              let fold_param (out_list, index) arg_type =
                let name = "p" ^ (string_of_int index) in
                let pair = (name, arg_type) in
                (out_list @ [pair], index + 1)
              in
              let (params, _) = List.fold_left fold_param ([], 0) arg_types in
              type_check_args context span name params actual_args 
            in
            if not success then
              (ctx_after_args, UnknownType, None)
            else begin
              (* If all typechecking went well, then fold the compiled args into a struct literal. *)
              let fold_arg (map, index) arg =
                let name = "p" ^ (string_of_int index) in
                let new_map = StringMap.add name arg map in
                (new_map, index + 1)
              in
              let initial_map = StringMap.add "a" (IntLiteral variant_index) StringMap.empty in
              let (value_map, _) = List.fold_left fold_arg (initial_map, 0) compiled_args in
              let result = StructLiteral (variant_type, value_map) in
              let coerced_result = PointerCast (result, variant_type) in
              (ctx_after_args, variant_type, Some coerced_result)
            end
          end
        | _ as sym ->  
          let error_msg =
            "The symbol \"" ^ (string_of_symbol sym) ^ "\" is not a callable value."
          in
          ((emit_error context span error_msg), UnknownType, None)
      in

      (* It's important to note, though, that at this time, we don't have closures,
       * so the only expressions that can actually be called are identifiers. *)
      match Ast.innermost_expr target with
      (* If it IS an identifier, look it up in the scope, to see if it's a function. *)
      | Ast.Ref (span, name) -> begin
          (* If it doesn't exist, report an error, of course. *)
          if not (Scope.mem name context.scope) then
            let error_msg = (Scope.does_not_exist name) ^ " It cannot be called as a function." in
            let dump name _ = print_endline name in
            Scope.iter dump context.scope;
            let new_ctx = emit_error context span error_msg in
            (new_ctx, UnknownType, None)
          else
            invoke_symbol [] (Scope.find name context.scope)
        end
      | _ ->
        let error_msg = "Only top-level symbols and class members may be called as functions." in
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
            (* TODO: Search for operators within classes, and emit a Ast.Call. *)
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
          | StructType field_types -> begin
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
            end
          | _ -> 
            let error_msg =
              "Values of type " ^ (string_of_type expr_type) ^ " do not have any fields."
            in
            let new_ctx = emit_error context span error_msg in
            (new_ctx, UnknownType, None)
        end
    end

and type_check_args context span func_name params args =
  if (List.length args) != (List.length params) then 
    let error_msg =
      "The function \"" ^ func_name ^ "\" expects "
      ^ (string_of_int (List.length params))
      ^ " argument(s), but "
      ^ (string_of_int (List.length args))
      ^ " argument(s) were provided instead."
    in
    ((emit_error context span error_msg), [], false)
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
    List.fold_left check_one_pair (context, [], true) params_to_args