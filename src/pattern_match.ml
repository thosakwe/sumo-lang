open Ssa
open Ssa_context

let report_existing_name context span name =
  let error_msg =
    "This expression already contains a binding for the name \"" ^ name ^ "\"." 
  in
  Ssa_context.emit_error context span error_msg

(** Deduces which type a given pattern expects. *)
let rec deduce_type_from_pattern context existing_names = function
  | Ast.IgnoredPattern _ -> (context, existing_names, AnyType)
  | Ast.NamedPattern (span, name) -> begin
      if StringMap.mem name existing_names then
        let new_ctx = report_existing_name context span name in
        (new_ctx, existing_names, UnknownType)
      else
        (context, (StringMap.add name AnyType existing_names), AnyType)
    end
  | Ast.AliasedPattern (span, inner, name) -> 
    if StringMap.mem name existing_names then
      let new_ctx = report_existing_name context span name in
      (new_ctx, existing_names, UnknownType)
    else
      deduce_type_from_pattern context existing_names inner
  | Ast.StructPattern (_, fields) -> 
    let fold_field (context, names, map) (span, name, pattern) =
      if StringMap.mem name names then
        let new_ctx = report_existing_name context span name in
        (new_ctx, names, map)
      else if StringMap.mem name map then
        let error_msg =
          "This struct pattern already contains a field named \"" ^ name ^ "\"." 
        in
        let new_ctx = Ssa_context.emit_error context span error_msg in
        (new_ctx, names, map)
      else
        let (new_ctx, set_after_field, typ) = deduce_type_from_pattern context names pattern in
        let new_names = StringMap.add name typ set_after_field in
        let new_map = StringMap.add name typ map in
        (new_ctx, new_names, new_map)
    in
    let initial_data = (context, existing_names, StringMap.empty) in
    let (new_ctx, new_names, type_map) = List.fold_left fold_field initial_data fields in
    let result = StructType type_map in
    (new_ctx, new_names, result)
  | Ast.ConstructorPattern (span, variant_name, patterns) -> begin
      if not (Scope.mem variant_name context.scope) then
        let error_msg = Scope.does_not_exist variant_name in
        let new_ctx = Ssa_context.emit_error context span error_msg in
        (new_ctx, existing_names, UnknownType)
      else begin
        match Scope.find variant_name context.scope with
        | ConstructorSymbol (variant_type, _, (name, arg_types)) ->
          if (List.length arg_types) != (List.length patterns) then
            let error_msg = 
              "The constructor \"" ^ name ^ "\" expects"
              ^ (string_of_int (List.length arg_types)) ^ "argument(s), but this pattern"
              ^ "only matches " ^ (string_of_int (List.length patterns)) ^ "argument(s)."
            in
            let new_ctx = Ssa_context.emit_error context span error_msg in
            (new_ctx, existing_names, UnknownType)
          else
            let fold_arg (context, names) pattern =
              let (new_ctx, new_names, _) = deduce_type_from_pattern context names pattern in
              (new_ctx, new_names)
            in
            let (new_ctx, new_names) = List.fold_left fold_arg (context, existing_names) patterns in
            (new_ctx, new_names, variant_type)
        | sym ->
          let error_msg = 
            "The name \"" ^ variant_name ^ "\" resolves to"
            ^ (string_of_symbol sym) ^ ", which is not a constructor."
          in
          let new_ctx = Ssa_context.emit_error context span error_msg in
          (new_ctx, existing_names, UnknownType)
      end
    end
  | Ast.MultiPattern (span, patterns) -> 
    (* Compile all patterns, and ensure they expect the exact same patterns. *)
    let fold_pattern (context, names, out_typ) pattern =
      let (ctx_after_pattern, new_names, new_typ) = deduce_type_from_pattern context names pattern in
      if new_names <> names then
        let error_msg =
          "If multiple patterns are combined, they must produce the same exact set of names." 
        in
        let new_ctx = Ssa_context.emit_error context span error_msg in
        (new_ctx, names, UnknownType)
      else 
        let success = (ctx_after_pattern, names, new_typ) in
        if new_typ <> out_typ then
          match (new_typ, out_typ) with
          | (UnknownType, _) | (_, UnknownType) -> success
          | _ -> 
            let error_msg =
              "A pattern that produces " ^ (string_of_type out_typ)
              ^ " cannot be combined with a pattern that produces "
              ^ (string_of_type new_typ) ^ "."
            in
            let new_ctx = Ssa_context.emit_error context span error_msg in
            (new_ctx, names, UnknownType)
        else
          success
    in
    let initial_data = (context, existing_names, UnknownType) in
    List.fold_left fold_pattern initial_data patterns

(** Converts a pattern into an "if" clause condition.
 * Takes an `input` value, from which subpatterns are computed.
 * For example, a struct pattern must be matched against some value. *)
let rec condition_of_pattern context input_value = function
  | Ast.IgnoredPattern _
  | Ast.NamedPattern _
  | Ast.AliasedPattern _ -> (context, BoolLiteral true)
  (* Combine all child patterns via OR *)
  | Ast.MultiPattern (_, children) ->
    let fold_child (context, out_cond) child =
      let (new_ctx, cond) = condition_of_pattern context input_value child in
      let new_cond = BoolCompare (out_cond, Ast.BooleanOr, cond) in
      (new_ctx, new_cond)
    in
    let (new_ctx, child_cond) = 
      List.fold_left fold_child (context, (BoolLiteral true)) children
    in
    (new_ctx, child_cond)
  (* If we are matching a struct, we need to match against all of its fields. *)
  | Ast.StructPattern (span, field_patterns) -> begin
      (* If this is not a struct type, we obviously can't match against it. *)
      match type_of_value input_value with
      | StructType field_types as typ -> begin
          (* Combine the patterns into a single boolean OR. *)
          let fold_pattern (context, out_cond) (span, name, pattern) =
            (* We have to lookup each field, in order to produce the next
             * input_value to pass to recursive calls.
             * This way, we can resolve an index to pass to GetElement. After that's
             * done, the rest is pretty trivial.
             *
             * However, if the name is "_", then the field is ignored, so continue. *)
            let initial_data = (None, 0) in
            let find_field field_name field_type (get_field_opt, index) =
              match get_field_opt with
              (* If we already found the field, continue. *)
              | Some _ -> (get_field_opt, index)
              (* Check to see if the name matches. If so, we are done. *)
              | None -> 
                if (field_name <> name) || ("_" = name) then
                  (None, index + 1)
                else
                  let value = GetElement (field_type, input_value, index) in
                  (Some value, index)
            in
            (* If we found the field, match the pattern. Otherwise, produce an error. *)
            let (get_field_opt, _) = StringMap.fold find_field field_types initial_data in
            match get_field_opt with
            | None ->
              let error_message =
                "The type " ^ (string_of_type typ) 
                ^ " has no field named \"" ^ name ^ "\"."
              in
              let new_ctx = emit_error context span error_message in
              (new_ctx, out_cond)
            | Some get_field ->
              let (ctx_after_cond, subcond) = condition_of_pattern context get_field pattern in
              (ctx_after_cond, BoolCompare (out_cond, Ast.BooleanOr, subcond))
          in
          let (new_ctx, child_cond) =
            List.fold_left fold_pattern (context, (BoolLiteral true)) field_patterns 
          in
          (new_ctx, child_cond)
        end
      | typ ->
        let error_message = "The type " ^ (string_of_type typ) ^ " is not a structure." in
        let new_ctx = emit_error context span error_message in
        (new_ctx, BoolLiteral false)
    end
  (* TODO: More matches *)
  | _ -> (context, BoolLiteral false)