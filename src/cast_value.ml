open Ssa
open Ssa_context

let rec cast_value context span value_opt from_type to_type =
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
    | (_, AnyType, value_opt) -> (context, Ok value_opt)
    | (TypeAlias (_, inner), _, _) ->
      cast_value context span value_opt inner to_type
    | (_, TypeAlias (_, inner), _) ->
      cast_value context span value_opt from_type inner
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
    | _ ->  failure