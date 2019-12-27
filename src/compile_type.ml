open Ssa
open Ssa_context

let rec compile_type context = function
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