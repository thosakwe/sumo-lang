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
  | Ast.VariantType (span, variants) -> begin
      let fold_variant_ast (context, map) (span, name, args) =
        if StringMap.mem name map then
          let error_msg =
            "This type already has a variant named \"" ^ name ^ "\"." 
          in
          let new_ctx = emit_error context span error_msg in
          (new_ctx, map)
        else begin
          let fold_arg (context, out_list) arg =
            let (ctx, compiled) = compile_type context arg in
            (ctx, out_list @ [compiled])
          in
          let (ctx_after_args, variant_args) = List.fold_left fold_arg (context, []) args in
          let variant = (name, variant_args) in
          let new_map = StringMap.add name variant map in
          (ctx_after_args, new_map)
        end
      in

      let (ctx_after_variants, variant_map) =
        List.fold_left fold_variant_ast (context, StringMap.empty) variants
      in

      if StringMap.is_empty variant_map then
        let error_msg = "A sum type must have at least one variant." in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, UnknownType)
      else
        let result = VariantType variant_map in
        (ctx_after_variants, result)
    end

(** Injects all variants of a given type into the scope. *)
let pairs_of_variant vis = function
  | VariantType variant_map as typ ->
    let fold_variant_ssa name variant (out_list, index) =
      let sym = ConstructorSymbol (typ, index, variant) in
      let new_list = out_list @ [(name, (vis, sym))] in
      (new_list, index + 1)
    in
    let (pair_list, _) = StringMap.fold fold_variant_ssa variant_map ([], 0) in
    pair_list
  | _ -> []

(** Injects all variants of a given type into the scope. *)
let fold_variants_into_context context = function
  | VariantType variant_map as typ ->
    let fold_variant_ssa name variant (context, index) =
      let sym = ConstructorSymbol (typ, index, variant) in
      let new_scope_map = StringMap.add name sym StringMap.empty in
      let new_scope = Scope.ChildScope (context.scope, new_scope_map) in
      let new_ctx = { context with scope = new_scope } in
      (new_ctx, index + 1)
    in

    let (new_ctx, _) = StringMap.fold fold_variant_ssa variant_map (context, 0) in
    new_ctx
  | _ -> context