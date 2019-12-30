open Compile_stmt
open Compile_type
open Ssa
open Ssa_context

let rec compile_function_signature context (_, params, returns) =
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