open Cast_value
open Compile_function
open Compile_expr
open Compile_type
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

  let default_context =
    {
      block_is_dead = false;
      this_module = path;
      errors = [];
      (* scope = Scope.empty; *)
      scope = builtin_scope;
      universe = default_universe;
      namer = Namer.empty;
      current_class = None;
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
              | (_, Ok result) -> (context, result)
              | (_, Error _) ->
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
  (* TODO: Handle duplicate symbols *)
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
        (* If we visit a class, we need to create its members. *)
        (* TODO: Create fields, functions *)
        | Ast.ClassDecl ((_, vis, abstract, class_name, _, members)) ->
          (* Read all modifiers to ensure we don't declare duplicate visibility. *)
          let fold_member_modifier (context, name, final, vis_opt) = function
            | Ast.MemberFinality _ -> (context, name, true, vis_opt)
            | Ast.MemberVisibility (span, vis) -> begin 
                match vis_opt with
                | None -> (context, name, final, Some vis)
                | Some existing ->
                  let error_msg =
                    "The field \"" ^ name ^ "\" has already been marked as a "
                    ^ (Visibility.string_of_visibility existing)
                    ^ " member." 
                  in
                  let new_ctx = emit_error context span error_msg in
                  (new_ctx, name, final, vis_opt)
              end
          in

          let fold_member (context, member_map) = function
            (* TODO: If the parent class has a constructor, the child class must have one (call super).
             * In addition, if you are implementing a class, the implemented class MUST NOT have any fields. *)
            | Ast.ClassField (span, modifiers, name, typ_opt, value_opt) -> begin
                if StringMap.mem name member_map then
                  let error_msg =
                    "The class \"" ^ class_name
                    ^ "\" has already defined a member named \"" ^ name ^ "\"."
                  in
                  let new_ctx = emit_error context span error_msg in
                  (new_ctx, member_map)
                else begin
                  let (ctx_after_modifiers, _, final, vis_opt) =
                    List.fold_left fold_member_modifier (context, name, false, None) modifiers
                  in
                  let vis = match vis_opt with
                    | None -> Visibility.Private
                    | Some v -> v
                  in
                  (* We must be able to figure out the type of this value.
                   * If no type is given, then there MUST be a given value.
                   * If a value is given, it must be castable to the provided type. 
                   * TODO: If no value is provided, there MUST be a constructor that initializes it. *)
                  match (typ_opt, value_opt) with
                  | (None, None) ->
                    let error_msg = "If no type is given, then a default value must be provided." in
                    let new_ctx = emit_error context span error_msg in
                    (new_ctx, member_map)
                  | _ -> begin
                      let (ctx_after_value, field_type, compiled_value_opt) = match value_opt with
                        | None -> (ctx_after_modifiers, UnknownType, None)
                        | Some value -> begin
                            let (ctx_after_value, value_type, compiled_value_opt) = 
                              compile_expr ctx_after_modifiers value
                            in
                            match typ_opt with
                            | None -> (ctx_after_value, value_type, compiled_value_opt)
                            | Some typ_ast -> begin
                                let (ctx_after_type, compiled_type) = compile_type ctx_after_value typ_ast in
                                let value_span = Ast.span_of_expr value in
                                match cast_value ctx_after_type value_span compiled_value_opt value_type compiled_type with
                                | (ctx_after_cast, Error _) ->
                                  (ctx_after_cast, compiled_type, None)
                                | (ctx_after_cast, Ok coerced_value_opt) ->
                                  (ctx_after_cast, compiled_type, coerced_value_opt)
                              end
                          end
                      in

                      let member = ClassField (span, final, name, field_type, compiled_value_opt) in
                      let new_map = StringMap.add name (vis, member) member_map in
                      (ctx_after_value, new_map)
                    end

                end
              end
            (* TODO: Other kinds of members *)
            | Ast.ClassFunc (_, vis, (_, name, ast_sig, _)) as ast_member ->
              let (ctx_after_sig, params, returns) = compile_function_signature context ast_sig in
              let qualified = Sema.qualify_class_function_name path class_name name in
              let member = ClassFunc (Method, qualified, params, returns, ast_member) in
              let new_map = StringMap.add name (vis, member) member_map in
              (ctx_after_sig, new_map)
            | _ -> (context, member_map)
          in

          let (ctx_after_members, member_map) =
            List.fold_left fold_member (context, StringMap.empty) members 
          in

          let typ = Class (abstract, class_name, None, [], member_map) in
          let symbol = TypeSymbol typ in
          let pair = (class_name, (vis, symbol)) in
          let new_scope = Scope.replace class_name symbol context.scope in
          ({ ctx_after_members with scope = new_scope }, pair_list @ [pair])
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
    | Ast.ClassDecl _ -> (context, out_list)
    | Ast.DummyDecl -> (context, out_list)
    (* | _ -> (context, out_list) *)
  in
  let (final_ctx, compiled_functions) = List.fold_left compile_decl (new_context, []) decls in
  this_module := {!this_module with compiled_functions };

  ({ final_ctx with block_is_dead = false }, new_universe, this_module)