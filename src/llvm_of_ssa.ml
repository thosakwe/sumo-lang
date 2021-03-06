open Ssa

module Int = struct
  type t = int
  let compare a b = a - b
end

module IntMap = Map.Make(Int)

type context =
  {
    errors: Sema.error list;
    builder: Llvm.llbuilder;
    func: Llvm.llvalue option;
    llvm_context : Llvm.llcontext;
    llvm_module: Llvm.llmodule;
    scope: Llvm.llvalue Scope.t;
    blocks: Llvm.llbasicblock StringMap.t;
  }

let rec compile_universe module_path errors universe =
  let llvm_context = Llvm.create_context () in
  let llvm_module = Llvm.create_module llvm_context module_path in

  (* Create an initial scope containing forward declarations
   * of all known functions. *)

  (* There's probably a more elegant way to do this, but basically:
   * 1. Get a list of all modules.
   * 2. Get a list of all symbols for a given module.
   * 3. Map each module's list of symbols into (name, LLVM values).
   * 4. Concat all these lists into one.
   * 5. Turn these pairs into a map. *)

  let map_of_all_symbols =
    let list_of_modules = List.of_seq (StringMap.to_seq universe.modules) in
    let list_of_symbols (path, m_ref) =
      let pairs = List.of_seq (StringMap.to_seq (!m_ref).symbols) in
      let llvm_of_pair out_list ((name:string), (_, sym)) =
        (* If this is the module we are compiling, then don't forward-declare anything
             * because names can only be defined once. Otherwise you get main.1, foo.1, etc.
             *
             * TODO: Prevent duplicates
             *
             * The only exception is "external" functions. *)
        let forward_decl ext params returns llvm_name prelude_params =
          (* let _ = path, out_list, ext in *)
          (* if (module_path == path) && (not ext) then begin *)
          if false then begin
            let _ = path, ext in
            (* print_endline module_path;
               print_endline path;
               print_endline (string_of_bool ext); *)
            None
          end
          else begin
            let llvm_function_type =
              compile_function_signature llvm_context params returns prelude_params
            in
            let value = Llvm.define_function llvm_name llvm_function_type llvm_module in
            let pair = (name, value) in
            Some pair
          end
        in

        match sym with
        | VarSymbol (_, qualified_name, typ) ->
          let llvm_type = compile_type llvm_context typ in
          let value = Llvm.declare_global llvm_type qualified_name llvm_module in
          out_list @ [(name, value)]
        | FuncSymbol (ext, llvm_name, params, returns, _) -> begin
            match forward_decl ext params returns llvm_name [] with
            | None -> out_list
            | Some pair -> out_list @ [pair]
          end
        | ParamSymbol _ -> out_list
        | ImportedSymbol _ -> out_list
        | TypeSymbol _ -> out_list
        | ConstructorSymbol _ -> out_list
      in
      List.fold_left llvm_of_pair [] pairs
    in
    let list_of_lists_of_symbols =
      List.map list_of_symbols list_of_modules
    in
    let all_symbols = List.concat list_of_lists_of_symbols in
    StringMap.of_seq (List.to_seq all_symbols)
  in

  let initial_context =
    {
      errors;
      llvm_context;
      llvm_module;
      func = None;
      builder = Llvm.builder llvm_context;
      scope = Scope.RootScope map_of_all_symbols;
      blocks = StringMap.empty;
    }
  in

  (* Once we have our scope, compile each function. *)
  let compile_functions_in_module context m_ref =
    let compile_one_function context f =
      let (new_ctx, _) = compile_function context f in
      new_ctx
    in
    List.fold_left compile_one_function context (!m_ref).compiled_functions
  in
  let folder _ m_ref context =
    compile_functions_in_module context m_ref
  in
  StringMap.fold folder universe.modules initial_context

and compile_function context (name, params, returns, instrs) =
  let llvm_function_type = compile_function_signature context.llvm_context params returns [] in

  (* If we have already declared the function, don't redeclare it *)
  let func = match Llvm.lookup_function name context.llvm_module with
    | Some value -> value
    | None -> Llvm.define_function name llvm_function_type context.llvm_module
  in

  (* Create a new scope, with the function name and params injected. *)
  let new_scope_map =
    let fold_param (map, index) (name, _) =
      let value = Llvm.param func index in
      let new_map = StringMap.add name value map in
      (new_map, index + 1)
    in

    let (map, _) = List.fold_left fold_param (StringMap.empty, 0) params in
    StringMap.add name func map
  in


  let new_scope = Scope.ChildScope (context.scope, new_scope_map) in
  let entry_block = Llvm.entry_block func in
  let new_builder = Llvm.builder context.llvm_context in
  Llvm.position_at_end entry_block new_builder;
  let new_context = { 
    context with
    scope = new_scope;
    builder = new_builder;
    func = Some func;
  } in
  let final_ctx =
    let compile_one_instr context (span, instr) =
      let (new_ctx, _) = compile_instr context span instr in
      new_ctx
    in
    List.fold_left compile_one_instr new_context instrs
  in
  (final_ctx, func)

and compile_instr context span = function
  | Value value -> compile_value context span value
  | Return (_, value) ->
    let (new_ctx, llvm_value)  = compile_value context span value in
    (new_ctx, Llvm.build_ret llvm_value context.builder)
  | ReturnVoid ->
    (context, Llvm.build_ret_void context.builder)
  | Jump label -> begin
      match StringMap.find_opt label context.blocks with
      | None ->
        let error_msg =
          "LLVM compiler error: Jump to unknown block \""
          ^ label
          ^ "\"."
        in
        let error_value = Llvm.const_null (Llvm.i64_type context.llvm_context) in
        ((emit_error context span error_msg), error_value)
      | Some block -> begin
          (context, Llvm.build_br block context.builder)
        end
    end
  | JumpIf (cond, if_true, if_false) -> begin
      let nonexistent_block name context =
        let error_msg =
          "LLVM compiler error: Conditional dump to unknown block \""
          ^ name
          ^ "\"."
        in
        let error_value = Llvm.const_null (Llvm.i64_type context.llvm_context) in
        ((emit_error context span error_msg), error_value)
      in
      match StringMap.find_opt if_true context.blocks with
      | None -> nonexistent_block if_true context
      | Some if_true_block -> begin
          match StringMap.find_opt if_false context.blocks with
          | None -> nonexistent_block if_false context
          | Some if_false_block ->
            let (new_ctx, llvm_cond) = compile_value context span cond in
            (new_ctx, Llvm.build_cond_br llvm_cond if_true_block if_false_block context.builder)
        end
    end
  | PositionAtEnd name -> begin
      match StringMap.find_opt name context.blocks with
      | None -> 
        let error_msg =
          "LLVM compiler error: Position at end of unknown block \""
          ^ name
          ^ "\"."
        in
        let error_value = Llvm.const_null (Llvm.i64_type context.llvm_context) in
        ((emit_error context span error_msg), error_value)
      | Some block ->
        Llvm.position_at_end block context.builder;
        (context, Llvm.value_of_block block)
    end
  (* If we encounter a block, just compile each statement in turn, in a new context. *)
  | Block (name, spanned_instrs) -> begin
      let error_value = Llvm.const_null (Llvm.i64_type context.llvm_context) in
      match context.func with
      | None -> 
        let error_msg = "LLVM compiler error: Encountered a Block, but we are not in a function." in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, error_value)
      | Some func -> 
        let block = Llvm.append_block context.llvm_context name func in
        let child_builder = Llvm.builder context.llvm_context in
        Llvm.position_at_end block child_builder;

        let child_context =
          { 
            context with 
            builder = child_builder;
            blocks = StringMap.add name block context.blocks;
          } 
        in
        let compile_one_child (context, _) (span, instr) =
          compile_instr context span instr
        in

        let (last_child_context, last_value) =
          List.fold_left compile_one_child (child_context, error_value) spanned_instrs
        in

        let new_ctx = {
          context with 
          errors = last_child_context.errors;
          blocks = StringMap.add name block context.blocks;
        }
        in
        (new_ctx, last_value)
    end

and compile_value context span value = 
  let error_value = Llvm.const_null (Llvm.i64_type context.llvm_context) in
  let zero = Llvm.const_int (Llvm.i1_type context.llvm_context) 0 in
  let one = Llvm.const_int (Llvm.i1_type context.llvm_context) 1 in

  match value with
  | IntLiteral v -> (context, Llvm.const_int (Llvm.i64_type context.llvm_context) v)
  | DoubleLiteral v -> (context, Llvm.const_float (Llvm.double_type context.llvm_context) v)
  | BoolLiteral v -> 
    let value = if v then 1 else 0 in
    (context, Llvm.const_int (Llvm.i1_type context.llvm_context) value)
  | Multi items -> 
    let fold_item (context, values) value =
      let (new_ctx, llvm_value) = compile_value context span value in
      (new_ctx, values @ [llvm_value])
    in
    let (new_ctx, llvm_values) = List.fold_left fold_item (context, []) items in
    (new_ctx, List.hd (List.rev llvm_values))
  | CastIntToDouble inner ->
    let (new_ctx, llvm_inner) = compile_value context span inner in
    let double_type = Llvm.double_type context.llvm_context in
    let new_value = Llvm.build_sitofp llvm_inner double_type "tmp" context.builder in
    (new_ctx, new_value)
  | CastDoubleToInt inner ->
    let (new_ctx, llvm_inner) = compile_value context span inner in
    let int_type = Llvm.i64_type context.llvm_context in
    let new_value = Llvm.build_fptosi llvm_inner int_type "tmp" context.builder in
    (new_ctx, new_value)
  | IntArithmetic (lhs, op, rhs) -> begin
      let (ctx_after_lhs, llvm_lhs) = compile_value context span lhs in
      let (ctx_after_rhs, llvm_rhs) = compile_value ctx_after_lhs span rhs in
      let f = match op with
        | Ast.Multiply -> Llvm.build_mul
        | Ast.Divide -> Llvm.build_sdiv
        | Ast.Modulo -> Llvm.build_srem
        | Ast.Plus -> Llvm.build_add
        | Ast.Minus -> Llvm.build_sub
        | Ast.Shift typ -> begin
            match typ with
            | Ast.Left -> Llvm.build_shl
            | Ast.Right -> Llvm.build_lshr
          end
        | Ast.Bitwise typ -> begin
            match typ with
            | Ast.BitwiseAnd -> Llvm.build_and
            | Ast.BitwiseXor -> Llvm.build_xor
            | Ast.BitwiseOr -> Llvm.build_or
          end
        | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq -> begin
            let f a b name builder =
              let icmp = match op with
                | Lt -> Llvm.Icmp.Slt
                | Lte -> Llvm.Icmp.Sle
                | Gt -> Llvm.Icmp.Sgt
                | Gte -> Llvm.Icmp.Sge
                | Eq -> Llvm.Icmp.Eq
                | _ -> Llvm.Icmp.Ne
              in
              let cmp = Llvm.build_icmp icmp a b name builder in
              Llvm.build_select cmp one zero "tmp" builder
            in
            f
          end
        (* Ints don't support booleans, so this case will never be reached. *)
        | Ast.BooleanAnd
        | Ast.BooleanOr -> Llvm.build_add
      in
      let result = f llvm_lhs llvm_rhs "tmp" context.builder in
      (ctx_after_rhs, result)
    end
  | DoubleArithmetic (lhs, op, rhs) -> begin
      let (ctx_after_lhs, llvm_lhs) = compile_value context span lhs in
      let (ctx_after_rhs, llvm_rhs) = compile_value ctx_after_lhs span rhs in
      let f = match op with
        | Ast.Multiply -> Llvm.build_fadd
        | Ast.Divide -> Llvm.build_fdiv
        | Ast.Modulo -> Llvm.build_frem
        | Ast.Plus -> Llvm.build_fadd
        | Ast.Minus -> Llvm.build_fsub
        | Ast.Lt | Ast.Lte | Ast.Gt | Ast.Gte | Ast.Eq | Ast.Neq -> begin
            let f a b name builder =
              let fcmp = match op with
                | Lt -> Llvm.Fcmp.Olt
                | Lte -> Llvm.Fcmp.Ole
                | Gt -> Llvm.Fcmp.Ogt
                | Gte -> Llvm.Fcmp.Oge
                | Eq -> Llvm.Fcmp.Oeq
                | _ -> Llvm.Fcmp.One
              in
              let cmp = Llvm.build_fcmp fcmp a b name builder in
              Llvm.build_select cmp one zero "tmp" builder
            in
            f
          end
        (* Doubles don't support shifts/bitwise, but we'll never reach this case. *)
        | Ast.Shift _
        | Ast.Bitwise _ 
        | Ast.BooleanAnd
        | Ast.BooleanOr -> Llvm.build_shl
      in
      let result = f llvm_lhs llvm_rhs "tmp" context.builder in
      (ctx_after_rhs, result)
    end
  | BoolCompare (lhs, op, rhs) -> begin
      let (ctx_after_lhs, llvm_lhs) = compile_value context span lhs in
      let (ctx_after_rhs, llvm_rhs) = compile_value ctx_after_lhs span rhs in
      let f = match op with
        | Ast.BooleanAnd -> Llvm.build_and
        | Ast.BooleanOr -> Llvm.build_or
        | _ ->
          let f a b name builder =
            let icmp = match op with
              | Eq -> Llvm.Icmp.Eq
              | _ -> Llvm.Icmp.Ne
            in
            let cmp = Llvm.build_icmp icmp a b name builder in
            Llvm.build_select cmp one zero "tmp" builder
          in
          f
      in
      let result = f llvm_lhs llvm_rhs "tmp" context.builder in
      (ctx_after_rhs, result)
    end
  | BooleanNegate inner
  | BitwiseNegate inner -> 
    let (ctx_after_inner, llvm_inner) = compile_value context span inner in
    let result = Llvm.build_neg llvm_inner "tmp" context.builder in
    (ctx_after_inner, result)
  | Positive (typ, inner)
  | Negative (typ, inner) -> begin
      let (ctx_after_inner, llvm_inner) = compile_value context span inner in
      let llvm_type = compile_type ctx_after_inner.llvm_context typ in
      let negative_one = Llvm.const_int llvm_type (-1) in
      let zero = Llvm.const_int llvm_type 0 in
      let change_sign = 
        let mul = match typ with
          | DoubleType -> Llvm.build_fmul
          | _ -> Llvm.build_mul
        in
        mul llvm_inner negative_one "tmp" context.builder
      in
      let less_than_zero = 
        let cmp =
          match typ with
          | DoubleType -> begin
              match  value with
              | Negative _ -> Llvm.build_fcmp Llvm.Fcmp.Ogt 
              |  _ -> Llvm.build_fcmp Llvm.Fcmp.Olt 
            end
          | _ -> begin
              match  value with
              | Negative _ -> Llvm.build_icmp Llvm.Icmp.Sgt 
              |  _ -> Llvm.build_icmp Llvm.Icmp.Slt 
            end
        in
        cmp llvm_inner zero "tmp" ctx_after_inner.builder
      in
      let result =
        Llvm.build_select less_than_zero change_sign llvm_inner "tmp" ctx_after_inner.builder
      in
      (ctx_after_inner, result)
    end
  | VarGet (name, _) ->
    if not (Scope.mem name context.scope) then
      let error_msg =
        "LLVM compiler error: The SSA form emitted VarGet \""
        ^ name
        ^ "\", but the current context has no variable with that name."
      in
      let new_ctx = emit_error context span error_msg in
      (new_ctx, error_value)
    else
      let target = Scope.find name context.scope in
      (context, Llvm.build_load target name context.builder)
  | ParamGet (index, name, _) -> begin
      match context.func with
      | Some llvm_func -> (context, Llvm.param llvm_func index)
      | None -> 
        let error_msg =
          "LLVM compiler error: The SSA form emitted ParamGet \""
          ^ name
          ^ "\", but the current context does not exist within a function."
        in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, error_value)
    end
  (* Create a new scope with the given value. *)
  | VarCreate (name, typ) -> 
    let llvm_type = compile_type context.llvm_context typ in
    let variable = Llvm.build_alloca llvm_type name context.builder in
    let new_scope = Scope.replace name variable context.scope in
    ({ context with scope = new_scope }, variable)
  (* Simply assign to a pointer. *)
  | VarSet (name, _, value) ->
    if not (Scope.mem name context.scope) then
      let error_msg =
        "LLVM compiler error: The SSA form emitted VarSet \""
        ^ name
        ^ "\", but the current context has no variable with that name."
      in
      let new_ctx = emit_error context span error_msg in
      (new_ctx, error_value)
    else
      let (new_ctx, llvm_value) = compile_value context span value in
      let variable = Scope.find name context.scope in
      let _ = Llvm.build_store llvm_value variable new_ctx.builder in
      let result = variable in
      let new_scope = Scope.replace name result context.scope in
      ({ new_ctx with scope = new_scope }, variable)
  | FunctionCall (returns, name, args) -> begin
      match Llvm.lookup_function name context.llvm_module with
      | None ->
        let error_msg = "LLVM compiler error: " ^ (Scope.does_not_exist name) in
        let new_ctx = emit_error context span error_msg in
        (new_ctx, Llvm.const_null (Llvm.i64_type context.llvm_context))
      | Some target ->
        let (new_ctx, llvm_args) =
          let compile_arg (context, out_list) arg =
            let (new_ctx, value) = compile_value context span arg in
            (new_ctx, out_list @ [value])
          in
          List.fold_left compile_arg (context, []) args
        in
        (* If the function returns void, return no value. *)
        let return_name =
          match returns with
          | VoidType -> ""
          | _ -> "tmp"
        in
        (new_ctx, Llvm.build_call target (Array.of_list llvm_args) return_name new_ctx.builder)
    end
  | OptionalSome (typ, _)
  | OptionalNone typ -> begin
      let bool_type = compile_type context.llvm_context BoolType in
      let llvm_type = compile_type context.llvm_context typ in
      let struct_type = Llvm.struct_type context.llvm_context [| bool_type; llvm_type |] in
      let struct_pointer = Llvm.build_alloca struct_type "opt_new_struct_ptr" context.builder in
      (* let struct_pointer = Llvm.build_malloc struct_type "opt_new_struct_ptr" context.builder in *)
      let bool_ptr = Llvm.build_struct_gep struct_pointer 0 "opt_bool_ptr" context.builder in
      let value_ptr = Llvm.build_struct_gep struct_pointer 1 "opt_value_ptr" context.builder in
      let new_ctx = match value with
        | OptionalSome (_, inner_value) ->
          let (new_ctx, llvm_value) = compile_value context span inner_value in
          let _ = Llvm.build_store one bool_ptr context.builder in
          let _ = Llvm.build_store llvm_value value_ptr context.builder in
          new_ctx
        |  _ ->
          let _ = Llvm.build_store zero bool_ptr context.builder in
          context
      in
      (new_ctx, struct_pointer)
    end
  | OptionalNullCheck value ->
    let (new_ctx, llvm_value) = compile_value context span value in
    let ptr = Llvm.build_struct_gep llvm_value 0 "opt_nc_struct_ptr" new_ctx.builder in
    let result = Llvm.build_load ptr "opt_nc" new_ctx.builder in
    (new_ctx, result)
  | OptionalGet (_, value) ->
    let (new_ctx, llvm_value) = compile_value context span value in
    let ptr = Llvm.build_struct_gep llvm_value 1 "opt_get_value_ptr" new_ctx.builder in
    let result = Llvm.build_load ptr "opt_get" new_ctx.builder in
    (new_ctx, result)
  | StructLiteral (struct_type, fields) -> begin
      (* If we are instantiating a variant, instead of generating the basic variant struct,
       * create the specialized struct type that corresponds to the given fields. *)
      let llvm_struct_type = match struct_type with
        | VariantType _ ->
          let fold_value_type _ value out_list =
            let ssa_typ = Ssa.type_of_value value in
            let llvm_type = compile_type context.llvm_context ssa_typ in
            out_list @ [llvm_type]
          in
          let fields = StringMap.fold fold_value_type fields [] in
          Llvm.struct_type context.llvm_context (Array.of_list fields)
        | _ -> compile_struct_type context.llvm_context struct_type 
      in
      let struct_pointer = Llvm.build_alloca llvm_struct_type "literal_struct_ptr" context.builder in
      let _ = struct_pointer, fields in

      (* Helper for emitting a set-field. *)
      let emit_field name value context index =
        let field_ptr_name = "literal_struct_value_" ^ name in
        let field_ptr = Llvm.build_struct_gep struct_pointer index field_ptr_name context.builder in
        let _ = Llvm.build_store value field_ptr context.builder in
        ()
      in

      let fold_field name value (context, index) =
        let (new_ctx, llvm_value) = compile_value context span value in
        emit_field name llvm_value new_ctx index;
        (new_ctx, index + 1)
      in
      let (new_ctx, _) = StringMap.fold fold_field fields (context, 0) in
      (new_ctx, struct_pointer)
    end
  | (GetElement (_, lhs, index) as instr)
  | (SetElement (_, lhs, index, _) as instr) -> begin
      let (ctx_after_lhs, llvm_lhs) = compile_value context span lhs in
      let field_ptr = Llvm.build_struct_gep llvm_lhs index "element_ptr" ctx_after_lhs.builder in
      match instr with
      | SetElement (_, _, _, rhs) -> 
        let (ctx_after_rhs, llvm_rhs) = compile_value ctx_after_lhs span rhs in
        (ctx_after_rhs, Llvm.build_store llvm_rhs field_ptr ctx_after_lhs.builder)
      | _ -> 
        (ctx_after_lhs, Llvm.build_load field_ptr "get_element" ctx_after_lhs.builder)
    end
  | PointerCast (value, typ) ->
    let (ctx_after_value, llvm_value) = compile_value context span value in
    let llvm_type = compile_type context.llvm_context typ in
    let result = Llvm.build_pointercast llvm_value llvm_type "pointer_cast" context.builder in
    (ctx_after_value, result)

and compile_function_signature llvm_context params returns prelude_params =
  let llvm_params =
    let llvm_of_param p = compile_type llvm_context p in
    let param_types = List.map (function (_, t) -> t) params in
    List.map llvm_of_param param_types
  in
  let all_params = prelude_params @ llvm_params in
  let llvm_returns = compile_type llvm_context returns in
  Llvm.function_type llvm_returns (Array.of_list all_params)

and compile_type context = function
  | TypeAlias (_, inner) -> compile_type context inner
  | IntType -> Llvm.i64_type context
  | DoubleType -> Llvm.double_type context
  | BoolType -> Llvm.i1_type context
  | VoidType -> Llvm.void_type context
  (* Regardless of what variant it is, it always has the same abstract shape. *)
  | VariantType _ as self ->
    let struct_type = compile_struct_type context self in
    Llvm.pointer_type struct_type
  | OptionalType inner ->
    let llvm_inner = compile_type context inner in
    let bool_type = compile_type context BoolType in
    let struct_type = Llvm.struct_type context [| bool_type; llvm_inner |] in
    Llvm.pointer_type struct_type
  | StructType _ as self ->
    let struct_type = compile_struct_type context self in
    Llvm.pointer_type struct_type
  (* Note: This case should never be reached. *)
  | AnyType
  | UnknownType -> Llvm.void_type context

and compile_struct_type context = function
  | StructType (fields) ->
    let fold_field _ typ out_list =
      out_list @ [compile_type context typ]
    in
    let llvm_fields = StringMap.fold fold_field fields [] in
    Llvm.struct_type context (Array.of_list llvm_fields)
  (* Regardless of what variant it is, it always has the same abstract shape. *)
  | VariantType _ ->
    Llvm.struct_type context [| Llvm.i64_type context |]
  (* Note: This case should never be reached. *)
  | _ -> Llvm.void_type context

(* and find_block name func =
   let rec find = function
    | [] -> None
    | x :: rest ->
      if (Llvm.value_name x) = name then
        Some x
      else
        find rest
   in
   let block_list = Array.to_list (Llvm.basic_blocks func) in
   Llvm.label_type
   find block_list *)

and emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}

and vtable_name class_name =
  class_name ^ "@vtable"