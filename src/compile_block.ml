open Ssa
open Ssa_context

let rec compile_block name initial_context compile_stmt expected_return (span, stmts) =
  compile_block_extra name initial_context compile_stmt expected_return (span, stmts) false [] []

and compile_block_extra
    name initial_context compile_stmt expected_return (span, stmts)
    use_verbatim_name prelude_instrs postlude_instrs =
  let context = handle_dead_code span initial_context in
  let new_scope = Scope.ChildScope (context.scope, StringMap.empty) in
  let child_context = { context with scope = new_scope } in
  let compile_one_stmt (context, out_list) stmt =
    let (new_ctx, new_out_list, _) = compile_stmt (context, out_list, expected_return) stmt in
    (new_ctx, new_out_list)
  in
  let (ctx_after_stmts, new_out_list) = List.fold_left compile_one_stmt (child_context, []) stmts in
  let (name, new_namer) = 
    if use_verbatim_name then
      (name, ctx_after_stmts.namer)
    else
      Namer.next_name name ctx_after_stmts.namer 
  in
  let new_ctx = { 
    context with 
    namer = new_namer; 
    errors = ctx_after_stmts.errors ;
    block_is_dead = context.block_is_dead || ctx_after_stmts.block_is_dead;
  } in
  let instrs = [
    (span, Block (name, prelude_instrs @ new_out_list @ postlude_instrs));
    (span, Jump name);
  ]
  in
  (new_ctx, instrs, expected_return)