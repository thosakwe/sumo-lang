open Ssa

type t =
  {
    block_is_dead: bool;
    this_module: string;
    errors: Sema.error list;
    namer: Namer.t;
    scope: symbol Scope.t;
    universe: universe;
    current_class: typ option;
  }

(** Shortcut for emitting an error, and returning a new context object. *)
let emit_error context span error_msg =
  let error = (span, Sema.Error, error_msg) in
  {context with errors = context.errors @ [error]}

let emit_warning context span error_msg =
  let error = (span, Sema.Warning, error_msg) in
  {context with errors = context.errors @ [error]}

let handle_dead_code span initial_context =
  if initial_context.block_is_dead then
    emit_warning initial_context span "Dead code."
  else initial_context
