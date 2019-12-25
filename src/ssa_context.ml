open Ssa

type t =
  {
    block_is_dead: bool;
    this_module: string;
    errors: Sema.error list;
    namer: Namer.t;
    scope: symbol Scope.t;
    universe: universe;
  }