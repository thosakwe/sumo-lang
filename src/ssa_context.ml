open Ssa

type t =
  {
    this_module: string;
    errors: Sema.error list;
    scope: symbol Scope.t;
    universe: universe;
  }