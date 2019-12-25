open Ssa

type t =
  {
    this_module: string;
    errors: Sema.error list;
    namer: Namer.t;
    scope: symbol Scope.t;
    universe: universe;
  }