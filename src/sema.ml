module StringMap = Map.Make(String)

type universe =
  {
    modules: sumo_module StringMap.t
  }
and sumo_module =
  {
    name: string;
    symbols: (Visibility.t * symbol) StringMap.t;
  }
and symbol =
  | Module
  | Type of typ
  | Value of Ast.span * typ
  | UnresolvedFunc of Ast.func
  | Func of Ast.span * string * (typ list) * typ
and typ =
  | IntType
  | FloatType
  | BoolType
  | OptionalType of typ