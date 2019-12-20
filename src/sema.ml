module StringMap = Map.Make(String)

type universe =
  {
    modules: sumo_module StringMap.t
  }
and symbol =
  | Module of string
  | ModuleMember of string * string
  | Value of Ast.span * typ
and sumo_module =
  {
    name: string;
    members: (Visibility.t * module_member) StringMap.t;
  }
and module_member =
  | Type of typ
  | Global of Ast.span * typ
  | UnresolvedFunc of Ast.func
  | Func of Ast.span * string * (typ list) * typ
and typ =
  | IntType
  | DoubleType
  | BoolType
  | VoidType
  | OptionalType of typ