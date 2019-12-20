module StringMap = Map.Make(String)

type universe =
  {
    modules: sumo_module StringMap.t
  }
and symbol =
  | Module of string
  | ModuleMember of string * string
  | ValueSymbol of Ast.span * typ
  | TypeSymbol of typ
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
  | FunctionType of (typ list) * typ

let empty_universe =
  {
    modules = StringMap.empty
  }

let rec string_of_type = function
  | IntType -> "int"
  | DoubleType -> "double"
  | BoolType -> "bool"
  | VoidType -> "void"
  | OptionalType inner -> (string_of_type inner) ^ "?"
  | FunctionType (params, returns) ->
    let param_str = String.concat ", " (List.map string_of_type params) in
    "(" ^ param_str ^ ") -> " ^ (string_of_type returns)