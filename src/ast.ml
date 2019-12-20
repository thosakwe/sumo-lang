type compilation_unit = decl list
and decl =
  | FuncDecl of span * Visibility.t * func
and func =
  | ConcreteFunc of span * string * func_signature * block
  | ExternalFunc of span * (string option) * string * func_signature
and func_signature = span * (param list) * typ
and stmt =
  | Block of span * block
  | Expr of span * expr
  | VarDecl of var_decl list
  | Return of span * (expr option)
and var_decl = span * bool * string * expr
and block = stmt list
and typ =
  | TypeRef of span * string
  | OptionalType of span * typ
and expr =
  | Ref of span * string
  | IntLiteral of span * int
  | DoubleLiteral of span * float
  | BoolLiteral of span * bool
  | Paren of span * expr
  | Call of span * expr * (expr list)
and param =
  | ThisParam of span * string
  | RegularParam of span * string * typ
and span = Lexing.position * Lexing.position

let name_of_func = function
  | ConcreteFunc (_, name, _, _) ->name
  | ExternalFunc(_, _, name, _) -> name