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
  (* | OptionalType of span * typ *)
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
  | ConcreteFunc (_, name, _, _) -> name
  | ExternalFunc(_, _, name, _) -> name

let name_of_param = function
  | ThisParam (_, name) -> name
  | RegularParam (_, name, _) -> name

let signature_of_func = function
  | ConcreteFunc (_, _, s, _) -> s
  | ExternalFunc (_, _, _, s) -> s

let rec innermost_expr = function
  | Paren (_, inner) -> inner
  | Call (span, target, args) ->
    let new_target = innermost_expr target in
    let new_args = List.map innermost_expr args in
    Call (span, new_target, new_args)
  | _ as self -> self