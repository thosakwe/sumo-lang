module StringMap = Map.Make(String)

type compilation_unit = (directive list) * (decl list)
and directive =
  | ImportDirective of span * string * (import_modifier option)
and import_modifier =
  | Show of span * ((span * string) list)
  | Hide of span * ((span * string) list)
and decl =
  | FuncDecl of span * Visibility.t * func
  | TypeDecl of span * Visibility.t * string * typ
and func =
  | ConcreteFunc of func_body
  | ExternalFunc of span * (string option) * string * func_signature
and func_body = span * string * func_signature * block
and func_signature = span * (param list) * typ
and stmt =
  | Block of span * block
  | Expr of span * expr
  | VarDecl of span * var_decl list
  | Return of span * (expr option)
  | If of span * if_clause * (if_clause list) * (stmt option)
  | While of span * expr * stmt
  | DoWhile of span * stmt * expr
  | ForLoop of span * (stmt option) * expr * (stmt list) * stmt
and var_decl = span * bool * (typ option) * string * expr
and if_clause =
  | BasicIfClause of span * expr * stmt
  | NullCheckIfClause of span * var_decl list * stmt
and assign_target =
  | VariableTarget of span * string
  | FieldTarget of span * expr * string
  (* | IndexTarget of span * expr * expr *)
and assign_op =
  | Equals
  | BinaryAssign of binary_op
and block = stmt list
and typ =
  | TypeRef of span * string
  | OptionalType of span * typ
  | StructType of span * ((span * string * typ) list)
and expr =
  | Ref of span * string
  | IntLiteral of span * int
  | DoubleLiteral of span * float
  | BoolLiteral of span * bool
  | Paren of span * expr
  | Call of span * expr * (expr list)
  | Assign of span * assign_target * assign_op * expr
  | Binary of span * expr * binary_op * expr
  | Unary of span * expr * unary_op
  | NoneLiteral of span
  | StructLiteral of span * ((span * string * expr) list)
  | GetField of span * expr * string
and binary_op =
  | Multiply
  | Divide
  | Modulo
  | Plus
  | Minus
  | Shift of shift_type
  | Bitwise of bitwise_type
  | Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq
  | BooleanAnd
  | BooleanOr
and unary_op =
  | PostfixIncrement
  | PostfixDecrement
  | PrefixIncrement
  | PrefixDecrement
  | UnaryPlus
  | UnaryMinus
  | LogicalNot
  | BitwiseNot
and shift_type = Left | Right
and bitwise_type = BitwiseAnd | BitwiseXor | BitwiseOr
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

let string_of_binary_op = function
  | Multiply -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | Plus -> "+"
  | Minus -> "-"
  | Shift typ -> begin
      match typ with
      | Left -> "<<"
      | Right -> ">>"
    end
  | Bitwise typ -> begin
      match typ with
      | BitwiseAnd -> "&"
      | BitwiseXor -> "^"
      | BitwiseOr -> "|"
    end
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Eq -> "=="
  | Neq -> "!="
  | BooleanAnd -> "&&"
  | BooleanOr -> "||"

let string_of_unary_op = function
  | PrefixDecrement
  | PostfixDecrement -> "--"
  | PrefixIncrement
  | PostfixIncrement -> "++"
  | UnaryPlus -> "+"
  | UnaryMinus -> "-"
  | LogicalNot -> "!"
  | BitwiseNot -> "~"

let expr_of_assign_target = function
  | VariableTarget (span, name) -> Ref (span, name)
  | FieldTarget (span, expr, name) -> GetField (span, expr, name)

let block_of_stmt = function
  | Block (span, stmts) -> (span, stmts)
  | VarDecl (span, _) as self -> (span, [self])
  | Expr (span, _) as self ->  (span, [self])
  | Return (span, _) as self ->  (span, [self])
  | If (span, _, _, _) as self -> (span, [self])
  | While (span, _, _) as self -> (span, [self])
  | DoWhile (span, _, _) as self -> (span, [self])
  | ForLoop (span, _, _, _, _) as self -> (span, [self])

let span_of_expr = function
  | Ref (span, _)
  | IntLiteral (span, _) 
  | DoubleLiteral (span, _) 
  | BoolLiteral (span, _) 
  | Paren (span, _) 
  | Call (span, _, _)
  | Assign (span, _, _, _) 
  | Binary (span, _, _, _) 
  | Unary (span, _, _) 
  | NoneLiteral span
  | StructLiteral (span, _)
  | GetField (span, _, _)
    -> span