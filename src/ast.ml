module StringMap = Map.Make(String)

type compilation_unit = decl list
and decl =
  | FuncDecl of span * Visibility.t * func
  | TypeDecl of span * Visibility.t * string * typ
and func =
  | ConcreteFunc of span * string * func_signature * block
  | ExternalFunc of span * (string option) * string * func_signature
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
  (* | FieldTarget of span * expr * string
     | IndexTarget of span * expr * expr *)
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
  | NoneLiteral
  | StructLiteral of span * ((span * string * expr) list)
  (* TODO: Postfix, prefix increment *)
(* TODO: Prefix plus/minus *)
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
  (* TODO: Bitwise, boolean operators *)
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

let block_of_stmt = function
  | Block (span, stmts) -> (span, stmts)
  | VarDecl (span, _) as self -> (span, [self])
  | Expr (span, _) as self ->  (span, [self])
  | Return (span, _) as self ->  (span, [self])
  | If (span, _, _, _) as self -> (span, [self])
  | While (span, _, _) as self -> (span, [self])
  | DoWhile (span, _, _) as self -> (span, [self])
  | ForLoop (span, _, _, _, _) as self -> (span, [self])