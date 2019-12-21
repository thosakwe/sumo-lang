type instr =
  | VarAssn of string * typ * value
  | VarGet of string
  | Return of typ * value
  | ReturnVoid
and typ =
  | IntType
  | DoubleType
  | BoolType
  | VoidType
and value =
  | FunctionCall of typ * value * (value list)
  | IntLiteral of int
  | DoubleLiteral of float
  | BoolLiteral of bool

let type_of_value = function
  | FunctionCall (typ, _, _) -> typ
  | IntLiteral _ -> IntType
  | DoubleLiteral _ -> DoubleType
  | BoolLiteral _ -> BoolType