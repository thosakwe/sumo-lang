module StringMap = Map.Make(String)

type 'a spanned = Ast.span * 'a

type universe =
  {
    modules: (sumo_module ref) StringMap.t
  }
and sumo_module =
  {
    path: string;
    symbols: (Visibility.t * symbol) StringMap.t;
    compiled_functions: func list;
  }
and func = string * (typ list) * typ * ((instr spanned) list)
and symbol =
  (* | FuncSymbol of string * typ * (typ list) * (instr list) *)
  | FuncSymbol of string * (typ list) * typ * Ast.decl
  | VarSymbol of string * typ
  | TypeSymbol of typ
  (* | ImportedSymbol of (sumo_module ref) * string *)
and instr =
  | VarAssn of string * typ * value
  | VarGet of string * typ
  | Return of typ * value
  | ReturnVoid
and typ =
  | IntType
  | DoubleType
  | BoolType
  | VoidType
  | UnknownType
and value =
  | FunctionCall of typ * (value spanned) * ((value spanned) list)
  | IntLiteral of int
  | DoubleLiteral of float
  | BoolLiteral of bool

let default_universe =
  {
    modules = StringMap.empty
  }

let type_of_value = function
  | FunctionCall (typ, _, _) -> typ
  | IntLiteral _ -> IntType
  | DoubleLiteral _ -> DoubleType
  | BoolLiteral _ -> BoolType

let rec string_of_symbol = function
  | FuncSymbol (name, params, returns, _) ->
    (* let param_string = String.concat ", " (List.map string_of_type params) in
       "fn " ^ name ^ "(" ^ param_string ^ "): " ^ (string_of_type returns)
       ^ " = \n" ^ (string_of_block instrs) *)
    let param_string = String.concat ", " (List.map string_of_type params) in
    "fn " ^ name ^ "(" ^ param_string ^ "): " ^ (string_of_type returns)
  | TypeSymbol typ -> "type " ^ (string_of_type typ)
  | VarSymbol (name, typ) -> name ^ ": " ^ (string_of_type typ)
  (* | ImportedSymbol (m, name) ->
    let {path; _} = !m in
    path ^ "::" ^ name *)
and string_of_block block =
  let indented_string_of_instr instr =
    "  " ^ (string_of_instr instr)
  in
  String.concat "\n" (List.map indented_string_of_instr block)
and string_of_instr = function
  | VarAssn (name, typ, value) ->
    "set " ^ name ^ ": " ^ (string_of_type typ)
    ^ " = " ^ (string_of_value value)
  | VarGet (name, typ) -> "get " ^ name ^ ": " ^ (string_of_type typ)
  | Return (typ, value) -> "return " ^ (string_of_type typ) ^ " " ^ (string_of_value value)
  | ReturnVoid -> "return void"
and string_of_type = function
  | IntType -> "int"
  | DoubleType -> "double"
  | BoolType -> "bool"
  | VoidType -> "void"
  | UnknownType -> "<unknown>"
and string_of_value = function
  | IntLiteral v -> string_of_int v
  | DoubleLiteral v -> string_of_float v
  | BoolLiteral v -> string_of_bool v
  | FunctionCall (_, (_, target), spanned_args) ->
    let args = List.map (function (_, x) -> x) spanned_args in
    let target_string = string_of_value target in
    let arg_string = String.concat ", " (List.map string_of_value args) in
    target_string ^ "(" ^ arg_string ^ ")"