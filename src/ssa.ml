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
and func = string * ((string * typ) list) * typ * ((instr spanned) list)
and symbol =
  (* | FuncSymbol of string * typ * (typ list) * (instr list) *)
  | FuncSymbol of bool * string * ((string * typ) list) * typ * Ast.decl
  | VarSymbol of bool * string * typ
  | TypeSymbol of typ
  (* | ImportedSymbol of (sumo_module ref) * string *)
and instr =
  | Value of value
  | Return of typ * value
  | ReturnVoid
and typ =
  | IntType
  | DoubleType
  | BoolType
  | VoidType
  | UnknownType
and value =
  | FunctionCall of typ * string * (value list)
  | IntLiteral of int
  | DoubleLiteral of float
  | BoolLiteral of bool
  | VarGet of string * typ
  | VarSet of string * typ * value
  | CastIntToDouble of value
  | CastDoubleToInt of value

let default_universe =
  {
    modules = StringMap.empty
  }

let type_of_value = function
  | FunctionCall (typ, _, _) -> typ
  | IntLiteral _ -> IntType
  | DoubleLiteral _ -> DoubleType
  | BoolLiteral _ -> BoolType
  | VarGet (_, typ) -> typ
  | VarSet (_, typ, _) -> typ
  | CastIntToDouble _ -> DoubleType
  | CastDoubleToInt _ -> IntType

let rec string_of_func (name, params, returns, spanned_instrs) =
  let instrs = List.map (function (_, x) -> x) spanned_instrs in
  let param_string = String.concat ", " (List.map string_of_param params) in
  "fn " ^ name ^ "(" ^ param_string ^ "): " ^ (string_of_type returns)
  ^ " = \n" ^ (string_of_block instrs)
and string_of_param (name, typ) =
  name ^ ": " ^ (string_of_type typ)
and string_of_symbol = function
  | FuncSymbol (ext, name, params, returns, _) ->
    (* let param_string = String.concat ", " (List.map string_of_type params) in
       "fn " ^ name ^ "(" ^ param_string ^ "): " ^ (string_of_type returns)
       ^ " = \n" ^ (string_of_block instrs) *)
    let param_string = String.concat ", " (List.map string_of_param params) in
    let lead = if ext then "external " else "fn " in
    lead ^ name ^ "(" ^ param_string ^ "): " ^ (string_of_type returns)
  | TypeSymbol typ -> "type " ^ (string_of_type typ)
  | VarSymbol (final, name, typ) -> 
    let mut_string =  if final then "final " else "var " in
    mut_string ^ name ^ ": " ^ (string_of_type typ)
(* | ImportedSymbol (m, name) ->
   let {path; _} = !m in
   path ^ "::" ^ name *)
and string_of_block block =
  let indented_string_of_instr instr =
    "  " ^ (string_of_instr instr)
  in
  String.concat "\n" (List.map indented_string_of_instr block)
and string_of_instr = function
  | Return (typ, value) -> "return " ^ (string_of_type typ) ^ " " ^ (string_of_value value)
  | ReturnVoid -> "return void"
  | Value value -> string_of_value value
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
  | VarGet (name, typ) -> "get " ^ name ^ ": " ^ (string_of_type typ)
  | VarSet (name, typ, value) ->
    "set " ^ name ^ ": " ^ (string_of_type typ)
    ^ " = " ^ (string_of_value value)
  | FunctionCall (_, target_string, args) ->
    (* let args = List.map (function (_, x) -> x) spanned_args in *)
    (* let target_string = string_of_value target in *)
    let arg_string = String.concat ", " (List.map string_of_value args) in
    target_string ^ "(" ^ arg_string ^ ")"
  | CastDoubleToInt inner ->
    "cast<double->int>(" ^ (string_of_value inner) ^ ")"
  | CastIntToDouble inner ->
    "cast<int->double>(" ^ (string_of_value inner) ^ ")"

let dump_module _ module_ref =
  let m = !module_ref in
  let dump_symbol name (_, sym) =
    print_endline (name ^ ": " ^ (string_of_symbol sym))
  in
  let dump_func f =
    print_endline (string_of_func f)
  in
  print_endline ("Module \"" ^ m.path ^ "\":");
  StringMap.iter dump_symbol m.symbols;
  List.iter dump_func m.compiled_functions

let dump_universe universe =
  StringMap.iter dump_module universe.modules