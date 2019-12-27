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
  | ParamSymbol of string * int * typ
  | TypeSymbol of typ
  | ImportedSymbol of (sumo_module ref) * string
and instr =
  | Value of value
  | Return of typ * value
  | ReturnVoid
  | Block of string * ((instr spanned) list)
  | Jump of string
  | JumpIf of value * string * string
  | PositionAtEnd of string
and typ =
  | IntType
  | DoubleType
  | BoolType
  | VoidType
  | OptionalType of typ
  | StructType of typ StringMap.t
  | UnknownType
and value =
  | FunctionCall of typ * string * (value list)
  | IntLiteral of int
  | DoubleLiteral of float
  | BoolLiteral of bool
  | StructLiteral of typ * (value StringMap.t)
  | VarGet of string * typ
  | ParamGet of int * string * typ
  | VarCreate of string * typ
  | VarSet of string * typ * value
  | Multi of value list
  | CastIntToDouble of value
  | CastDoubleToInt of value
  | IntArithmetic of value * Ast.binary_op * value
  | DoubleArithmetic of value * Ast.binary_op * value
  | BoolCompare of value * Ast.binary_op * value
  | BooleanNegate of value
  | BitwiseNegate of value
  | Positive of typ * value
  | Negative of typ * value
  | OptionalNone of typ
  | OptionalSome of typ * value
  | OptionalNullCheck of value
  | OptionalGet of typ * value
  | GetElement of typ * value * int
  | SetElement of typ * value * int * value

let default_universe =
  {
    modules = StringMap.empty
  }

let rec type_of_value = function
  | FunctionCall (typ, _, _) -> typ
  | IntLiteral _ -> IntType
  | DoubleLiteral _ -> DoubleType
  | BoolLiteral _ -> BoolType
  | StructLiteral (typ, _) -> typ
  | VarGet (_, typ) -> typ
  | ParamGet (_, _, typ) -> typ
  | VarCreate (_, typ) -> typ
  | VarSet (_, typ, _) -> typ
  | CastIntToDouble _ -> DoubleType
  | CastDoubleToInt _ -> IntType
  | IntArithmetic _ -> IntType
  | DoubleArithmetic _ -> DoubleType
  | Multi items -> begin
      let rec f =
        function
        | [] -> raise (Invalid_argument "empty list in Multi")
        | [single] -> type_of_value single
        | _ :: rest -> f rest
      in
      f items
    end
  | BoolCompare _ -> BoolType
  | BooleanNegate _ -> BoolType
  | BitwiseNegate _ -> IntType
  | Positive (typ, _) -> typ
  | Negative (typ, _) -> typ
  | OptionalNone typ -> typ
  | OptionalSome (typ, _) -> typ
  | OptionalNullCheck _ -> BoolType
  | OptionalGet (typ, _) -> typ
  | GetElement (typ, _, _) -> typ
  | SetElement (typ, _, _, _) -> typ

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
  | ParamSymbol (name, index, typ) -> 
    let mut_string = string_of_int index in
    "param " ^ mut_string ^ ": " ^ name ^ ": " ^ (string_of_type typ)
| ImportedSymbol (m, name) ->
   let {path; _} = !m in
   path ^ "::" ^ name
and string_of_block block =
  let indented_string_of_instr instr =
    "  " ^ (string_of_instr instr)
  in
  String.concat "\n" (List.map indented_string_of_instr block)
and string_of_instr = function
  | Return (typ, value) -> "return " ^ (string_of_type typ) ^ " " ^ (string_of_value value)
  | ReturnVoid -> "return void"
  | Value value -> string_of_value value
  | Block (name, spanned_instrs) ->
    let instrs = List.map (function (_, v) -> v) spanned_instrs in
    "block " ^ name ^ " {\n"
    ^ string_of_block instrs
    ^ "\n}"
  | Jump name -> "jump " ^ name
  | JumpIf (cond, if_true, if_false) ->
    "if " ^ (string_of_value cond)
    ^ " then jump " ^ if_true
    ^ " else jump " ^ if_false
  | PositionAtEnd name -> "position_at_end_of_block " ^ name
and string_of_type = function
  | IntType -> "int"
  | DoubleType -> "double"
  | BoolType -> "bool"
  | VoidType -> "void"
  | OptionalType inner -> (string_of_type inner) ^ "?"
  | UnknownType -> "<unknown>"
  | StructType (fields) -> begin
      let string_of_field name typ out_list =
        out_list @ [name ^ ": " ^ (string_of_type typ)]
      in
      let field_str_list = StringMap.fold string_of_field fields [] in
      let field_str = String.concat ", " field_str_list in
      if StringMap.is_empty fields then
        "{ <empty struct> }"
      else
        "{ " ^ field_str ^ " }"
    end
and string_of_value = function
  | IntLiteral v -> string_of_int v
  | DoubleLiteral v -> string_of_float v
  | BoolLiteral v -> string_of_bool v
  | StructLiteral (_, values) ->
    let fold_value name value out_list =
      let str = name ^ ": " ^ (string_of_value value) in
      out_list @ [str]
    in
    let str_list = StringMap.fold fold_value values [] in
    let value_str = String.concat ", " str_list in
    "{ " ^ value_str ^ " }"
  | VarGet (name, typ) -> "get " ^ name ^ ": " ^ (string_of_type typ)
  | ParamGet (index, _, typ) -> "param " ^ (string_of_int index) ^ ": " ^ (string_of_type typ)
  | VarCreate (name, typ) ->  "create " ^ name ^ ": " ^ (string_of_type typ)
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
  | IntArithmetic (left, op, right)
  | DoubleArithmetic (left, op, right)
  | BoolCompare (left, op, right) ->
    let left_str = string_of_value left in
    let op_str = Ast.string_of_binary_op op in
    let right_str = string_of_value right in
    left_str ^ " " ^ op_str ^ " " ^ right_str
  | Multi items -> String.concat "\n" (List.map string_of_value items)
  | BooleanNegate inner -> "!" ^ (string_of_value inner)
  | BitwiseNegate inner -> "~" ^ (string_of_value inner)
  | Positive (_, inner) -> "+" ^ (string_of_value inner)
  | Negative (_, inner) -> "+" ^ (string_of_value inner)
  | OptionalNone typ -> "none(" ^ (string_of_type typ) ^ ")"
  | OptionalSome (typ, value) ->
    "some(" ^ (string_of_type typ)
    ^ ", " ^ (string_of_value value) ^ ")"
  | OptionalNullCheck inner -> "not_null? " ^ (string_of_value inner)
  | OptionalGet (_, inner) -> "*" ^ (string_of_value inner)
  | GetElement (typ, lhs, index) ->
    "(getelement(" ^ (string_of_type typ) ^ ", " ^ (string_of_int index) ^ ") of "
    ^ (string_of_value lhs) ^ ")"
  | SetElement (typ, lhs, index, rhs) ->
    "(getelement(" ^ (string_of_type typ) ^ ", " ^ (string_of_int index) ^ ") of "
    ^ (string_of_value lhs) ^ " = " ^ (string_of_value rhs) ^ ")"

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