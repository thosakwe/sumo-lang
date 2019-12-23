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
  (* | OptionalType of typ *)
  | FunctionType of (typ list) * typ
and error_level =
  | Error
  | Warning
and error = Ast.span * error_level * string

let empty_universe =
  {
    modules = StringMap.empty
  }

let qualify names =
  let normalize name =
    let rgx = Str.regexp "[^A-Za-z0-9_]+" in
    Str.global_replace rgx "_" name
  in
  String.concat "_" (List.map normalize names)

let qualify_function_name module_path = function
  | "main" -> "main"
  | _ as name -> qualify [module_path; name]

let rec string_of_type = function
  | IntType -> "int"
  | DoubleType -> "double"
  | BoolType -> "bool"
  | VoidType -> "void"
  (* | OptionalType inner -> (string_of_type inner) ^ "?" *)
  | FunctionType (params, returns) ->
    let param_str = String.concat ", " (List.map string_of_type params) in
    "(" ^ param_str ^ ") -> " ^ (string_of_type returns)

let string_of_symbol = function
  | Module name -> "module " ^ name
  | ModuleMember (m, s) -> m ^ "." ^ s
  | ValueSymbol (_, typ) -> string_of_type typ
  | TypeSymbol typ -> "type " ^ string_of_type typ

let string_of_error_level = function
  | Error -> "error"
  | Warning -> "warning"

let string_of_position pos =
  let open Lexing in
  pos.pos_fname
  ^ ":"
  ^ (string_of_int pos.pos_lnum)
  ^ ":"
  ^ (string_of_int pos.pos_cnum)

let string_of_error e =
  let ((start, _), level, msg) = e in
  (string_of_error_level level)
  ^ ": "
  ^ (string_of_position start)
  ^ (string_of_error_level level)
  ^ ": "
  ^ msg

(** Splits a list of errors into those categorized by type.
 * Returns (errors, warnings, hints, info). *)
let organize_errors (lst: error list) =
  let fold_error (e, w, h, i) err =
    let (_, level, _) = err in
    match level with
    | Error -> (e @ [err], w, h, i)
    | Warning -> (e, w @ [err], h, i)
  in
  List.fold_left fold_error ([], [], [], []) lst