module StringMap = Map.Make(String)

exception ScopeError of string

type 'a t =
  | RootScope of 'a StringMap.t
  | ChildScope of ('a t) * 'a StringMap.t

let rec mem name = function
  | RootScope map -> StringMap.mem name map
  | ChildScope (parent, map) ->
    (StringMap.mem name map) || (mem name parent)

let rec find name = function
  | RootScope map -> StringMap.find name map
  | ChildScope (parent, map) ->
    if StringMap.mem name map then
      StringMap.find name map
    else
      find name parent

let rec find_opt name = function
  | RootScope map -> StringMap.find_opt name map
  | ChildScope (parent, map) ->
    if StringMap.mem name map then
      Some (StringMap.find name map)
    else
      find_opt name parent

let replace name value scope =
  let helper map = StringMap.add name value map
  in
  match scope with
  | RootScope map -> RootScope (helper map)
  | ChildScope (parent, map) -> ChildScope (parent, (helper map))

let add name value scope =
  let helper map =
    if StringMap.mem name map then
      raise (ScopeError ("The name \"" ^ name ^ "\" already exists within this context."))
    else
      StringMap.add name value map
  in
  match scope with
  | RootScope map -> RootScope (helper map)
  | ChildScope (parent, map) -> ChildScope (parent, (helper map))

let iter f scope =
  let rec inner f = function
    | RootScope map -> StringMap.iter f map
    | ChildScope (parent, map) ->
      StringMap.iter f map;
      inner f parent
  in
  inner f scope

let empty = RootScope (StringMap.empty)

let does_not_exist name =
  "The name \"" ^ name ^ "\" does not exist within this context."

let of_seq seq =
  RootScope (StringMap.of_seq seq)