module StringMap = Map.Make(String)

exception ScopeError of string

(* type 'a t =
   {
    parent: ('a t) option;
    locals: 'a StringMap.t
   } *)

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

let add name value scope =
  let helper map =
    if StringMap.mem name map then
      raise (ScopeError "?")
    else
      StringMap.add name value map
  in
  match scope with
  | RootScope map -> RootScope (helper map)
  | ChildScope (parent, map) -> ChildScope (parent, (helper map))