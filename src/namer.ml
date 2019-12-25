module StringMap = Sema.StringMap

type t = int StringMap.t

let empty = StringMap.empty

let next_name name namer =
  if not (Scope.mem name namer) then
    (name, Scope.add name 1 namer)
  else
    let current = Scope.find name namer in
    let new_name = name ^ (string_of_int current) in
    let new_count = current + 1 in
    (new_name, Scope.add name new_count namer)