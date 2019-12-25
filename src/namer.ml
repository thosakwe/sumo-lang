module StringMap = Sema.StringMap

type t = int StringMap.t

let empty = StringMap.empty

let next_name name namer =
  if not (StringMap.mem name namer) then
    (name, StringMap.add name 1 namer)
  else
    let current = StringMap.find name namer in
    let new_name = name ^ (string_of_int current) in
    let new_count = current + 1 in
    (new_name, StringMap.add name new_count namer)

let current name namer =
  if not (StringMap.mem name namer) then
    name
  else
    let current = StringMap.find name namer in
    let new_name = name ^ (string_of_int current) in
    new_name