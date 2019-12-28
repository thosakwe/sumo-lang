type t =
  | Public
  | Protected
  | Private

let string_of_visibility = function
  | Public -> "public"
  | Protected -> "protected"
  | Private -> "private"