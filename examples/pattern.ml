let describe pair =
  match pair with
  | (0, false) -> "origin"
  | (value, true) -> "enabled"
  | (value, false) -> "disabled"
in
describe (42, true)
