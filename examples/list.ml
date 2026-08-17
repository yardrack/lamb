let rec length values =
  match values with
  | [] -> 0
  | value :: rest -> 1 + length rest
in
length [3; 5; 8; 13; 21]
