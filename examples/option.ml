let map functionvalue optional =
  match optional with
  | None -> None
  | Some value -> Some (functionvalue value)
in
map (fun value -> value * 2) (Some 21)
