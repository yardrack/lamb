open Syntax

module Names = Set.Make (String)

type metrics = {
  nodes : int;
  depth : int;
  functions : int;
  applications : int;
  matches : int;
  allocations : int;
  recursivebindings : int;
}

type warning = {
  code : string;
  message : string;
  location : span;
}

type report = {
  metrics : metrics;
  warnings : warning list;
  free : string list;
}

let empty =
  { nodes = 0;
    depth = 0;
    functions = 0;
    applications = 0;
    matches = 0;
    allocations = 0;
    recursivebindings = 0 }

let pattern_names pattern =
  bound_names pattern
  |> List.fold_left (fun names name -> Names.add name names) Names.empty

let rec free_variables bound found node =
  match node.expression with
  | Variable name ->
      if Names.mem name bound then found else Names.add name found
  | Function (parameter, body) ->
      let bound = Names.union bound (pattern_names parameter) in
      free_variables bound found body
  | Let (binding, body) ->
      let valuebound =
        if binding.recursive then Names.add binding.name bound else bound
      in
      let found = free_variables valuebound found binding.value in
      free_variables (Names.add binding.name bound) found body
  | Match (target, cases) ->
      let found = free_variables bound found target in
      List.fold_left
        (fun found branch ->
          let casebound =
            Names.union bound (pattern_names branch.casepattern)
          in
          free_variables casebound found branch.casebody)
        found
        cases
  | _ ->
      List.fold_left (free_variables bound) found (children node)

let is_catchall pattern =
  match pattern.pattern with
  | Pwildcard | Pvariable _ -> true
  | _ -> false

let exhaustive cases =
  let patterns = List.map (fun branch -> branch.casepattern) cases in
  let total = List.exists is_catchall patterns in
  let booleans =
    List.fold_left
      (fun values pattern ->
        match pattern.pattern with
        | Pliteral (Boolean value) -> value :: values
        | _ -> values)
      []
      patterns
  in
  let hastrue = List.mem true booleans in
  let hasfalse = List.mem false booleans in
  let hasnil =
    List.exists (fun pattern -> pattern.pattern = Pnil) patterns
  in
  let hascons =
    List.exists
      (fun pattern ->
        match pattern.pattern with Pcons _ -> true | _ -> false)
      patterns
  in
  let constructors =
    List.filter_map
      (fun pattern ->
        match pattern.pattern with
        | Pconstructor (name, _) -> Some name
        | _ -> None)
      patterns
  in
  total
  || (hastrue && hasfalse)
  || (hasnil && hascons)
  || (List.mem "None" constructors && List.mem "Some" constructors)

let redundant cases =
  let rec search = function
    | [] | [_] -> None
    | branch :: remaining ->
        if is_catchall branch.casepattern then Some (List.hd remaining).casespan
        else search remaining
  in
  search cases

let analyze root =
  let warnings = ref [] in
  let rec descend depth metrics node =
    let metrics =
      { metrics with
        nodes = metrics.nodes + 1;
        depth = max metrics.depth depth }
    in
    let metrics =
      match node.expression with
      | Function _ ->
          { metrics with
            functions = metrics.functions + 1;
            allocations = metrics.allocations + 1 }
      | Apply _ ->
          { metrics with applications = metrics.applications + 1 }
      | List _ | Tuple _ | Cons _ | Constructor _ ->
          { metrics with allocations = metrics.allocations + 1 }
      | Let (binding, _) when binding.recursive ->
          { metrics with
            recursivebindings = metrics.recursivebindings + 1 }
      | Match (_, cases) ->
          if not (exhaustive cases) then
            warnings :=
              { code = "W_NONEXHAUSTIVE";
                message = "this pattern matching is not exhaustive";
                location = node.span }
              :: !warnings;
          begin
            match redundant cases with
            | Some location ->
                warnings :=
                  { code = "W_REDUNDANT_CASE";
                    message = "a case after a catch-all pattern is unreachable";
                    location }
                  :: !warnings
            | None -> ()
          end;
          { metrics with matches = metrics.matches + 1 }
      | _ -> metrics
    in
    List.fold_left (descend (depth + 1)) metrics (children node)
  in
  let metrics = descend 0 empty root in
  let free =
    free_variables Names.empty Names.empty root
    |> Names.elements
  in
  { metrics; warnings = List.rev !warnings; free }

let metric_lines metrics =
  [ "nodes", metrics.nodes;
    "depth", metrics.depth;
    "functions", metrics.functions;
    "applications", metrics.applications;
    "matches", metrics.matches;
    "allocations", metrics.allocations;
    "recursive", metrics.recursivebindings ]

let format report =
  let metrics =
    metric_lines report.metrics
    |> List.map (fun (name, value) -> Printf.sprintf "%-12s %d" name value)
  in
  let free =
    "free         "
    ^ if report.free = [] then "none" else String.concat ", " report.free
  in
  let warnings =
    List.map
      (fun warning -> warning.code ^ " " ^ warning.message)
      report.warnings
  in
  String.concat "\n" (metrics @ [free] @ warnings)
