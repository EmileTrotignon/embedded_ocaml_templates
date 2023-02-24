open Ast

let force_mutual_recursion struct_ =
  let aux (functions, others) = function
    | SIDef (pattern, expr) ->
        ((pattern, expr) :: functions, others)
    | SIRecDefs functions' ->
        (functions' @ functions, others)
    | other ->
        (functions, other :: others)
  in
  let functions, others = List.fold_left aux ([], []) struct_ in
  others @ [SIRecDefs functions]
