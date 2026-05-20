open Common.Template

let output ?escape ?format s = output ?escape ?format (Prim.with_dummy_pos s)

let output' ?escape ?format s = output' ?escape ?format (Prim.with_dummy_pos s)

let%test "no slurp" =
  let ast' =
    ( ""
    , [ Text "atom 1\n"
      ; tag false (output' {|"string 1\n"|}) false
      ; Whitespace "   \n"
      ; Text "atom 2\n"
      ; tag false (output' ~format:"d" {|35|}) false ] )
  in
  let ast =
    ( ""
    , [ text "atom 1\n"
      ; output {|"string 1\n"|}
      ; text "   \n"
      ; text "atom 2\n"
      ; output ~format:"d" {|35|} ] )
  in
  let ast'' = t_of_t' ast' in
  ast'' = ast

let%test "slurp" =
  let ast' =
    ( ""
    , [ text' "atom1"
      ; tag false (output' {|"string 1\n"|}) true
      ; Whitespace "   \n"
      ; Text "atom2"
      ; tag false (output' ~format:"d" {|35|}) false ] )
  in
  let ast =
    ( ""
    , ( [ Text "atom1"
        ; output {|"string 1\n"|}
        ; Text "atom2"
        ; output ~format:"d" {|35|} ]
        : elt list ) )
  in
  let ast'' = t_of_t' ast' in
  ast'' = ast
