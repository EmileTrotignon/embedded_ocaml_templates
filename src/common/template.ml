type elt =
  | Text of string
  | Code of string
  | Output of {code: string; escape: bool; format: string option}

type t = string * elt list
type tag_options = {slurp_before: bool; slurp_after: bool}

type tag =
  | Code of string
  | Output of {code: string; escape: bool; format: string option}

type elt' = Text of string | Whitespace of string | Tag of tag_options * tag
type t' = string * elt' list

let elt_of_tag (tag : tag) : elt =
  match tag with
  | Code s -> Code s
  | Output {code; escape; format} -> Output {code; escape; format}

let t_of_t' (args, elts) =
  ( args
  , let remove_whitespaces elts =
      let rec aux elts slurp_next (acc : elt' list) =
        match (elts, acc) with
        | [], _ -> acc
        | (Text _ as x) :: xs, _ -> aux xs false (x :: acc)
        | (Whitespace _ as x) :: xs, _ ->
            aux xs false (if slurp_next then acc else x :: acc)
        | ( (Tag ({slurp_before= true; slurp_after}, _) as x) :: xs
          , Whitespace _ :: acc_s ) ->
            aux xs slurp_after (x :: acc_s)
        | (Tag ({slurp_before= _; slurp_after}, _) as x) :: xs, _ ->
            aux xs slurp_after (x :: acc) in
      List.rev (aux elts false []) in
    let elts' = remove_whitespaces elts in
    List.map
      (function
        | Tag (_, tag) -> elt_of_tag tag
        | Text s -> Text s
        | Whitespace s -> Text s)
      elts' )

let text s : elt = Text s
let text' s : elt' = Text s
let code s : elt = Code s
let code' s : tag = Code s
let output ?(escape = true) ?format code : elt = Output {code; escape; format}
let output' ?(escape = true) ?format code : tag = Output {code; escape; format}
let tag slurp_before tag slurp_after = Tag ({slurp_before; slurp_after}, tag)

let%test "no slurp" =
  let ast' =
    ( ""
    , [ Text "atom 1\n"; tag false (output' {|"string 1\n"|}) false
      ; Whitespace "   \n"; Text "atom 2\n"
      ; tag false (output' ~format:"d" {|35|}) false ] ) in
  let ast =
    ( ""
    , [ text "atom 1\n"; output {|"string 1\n"|}; text "   \n"; text "atom 2\n"
      ; output ~format:"d" {|35|} ] ) in
  let ast'' = t_of_t' ast' in
  ast'' = ast

let%test "slurp" =
  let ast' =
    ( ""
    , [ text' "atom1"; tag false (output' {|"string 1\n"|}) true
      ; Whitespace "   \n"; Text "atom2"
      ; tag false (output' ~format:"d" {|35|}) false ] ) in
  let ast =
    ( ""
    , ( [ Text "atom1"; output {|"string 1\n"|}; Text "atom2"
        ; output ~format:"d" {|35|} ]
        : elt list ) ) in
  let ast'' = t_of_t' ast' in
  ast'' = ast
