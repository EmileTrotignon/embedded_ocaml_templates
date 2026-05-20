module Prim = Mocaml.Primitive

type elt =
  | Text of string
  | Code of Prim.t
  | Output of {code: Prim.t; escape: bool; format: string option}

type t = Prim.t option * elt list

type tag_options = {slurp_before: bool; slurp_after: bool}

type tag =
  | Code of Prim.t
  | Output of {code: Prim.t; escape: bool; format: string option}

type elt' = Text of string | Whitespace of string | Tag of tag_options * tag

type t' = Prim.t option * elt' list

let elt_of_tag (tag : tag) : elt =
  match tag with
  | Code s ->
      Code s
  | Output {code; escape; format} ->
      Output {code; escape; format}

let t_of_t' (args, elts) =
  ( args
  , let remove_whitespaces elts =
      let rec aux elts slurp_next (acc : elt' list) =
        match (elts, acc) with
        | [], _ ->
            acc
        | (Text _ as x) :: xs, _ ->
            aux xs false (x :: acc)
        | (Whitespace _ as x) :: xs, _ ->
            aux xs false (if slurp_next then acc else x :: acc)
        | ( (Tag ({slurp_before= true; slurp_after}, _) as x) :: xs
          , Whitespace _ :: acc_s ) ->
            aux xs slurp_after (x :: acc_s)
        | (Tag ({slurp_before= _; slurp_after}, _) as x) :: xs, _ ->
            aux xs slurp_after (x :: acc)
      in
      List.rev (aux elts false [])
    in
    let elts' = remove_whitespaces elts in
    List.map
      (function
        | Tag (_, tag) ->
            elt_of_tag tag
        | Text s ->
            Text s
        | Whitespace s ->
            Text s )
      elts' )

let text s : elt = Text s

let text' s : elt' = Text s

let code s : elt = Code s

let code' s : tag = Code s

let tag slurp_before tag slurp_after = Tag ({slurp_before; slurp_after}, tag)

let output ?(escape = true) ?format code : elt = Output {code; escape; format}

let output' ?(escape = true) ?format code : tag = Output {code; escape; format}
