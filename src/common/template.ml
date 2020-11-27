type elt =
  | Text of string
  | Code of string
  | Output_code of string
  | Output_format of string * string
[@@deriving show]

type t = string * elt list [@@deriving show]

type tag_options = { slurp_before : bool; slurp_after : bool } [@@deriving show]

type tag =
  | Code of string
  | Output_code of string
  | Output_format of string * string
[@@deriving show]

type elt' = Text of string | Whitespace of string | Tag of tag_options * tag
[@@deriving show]

type t' = string * elt' list [@@deriving show]

let elt_of_tag (tag : tag) : elt =
  match tag with
  | Code s -> Code s
  | Output_code s -> Output_code s
  | Output_format (s1, s2) -> Output_format (s1, s2)

let t_of_t' (args, elts) =
  ( args,
    let remove_whitespaces elts =
      let rec aux elts slurp_next (acc : elt' list) =
        match (elts, acc) with
        | [], _ -> acc
        | (Text _ as x) :: xs, _ -> aux xs false (x :: acc)
        | (Whitespace _ as x) :: xs, _ ->
            aux xs false (if slurp_next then acc else x :: acc)
        | ( (Tag ({ slurp_before = true; slurp_after }, _) as x) :: xs,
            Whitespace _ :: acc_s ) ->
            aux xs slurp_after (x :: acc_s)
        | (Tag ({ slurp_before = _; slurp_after }, _) as x) :: xs, _ ->
            aux xs slurp_after (x :: acc)
      in
      List.rev (aux elts false [])
    in
    let elts' = remove_whitespaces elts in
    List.map
      (function
        | Tag (_, tag) -> elt_of_tag tag
        | Text s -> Text s
        | Whitespace s -> Text s)
      elts' )

let%test "no slurp" =
  let ast' =
    ( "",
      [
        Text "atom 1\n";
        Tag
          ( { slurp_before = false; slurp_after = false },
            Output_code {|"string 1\n"|} );
        Whitespace "   \n";
        Text "atom 2\n";
        Tag
          ( { slurp_before = false; slurp_after = false },
            Output_format ("d", {|35|}) );
      ] )
  in
  let ast =
    ( "",
      ( [
          Text "atom 1\n";
          Output_code {|"string 1\n"|};
          Text "   \n";
          Text "atom 2\n";
          Output_format ("d", {|35|});
        ]
        : elt list ) )
  in
  let ast'' = t_of_t' ast' in
  ast'' = ast

let%test "slurp" =
  let ast' =
    ( "",
      [
        Text "atom1";
        Tag
          ( { slurp_before = false; slurp_after = true },
            Output_code {|"string 1\n"|} );
        Whitespace "   \n";
        Text "atom2";
        Tag
          ( { slurp_before = false; slurp_after = false },
            Output_format ("d", {|35|}) );
      ] )
  in
  let ast =
    ( "",
      ( [
          Text "atom1";
          Output_code {|"string 1\n"|};
          Text "atom2";
          Output_format ("d", {|35|});
        ]
        : elt list ) )
  in
  let ast'' = t_of_t' ast' in
  ast'' = ast
