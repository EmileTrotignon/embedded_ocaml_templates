open Result.O
open Template

type error = [`Unmatched_tag of Lexing.position]

type 'a or_error = ('a, error) result

let digit = [%sedlex.regexp? '0' .. '9']

let output_marker = [%sedlex.regexp? '-' | '=']

let format_flag = [%sedlex.regexp? '#' | '0' | '-' | '+']

let format_width = [%sedlex.regexp? Plus digit]

let format_precision = [%sedlex.regexp? '.', Plus digit]

let escape marker = marker = '='

let simple_format =
  [%sedlex.regexp?
    ( 'd'
    | 'i'
    | 'u'
    | 'n'
    | 'l'
    | 'N'
    | 'L'
    | 'x'
    | 'o'
    | 'X'
    | 's'
    | 'c'
    | 'S'
    | 'C'
    | 'f'
    | 'e'
    | 'E'
    | 'g'
    | 'G'
    | 'h'
    | 'H'
    | 'b'
    | 'B'
    | ('l' | 'n' | 'L'), ('d' | 'i' | 'u' | 'x' | 'X' | 'o')
    | 't' )]

let format =
  [%sedlex.regexp?
    Opt format_flag, Opt format_width, Opt format_precision, simple_format]

let tag_left_par =
  [%sedlex.regexp? '<', Opt '_', '%', (Opt simple_format, Opt output_marker)]

let tag_right_par_slurp = [%sedlex.regexp? '%', '_', '>']

let tag_right_par_no_slurp = [%sedlex.regexp? '%', '>']

let tag_right_par = [%sedlex.regexp? '%', Opt '_', '>']

let tagpar = [%sedlex.regexp? tag_left_par | tag_right_par]

let text buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | tagpar | white_space ->
        Sedlexing.rollback buffer
    | eof ->
        ()
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux ()
    | _ ->
        assert false
  in
  aux () ;
  let text = text |> CCVector.to_array |> Ustring.to_string in
  Template.Text text

let code buffer : _ or_error =
  let startpos = fst @@ Sedlexing.lexing_positions buffer in
  let text = CCVector.create () in
  let rec aux depth =
    let endpos = snd @@ Sedlexing.lexing_positions buffer in
    match%sedlex buffer with
    | tag_left_par ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux (depth + 1)
    | tag_right_par_slurp ->
        let depth = depth - 1 in
        if depth = 0 then Ok (true, endpos)
        else (
          CCVector.append_array text (Sedlexing.lexeme buffer) ;
          aux depth )
    | tag_right_par_no_slurp ->
        let depth = depth - 1 in
        if depth = 0 then Ok (false, endpos)
        else (
          CCVector.append_array text (Sedlexing.lexeme buffer) ;
          aux depth )
    | white_space ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux depth
    | eof ->
        Error (`Unmatched_tag startpos)
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux depth
    | _ ->
        assert false
  in
  let+ slurp, endpos = aux 1 in
  let text = text |> CCVector.to_array |> Ustring.to_string in
  let prim = Mocaml.Primitive.build text startpos endpos in
  (slurp, prim)

let get_whitespaces buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | eof ->
        ()
    | white_space ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux ()
    | _ ->
        Sedlexing.rollback buffer
  in
  aux () ; CCVector.to_array text

let slurp buffer =
  match%sedlex buffer with "_" -> true | "" -> false | _ -> assert false

(** [Ok (Some t)] means there was a tag, [Error e] means an error, and [Ok None]
    means no error, but no tag either. *)
let tag buffer =
  let slurp_before = slurp buffer in
  match%sedlex buffer with
  | "%", simple_format, output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let n = String.length matched in
      let format = Some (String.sub matched 1 (n - 2)) in
      let output_marker = matched.[n - 1] in
      let escape = output_marker = '=' in
      let+ slurp_after, code = code buffer in
      Some (Tag ({slurp_after; slurp_before}, Output {code; escape; format}))
  | "%" ->
      let+ slurp_after, code = code buffer in
      Some (Tag ({slurp_after; slurp_before}, Code code))
  | "%", output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let output_marker = matched.[1] in
      let escape = output_marker = '=' in
      let+ slurp_after, code = code buffer in
      Some
        (Tag ({slurp_after; slurp_before}, Output {code; escape; format= None}))
  | "%[", format, "%]", output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let n = String.length matched in
      let format = Some (String.sub matched 2 (String.length matched - 5)) in
      let output_marker = matched.[n - 1] in
      let escape = escape output_marker in
      let+ slurp_after, code = code buffer in
      Some (Tag ({slurp_after; slurp_before}, Output {code; escape; format}))
  | _ ->
      Ok None

let elt' buffer =
  match%sedlex buffer with
  | "<" ->
      Some
        (let+ tag = tag buffer in
         match tag with Some tag -> tag | None -> Text "<" )
  | eof ->
      None
  | white_space ->
      Some
        (Ok
           (Whitespace
              (Ustring.to_string
                 (get_whitespaces buffer (Sedlexing.lexeme buffer)) ) ) )
  | any ->
      Some (Ok (text buffer (Sedlexing.lexeme buffer)))
  | _ ->
      assert false

let rec elt'_list acc buffer : _ or_error =
  match elt' buffer with
  | None ->
      Ok (List.rev acc)
  | Some (Ok elt') ->
      elt'_list (elt' :: acc) buffer
  | Some (Error e) ->
      Error e

let elt'_list = elt'_list []

let params buffer =
  match%sedlex buffer with
  | "<%#" ->
      let+ _, code = code buffer in
      Some code
  | _ ->
      Ok None

let template' buffer =
  let* params = params buffer in
  let+ elt's = elt'_list buffer in
  (params, elt's)

let template buffer =
  let+ template' = template' buffer in
  t_of_t' template'

let pp_pos fmt pos =
  Ppxlib.(
    Format.fprintf fmt "at line %d:%d in file %s" pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      pos.pos_fname )

let pp_error fmt (`Unmatched_tag loc) =
  let message = "Unmatched tag" in
  Format.fprintf fmt "%s at %a\n%!" message pp_pos loc
