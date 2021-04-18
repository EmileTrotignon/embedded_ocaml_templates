open Parser

(* let new_line_assert buffer =
  let line = (snd @@ Sedlexing.lexing_positions buffer).pos_lnum in
  Sedlexing.new_line buffer;
  let line' = (snd @@ Sedlexing.lexing_positions buffer).pos_lnum in
  if line <> line' - 1 then
    failwith (Printf.sprintf "line : %d, line' : %d" line line') *)
let output_marker = [%sedlex.regexp? '-' | '=']
let format_flag = [%sedlex.regexp? '#' | '0' | '-' | '+']

let simple_format =
  [%sedlex.regexp?
    ( Opt format_flag
    , ( 'd' | 'i' | 'u' | 'n' | 'l' | 'N' | 'L' | 'x' | 'o' | 'X' | 's' | 'c'
      | 'S' | 'C' | 'f' | 'e' | 'E' | 'g' | 'G' | 'h' | 'H' | 'b' | 'B'
      | ('l' | 'n' | 'L'), ('d' | 'i' | 'u' | 'x' | 'X' | 'o')
      | 'a' | 't' ) )]

let get_text buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | "<%", simple_format, "-"
     |"<_%", simple_format, "-"
     |"<%" | "<%#" | "<%-" | "%>" | "<%(" | "%)-" | "<_%" | "<_%-" | "%_>"
     |"<_%(" ->
        Sedlexing.rollback buffer
    | eof -> ()
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux ()
    | _ -> assert false in
  aux () ; CCVector.to_array text

let get_whitespaces buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | eof -> ()
    | white_space ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux ()
    | _ -> Sedlexing.rollback buffer in
  aux () ; CCVector.to_array text

(*
let print_token token =
  print_endline
    Printf.(
      match token with
      | LPar b -> sprintf "LPar(%B)" b
      | RPar b -> sprintf "LPar(%B)" b
      | LParArgs -> "LParArgs"
      | LParOutput b -> sprintf "LParOutput(%B)" b
      | LFormatOutput (b, format) -> sprintf "LFormatOutput(%B, %S)" b format
      | LParFormat b -> sprintf "LParFormat(%B)" b
      | RParFormat -> "RParFormat"
      | Text t -> sprintf "Text(%S)" t
      | Whitespaces t -> sprintf "Whitespaces(%S)" t
      | EOF -> "EOF") *)

let slurp buffer =
  match%sedlex buffer with "_" -> true | "" -> false | _ -> assert false

let lpar buffer =
  let slurp = slurp buffer in
  match%sedlex buffer with
  | "%", simple_format, output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let n = String.length matched in
      let format = Some (String.sub matched 1 (String.length matched - 2)) in
      let output_marker = matched.[n - 1] in
      let escape = output_marker = '=' in
      LParOutput {slurp; escape; format}
  | "%" -> LPar slurp
  | "%#" -> LParArgs
  | "%", output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let output_marker = matched.[1] in
      let escape = output_marker = '=' in
      LParOutput {slurp; escape; format= None}
  | "%(" -> LParFormat slurp
  | _ -> Text "<"

let token buffer =
  match%sedlex buffer with
  | "<" -> lpar buffer
  | "%>" -> RPar false
  | "%)", output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let output_marker = matched.[1] in
      let escape = output_marker = '=' in
      RParFormat escape
  | "%_>" -> RPar true
  | eof -> EOF
  | white_space ->
      Whitespaces
        (Ustring.to_string (get_whitespaces buffer (Sedlexing.lexeme buffer)))
  | any -> Text (Ustring.to_string (get_text buffer (Sedlexing.lexeme buffer)))
  | _ -> assert false

(*let token buffer =
  let tok = token buffer in
  print_token tok;
  tok*)

let lexer buffer = Sedlexing.with_tokenizer token buffer
