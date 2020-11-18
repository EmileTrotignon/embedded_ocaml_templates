open Parser

(* let new_line_assert buffer =
  let line = (snd @@ Sedlexing.lexing_positions buffer).pos_lnum in
  Sedlexing.new_line buffer;
  let line' = (snd @@ Sedlexing.lexing_positions buffer).pos_lnum in
  if line <> line' - 1 then
    failwith (Printf.sprintf "line : %d, line' : %d" line line') *)

let format_flag = [%sedlex.regexp? '#' | '0' | '-' | '+']

let simple_format =
  [%sedlex.regexp?
    ( Opt format_flag,
      ( 'd' | 'i' | 'u' | 'n' | 'l' | 'N' | 'L' | 'x' | 'o' | 'X' | 's' | 'c'
      | 'S' | 'C' | 'f' | 'e' | 'E' | 'g' | 'G' | 'h' | 'H' | 'b' | 'B'
      | ('l' | 'n' | 'L'), ('d' | 'i' | 'u' | 'x' | 'X' | 'o')
      | 'a' | 't' ) )]

let get_text buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | "<%", simple_format, "-"
    | "<_%", simple_format, "-"
    | "<%" | "<%#" | "<%-" | "%>" | "<%(" | "%)-" | "<_%" | "<_%-" | "%_>"
    | "<_%(" ->
        Sedlexing.rollback buffer
    | eof -> ()
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer);
        aux ()
    | _ -> assert false
  in
  aux ();
  CCVector.to_array text

let get_whitespaces buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | eof -> ()
    | white_space ->
        CCVector.append_array text (Sedlexing.lexeme buffer);
        aux ()
    | _ -> Sedlexing.rollback buffer
  in
  aux ();
  CCVector.to_array text

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
      | EOF -> "EOF")

let token buffer =
  match%sedlex buffer with
  | "<%", simple_format, "-" ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      LFormatOutput (false, String.sub matched 2 (String.length matched - 3))
  | "<_%", simple_format, "-" ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      LFormatOutput (true, String.sub matched 3 (String.length matched - 4))
  | "<%" -> LPar false
  | "<%#" -> LParArgs
  | "<%-" -> LParOutput false
  | "%>" -> RPar false
  | "<%(" -> LParFormat false
  | "%)-" -> RParFormat
  | "<_%" -> LPar true
  | "<_%-" -> LParOutput true
  | "%_>" -> RPar true
  | "<_%(" -> LParFormat true
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
