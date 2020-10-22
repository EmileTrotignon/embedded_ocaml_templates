open Parser
let get_text buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | "<%#" -> Sedlexing.rollback buffer
    | "<%-" -> Sedlexing.rollback buffer
    | "<%" -> Sedlexing.rollback buffer
    | "%>" -> Sedlexing.rollback buffer
    | eof -> ()
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer);
        aux ()
    | _ -> assert false
  in
  aux ();
  CCVector.to_array text

let token buffer =
  match%sedlex buffer with
  | "<%" -> LeftPar
  | "<%#" -> LeftParArgs
  | "<%-" -> LeftParOutput
  | "%>" -> RightPar
  | eof -> EOF
  | any -> Text (Ustring.to_string (get_text buffer (Sedlexing.lexeme buffer)))
  | _ -> assert false

let lexer buffer = Sedlexing.with_tokenizer token buffer
