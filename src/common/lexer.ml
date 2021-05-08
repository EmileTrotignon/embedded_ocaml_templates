open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let output_marker = [%sedlex.regexp? '-' | '=']
let format_flag = [%sedlex.regexp? '#' | '0' | '-' | '+']
let format_width = [%sedlex.regexp? Plus digit]
let format_precision = [%sedlex.regexp? '.', Plus digit]

let simple_format =
  [%sedlex.regexp?
    ( 'd' | 'i' | 'u' | 'n' | 'l' | 'N' | 'L' | 'x' | 'o' | 'X' | 's' | 'c'
    | 'S' | 'C' | 'f' | 'e' | 'E' | 'g' | 'G' | 'h' | 'H' | 'b' | 'B'
    | ('l' | 'n' | 'L'), ('d' | 'i' | 'u' | 'x' | 'X' | 'o')
    | 't' )]

let format =
  [%sedlex.regexp?
    Opt format_flag, Opt format_width, Opt format_precision, simple_format]

let tagpar =
  [%sedlex.regexp?
    ( '<', Opt '_', '%', (Opt simple_format, Opt output_marker | "%[")
    | "%]", output_marker
    | '%', Opt '_', '>' )]

let text buffer first =
  let text = CCVector.of_array first in
  let rec aux () =
    match%sedlex buffer with
    | tagpar | white_space -> Sedlexing.rollback buffer
    | eof -> ()
    | any ->
        CCVector.append_array text (Sedlexing.lexeme buffer) ;
        aux ()
    | _ -> assert false in
  aux () ;
  let text = CCVector.to_array text in
  Text (Ustring.to_string text)

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

let slurp buffer =
  match%sedlex buffer with "_" -> true | "" -> false | _ -> assert false

let lpar buffer =
  let slurp = slurp buffer in
  match%sedlex buffer with
  | "%", simple_format, output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let n = String.length matched in
      let format = Some (String.sub matched 1 (n - 2)) in
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
  | "%[", format, "%]", output_marker ->
      let matched = Ustring.to_string (Sedlexing.lexeme buffer) in
      let n = String.length matched in
      let format = Some (String.sub matched 2 (String.length matched - 5)) in
      let output_marker = matched.[n - 1] in
      let escape = output_marker = '=' in
      LParOutput {slurp; escape; format}
  | _ -> Text "<"

let token buffer =
  match%sedlex buffer with
  | "<" -> lpar buffer
  | "%>" -> RPar false
  | "%_>" -> RPar true
  | eof -> EOF
  | white_space ->
      Whitespaces
        (Ustring.to_string (get_whitespaces buffer (Sedlexing.lexeme buffer)))
  | any -> text buffer (Sedlexing.lexeme buffer)
  | _ -> assert false

let lexer buffer = Sedlexing.with_tokenizer token buffer
