type t =
  | Textual of {code: string; startpos: Lexing.position; endpos: Lexing.position}
  | Parsed of Parsetree.expression

let build code startpos endpos = Textual {code; startpos; endpos}

let with_dummy_pos code =
  Textual {code; startpos= Lexing.dummy_pos; endpos= Lexing.dummy_pos}

let is_empty = function Textual {code= ""; _} -> true | _ -> false
