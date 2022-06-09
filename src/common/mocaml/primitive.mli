type t =
  | Textual of {code: string; startpos: Lexing.position; endpos: Lexing.position}
  | Parsed of Parsetree.expression

val build : string -> Lexing.position -> Lexing.position -> t

val with_dummy_pos : string -> t

val is_empty : t -> bool
