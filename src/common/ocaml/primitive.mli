type t = {code: string; startpos: Lexing.position; endpos: Lexing.position}

val build : string -> Lexing.position -> Lexing.position -> t

val with_dummy_pos : string -> t
