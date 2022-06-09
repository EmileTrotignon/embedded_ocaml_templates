type error_or_template = Error of Sedlexing.lexbuf | Template of Template.t

val of_lexing_buffer : Sedlexing.lexbuf -> error_or_template
val of_ustring : ?startpos:Lexing.position -> Uchar.t array -> error_or_template
val of_string : ?startpos:Lexing.position -> string -> error_or_template
val of_filename : string -> error_or_template
val handle_syntax_error : Sedlexing.lexbuf -> unit