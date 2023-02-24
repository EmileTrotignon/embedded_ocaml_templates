type error_or_template = Template.t Lexer.or_error

val of_lexing_buffer : Sedlexing.lexbuf -> error_or_template

val of_ustring : ?startpos:Lexing.position -> Uchar.t array -> error_or_template

val of_string : ?startpos:Lexing.position -> string -> error_or_template

val of_filename : string -> error_or_template
