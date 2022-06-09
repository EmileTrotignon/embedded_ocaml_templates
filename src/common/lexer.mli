val token : Sedlexing.lexbuf -> Parser.token

val lexer :
  Sedlexing.lexbuf -> unit -> Parser.token * Lexing.position * Lexing.position
