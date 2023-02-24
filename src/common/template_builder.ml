type error_or_template = Template.t Lexer.or_error

let of_lexing_buffer lexbuf = Lexer.template lexbuf

let of_ustring ?(startpos = Lexing.dummy_pos) ustring =
  Lexer.template
    (let buffer = Sedlexing.from_uchar_array ustring in
     Sedlexing.set_position buffer startpos ;
     buffer )

let of_string ?(startpos = Lexing.dummy_pos) string =
  of_ustring ~startpos (Ustring.of_string string)

let of_filename filename =
  let gen =
    Gen.of_array (Ustring.of_string @@ CCIO.with_in filename CCIO.read_all)
  in
  let buffer = Sedlexing.from_gen gen in
  Sedlexing.set_filename buffer filename ;
  Lexer.template buffer
