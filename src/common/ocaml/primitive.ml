type t = {code: string; startpos: Lexing.position; endpos: Lexing.position}

let build code startpos endpos = {code; startpos; endpos}

let with_dummy_pos code =
  {code; startpos= Lexing.dummy_pos; endpos= Lexing.dummy_pos}
