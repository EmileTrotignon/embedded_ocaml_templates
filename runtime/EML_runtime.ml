
let escape s =
  let buffer = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&#x27;"
      | c -> Buffer.add_char buffer c)
    s ;
  Buffer.contents buffer