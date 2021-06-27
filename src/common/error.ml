open Printf

let fail message = eprintf "%s\n" message ; flush stderr; exit 1
let fail format = ksprintf fail format
