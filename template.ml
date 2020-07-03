type elt = Text of string | Code of string | Output_code of string

type t = string * elt list
