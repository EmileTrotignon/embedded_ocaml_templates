type error = [`Unmatched_tag of Lexing.position]

type 'a or_error = ('a, error) result

val elt' : Sedlexing.lexbuf -> Template.elt' or_error option
(** None means `EOF` was reached *)

val template : Sedlexing.lexbuf -> Template.t or_error

val pp_error : Format.formatter -> error -> unit
