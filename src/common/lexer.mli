type error = [`Unmatched_tag of Lexing.position]

type 'a or_error = ('a, error) result


(** None means `EOF` was reached *)
val elt' : Sedlexing.lexbuf -> Template.elt' or_error option

val template : Sedlexing.lexbuf -> Template.t or_error

val pp_error : Format.formatter -> error -> unit