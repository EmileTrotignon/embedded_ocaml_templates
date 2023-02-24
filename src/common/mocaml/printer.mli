val expr_to_string : Ast.expr -> string

val program_to_string : Ast.struct_item list -> string

val print_program : out_channel -> Ast.struct_item list -> unit
