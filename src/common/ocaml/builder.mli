val ( ^-> ) : 'a -> 'b -> 'a * 'b
val ( ^= ) : 'a -> 'b -> 'a * 'b
val t_name : string -> Ast.type_
val t_primitive : Primitive.t -> Ast.type_
val t_app : Ast.type_ list -> string -> Ast.type_
val t_module_field : string list -> Ast.type_
val t_arrow : Ast.type_ list * Ast.type_ -> Ast.type_
val e_let : (Ast.pattern * Ast.expr) list -> Ast.expr -> Ast.expr
val e_leti : (Ast.pattern * Ast.expr) list * Ast.expr -> Ast.expr
val e_app : Ast.expr -> Ast.expr list -> Ast.expr
val e_cons : string -> Ast.expr list -> Ast.expr
val e_var : string -> Ast.expr
val e_tuple : Ast.expr list -> Ast.expr
val e_lit_list : Ast.expr list -> Ast.expr
val e_lit_string : string -> Ast.expr
val e_lit_int : int -> Ast.expr
val e_fun : Ast.pattern list * Ast.expr -> Ast.expr
val e_prim : Primitive.t -> Ast.expr
val e_ref : Ast.expr -> Ast.expr
val e_deref : Ast.expr -> Ast.expr
val e_assign_to_ref : Ast.expr -> Ast.expr -> Ast.expr
val e_sequence : Ast.expr list -> Ast.expr -> Ast.expr
val e_open_module : string -> Ast.expr -> Ast.expr
val e_module_field : string list -> Ast.expr
val e_mixed_seq : Ast.mixed list -> Ast.expr -> Ast.expr
val e_match : Ast.expr -> Ast.branch list -> Ast.expr
val e_unit : Ast.expr
val e_annot : Ast.expr -> Ast.type_ -> Ast.expr
val ( ^: ) : Ast.expr -> Ast.type_ -> Ast.expr
val e_li_cons : Ast.expr -> Ast.expr -> Ast.expr
val e_empty_list : Ast.expr
val e_empty_string : Ast.expr
val e_function : Ast.branch list -> Ast.expr
val p_wildcard : Ast.pattern
val p_char : char -> Ast.pattern
val p_string : string -> Ast.pattern
val p_int : int -> Ast.pattern
val p_tuple : Ast.pattern list -> Ast.pattern
val p_var : string -> Ast.pattern
val p_prim : Primitive.t -> Ast.pattern
val mix_unit : Ast.expr -> Ast.mixed
val mix_prim : Primitive.t -> Ast.mixed
val si_def : Ast.pattern * Ast.expr -> Ast.struct_item
val si_module : string * Ast.module_ -> Ast.struct_item
val m_struct : Ast.struct_ -> Ast.module_
val m_alias : string -> Ast.module_
val m_field : string list -> Ast.module_
val primitive : string -> Lexing.position -> Lexing.position -> Primitive.t
