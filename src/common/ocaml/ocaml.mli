type type_
type expr
type pattern
type struct_item
type struct_ = struct_item list
type module_
type mixed
type branch = pattern * expr
type primitive = Primitive.t

(* -------------------------------------------------------------------------- *)

module Builder : sig
  (* ------------------------------------------------------------------------ *)
  val ( ^-> ) : 'a -> 'b -> 'a * 'b
  val ( ^= ) : 'a -> 'b -> 'a * 'b

  (* ------------------------------------------------------------------------ *)

  val t_name : string -> type_
  val t_primitive : Primitive.t -> type_
  val t_app : type_ list -> string -> type_
  val t_arrow : type_ list * type_ -> type_
  val t_module_field : string list -> type_

  (* ------------------------------------------------------------------------ *)
  val e_let : (pattern * expr) list -> expr -> expr
  val e_leti : (pattern * expr) list * expr -> expr
  val e_app : expr -> expr list -> expr
  val e_cons : string -> expr list -> expr
  val e_var : string -> expr
  val e_tuple : expr list -> expr
  val e_lit_list : expr list -> expr
  val e_lit_string : string -> expr
  val e_lit_int : int -> expr
  val e_fun : pattern list * expr -> expr
  val e_prim : primitive -> expr
  val e_ref : expr -> expr
  val e_deref : expr -> expr
  val e_assign_to_ref : expr -> expr -> expr
  val e_sequence : expr list -> expr -> expr
  val e_open_module : string -> expr -> expr
  val e_module_field : string list -> expr
  val e_mixed_seq : mixed list -> expr -> expr
  val e_match : expr -> branch list -> expr
  val e_unit : expr
  val e_li_cons : expr -> expr -> expr
  val e_empty_list : expr
  val e_empty_string : expr
  val e_function : branch list -> expr
  val e_annot : expr -> type_ -> expr
  val ( ^: ) : expr -> type_ -> expr

  (* ------------------------------------------------------------------------ *)
  val p_wildcard : pattern
  val p_char : char -> pattern
  val p_string : string -> pattern
  val p_int : int -> pattern
  val p_tuple : pattern list -> pattern
  val p_var : string -> pattern
  val p_prim : primitive -> pattern

  (* ------------------------------------------------------------------------ *)
  val mix_unit : expr -> mixed
  val mix_prim : primitive -> mixed

  (* ------------------------------------------------------------------------ *)
  val si_def : pattern * expr -> struct_item
  val si_module : string * module_ -> struct_item

  (* ------------------------------------------------------------------------ *)
  val m_struct : struct_ -> module_
  val m_alias : string -> module_
  val m_field : string list -> module_
end

(* -------------------------------------------------------------------------- *)

module Printer : sig
  val expr_to_string : expr -> string
  val program_to_string : struct_item list -> string
  val print_program : out_channel -> struct_item list -> unit
end

(* -------------------------------------------------------------------------- *)

module Transform : sig
  val force_mutual_recursion : struct_ -> struct_
end

module Primitive : module type of Primitive
