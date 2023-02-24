open Ast

val ( ^-> ) : 'a -> 'b -> 'a * 'b

val ( ^^-> ) : 'a -> 'b -> 'a * 'b

val ( ^= ) : 'a -> 'b -> 'a * 'b

module T : sig
  val name : string -> type_

  val primitive : primitive -> type_

  val apply : type_ list -> string -> type_

  val module_field : string list -> type_

  val arrow : type_ list -> type_ -> type_

  val ( ^-> ) : type_ -> type_ -> type_

  val int : type_

  val bool : type_

  val string : type_

  val char : type_

  val unit : type_

  val int64 : type_

  val nativeint : type_

  val float : type_
end

module E : sig
  val if_ : expr -> expr -> expr -> expr

  val let_ : (pattern * expr) list -> expr -> expr

  val leti : (pattern * expr) list * expr -> expr

  val apply : expr -> ?named_args:(string * expr) list -> expr list -> expr

  val cons : ?payload:expr list -> string -> expr

  val var : string -> expr

  val tuple : expr list -> expr

  val lit_list : expr list -> expr

  val lit_string : string -> expr

  val lit_int : int -> expr

  val fun_ : pattern list * expr -> expr

  val prim : primitive -> expr

  val ref : expr -> expr

  val deref : expr -> expr

  val assign_to_ref : expr -> expr -> expr

  val sequence : expr list -> expr -> expr

  val open_module : string -> expr -> expr

  val module_field : string list -> expr

  val mixed_seq : mixed list -> expr -> expr

  val match_ : expr -> branch list -> expr

  val unit : expr

  val annot : expr -> type_ -> expr

  val ( ^: ) : expr -> type_ -> expr

  val li_cons : expr -> expr -> expr

  val empty_list : expr

  val empty_string : expr

  val function_ : branch list -> expr
end

module P : sig
  val wildcard : pattern

  val char : char -> pattern

  val string : string -> pattern

  val int : int -> pattern

  val tuple : pattern list -> pattern

  val var : string -> pattern

  val prim : primitive -> pattern

  val cons : ?payload:pattern list -> string -> pattern

  val unit : pattern
end

module Mixed : sig
  val unit : expr -> mixed

  val prim : primitive -> mixed
end

module SI : sig
  val def : pattern * expr -> struct_item

  val module_ : string * module_ -> struct_item
end

module M : sig
  val struct_ : struct_ -> module_

  val alias : string -> module_

  val field : string list -> module_
end

module Prim : sig
  val textual : string -> Lexing.position -> Lexing.position -> primitive

  val parsed : Parsetree.expression -> primitive
end
