(** Mocaml provides a way to construct Ocaml ASTs with regular AST mixed
    with textual fragments that may not be syntactically correct on their own.
    Every Ocaml construct is not provided : they are added on a per-need basis.
  *)

(** The representation of a type. *)
type type_

(** The representation of an expression. *)
type expr

(** The representation of a pattern. *)
type pattern

(** The representation of a structure item. *)
type struct_item

(** The representation of a structure. *)
type struct_ = struct_item list

(** The representation of a module. *)
type module_

(** Either textual code, or AST representation. *)
type mixed

(** The representation of a match branch. *)
type branch

type primitive = Primitive.t

(* -------------------------------------------------------------------------- *)

module Builder : sig
  val ( ^-> ) : pattern -> expr -> branch
  (** [p ^-> e] is the branch [p -> e]. *)

  val ( ^^-> ) : pattern list -> expr -> pattern list * expr

  val ( ^= ) : 'a -> 'b -> 'a * 'b

  (** This module provides values to build types. *)
  module T : sig
    val name : string -> type_
    (** [T.name "ident"] is the type of name [ident]. *)

    val primitive : primitive -> type_
    (** [T.primitive prim] is the type [prim]. *)

    val apply : type_ list -> string -> type_
    (** [T.apply ([a1; a2; ... ; an], "name")] is the type
    [(a1, a2, ... , an) name]. *)

    val module_field : string list -> type_
    (** [T.module_field (["Module1"; "Module2"; ... ; "Modulen"; "ident"])] is
    the type [Module1.Module2...Modulen.ident]. *)

    val arrow : type_ list -> type_ -> type_
    (** [T.arrow ([t1; t2; ... ; tn], t)] is the type
    [t1 -> t2 -> ... -> tn -> t]. *)

    val ( ^-> ) : type_ -> type_ -> type_
    (** [t1 ^-> t2] is the type [t1 -> t2]. *)

    val int : type_
    (** [T.int] is the type [int]. *)

    val bool : type_
    (** [T.bool] is the type [bool]. *)

    val string : type_
    (** [T.string] is the type [string]. *)

    val char : type_
    (** [T.char] is the type [char]. *)

    val unit : type_
    (** [T.unit] is the type [unit]. *)

    val int64 : type_
    (** [T.int64] is the type [int64]. *)

    val nativeint : type_
    (** [T.nativeint] is the type [nativeint]. *)

    val float : type_
    (** [T.float] is the type [float]. *)
  end

  (** This module provides values to build expressions *)
  module E : sig
    val if_ : expr -> expr -> expr -> expr
    (** [E.if_ cond e1 e2] is the expression [if cond then e1 else e2]. *)

    val let_ : (pattern * expr) list -> expr -> expr
    (** [E.let [(p1 ^= e1); ... ; (pn ^= en)] e] is the expression
    [let p1 = e1 in ... let pn = en in e]. *)

    val apply : expr -> ?named_args:(string * expr) list -> expr list -> expr
    (** [E.apply func ~named_args:[("lab1", el1); ...; ("labn", eln)] [e1; ... ; en]]
    is the expression [func ~lab1:el1 ... ~labn:eln e1 ... en]. *)

    val cons : ?payload:expr list -> string -> expr
    (** [E.cons ~payload:[e1; e2; ...; en] "Cons"] is the expression
    [Cons (e1, e2, ... , en)]. *)

    val var : string -> expr
    (** [E.var "ident"] is the expression [ident]. *)

    val tuple : expr list -> expr
    (** [E.tuple [e1; e2; ... ; en]] is the expression [(e1, e2, ... , en)]. *)

    val lit_list : expr list -> expr
    (** [E.lit_list [e1; e2; ... ; en]] is the expression [[e1; e2; ... ; en]]. *)

    val lit_string : string -> expr
    (** [E.lit_string "some text"] is the expression ["some text"]. *)

    val lit_int : int -> expr
    (** [E.lit_int i] is the expression [i]. *)

    val fun_ : pattern list * expr -> expr
    (** [E.fun_ [p1; ...; pn] ^^-> body] is the expression
    [fun p1 ... pn -> body]. *)

    val prim : primitive -> expr
    (** [E.prim prim] is the expression [prim]. *)

    val ref : expr -> expr
    (** [E.ref e] is the expression [ref e]. *)

    val deref : expr -> expr
    (** [E.deref e] is the expression [!e]. *)

    val assign_to_ref : expr -> expr -> expr
    (** [E.assign_to_ref r v] is the expression [r := v]. *)

    val sequence : expr list -> expr -> expr
    (** [E.sequence [e1; e2; ... ; en] e] is the expression
    [e1; e2; ... ; en ; e]. *)

    val open_module : string -> expr -> expr
    (** [E.open_module "Module" e] is the expression [Module.( e )]. *)

    val module_field : string list -> expr
    (** [E.module_field (["Module1"; ... ; "Modulen" ; "ident"])] is the
    expression [Module1...Modulen.ident]. *)

    val mixed_seq : mixed list -> expr -> expr
    (** [E.mixed_seq ([m1; m2; ... ; mn], e)] is the expression
    [m1 m2 ... mn e]. This is not function application, but textual
    concatenation. *)

    val match_ : expr -> branch list -> expr
    (** [E.match e [b1; b2; ... ; bn]] is the expression
    [match e with b1 | b2 | ... | bn]. *)

    val unit : expr
    (** [E.unit] is the expression [()]. *)

    val annot : expr -> type_ -> expr
    (** [E.annot e t] is the expression [(e : t)]. Same as [( ^: )]. *)

    val ( ^: ) : expr -> type_ -> expr
    (** [e ^: t] is the expression [(e : t)]. Same as [annot]. *)

    val li_cons : expr -> expr -> expr
    (** [E.li_cons x xs] is the expression [x :: xs]. *)

    val empty_list : expr
    (** [E.empty_list] is the expression [[]]. *)

    val empty_string : expr
    (** [E.empty_string] is the expression [""]. *)

    val function_ : branch list -> expr
    (** [E.function_ [b1; b2; ... ; bn]] is the expression
    [function b1 | b2 | ... | bn]. *)
  end

  (** This module provides values to build patterns *)
  module P : sig
    val wildcard : pattern
    (** [P.wildcard] is the pattern [_]. *)

    val char : char -> pattern
    (** [P.char 'c'] is the pattern ['c']. *)

    val string : string -> pattern
    (** [P.string "s"] is the pattern ["s"]. *)

    val int : int -> pattern
    (** [PInt i] is the pattern [i]. *)

    val tuple : pattern list -> pattern
    (** [P.tuple [p1; p2; ... ; pn]] is the pattern
      [(p1, p2, ... , pn)]. *)

    val var : string -> pattern
    (** [P.var "ident"] is the pattern binding a single variable named [ident]. *)

    val prim : primitive -> pattern

    val cons : ?payload:pattern list -> string -> pattern
    (** [P.cons ("Cons", [a1; a2; ...; an])] is the pattern
    [Cons(a1, a2, ... , an)]. *)

    val unit : pattern
    (** [P.unit] is the pattern [()]. *)
  end

  module Mixed : sig
    val unit : expr -> mixed
    (** A complete expression, of type unit. *)

    val prim : primitive -> mixed
    (** A textual OCaml node. May be incomplete. *)
  end

  (** This module provides values to build structure items. *)
  module SI : sig
    val def : pattern * expr -> struct_item
    (** [SI.def (p ^= e)] is the structure item [let p = e]. *)

    val module_ : string * module_ -> struct_item
    (** [ SI.module ("MyModule" ^= m)] is the structure item
    [module MyMOdule p = m]. *)
  end

  (** This module provides values to build (the representation of) modules *)
  module M : sig
    val struct_ : struct_ -> module_
    (** A struct. *)

    val alias : string -> module_
    (** An alias for another module. *)

    val field : string list -> module_
    (** A module that is a sub-module of another one. *)
  end

  (** This modules provides values to insert user code in our programs. *)
  module Prim : sig
    val textual : string -> Lexing.position -> Lexing.position -> primitive
    (** A textual representation of the code. *)

    val parsed : Parsetree.expression -> primitive
    (** A parsed AST *)
  end
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
  (** [force_mutual_recursion struct_] is [struct_] with every value being
      mutually recursive with every other value. *)
end

module Primitive : module type of Primitive
