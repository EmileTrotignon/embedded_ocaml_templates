type primitive = Primitive.t

(** represent patterns *)
type pattern =
  | PWildcard  (** [PWildcard] is the pattern [_]. *)
  | PChar of char  (** [PChar 'c'] is the pattern ['c']. *)
  | PString of string  (** [PString "s"] is the pattern ["s"]. *)
  | PInt of int  (** [PInt i] is the pattern [i]. *)
  | PTuple of pattern list
      (** [PTuple [p1; p2; ... ; pn]] is the pattern
      [(p1, p2, ... , pn)]. *)
  | PVar of string
      (** [PVar "ident"] is the pattern binding a single variable named [ident]. *)
  | PCons of string * pattern list
      (** [PCons ("Cons", [a1; a2; ...; an])] is the pattern
      [Cons(a1, a2, ... , an)]. *)
  | PPrimitive of primitive  (** [PPrimitive prim] is the pattern [prim]. *)

type type_ =
  | TName of string  (** [TName "ident"] is the type of name [ident]. *)
  | TPrimitive of primitive  (** [TPrimitive prim] is the type [prim]. *)
  | TApply of type_ list * string
      (** [TApply ([a1; a2; ... ; an], "name")] is the type
      [(a1, a2, ... , an) name]. *)
  | TModuleField of string list
      (** [TModuleField (["Module1"; "Module2"; ... ; "Modulen"; "ident"])] is
      the type [Module1.Module2...Modulen.ident]. *)
  | TArrow of type_ list * type_
      (** [TArrow ([t1; t2; ... ; tn], t)] is the type
      [t1 -> t2 -> ... -> tn -> t]. *)

type expr =
  | EIf of expr * expr * expr
      (** [EIf (cond, e1, e2)] is the expression [if cond then e1 else e2]. *)
  | ELet of (pattern * expr) list * expr
      (** [ELet ([(p1, e1); ... ; (pn, en)], e)] is the expression
      [let p1 = e1 in ... let pn = en in e]. *)
  | EApply of expr * (string * expr) list * expr list
      (** [EApply (func, [("lab1", el1); ...; ("labn", eln)], [e1; ... ; en]))]
      is the expression [func ~lab1:el1 ... ~labn:eln e1 ... en]. *)
  | ECons of string * expr list
      (** [ECons ("Cons", [e1; e2; ...; en])] is the expression
      [Cons (e1, e2, ... , en)]. *)
  | EVar of string  (** [EVar "ident"] is the expression [ident]. *)
  | ETuple of expr list
      (** [ETuple [e1; e2; ... ; en]] is the expression [(e1, e2, ... , en)]. *)
  | ELitList of expr list
      (** [ELitList [e1; e2; ... ; en]] is the expression [[e1; e2; ... ; en]]. *)
  | ELitInt of int  (** [ELitInt i] is the expression [i]. *)
  | ELitString of string
      (** [ELitString "some text"] is the expression ["some text"]. *)
  | EFun of pattern list * expr
      (** [EFun([p1; ...; pn], body)] is the expression [fun p1 ... pn -> body]. *)
  | EPrimitive of primitive  (** [EPrim prim] is the expression [prim]. *)
  | ESequence of expr list * expr
      (** [ESequence ([e1; e2; ... ; en], e)] is the expression
      [e1; e2; ... ; en ; e]. *)
  | EOpenModule of string * expr
      (** [EOpenModule ("Module", e)] is the expression [Module.(e)] *)
  | EModuleField of string list
      (** [EModuleField (["Module1"; ... ; "Modulen" ; "ident"])] is the
      expression [Module1...Modulen.ident] *)
  | EMixedSequence of mixed list * expr
      (** [EMixedSequence ([m1; m2; ... ; mn], e)] is the expression
      [m1 m2 ... mn e]. This is not function application, but textual
      concatenation. *)
  | EMatch of expr * branch list
      (** [EMatch(e, [b1; b2; ... ; bn])] is the expression
      [match e with b1 | b2 | ... | bn] *)
  | EUnit  (** [EUnit] is the expression [()] *)
  | EAnnotated of expr * type_
      (** [EAnnotated (e, t)] is the expression [(e : t)] *)

and branch = pattern * expr

and mixed = MiUnit of expr | MiPrimitive of primitive

type struct_item =
  | SIDef of pattern * expr
  | SIModule of string * module_
  | SIRecDefs of (pattern * expr) list

and struct_ = struct_item list

and module_ = MStruct of struct_ | MAlias of string | MField of string list
