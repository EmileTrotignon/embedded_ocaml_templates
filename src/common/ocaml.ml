type pattern =
  | PWildcard
  | PTuple of pattern list
  | PVar of string
  | PPrimitive of string

type expr =
  | ELet of (pattern * expr) list * expr
  | EApp of expr * expr list
  | ECons of string * expr list
  | EVar of string
  | ETuple of expr list
  | ELitList of expr list
  | EFun of pattern list * expr
  | EPrimitive of string
  | ERef of expr
  | EDeRef of expr
  | EAssignToRef of expr * expr
  | ESequence of expr list * expr
  | EOpenModule of string * expr
  | EModuleField of string list
  | EMixedSequence of mixed list * expr
  | ELitString of string
  | EUnit

and mixed = MiUnit of expr | MiPrimitive of string

type struct_item =
  | SIDef of pattern * expr
  | SIModule of string * module_
  | SIRecDefs of (pattern * expr) list

and struct_ = struct_item list

and module_ = MStruct of struct_ | MAlias of string | MField of string list

let force_mutual_recursion struct_ =
  let aux (functions, others) = function
    | SIDef (pattern, expr) -> ((pattern, expr) :: functions, others)
    | SIRecDefs functions' -> (functions' @ functions, others)
    | other -> (functions, other :: others) in
  let functions, others = List.fold_left aux ([], []) struct_ in
  others @ [SIRecDefs functions]
