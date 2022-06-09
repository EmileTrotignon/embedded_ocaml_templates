open Ast
open Primitive

(* -------------------------------------------------------------------------- *)
(* Infix sugar *)

let ( ^-> ) a b = (a, b)

let ( ^^-> ) a b = (a, b)

let ( ^= ) a b = (a, b)

(* -------------------------------------------------------------------------- *)
(* Type builder *)

module T = struct
  let name s = TName s

  let primitive p = TPrimitive p

  let apply li n = TApply (li, n)

  let module_field f = TModuleField f

  let arrow li final = TArrow (li, final)

  let ( ^-> ) t1 t2 = arrow [t1] t2

  let int = name "int"

  let bool = name "bool"

  let string = name "string"

  let char = name "char"

  let unit = name "unit"

  let int64 = name "int64"

  let nativeint = name "nativeint"

  let float = name "float"
end
(* -------------------------------------------------------------------------- *)
(* Expression builders *)

module E = struct
  let if_ cond e1 e2 = EIf (cond, e1, e2)

  let let_ li e = ELet (li, e)

  let leti (li, e) = let_ li e

  let apply f ?(named_args = []) args = EApply (f, named_args, args)

  let cons ?(payload = []) cons = ECons (cons, payload)

  let var name = EVar name

  let tuple li = ETuple li

  let lit_list li = ELitList li

  let lit_string li = ELitString li

  let lit_int i = ELitInt i

  let fun_ (args, body) = if args = [] then body else EFun (args, body)

  let prim p = EPrimitive p

  let ref e = apply (var "ref") [e]

  let deref e = apply (var "!") [e]

  let assign_to_ref r v = apply (var "( := )") [r; v]

  let sequence li e = if li = [] then e else ESequence (li, e)

  let open_module m e = EOpenModule (m, e)

  let module_field path = EModuleField path

  let mixed_seq li e = EMixedSequence (li, e)

  let match_ e li = EMatch (e, li)

  let unit = EUnit

  let annot e t = EAnnotated (e, t)

  let ( ^: ) a b = EAnnotated (a, b)

  let li_cons e1 e2 = cons "(::)" ~payload:[e1; e2]

  let empty_list = lit_list []

  let empty_string = lit_string ""

  let function_ branches =
    fun_
    @@ [PVar "__mocaml_improbable_v"]
    ^^-> match_ (var "__mocaml_improbable_v") branches
end
(* -------------------------------------------------------------------------- *)
(* Pattern builders *)

module P = struct
  let wildcard = PWildcard

  let char c = PChar c

  let string s = PString s

  let int i = PInt i

  let tuple li = PTuple li

  let var s = PVar s

  let prim p = PPrimitive p

  let cons ?(payload = []) s = PCons (s, payload)

  let unit = cons "()"
end
(* -------------------------------------------------------------------------- *)
(* mixed builders *)

module Mixed = struct
  let unit e = MiUnit e

  let prim s = MiPrimitive s
end

(* -------------------------------------------------------------------------- *)
(* struct item builders *)

module SI = struct
  let def (pattern, expr) = SIDef (pattern, expr)

  let module_ (name, module_) = SIModule (name, module_)
end
(* -------------------------------------------------------------------------- *)
(* modules builders *)

module M = struct
  let struct_ s = MStruct s

  let alias n = MAlias n

  let field li = MField li
end

(* -------------------------------------------------------------------------- *)
(* primitives *)

module Prim = struct
  let textual code startpos endpos = Textual {code; startpos; endpos}

  let parsed expr = Parsed expr
end