open Ast
open Primitive

(* -------------------------------------------------------------------------- *)
(* Infix sugar *)

let ( ^-> ) a b = (a, b)
let ( ^= ) a b = (a, b)

(* -------------------------------------------------------------------------- *)
(* Type builder *)
(*
type type_ =
  | TName of string
  | TPrimitive of primitive
  | TApp of string list * string
  | TModuleField of string list*)

let t_name s = TName s
let t_primitive p = TPrimitive p
let t_app li n = TApp (li, n)
let t_module_field f = TModuleField f
let t_arrow (li, final) = TArrow (li, final)

(* -------------------------------------------------------------------------- *)
(* Expression builders *)
let e_let li e = ELet (li, e)
let e_leti (li, e) = e_let li e
let e_app f args = EApp (f, args)
let e_cons cons li = ECons (cons, li)
let e_var name = EVar name
let e_tuple li = ETuple li
let e_lit_list li = ELitList li
let e_lit_string li = ELitString li
let e_lit_int i = ELitInt i
let e_fun (args, body) = EFun (args, body)
let e_prim p = EPrimitive p
let e_ref e = ERef e
let e_deref e = EDeRef e
let e_assign_to_ref r v = EAssignToRef (r, v)
let e_sequence li e = ESequence (li, e)
let e_open_module m e = EOpenModule (m, e)
let e_module_field path = EModuleField path
let e_mixed_seq li e = EMixedSequence (li, e)
let e_match e li = EMatch (e, li)
let e_unit = EUnit
let e_annot e t = EAnnotated (e, t)
let ( ^: ) a b = EAnnotated (a, b)
let e_li_cons e1 e2 = e_cons "(::)" [e1; e2]
let e_empty_list = e_lit_list []
let e_empty_string = e_lit_string ""

let e_function branches =
  e_fun
  @@ [PVar "__eml_improbable_v"]
  ^-> e_match (e_var "__eml_improbable_v") branches

(* -------------------------------------------------------------------------- *)
(* Pattern builders *)

let p_wildcard = PWildcard
let p_char c = PChar c
let p_string s = PString s
let p_int i = PInt i
let p_tuple li = PTuple li
let p_var s = PVar s
let p_prim p = PPrimitive p

(* -------------------------------------------------------------------------- *)
(* mixed builders *)

let mix_unit e = MiUnit e
let mix_prim s = MiPrimitive s

(* -------------------------------------------------------------------------- *)
(* struct item builders *)

let si_def (pattern, expr) = SIDef (pattern, expr)
let si_module (name, modul) = SIModule (name, modul)

(* -------------------------------------------------------------------------- *)
(* modules builders *)

let m_struct s = MStruct s
let m_alias n = MAlias n
let m_field li = MField li

(* -------------------------------------------------------------------------- *)
(* primitives *)
let primitive code startpos endpos = {code; startpos; endpos}
