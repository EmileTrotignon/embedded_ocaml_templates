module Prim = Mocaml.Primitive

type elt =
  | Text of string
  | Code of Prim.t
  | Output of {code: Prim.t; escape: bool; format: string option}

type t = Prim.t * elt list
type tag_options = {slurp_before: bool; slurp_after: bool}

type tag =
  | Code of Prim.t
  | Output of {code: Prim.t; escape: bool; format: string option}

type elt' = Text of string | Whitespace of string | Tag of tag_options * tag
type t' = Prim.t * elt' list

val elt_of_tag : tag -> elt
val t_of_t' : 'a * elt' list -> 'a * elt list
val text : string -> elt
val text' : string -> elt'
val code : Prim.t -> elt
val code' : Prim.t -> tag
val tag : bool -> tag -> bool -> elt'
val output : ?escape:bool -> ?format:string -> Prim.t -> elt
val output' : ?escape:bool -> ?format:string -> Prim.t -> tag
