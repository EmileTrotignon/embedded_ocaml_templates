open Ast
open PPrint

let ( @> ) f g x = f (g x)
let ( ^-^ ) a b = a ^^ space ^^ b
let ( ^/^ ) a b = a ^^ hardline ^^ b
let ( ^|^ ) a b = a ^^ break 1 ^^ b
let nest = nest 2
let nest_break i doc = nest (break i ^^ doc)
let nl = hardline
let rarrow = string "->"
let equal = string "="
let pipe = string "|"
let comma_list printer = separate_map (break 0 ^^ comma ^^ space) printer
let dot_list printer = separate_map dot printer
let semicolon_list printer = separate_map (break 0 ^^ semi ^^ space) printer
let space_list printer = separate_map (break 1) printer
let nl_list printer = separate_map hardline printer
let pipe_list printer = separate_map (break 1 ^^ pipe ^^ space) printer
let rarrow_list printer = separate_map (space ^^ rarrow ^^ break 1) printer
let parens doc = lparen ^-^ doc ^-^ rparen

(* let ifdoc b doc = if b then doc else empty *)
let comment doc = !^"(*" ^^ doc ^^ !^"*)"

let position p =
  Lexing.(
    break 0
    ^^ comment
         (OCaml.record "position"
            [ ("pos_fname", OCaml.string p.pos_fname)
            ; ("pos_lnum", OCaml.int p.pos_lnum)
            ; ("pos_bol", OCaml.int p.pos_bol)
            ; ("pos_cnum", OCaml.int p.pos_cnum) ] )
    ^^ !^"\n" ^^ sharp ^-^ OCaml.int p.pos_lnum ^-^ OCaml.string p.pos_fname
    ^^ !^"\n")

let primitive Primitive.{code; startpos; endpos= _} =
  let cnum = startpos.pos_cnum - startpos.pos_bol in
  comment
    ( !^"cnum =" ^-^ OCaml.int cnum ^^ !^" ; pos_cnum="
    ^^ OCaml.int startpos.pos_cnum
    ^^ !^" ; pos_bol=" ^^ OCaml.int startpos.pos_bol )
  ^^ position startpos ^^ repeat cnum space ^^ !^code ^^ nl

let rec pattern p =
  group
    ( match p with
    | PWildcard -> underscore
    | PTuple li -> parens (semicolon_list pattern li)
    | PVar name -> !^name
    | PPrimitive prim -> primitive prim
    | PChar c -> OCaml.char c
    | PString s -> OCaml.string s
    | PInt i -> OCaml.int i )

and def (p, e) =
  nl
  ^^ group (!^"let" ^-^ pattern p ^-^ equal)
  ^^ nest_break 1 (expr e)
  ^|^ string "in"

and mixed = function
  | MiUnit e -> expr e ^-^ semi
  | MiPrimitive p -> primitive p

and type_ t =
  group
    ( match t with
    | TName name -> !^name
    | TModuleField li -> dot_list string li
    | TPrimitive p -> primitive p
    | TApp (li, name) -> parens (comma_list type_ li) ^-^ !^name
    | TArrow (li, final) -> rarrow_list type_ li ^-^ rarrow ^|^ type_ final )

and expr e =
  group
    ( match e with
    | EUnit -> !^"()"
    | ELet (defs, e) -> space_list def defs ^-^ expr e
    | EApp (f, args) -> expr f ^-^ space_list (parens @> expr) args
    | ECons (cons, args) -> !^cons ^-^ parens (comma_list expr args)
    | ETuple exprs -> parens (comma_list expr exprs)
    | ELitList exprs -> brackets (semicolon_list expr exprs)
    | ELitInt i -> PPrint.OCaml.int i
    | EVar name -> !^name
    | EFun (args, body) ->
        group (!^"fun" ^-^ space_list pattern args ^-^ rarrow)
        ^^ nest_break 1 (expr body)
    | ERef e -> !^"ref" ^^ nest_break 1 (expr e)
    | EDeRef e -> !^"!" ^^ expr e
    | EAssignToRef (e, v) -> expr e ^-^ !^":=" ^^ nest_break 1 (expr v)
    | ESequence (li, e) -> semicolon_list expr li ^|^ semi ^-^ expr e
    | EPrimitive prim -> primitive prim
    | EOpenModule (m, e) ->
        string m ^^ dot ^^ lparen ^^ nest_break 0 (expr e) ^|^ rparen
    | EModuleField path -> dot_list string path
    | EMixedSequence (li, e) -> space_list mixed li ^|^ expr e
    | ELitString s -> OCaml.string s
    | EMatch (e, branches) ->
        group (!^"match" ^^ nest_break 1 (expr e) ^|^ !^"with")
        ^|^ pipe_list branch branches
    | EAnnotated (e, t) -> parens (expr e ^-^ colon ^-^ type_ t) )

and branch (p, e) = group (pattern p ^-^ rarrow ^^ nest_break 1 (expr e))
and mutual_def (p, e) = nl ^^ !^"and" ^-^ pattern p ^-^ equal ^|^ expr e

and struct_item = function
  | SIDef (p, e) ->
      !^"let" ^^ nest_break 1 (pattern p) ^|^ equal ^^ nest_break 1 (expr e)
  | SIModule (name, m) ->
      !^"module" ^-^ !^name ^-^ equal ^^ nest_break 1 (module_ m)
  | SIRecDefs defs -> (
    match defs with
    | (p, e) :: defs ->
        group
          (!^{|let[@warning "-39"] rec|} ^^ nest_break 1 (pattern p) ^|^ equal)
        ^^ nest_break 1 (expr e)
        ^|^ concat_map mutual_def defs
    | [] -> assert false )

and module_ = function
  | MStruct s -> struct_ s
  | MAlias a -> !^a
  | MField li -> dot_list string li

and struct_ s = !^"struct" ^/^ nl_list struct_item s ^/^ !^"end"
and program s = nl_list struct_item s

let to_channel f channel args =
  let doc = f args in
  ToChannel.pretty 0.8 80 channel doc

let to_string f arg =
  let doc = f arg in
  let buffer = Buffer.create 10 in
  ToBuffer.pretty 0.8 80 buffer doc ;
  Buffer.contents buffer

let expr_to_string = to_string expr
let program_to_string = to_string program
let print_program = to_channel program
